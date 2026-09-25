# ==============================================================
# P03  Code to download HRRR
# Auto-split from Suncor_plume.Rmd  (section 3 of 10)
# ==============================================================

#Code to download HRRR

run_hrrr_uv_pbl_clouds_on_df_fast <- function(df,
                                              time_col = "datetime",
                                              lat_col  = "lat",
                                              lon_col  = "lon",
                                              fxx      = 0L,
                                              parallel = FALSE) {
  stopifnot(is.data.frame(df))
  need <- c(time_col, lat_col, lon_col)
  if (!all(need %in% names(df))) stop("Input df must contain: ", paste(need, collapse=", "))

  df$.row_id__ <- seq_len(nrow(df))
  df[[time_col]] <- as.character(df[[time_col]])
  df[[lat_col]]  <- suppressWarnings(as.numeric(df[[lat_col]]))
  df[[lon_col]]  <- suppressWarnings(as.numeric(df[[lon_col]]))

  groups <- split(df, df[[time_col]], drop = TRUE)
  jobs <- lapply(groups, function(g) {
    list(
      dt      = unique(g[[time_col]])[1],
      lats    = as.numeric(g[[lat_col]]),
      lons    = as.numeric(g[[lon_col]]),
      row_ids = g$.row_id__
    )
  })

  # Ensure the Python fast helper exists in THIS process (main or worker)
  ensure_py_fast_helper <- function() {
    main <- reticulate::import_main(convert = TRUE)
    if (!reticulate::py_has_attr(main, "hrrr_fetch_uv_pbl_clouds_batch_fast")) {
      pycode <- "
from herbie import Herbie
import numpy as np
from collections import OrderedDict

_DS_CACHE = OrderedDict()
_DS_CACHE_MAX = 24
def _cache_key(dt_str, product, fxx, pattern):
    return (str(dt_str), str(product), int(fxx), str(pattern or ''))
def _cache_put(key, value):
    if key in _DS_CACHE: _DS_CACHE.move_to_end(key)
    _DS_CACHE[key] = value
    while len(_DS_CACHE) > _DS_CACHE_MAX:
        _DS_CACHE.popitem(last=False)
def _cache_get(key):
    if key in _DS_CACHE:
        _DS_CACHE.move_to_end(key); return _DS_CACHE[key]
    return None

def _grid_arrays(d):
    lat_name = next((c for c in ('latitude','lat','gridlat','gridlat_0') if c in d), None)
    lon_name = next((c for c in ('longitude','lon','gridlon','gridlon_0') if c in d), None)
    if lat_name is None or lon_name is None: return None
    la2 = np.asarray(d[lat_name].values); lo2 = np.asarray(d[lon_name].values)
    dims = d[lat_name].dims
    if la2.ndim != 2 or len(dims) != 2: return None
    return la2, lo2, dims

def _nearest_ij_batch(d, lats, lons):
    # SPEED FIX (2026-09-24). The previous _nearest_ij scanned the FULL HRRR
    # grid (1059 x 1799 = 1.9M cells) once PER POINT and PER CUBE: with 5
    # cubes and ~2,390 points per hour-group that is ~12,000 full-grid scans,
    # ~3 min per group, i.e. ~62 h for the campaign. Every query point here
    # lies inside a box a few tens of km across, so crop the grid to the
    # points bounding box plus a 0.5 deg margin - far larger than the 3 km
    # grid spacing, so the true nearest cell is always inside the crop - and
    # search only that. Same nearest cell, same numbers, ~1000x less work.
    g = _grid_arrays(d)
    n = len(lats)
    if g is None: return [None]*n
    la2, lo2, dims = g
    qla = np.asarray(lats, dtype=float); qlo = np.asarray(lons, dtype=float)
    lo_min = np.nanmin(lo2); lo_max = np.nanmax(lo2)
    if lo_min >= -180.0 and lo_max <= 180.0:
        qlo = ((qlo + 180.0) % 360.0) - 180.0
    else:
        qlo = qlo % 360.0
    M = 0.5
    ok = ((la2 >= np.nanmin(qla) - M) & (la2 <= np.nanmax(qla) + M) &
          (lo2 >= np.nanmin(qlo) - M) & (lo2 <= np.nanmax(qlo) + M))
    if not ok.any():
        ok = np.ones(la2.shape, dtype=bool)
    iy, ix = np.nonzero(ok)
    cla = la2[iy, ix]; clo = lo2[iy, ix]
    out = []
    for k in range(n):
        dd = (cla - qla[k])**2 + (clo - qlo[k])**2
        if np.all(np.isnan(dd)):
            out.append(None); continue
        j = int(np.nanargmin(dd))
        out.append({dims[0]: int(iy[j]), dims[1]: int(ix[j])})
    return out

def _nearest_ij(d, lat, lon):
    r = _nearest_ij_batch(d, [lat], [lon])
    return r[0] if r else None

def _reduce_to_scalar(da):
    try:
        da = da.squeeze(drop=True)
        if getattr(da, 'ndim', None) and da.ndim > 0:
            da = da.isel(**{d:0 for d in da.dims})
        try:
            val = da.values.item()
        except Exception:
            try: val = float(da.item())
            except Exception: return np.nan
        try: return float(val)
        except Exception: return np.nan
    except Exception:
        return np.nan

def _get_first(pt, names):
    for nm in names:
        if nm in pt.data_vars: return _reduce_to_scalar(pt[nm])
        locase = [k for k in pt.data_vars if k.lower() == nm.lower()]
        if locase: return _reduce_to_scalar(pt[locase[0]])
    return np.nan

def _open_cubes(dt, product, fxx, pattern=None):
    key = _cache_key(dt, product, fxx, pattern)
    cached = _cache_get(key)
    if cached is not None: return cached
    try:
        H = Herbie(dt, model='hrrr', product=product, fxx=int(fxx))
    except Exception:
        _cache_put(key, []); return []
    try:
        ds = H.xarray(pattern) if pattern else H.xarray()
    except Exception:
        _cache_put(key, []); return []
    cubes = ds if isinstance(ds, list) else [ds]
    _cache_put(key, cubes)
    return cubes

def hrrr_fetch_uv_pbl_clouds_batch_fast(dt, lats, lons, fxx=0, model='hrrr'):
    if isinstance(dt, (list, tuple)) and len(dt)==1: dt = dt[0]
    if isinstance(lats, (float, int)): lats = [lats]
    if isinstance(lons, (float, int)): lons = [lons]
    patt = 'u10|v10|tcc|lcc|blh|UGRD:10 m|VGRD:10 m|TCDC|LCDC|HPBL|ugrd|vgrd'
    cubes_sfc = _open_cubes(dt, 'sfc', fxx, patt)
    have_blh_sfc = any(any(var in d.data_vars for var in ('blh','HPBL')) for d in cubes_sfc)
    cubes_nat = _open_cubes(dt, 'nat', fxx, 'blh|HPBL') if not have_blh_sfc else []
    cubes_prs = _open_cubes(dt, 'prs', fxx, 'blh|HPBL') if (not have_blh_sfc and not cubes_nat) else []

    def precompute_ij_list(cubes, lats, lons):
        return [_nearest_ij_batch(d, lats, lons) for d in cubes]
    ij_sfc = precompute_ij_list(cubes_sfc, lats, lons)
    ij_nat = precompute_ij_list(cubes_nat, lats, lons) if cubes_nat else []
    ij_prs = precompute_ij_list(cubes_prs, lats, lons) if cubes_prs else []

    rows = []
    for i,(la,lo) in enumerate(zip(lats,lons)):
        rec = {'row_id': i, 'datetime': str(dt), 'lat': float(la), 'lon': float(lo),
               'u10': np.nan, 'v10': np.nan, 'lcc': np.nan, 'tcdc': np.nan, 'blh': np.nan}
        for d,idxs in zip(cubes_sfc, ij_sfc):
            ij = idxs[i]
            if ij is None:
                try: pt = d.sel(latitude=la, longitude=lo, method='nearest')
                except Exception: continue
            else:
                try: pt = d.isel(**ij)
                except Exception: continue
            if np.isnan(rec['u10']):  rec['u10']  = _get_first(pt, ['u10','UGRD:10 m','ugrd'])
            if np.isnan(rec['v10']):  rec['v10']  = _get_first(pt, ['v10','VGRD:10 m','vgrd'])
            if np.isnan(rec['lcc']):  rec['lcc']  = _get_first(pt, ['lcc','LCDC'])
            if np.isnan(rec['tcdc']): rec['tcdc'] = _get_first(pt, ['tcc','TCDC'])
            if np.isnan(rec['blh']):  rec['blh']  = _get_first(pt, ['blh','HPBL'])
            if all(np.isfinite([rec['u10'], rec['v10'], rec['lcc'], rec['tcdc'], rec['blh']])): break
        if np.isnan(rec['blh']):
            for d,idxs in list(zip(cubes_nat, ij_nat)) + list(zip(cubes_prs, ij_prs)):
                ij = idxs[i]
                if ij is None:
                    try: pt = d.sel(latitude=la, longitude=lo, method='nearest')
                    except Exception: continue
                else:
                    try: pt = d.isel(**ij)
                    except Exception: continue
                rec['blh'] = _get_first(pt, ['blh','HPBL'])
                if np.isfinite(rec['blh']): break
        rows.append(rec)
    return rows
"
      reticulate::py_run_string(pycode)  # <<< EXECUTE the Python definition
    }
    invisible(TRUE)
  }

worker_fetch <- function(job, fxx_int) {
  ensure_py_fast_helper()  # make sure helper exists in this process

  main <- reticulate::import_main(convert = FALSE)  # <-- keep Python object
  py_obj <- main$hrrr_fetch_uv_pbl_clouds_batch_fast(job$dt, job$lats, job$lons, as.integer(fxx_int))

  # Robust conversion from Python -> R
  rows <- reticulate::py_to_r(py_obj)

  # Build tibble defensively
  if (is.data.frame(rows)) {
    tib <- tibble::as_tibble(rows)
  } else if (is.list(rows) && length(rows) > 0 && all(vapply(rows, is.list, TRUE))) {
    # list of dicts -> bind rows
    tmp <- lapply(rows, function(x) {
      # ensure named list -> data.frame
      if (is.null(names(x)) || any(names(x) == "")) {
        # try to pull names from the first dict in the list
        nms <- unique(unlist(lapply(rows, names)))
        # align keys
        x2 <- setNames(vector("list", length(nms)), nms)
        for (nm in names(x)) x2[[nm]] <- x[[nm]]
        as.data.frame(x2, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
      }
    })
    tib <- dplyr::bind_rows(tmp)
  } else {
    # Fallback: make an empty tibble with expected columns
    tib <- tibble::tibble(
      row_id = integer(), datetime = character(), lat = numeric(), lon = numeric(),
      u10 = numeric(), v10 = numeric(), lcc = numeric(), tcdc = numeric(), blh = numeric()
    )
  }

  # Rename ECMWF-style -> requested names
  if ("tcc" %in% names(tib)) names(tib)[names(tib) == "tcc"] <- "tcdc"
  if ("blh" %in% names(tib)) names(tib)[names(tib) == "blh"] <- "hpbl"

  # Attach back the row ids (preserves input order in join)
  tib$.row_id__ <- job$row_ids
  tib
}


  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("parallel=TRUE requested but {future.apply} not installed; running sequentially.")
      res_list <- lapply(jobs, worker_fetch, fxx_int = fxx)
    } else {
      res_list <- future.apply::future_lapply(
        jobs,
        FUN = function(job) worker_fetch(job, fxx),
        future.globals  = FALSE,
        future.packages = c("reticulate", "tibble")
      )
    }
  } else {
    # PROGRESS (2026-09-23): the sequential path used to run silently, which
    # is indistinguishable from the fork deadlock this replaced. Print every
    # 25 hour-groups with an ETA so a long fetch can be told from a hung one.
    .n <- length(jobs); .t0 <- Sys.time()
    res_list <- lapply(seq_along(jobs), function(i) {
      r <- worker_fetch(jobs[[i]], fxx_int = fxx)
      if (i %% 25 == 0 || i == .n) {
        .el <- as.numeric(difftime(Sys.time(), .t0, units = "mins"))
        message(sprintf("[HRRR] %d/%d hour-groups (%.0f%%) - %.1f min elapsed, ~%.0f min left",
                        i, .n, 100 * i / .n, .el, .el / i * (.n - i)))
      }
      r
    })
  }

  res <- dplyr::bind_rows(res_list)

  out <- df |>
    dplyr::left_join(
      res |> dplyr::select(.row_id__, u10, v10, hpbl, lcc, tcdc),
      by = ".row_id__"
    ) |>
    dplyr::select(-.row_id__)

  to_num <- function(x) suppressWarnings(as.numeric(x))
  for (nm in intersect(c("u10","v10","hpbl","lcc","tcdc"), names(out))) {
    out[[nm]] <- to_num(out[[nm]])
  }
  out
}

res_test <- run_hrrr_uv_pbl_clouds_on_df_fast(
  tibble::tibble(datetime = "2024-08-01 12:00", lat = 39.7392, lon = -104.9903),
  parallel = FALSE
)
print(res_test)

#diagnose_hrrr_vars("2024-08-01 12:00", 39.7392, -104.9903)
