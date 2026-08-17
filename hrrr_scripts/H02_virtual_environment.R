# ==============================================================
# H02  Virtual environment
# Auto-split from Suncor_plume2.Rmd  (section 2 of 4)
# ==============================================================

#Virtual environment

virtualenv_create("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)
py_install("pandas", envname = "r-reticulate")
reticulate::py_install("pyreadr")
reticulate::py_install("xarray")
reticulate::py_install("pyarrow")
reticulate::py_install("herbie-data")
reticulate::py_install("cfgrib")
reticulate::py_install("pygrib")
reticulate::py_install("netcdf4")
#use_virtualenv("r-reticulate", required = TRUE)
pd <- import("pandas")
pyreadr<-import("pyreadr")
xarray<-import("xarray")
pyarrow<-import("pyarrow")
herbie<-import("herbie")
cfgrib<-import("cfgrib")
pygrib<-import("pygrib")
#netcdf4<-import("netcdf4")
