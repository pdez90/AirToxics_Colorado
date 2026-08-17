# ==============================================================
# 00_run_all.R  -- source every section script in order
# Auto-generated from Suncor.Rmd
# ==============================================================

scripts <- c(
  "01_libraries.R",
  "02_newmobile_data.R",
  "03_checks_flags.R",
  "04_stats_hour_of_day_weekend_weekday.R",
  "05_wind_speed_and_direction.R",
  "06_merge_with_wind.R",
  "07_timeplot_timevariation_ggally_correlation_plots.R",
  "08_polarplot_maps.R",
  "09_creating_figures.R",
  "10_calculating_background_air_pollution_concentrations_rolling_.R",
  "11_correcting_for_background.R",
  "12_finding_500_meter_road_segment.R",
  "13_suncor_terminal_calculating_aggregate_stats_for_each_500_m_s.R",
  "14_aggregate_stats_for_each_500_m_segment.R",
  "15_maps_for_500_m_segment.R",
  "16_500_m_plotting_ratios.R",
  "17_suncor_terminal_calculating_scaling_factors_to_calculate_dai.R",
  "18_suncor_terminal_calculating_census_block_level_stats.R",
  "19_plot_maps_census_blocks.R",
  "20_census_block_level_health_risks.R",
  "21_road_network_plot.R",
  "22_distance_decay_function.R",
  "23_tri.R",
  "24_join_with_tri.R",
  "25_tri_buffers_with_different_distances.R",
  "26_hotspot_rotated_wind_source_probability_profiles.R",
  "27_hotspot_sensitivity_sensitivity.R",
  "28_hotspot_analysis_identifying_most_persistent_hotspots.R",
  "29_add_on_identifying_hotspots_across_pollutants.R",
  "30_multiple_pollutant_hotspots.R",
  "31_plotting.R",
  "32_identifying_close_to_tri.R",
  "33_final_hotspot_plots.R",
  "34_fancy_plots_of_hotspots.R",
  "35_hotspot_figure_2.R",
  "36_hysplit_of_lowest_trimethylbenzene_benzene_ratios.R",
  "37_creating_gif_claude_figure_s1_1.R",
  "38_download_roads.R",
  "39_la_casa.R",
  "40_alert.R"
)

for (s in scripts) {
  message("==== Running: ", s, " ====")
  source(s, echo = TRUE, max.deparse.length = Inf)
}
