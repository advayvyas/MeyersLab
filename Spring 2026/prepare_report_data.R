# prepare_report_data.R
# Run once from the project root before knitting any report.
# Saves report_data_cache.rds to the same folder.
# Usage: source("prepare_report_data.R")  OR  Rscript prepare_report_data.R

library(dplyr)
library(tidyverse)
library(sf)
source("metric_functions.R")
source("graphstat_functions.R")
source("write_dtw.R")
source("write_prob.R")

# ── Raw data ──────────────────────────────────────────────────────────────────
message("Loading raw data...")
hsa_state   = read.csv("hsa_state_inc.csv")
all_metrics = read_csv("hsa_vs_state_metrics.csv")
us_map      = readRDS("us_map_pop_sf.rds")

lower48 = setdiff(state.name, c("Alaska", "Hawaii"))

# ── Window definitions ────────────────────────────────────────────────────────
windows = list(
  w1 = c(1,   25),
  w2 = c(51,  100),
  w3 = c(101, Inf)
)

hsa_unique = hsa_state %>% distinct(hsa_nci_id, state)
hsa_list   = hsa_unique$hsa_nci_id

# ── DTW metrics by window ─────────────────────────────────────────────────────
message("Computing DTW metrics (this may take a while)...")

res     = vector("list", length(hsa_list) * length(windows))
counter = 1

for (ID in hsa_list) {
  for (w in names(windows)) {
    bounds = windows[[w]]
    out    = pivot_wider(
      dtw_stats(ID, start = bounds[1], end = bounds[2]),
      names_from  = Metric,
      values_from = Value
    )
    out$ID     = ID
    out$window = w
    res[[counter]] = out
    counter = counter + 1
  }
}

dtw_results_window = do.call(rbind, res) %>%
  mutate(peak_window = as.numeric(sub("w", "", window))) %>%
  select(-window)

# ── Soft-DTW / probabilistic metrics by window ────────────────────────────────
message("Computing soft-DTW metrics by window (this may take a while)...")

prob_res     = vector("list", length(hsa_list) * length(windows))
prob_counter = 1

for (ID in hsa_list) {
  for (w in names(windows)) {
    bounds = windows[[w]]
    
    out = tryCatch(
      pivot_wider(
        prob_stats(hsa_id = ID, start = bounds[1], end = bounds[2]),
        names_from  = Metric,
        values_from = Value
      ),
      error = function(e) {
        message(sprintf("  prob_stats failed: HSA %s window %s — %s", ID, w, e$message))
        NULL
      }
    )
    
    if (!is.null(out)) {
      out$ID     = ID
      out$window = w
      prob_res[[prob_counter]] = out
    }
    
    prob_counter = prob_counter + 1
  }
}

prob_results_window = do.call(rbind, Filter(Negate(is.null), prob_res)) %>%
  mutate(peak_window = as.numeric(sub("w", "", window))) %>%
  select(-window)

# ── Ground truth peaks ────────────────────────────────────────────────────────
message("Computing ground truth peaks...")

peak_centroid = function(x, weeks, q = 0.80) {
  threshold = quantile(x, q, na.rm = TRUE)
  mask      = x >= threshold & !is.na(x)
  if (sum(mask) == 0) return(NA_real_)
  weighted.mean(weeks[mask], x[mask])
}

all_metrics_modified = all_metrics %>%
  mutate(
    week_index  = epi_week + (epi_year - 2022) * 52 - 38,
    peak_window = case_when(
      week_index <= 50  ~ 1,
      week_index <= 100 ~ 2,
      TRUE              ~ 3
    )
  )

truth_peaks = all_metrics_modified %>%
  group_by(ID, state, peak_window) %>%
  summarise(
    hsa_peak_week          = week_index[which.max(inc_hsa)],
    hsa_peak_value         = max(inc_hsa, na.rm = TRUE),
    hsa_centroid           = peak_centroid(inc_hsa, week_index),
    state_peak_week        = week_index[which.max(inc_state)],
    state_peak_value       = max(inc_state, na.rm = TRUE),
    state_centroid         = peak_centroid(inc_state, week_index),
    truth_time_diff        = hsa_peak_week - state_peak_week,
    truth_time_diff_abs    = abs(truth_time_diff),
    centroid_time_diff     = hsa_centroid - state_centroid,
    centroid_time_diff_abs = abs(centroid_time_diff),
    truth_mag_ratio        = hsa_peak_value / state_peak_value,
    .groups = "drop"
  )

# ── Join and unified filter ───────────────────────────────────────────────────
message("Joining and filtering...")

diff_peak_data = truth_peaks %>%
  left_join(dtw_results_window,  by = c("ID", "peak_window")) %>%
  left_join(prob_results_window, by = c("ID", "peak_window"))

diff_peak_filtered = diff_peak_data %>%
  filter(
    !is.na(signed_lag),
    !is.na(centroid_time_diff),
    signed_lag         >= -10, signed_lag         <= 10,
    centroid_time_diff >= -10, centroid_time_diff <= 10,
    state_peak_value   >  0,
    hsa_peak_value     >  0,
    frechet            <  25,
    truth_mag_ratio    >  0.2,
    truth_mag_ratio    <  5
  )

message(sprintf("Rows before filter: %d | after: %d", nrow(diff_peak_data), nrow(diff_peak_filtered)))

# ── Save ──────────────────────────────────────────────────────────────────────
message("Saving cache...")

saveRDS(
  list(
    dtw_results_window   = dtw_results_window,
    prob_results_window  = prob_results_window,
    all_metrics_modified = all_metrics_modified,
    truth_peaks          = truth_peaks,
    diff_peak_data       = diff_peak_data,
    diff_peak_filtered   = diff_peak_filtered,
    us_map               = us_map
  ),
  "report_data_cache.rds"
)

message("Done. Cache saved to report_data_cache.rds")