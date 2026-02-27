graph_hsa = function(hsa_id = 688) {
  hsa = hsa_state[hsa_state$hsa_nci_id == hsa_id, ]
  
  P = as.matrix(hsa$inc_hsa)
  Q = as.matrix(hsa$inc_state)
  
  res_dtw = dtw_distance(P, Q)
  
  res = discrete_frechet(P, Q)
  
  normal = ggplot(hsa) + geom_line(linewidth = 1, (aes(x = seq_len(148),
                                                       y = inc_hsa, color = "HSA"))) + geom_line(linewidth = 1,
                                                                                                 (aes(x = seq_len(148), y = inc_state, color = "State"))) +
    labs(x = "index (weeks, 2022 Week 39 to 2025 Week 30)", y = "infected_value (?)",
         title = paste0("Local vs. state inc values for HSA ID ", hsa_id, " from ", hsa$state)) +
    theme_minimal() + scale_color_manual(name = "Legend", values = c(HSA = "blue",
                                                                     State = "red"))
  
  frechet = ggplot() + geom_line(aes(x=seq_len(length(res$frechet_by_step)), y=res$frechet_by_step), linewidth = 1) + 
    theme_minimal() + labs(x="Steps", y="Frechet distance", title = "Frechet distance by step")
  
  dtw = ggplot() + geom_line(aes(x = seq_len(length(res_dtw$cum_cost)), y = res_dtw$cum_cost), linewidth = 1) + 
    theme_minimal() + labs(x = "Steps", y = "Cumulative DTW cost", title = "DTW cumulative cost along optimal path by step")
  
  # dtw adjusted
  P_warped = rep(NA, length(Q))
  i = res_dtw$path[,1]
  j = res_dtw$path[,2]
  P_warped[j] = P[i]
  #P_warped = na.approx(P_warped, rule = 2)
  
  df_plot = data.frame(
    index = seq_len(length(Q)),
    HSA_aligned = P_warped,
    State = Q
  )
  
  warped = ggplot(df_plot) +
    geom_line(aes(x = index, y = HSA_aligned, color = "HSA (warped)"), linewidth = 1) +
    geom_line(aes(x = index, y = State, color = "State"), linewidth = 1) +
    labs(
      x = "index (weeks, 2022 Week 39 to 2025 Week 30)",
      y = "% of emergency department hospitalizations",
      title = paste0("DTW-aligned Local vs. state inc values for HSA ID ", hsa_id, " from ", hsa$state)
    ) +
    theme_minimal() +
    scale_color_manual(name = "Legend", values = c("HSA (warped)" = "blue", State = "red"))
  
  (normal / warped) / (frechet + dtw)
}

dtw_stats = function(hsa_id = 688, start = 1, end = Inf) {
  hsa = hsa_state[hsa_state$hsa_nci_id == hsa_id, ]
  
  n = nrow(hsa)
  end = min(end, n)
  hsa = hsa[start:end, ]
  
  P = as.matrix(hsa$inc_hsa)
  Q = as.matrix(hsa$inc_state)
  
  res_dtw = dtw_distance(P, Q)
  
  res = discrete_frechet(P, Q)
  
  i = res_dtw$path[,1]
  j = res_dtw$path[,2]
  
  delta_t = j - i
  delta_x = P[i,1] - Q[j,1]
  
  stats_list = list(
    mean_abs_lag = mean(abs(delta_t)),
    signed_lag   = mean(delta_t),
    median_lag = median(delta_t),
    lag_sd       = sd(delta_t),
    dtw_mae      = mean(abs(delta_x)),
    dtw_rmse     = sqrt(mean(delta_x^2)),
    rmse = sqrt(mean((P[,1] - Q[,1])^2)),
    amp_ratio    = median(P[,1] / Q[,1], na.rm=TRUE),
    dtw_amp_ratio    = median(P[i,1] / Q[j,1], na.rm=TRUE),
    lag_amp_corr = cor(abs(delta_t), abs(delta_x)),
    frechet = res$frechet
  )
  
  stats = data.frame(
    Metric = names(stats_list),
    Value  = unname(unlist(stats_list))
  )
  
  return(stats)
}