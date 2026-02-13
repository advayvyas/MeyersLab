write_dtw = function() {
  hsa_unique = hsa_state %>%
    distinct(hsa_nci_id, state)
  
  state_sizes = hsa_unique %>%
    count(state, name = "N_state") 
  
  hsa_list = list()
  for (state_name in unique(hsa_state$state)) {
    state_subset = hsa_unique %>% filter(state == state_name)
    state_temp = sample(state_subset$hsa_nci_id, state_sizes[state_sizes$state == state_name, "N_state"])
    hsa_list = append(hsa_list, state_temp)
  }
  
  res = vector("list", length(hsa_list))
  
  for (i in seq_along(hsa_list)) {
    ID = hsa_list[[i]]
    
    out = pivot_wider(as.data.frame(dtw_stats(hsa_id = ID)), names_from  = Metric, values_from = Value)
    
    out$ID = ID
    out = out[, c("ID", setdiff(names(out), "ID"))]
    
    res[[i]] = out
  }
  
  dtw_results = do.call(rbind, res)
  
  write_csv(dtw_results, "../all_DTW_results.csv")
  return(dtw_results)
}
