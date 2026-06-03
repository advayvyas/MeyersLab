safe_softmin_probs = function(x, tau = 0.1) {
  
  x = as.numeric(x)
  
  # replace non-finite values
  x[!is.finite(x)] = max(x[is.finite(x)], na.rm = TRUE)
  
  # fallback if broken
  if (length(x) == 0 || all(!is.finite(x))) {
    return(rep(1/length(x), length(x)))
  }
  
  # numerical stability shift
  x = x - min(x, na.rm = TRUE)
  
  w = exp(-x / tau)
  
  # fallback if collapse
  if (!all(is.finite(w)) || sum(w) <= 0) {
    return(rep(1/length(x), length(x)))
  }
  
  w / sum(w)
}


# summarize_lag = function(lag_list) {
#   all_lags = unlist(lag_list)
#   
#   if (length(all_lags) == 0 || all(is.na(all_lags))) {
#     all_lags = 0
#   }
#   
#   list(
#     mean_lag = mean(all_lags),
#     sd_lag = sd(all_lags),
#     median_lag = median(all_lags)
#   )
# }

# probabilistic_frechet = function(P, Q, tau = 0.1, n_samples = 20, dist_fun = NULL) {
#   
#   if (is.null(dist_fun)) {
#     dist_fun = function(x, y) sqrt(sum((x - y)^2))
#   }
#   
#   P = as.matrix(P)
#   Q = as.matrix(Q)
#   
#   n = nrow(P)
#   m = nrow(Q)
#   
#   dist_mat = matrix(0, n, m)
#   for (i in 1:n) {
#     for (j in 1:m) {
#       dist_mat[i, j] = dist_fun(P[i, ], Q[j, ])
#     }
#   }
#   
#   frechet_vals = numeric(n_samples)
#   lag_samples = vector("list", n_samples)
#   paths = vector("list", n_samples)
#   
#   for (s in 1:n_samples) {
#     
#     D = matrix(Inf, n, m)
#     Prev = array(NA_integer_, dim = c(n, m, 2))
#     
#     D[1,1] = dist_mat[1,1]
#     
#     for (i in 2:n) {
#       D[i,1] = max(D[i-1,1], dist_mat[i,1])
#       Prev[i,1,] = c(i-1,1)
#     }
#     
#     for (j in 2:m) {
#       D[1,j] = max(D[1,j-1], dist_mat[1,j])
#       Prev[1,j,] = c(1,j-1)
#     }
#     
#     for (i in 2:n) {
#       for (j in 2:m) {
#         
#         candidates = c(
#           D[i-1,j],
#           D[i,j-1],
#           D[i-1,j-1]
#         )
#         
#         probs = safe_softmin_probs(candidates, tau)
#         
#         if (any(!is.finite(probs)) || sum(probs) == 0) {
#           probs = rep(1/3, 3)
#         }
#         
#         k = sample(1:3, 1, prob = probs)
#         
#         if (k == 1) Prev[i,j,] = c(i-1,j)
#         if (k == 2) Prev[i,j,] = c(i,j-1)
#         if (k == 3) Prev[i,j,] = c(i-1,j-1)
#         
#         D[i,j] = max(dist_mat[i,j], candidates[k])
#       }
#     }
#     
#     i = n; j = m
#     path = matrix(c(i,j), ncol=2)
#     
#     while (!(i == 1 && j == 1)) {
#       p = Prev[i,j,]
#       i = p[1]; j = p[2]
#       path = rbind(c(i,j), path)
#     }
#     
#     step_dist = apply(path, 1, function(idx)
#       dist_mat[idx[1], idx[2]]
#     )
#     
#     frechet_vals[s] = max(step_dist)
#     lag_samples[[s]] = path[,1] - path[,2]
#     paths[[s]] = path
#   }
#   
#   list(
#     mean_frechet = mean(frechet_vals),
#     sd_frechet = sd(frechet_vals),
#     frechet_samples = frechet_vals,
#     lag_samples = lag_samples,
#     paths = paths
#   )
# }


# =============================================================================
# SOFT-DTW INTERNALS
# Forward + backward pass to produce the soft alignment matrix E.
#
# E[i,j] = d(sdtw) / d(D[i,j]) — the gradient of the soft-DTW distance
# w.r.t. the pairwise cost matrix. Interpreted as the expected contribution
# of pairing HSA week i with state week j under the soft alignment.
#
# Convention: E rows = P (HSA), cols = Q (state).
#   expected_j(i) = sum_j( j * E[i,j] ) / sum_j( E[i,j] )
#   lag_profile(i) = expected_j(i) - i
#   positive lag  -> HSA week i aligns to a later state week  (HSA leads)
#   negative lag  -> HSA week i aligns to an earlier state week (HSA lags)
# =============================================================================

# Numerically stable softmin value
.sm_val = function(vals, gamma) {
  f = vals[is.finite(vals)]
  if (length(f) == 0) return(Inf)
  s = min(f)
  s - gamma * log(sum(exp(-(f - s) / gamma)))
}

# Softmin gradient weights (same length as vals, zero for Inf entries)
.sm_weights = function(vals, gamma) {
  w = rep(0.0, length(vals))
  mask = is.finite(vals)
  if (!any(mask)) return(w)
  s = min(vals[mask])
  raw = exp(-(vals - s) / gamma)
  raw[!mask] = 0
  total = sum(raw)
  if (total == 0) return(w)
  raw / total
}

# Compute the soft alignment matrix E via the forward-backward algorithm.
# Returns an n x m matrix.
.soft_dtw_alignment = function(x, y, gamma) {
  
  x = as.numeric(x)
  y = as.numeric(y)
  n = length(x)
  m = length(y)
  
  # Pairwise squared distance matrix
  D = outer(x, y, function(a, b) (a - b)^2)
  
  # --- Forward pass ---
  # R[i,j] = D[i,j] + softmin( R[i-1,j], R[i,j-1], R[i-1,j-1] )
  # Boundary: R[0,*] = R[*,0] = +Inf,  R[1,1] = D[1,1]
  R = matrix(Inf, n, m)
  R[1, 1] = D[1, 1]
  for (i in 2:n) R[i, 1] = D[i, 1] + R[i-1, 1]   # only one finite predecessor
  for (j in 2:m) R[1, j] = D[1, j] + R[1, j-1]   # only one finite predecessor
  for (i in 2:n) {
    for (j in 2:m) {
      R[i, j] = D[i, j] + .sm_val(c(R[i-1,j], R[i,j-1], R[i-1,j-1]), gamma)
    }
  }
  
  # --- Backward pass ---
  # E[i,j] accumulates gradient contributions from its three downstream cells:
  #   (i+1, j)   — R[i,j] was the "above"    predecessor (index 1)
  #   (i, j+1)   — R[i,j] was the "left"     predecessor (index 2)
  #   (i+1, j+1) — R[i,j] was the "diagonal" predecessor (index 3)
  E = matrix(0, n, m)
  E[n, m] = 1
  
  for (i in n:1) {
    for (j in m:1) {
      if (i == n && j == m) next
      
      g = 0
      
      # Contribution from downstream cell (i+1, j)
      # predecessors of (i+1,j): R[i,j] [1], R[i+1,j-1] [2], R[i,j-1] [3]
      if (i < n) {
        preds = c(R[i,   j],
                  if (j > 1) R[i+1, j-1] else Inf,
                  if (j > 1) R[i,   j-1] else Inf)
        g = g + E[i+1, j] * .sm_weights(preds, gamma)[1]
      }
      
      # Contribution from downstream cell (i, j+1)
      # predecessors of (i,j+1): R[i-1,j+1] [1], R[i,j] [2], R[i-1,j] [3]
      if (j < m) {
        preds = c(if (i > 1) R[i-1, j+1] else Inf,
                  R[i,   j],
                  if (i > 1) R[i-1, j  ] else Inf)
        g = g + E[i, j+1] * .sm_weights(preds, gamma)[2]
      }
      
      # Contribution from downstream cell (i+1, j+1)
      # predecessors of (i+1,j+1): R[i,j+1] [1], R[i+1,j] [2], R[i,j] [3]
      if (i < n && j < m) {
        preds = c(R[i,   j+1],
                  R[i+1, j  ],
                  R[i,   j  ])
        g = g + E[i+1, j+1] * .sm_weights(preds, gamma)[3]
      }
      
      E[i, j] = g
    }
  }
  
  E
}


# =============================================================================
# SOFT-DTW WRAPPER
# Calls dtwclust::sdtw for the validated distance value, then computes the
# soft alignment matrix via the forward-backward pass above.
# =============================================================================

soft_dtw_distance = function(P, Q, gamma = 0.01) {
  if (!requireNamespace("dtwclust", quietly = TRUE)) {
    stop("Please install dtwclust: install.packages('dtwclust')")
  }
  
  P = as.matrix(P)
  Q = as.matrix(Q)
  
  d     = dtwclust::sdtw(P, Q, gamma = gamma)
  align = .soft_dtw_alignment(as.numeric(P), as.numeric(Q), gamma)
  
  list(
    soft_dtw       = d,
    soft_alignment = align,
    gamma          = gamma
  )
}


# =============================================================================
# LAG EXTRACTION FROM SOFT ALIGNMENT MATRIX
#
# For each HSA time index i, compute the expected aligned state index:
#   warp(i) = sum_j( j * E[i,j] ) / sum_j( E[i,j] )
# Lag profile: lag(i) = warp(i) - i
#   positive -> HSA week i maps to a later state week  (HSA leads the state)
#   negative -> HSA week i maps to an earlier state week (HSA lags the state)
# =============================================================================

extract_soft_lag = function(E) {
  
  n = nrow(E)
  m = ncol(E)
  
  warp = sapply(seq_len(n), function(i) {
    row = E[i, ]
    s   = sum(row)
    if (s == 0 || !is.finite(s)) return(i)   # degenerate row: identity warp
    sum(seq_len(m) * row) / s
  })
  
  lag_profile = warp - seq_len(n)
  
  list(
    warp         = warp,
    lag_profile  = lag_profile,
    mean_lag     = mean(lag_profile),
    median_lag   = median(lag_profile),
    sd_lag       = sd(lag_profile),
    mean_abs_lag = mean(abs(lag_profile))
  )
}


# =============================================================================
# MAIN STATS FUNCTION
# =============================================================================

prob_stats = function(hsa_id = 688, start = 1, end = Inf,
                      gamma = 0.01, tau = 0.1, n_samples = 20) {
  
  hsa = hsa_state[hsa_state$hsa_nci_id == hsa_id, ]
  
  n   = nrow(hsa)
  end = min(end, n)
  hsa = hsa[start:end, ]
  
  P = as.numeric(hsa$inc_hsa)
  Q = as.numeric(hsa$inc_state)
  
  # Soft-DTW distance + soft alignment matrix
  soft = soft_dtw_distance(P, Q, gamma)
  
  # Lag profile derived from soft alignment
  lag = extract_soft_lag(soft$soft_alignment)
  
  # Peak-region lag: restrict to weeks where HSA incidence >= 75th percentile
  peak_threshold = quantile(P, 0.75, na.rm = TRUE)
  peak_idx       = which(P >= peak_threshold)
  peak_region_lag = if (length(peak_idx) > 0)
    mean(lag$lag_profile[peak_idx], na.rm = TRUE) else NA_real_
  
  stats_list = list(
    soft_dtw_distance = as.numeric(soft$soft_dtw),
    
    # Timing metrics from soft alignment
    mean_lag          = lag$mean_lag,
    median_lag        = lag$median_lag,
    sd_lag            = lag$sd_lag,
    mean_abs_lag      = lag$mean_abs_lag,
    peak_region_lag   = peak_region_lag,
    
    # Magnitude metric (no alignment)
    rmse              = sqrt(mean((P - Q)^2))
  )
  
  data.frame(
    Metric = names(stats_list),
    Value  = unname(unlist(stats_list))
  )
}


# =============================================================================
# BATCH RUNNER
# =============================================================================

write_prob_results = function() {
  library(dplyr)
  library(tidyr)
  library(readr)
  
  set.seed(123)
  
  hsa_unique = hsa_state %>%
    distinct(hsa_nci_id, state)
  
  state_sizes = hsa_unique %>%
    count(state, name = "N_state")
  
  hsa_list = list()
  
  for (state_name in unique(hsa_state$state)) {
    
    state_subset = hsa_unique %>% filter(state == state_name)
    
    n_take = state_sizes$N_state[state_sizes$state == state_name]
    n_take = min(n_take, nrow(state_subset))
    
    state_temp = sample(state_subset$hsa_nci_id, n_take)
    
    hsa_list = c(hsa_list, state_temp)
  }
  
  res = vector("list", length(hsa_list))
  
  for (i in seq_along(hsa_list)) {
    
    ID = hsa_list[[i]]
    cat(sprintf("Processing %s (%d/%d, %.1f%%)\n",
                ID, i, length(hsa_list), 100*i/length(hsa_list)))
    
    out = tryCatch(
      prob_stats(hsa_id = ID),
      error = function(e) NULL
    )
    
    if (is.null(out)) next
    
    out = pivot_wider(
      as.data.frame(out),
      names_from  = Metric,
      values_from = Value
    )
    
    out$ID = ID
    out = out[, c("ID", setdiff(names(out), "ID"))]
    
    res[[i]] = out
  }
  
  prob_results = dplyr::bind_rows(res)
  
  write_csv(prob_results, "../all_prob_results.csv")
  
  return(prob_results)
}