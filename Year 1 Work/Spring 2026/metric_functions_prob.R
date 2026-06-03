soft_dtw_distance = function(P, Q, gamma = 0.01) {
  if (!requireNamespace("dtwclust", quietly = TRUE)) {
    stop("Please install dtwclust: install.packages('dtwclust')")
  }
  
  P = as.matrix(P)
  Q = as.matrix(Q)
  
  d = dtwclust::sdtw(P, Q, gamma = gamma)
  
  list(
    soft_dtw = d,
    gamma = gamma
  )
}
# 
# 

# probabilistic_frechet = function(P, Q, tau = 0.1, n_samples = 100, dist_fun = NULL) {
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
#   # precompute distance matrix
#   dist_mat = matrix(0, n, m)
#   for (i in 1:n) {
#     for (j in 1:m) {
#       dist_mat[i, j] = dist_fun(P[i, ], Q[j, ])
#     }
#   }
#   
#   # storage
#   frechet_vals = numeric(n_samples)
#   lag_list = vector("list", n_samples)
#   paths = vector("list", n_samples)
#   
#   softmin_probs = function(x, tau) {
#     w = exp(-x / tau)
#     w / sum(w)
#   }
#   
#   for (s in 1:n_samples) {
#     
#     # DP table
#     D = matrix(Inf, n, m)
#     Prev = array(NA_integer_, dim = c(n, m, 2))
#     
#     D[1, 1] = dist_mat[1, 1]
#     
#     # init
#     for (i in 2:n) {
#       D[i, 1] = max(D[i - 1, 1], dist_mat[i, 1])
#       Prev[i, 1, ] = c(i - 1, 1)
#     }
#     
#     for (j in 2:m) {
#       D[1, j] = max(D[1, j - 1], dist_mat[1, j])
#       Prev[1, j, ] = c(1, j - 1)
#     }
#     
#     # probabilistic DP
#     for (i in 2:n) {
#       for (j in 2:m) {
#         
#         candidates = c(
#           D[i - 1, j],
#           D[i, j - 1],
#           D[i - 1, j - 1]
#         )
#         
#         probs = softmin_probs(candidates, tau)
#         k = sample(1:3, size = 1, prob = probs)
#         
#         if (k == 1) Prev[i, j, ] = c(i - 1, j)
#         if (k == 2) Prev[i, j, ] = c(i, j - 1)
#         if (k == 3) Prev[i, j, ] = c(i - 1, j - 1)
#         
#         D[i, j] = max(dist_mat[i, j], candidates[k])
#       }
#     }
#     
#     # backtrack
#     i = n
#     j = m
#     path = matrix(c(i, j), ncol = 2)
#     
#     while (!(i == 1 && j == 1)) {
#       p = Prev[i, j, ]
#       i = p[1]
#       j = p[2]
#       path = rbind(c(i, j), path)
#     }
#     
#     # step distances
#     step_dist = apply(path, 1, function(idx)
#       dist_mat[idx[1], idx[2]]
#     )
#     
#     frechet_vals[s] = max(step_dist)
#     lag_list[[s]] = path[,1] - path[,2]
#     paths[[s]] = path
#   }
#   
#   list(
#     mean_frechet = mean(frechet_vals),
#     sd_frechet = sd(frechet_vals),
#     frechet_samples = frechet_vals,
#     lag_samples = lag_list,
#     paths = paths,
#     tau = tau,
#     n_samples = n_samples
#   )
# }
# 
# 
# # Helper: summarize lag
# summarize_lag = function(lag_list) {
#   all_lags = unlist(lag_list)
#   
#   list(
#     mean_lag = mean(all_lags),
#     sd_lag = sd(all_lags),
#     median_lag = median(all_lags),
#     lag_quantiles = quantile(all_lags, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
#   )
# }
# 
# 
# # noise-based Fréchet
# noisy_frechet = function(P, Q, sigma = 0.1, n_sim = 100) {
#   
#   P = as.matrix(P)
#   Q = as.matrix(Q)
#   
#   vals = numeric(n_sim)
#   
#   for (k in 1:n_sim) {
#     Pn = P + matrix(rnorm(length(P), 0, sigma), nrow(P))
#     Qn = Q + matrix(rnorm(length(Q), 0, sigma), nrow(Q))
#     
#     vals[k] = probabilistic_frechet(Pn, Qn, n_samples = 1)$mean_frechet
#   }
#   
#   list(
#     mean = mean(vals),
#     sd = sd(vals),
#     samples = vals
#   )
# }