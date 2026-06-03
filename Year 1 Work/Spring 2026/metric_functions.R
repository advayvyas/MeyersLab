discrete_frechet = function(P, Q, dist_fun = NULL) {
  # P: n x d matrix or data.frame
  # Q: m x d matrix or data.frame
  # dist_fun: function(x, y) returning scalar distance
  
  if (is.null(dist_fun)) {
    dist_fun = function(x, y) sqrt(sum((x - y)^2))
  }
  
  P = as.matrix(P)
  Q = as.matrix(Q)
  
  n = nrow(P)
  m = nrow(Q)
  
  # distance matrix
  dist_mat = matrix(0, n, m)
  for (i in 1:n)
    for (j in 1:m)
      dist_mat[i, j] = dist_fun(P[i, ], Q[j, ])
  
  # DP table
  D = matrix(Inf, n, m)
  Prev = array(NA_integer_, dim = c(n, m, 2))
  
  # initialization
  D[1, 1] = dist_mat[1, 1]
  
  for (i in 2:n) {
    D[i, 1] = max(D[i - 1, 1], dist_mat[i, 1])
    Prev[i, 1, ] = c(i - 1, 1)
  }
  
  for (j in 2:m) {
    D[1, j] = max(D[1, j - 1], dist_mat[1, j])
    Prev[1, j, ] = c(1, j - 1)
  }
  
  # DP recursion
  for (i in 2:n) {
    for (j in 2:m) {
      candidates = c(
        D[i - 1, j],
        D[i, j - 1],
        D[i - 1, j - 1]
      )
      
      k = which.min(candidates)
      
      if (k == 1) Prev[i, j, ] = c(i - 1, j)
      if (k == 2) Prev[i, j, ] = c(i, j - 1)
      if (k == 3) Prev[i, j, ] = c(i - 1, j - 1)
      
      D[i, j] = max(dist_mat[i, j], candidates[k])
    }
  }
  
  # backtrack optimal path
  i = n; j = m
  path = matrix(c(i, j), ncol = 2)
  
  while (!(i == 1 && j == 1)) {
    p = Prev[i, j, ]
    i = p[1]; j = p[2]
    path = rbind(c(i, j), path)
  }
  
  # stepwise Frechet values
  step_dist = apply(path, 1, function(idx)
    dist_mat[idx[1], idx[2]]
  )
  
  frechet_steps = cummax(step_dist)
  
  list(
    frechet = D[n, m],
    path = path,
    step_distance = step_dist,
    frechet_by_step = frechet_steps
  )
}

dtw_distance = function(P, Q, dist_fun = NULL) {
  # P, Q: numeric vectors or matrices (rows = time
  # points)
  if (is.null(dist_fun))
    dist_fun = function(x, y) sqrt(sum((x - y)^2))
  
  P = as.matrix(P)
  Q = as.matrix(Q)
  
  n = nrow(P)
  m = nrow(Q)
  
  # distance matrix
  dist_mat = matrix(0, n, m)
  for (i in 1:n) for (j in 1:m) dist_mat[i, j] = dist_fun(P[i,
  ], Q[j, ])
  
  # cumulative cost matrix
  D = matrix(Inf, n, m)
  D[1, 1] = dist_mat[1, 1]
  
  # initialize first row/column
  for (i in 2:n) D[i, 1] = dist_mat[i, 1] + D[i - 1, 1]
  for (j in 2:m) D[1, j] = dist_mat[1, j] + D[1, j - 1]
  
  # fill rest of matrix
  for (i in 2:n) {
    for (j in 2:m) {
      D[i, j] = dist_mat[i, j] + min(D[i - 1, j], D[i,
                                                    j - 1], D[i - 1, j - 1])
    }
  }
  
  # backtrack optimal path
  i = n
  j = m
  path = matrix(c(i, j), ncol = 2)
  while (!(i == 1 && j == 1)) {
    if (i == 1) {
      j = j - 1
    } else if (j == 1) {
      i = i - 1
    } else {
      # choose minimum predecessor
      k = which.min(c(D[i - 1, j - 1], D[i - 1, j], D[i,
                                                      j - 1]))
      if (k == 1) {
        i = i - 1
        j = j - 1
      } else if (k == 2) {
        i = i - 1
      } else {
        j = j - 1
      }
    }
    path = rbind(c(i, j), path)
  }
  
  # stepwise cumulative cost along path
  step_cost = dist_mat[path]
  cum_cost = cumsum(step_cost)
  
  list(dtw_distance = D[n, m], path = path, step_cost = step_cost,
       cum_cost = cum_cost)
}