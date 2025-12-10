library(dplyr)
library(ggplot2)

all_metric2 <- read.csv("../forecasting_metrics_overall.csv")
us_hsa_county_popdesc <- read.csv("../us_hsa_county_popdesc.csv")

us_hsa_county_popdesc_bygroup <- us_hsa_county_popdesc %>%
  select(state, hsa_nci_id, area_km2_hsa, area_km2_state, n_hsa) %>% 
  distinct()

all_metric <- all_metric2 %>%
  left_join(us_hsa_county_popdesc_bygroup, by = c("state", "hsa_nci_id"))

tmp <- all_metric %>% 
  filter(season == "2023/24", horizon == 3) %>%
  distinct()



coverage_hist_plot <- function(df_pred){
  coverage_95 <- mean(df_pred$diff_wis_overall >= df_pred$lwr95 & df_pred$diff_wis_overall <= df_pred$upr95, na.rm = TRUE)
  message(sprintf("95%% coverage = %.1f%% (n=%d)", 100*coverage_95, nrow(df_pred)))
  
  p1 <- ggplot(df_pred, aes(x = diff_wis_overall_pred, y = diff_wis_overall)) +
    geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0, alpha = 0.35) +
    geom_point(alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Predicted", y = "Observed",
         #title = "Observed vs Predicted with 95% Prediction Intervals",
         title = sprintf("95%% coverage = %.1f%% (n=%d)", 100*coverage_95, nrow(df_pred))) +
    theme_minimal() +theme(
      panel.spacing = unit(0, "mm"),
      legend.position = "bottom",
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20, face = "bold"),
      strip.text = element_text(size = 15),
      plot.title = element_text(size = 20)
    ) 
  print(p1)
  
  x <- df_pred$diff_wis_overall_pred
  y <- df_pred$diff_wis_overall
  

  brks <- pretty(range(c(x, y), na.rm = TRUE), n = 30)
  

  hist(x, breaks = brks, freq = FALSE,
       col = adjustcolor("steelblue", 0.4), border = NA,
       xlab = "diff_wis_overall", main = "Histogram overlap")
  

  hist(y, breaks = brks, freq = FALSE,
       col = adjustcolor("tomato", 0.4), border = NA, add = TRUE)
  
  legend("topright", fill = c(adjustcolor("steelblue",0.4), adjustcolor("tomato",0.4)),
         border = NA, legend = c("Predicted (mean)", "Observed"))
  
  
}




### LM
lm_fit <- lm(diff_wis_overall~pop_ratio + pct_urban + log(area_km2_state) + log(area_km2_hsa) + log(density_state) + log(density_hsa) + 
               n_hsa + log(population_state), data = tmp)

lm_fit <- lm(diff_wis_overall~pct_urban + log(area_km2_state) + log(area_km2_hsa) + 
               n_hsa + log(population_state) + log(population_hsa), data = tmp)

summary(lm_fit)
step(lm_fit)
#lm_final <- lm(formula = diff_wis_overall ~ pct_urban + pop_ratio, data = tmp)
lm_final <- lm(formula = diff_wis_overall ~ pct_urban + n_hsa, data = tmp)

summary(lm_final)
pred <- predict(lm_final, newdata = tmp, interval = "prediction", se.fit = TRUE)
#pred <- predict(lm_final, se.fit = TRUE)  # return $fit, $se.fit, $residual.scale 
df_pred_lm <- cbind(
  tmp,
  diff_wis_overall_pred = pred$fit[, "fit"],
  se_hat = as.numeric(pred$se.fit),
  lwr95 = pred$fit[, "fit"] - 1.96 * pred$se.fit,
  upr95 = pred$fit[, "fit"] + 1.96 * pred$se.fit,
  lwr95_pi = pred$fit[, "lwr"], #pred$fit - 1.96 * pred$se.fit,
  upr95_pi = pred$fit[, "upr"] #pred$fit + 1.96 * pred$se.fit
)

coverage_hist_plot(df_pred_lm)







### GAM
library(mgcv) 
fit_gam <- gam(diff_wis_overall~
                 s(pop_ratio) 
               + s(pct_urban)
               #+ s(log(area_km2_state))
               #+ s(log(area_km2_hsa))
               #+ s(log(density_state))
               #+ s(log(density_hsa))
               #+ s(n_hsa)
               #+ s(log(population_state))
               #+ pop_ratio
               #+log(density_state)
               #+ log(area_km2_hsa)
               , data = tmp)
summary(fit_gam)

fit_gam <- gam(diff_wis_overall~
                 #  s(pop_ratio) 
                 + s(pct_urban)
               #+ s(log(area_km2_state))
               #+ s(log(area_km2_hsa))
               #+ s(log(density_state))
               #+ s(log(density_hsa))
               + s(n_hsa)
               #+ s(log(population_state))
               #+ pop_ratio
               #+log(density_state)
               #+ log(area_km2_hsa)
               , data = tmp)
summary(fit_gam)
plot(fit_gam, shade = TRUE, select = 1); abline(h = 0, col = "red", lty = 2)
plot(fit_gam, shade = TRUE, select = 2); abline(h = 0, col = "red", lty = 2)

pred <- predict(fit_gam, se.fit = TRUE)  # return $fit, $se.fit, $residual.scale 등 반환
#pred <- predict(fit_gam, newdata = tmp, se.fit = TRUE, type = "response")
#sigma <- sqrt(summary(fit_gam)$scale)  
#tval  <- qt(0.975, df = fit_gam$df.residual)

df_pred_gam <- cbind(
  tmp,
  diff_wis_overall_pred = as.numeric(pred$fit),
  sigma = sqrt(summary(fit_gam)$scale),  
  tval  = qt(0.975, df = fit_gam$df.residual),
  se_hat = as.numeric(pred$se.fit),
  lwr95 = pred$fit - 1.96 * pred$se.fit,
  upr95 = pred$fit + 1.96 * pred$se.fit,
  lwr95_pi = pred$fit - tval * sqrt(pred$se.fit^2 + sigma^2),
  upr95_pi = pred$fit + tval * sqrt(pred$se.fit^2 + sigma^2)
  
)

coverage_hist_plot(df_pred_gam)

### BART

vars <- c("pop_ratio","pct_urban","population_state","density_state",
          "density_hsa","n_hsa","area_km2_hsa","area_km2_state")
#vars <- c("pct_urban","population_state", "population_hsa","n_hsa","area_km2_hsa","area_km2_state")
vars <- c("pct_urban", "pop_ratio","population_state","n_hsa", "density_state", "density_hsa")

#vars <- c("pct_urban","n_hsa","density_state")

library(dbarts)
bart_fit <- bart(
  x.train = tmp[, vars],
  y.train = tmp$diff_wis_overall,
  ntree = 200,           # 200~250 
  k = 1.0,               # ↓ shrinkage 완화 (기본보다 작게)
  power = 2.0, base = 0.95,   # tree depth prior 
  sigdf = 10, sigquant = 0.9, # sigma prior (if it's too big, wide)
  nskip = 1000, ndpost = 4000,
  verbose = FALSE, keeptrees = TRUE,
)
#pred = predict(bart_fit, newdata = tmp, type = "ppd", combineChains = TRUE)
pred = predict(bart_fit, newdata = tmp)

df_pred_bart <- cbind(
  tmp,
  diff_wis_overall_pred = apply(pred, 2,  mean),
  #se_hat = as.numeric(pred$se.fit),
  lwr95 = apply(pred, 2, quantile, probs = c(0.025)),
  upr95 = apply(pred, 2, quantile, probs = c(0.975))
)

coverage_hist_plot(df_pred_bart)



### GBM
library(gbm)

form <- as.formula(paste("diff_wis_overall ~", paste(vars, collapse = " + ")))

# 1) CV
fit_gbm_q <- function(alpha) {
  gbm(
    formula = form,
    data = tmp,
    distribution = list(name = "quantile", alpha = alpha),
    n.trees = 5000,
    interaction.depth = 3,
    shrinkage = 0.01,
    n.minobsinnode = 10,
    bag.fraction = 0.8,
    cv.folds = 5,
    keep.data = TRUE,
    verbose = FALSE
  )
}

gbm50  <- fit_gbm_q(0.5)
gbm025 <- fit_gbm_q(0.025)
gbm975 <- fit_gbm_q(0.975)

best50  <- gbm.perf(gbm50,  method = "cv", plot.it = FALSE)
best025 <- gbm.perf(gbm025, method = "cv", plot.it = FALSE)
best975 <- gbm.perf(gbm975, method = "cv", plot.it = FALSE)

p50  <- predict(gbm50,  newdata = tmp, n.trees = best50)
p025 <- predict(gbm025, newdata = tmp, n.trees = best025)
p975 <- predict(gbm975, newdata = tmp, n.trees = best975)

df_gbm <- tmp %>%
  mutate(
    y_hat = as.numeric(p50),
    lwr95 = as.numeric(p025),
    upr95 = as.numeric(p975)
  )

# variable importance
vi <- summary(gbm50, plotit = FALSE)  # var, rel.inf

ggplot(vi, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Relative influence (%)",
       title = "gbm Variable Importance (median model)") +
  theme_bw()

df_pred_gbm <- cbind(
  tmp,
  diff_wis_overall_pred = p50,
  #se_hat = as.numeric(pred$se.fit),
  lwr95 = p025,
  upr95 = p975
)
coverage_hist_plot(df_pred_gbm)


bart_plot_importance <- function(bart_fit, vars = NULL,
                                 method = c("ip","count"), top_n = 15) {
  method <- match.arg(method)
  stopifnot(!is.null(bart_fit$varcount))
  
  # 변수명 정리
  vc <- as.matrix(bart_fit$varcount)
  if (is.null(vars)) {
    # dbarts가 x.train 이름을 잃는 경우가 있어 직접 지정 가능
    vars <- colnames(bart_fit$x.train)
  }
  if (is.null(colnames(vc)) && !is.null(vars)) colnames(vc) <- vars
  
  if (method == "ip") {
    # Inclusion proportion: 각 draw에서 변수별 split 비중
    row_tot <- rowSums(vc); row_tot[row_tot == 0] <- 1
    ip <- sweep(vc, 1, row_tot, "/")
    
    ip_df <- data.frame(
      Feature = colnames(vc),
      Mean = colMeans(ip),
      Q025 = apply(ip, 2, quantile, 0.025),
      Q975 = apply(ip, 2, quantile, 0.975)
    )
    ip_df <- ip_df[order(-ip_df$Mean), ]
    ip_df <- head(ip_df, top_n)
    
    library(ggplot2)
    ggplot(ip_df, aes(x = reorder(Feature, Mean), y = Mean)) +
      geom_pointrange(aes(ymin = Q025, ymax = Q975)) +
      coord_flip() +
      labs(x = NULL, y = "Inclusion proportion (mean, 95% CI)",
           title = "BART Variable Importance (Proportion)") +
      theme_bw()
  } else {
    # Mean split count: draw별 split 횟수의 평균
    cnt <- data.frame(Feature = colnames(vc),
                      MeanSplits = colMeans(vc))
    cnt <- cnt[order(-cnt$MeanSplits), ]
    cnt <- head(cnt, top_n)
    
    library(ggplot2)
    ggplot(cnt, aes(x = reorder(Feature, MeanSplits), y = MeanSplits)) +
      geom_col(fill = "steelblue", color = "black") +
      coord_flip() +
      labs(x = NULL, y = "Mean split count",
           title = "BART Variable Importance (Mean Splits)") +
      theme_bw()
  }
}



pdf("Local-Level-Forecasting/figures/fourmodel_diagnostics.pdf", width = 5, height = 5)
coverage_hist_plot(df_pred_lm)
coverage_hist_plot(df_pred_gam)
plot(fit_gam, shade = TRUE, select = 1); abline(h = 0, col = "red", lty = 2)
plot(fit_gam, shade = TRUE, select = 2); abline(h = 0, col = "red", lty = 2)

coverage_hist_plot(df_pred_bart)
bart_plot_importance(bart_fit, vars = vars, method = "count", top_n = 15)
bart_plot_importance(bart_fit, vars = vars, method = "ip", top_n = 15)

coverage_hist_plot(df_pred_gbm)
ggplot(vi, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Relative influence (%)",
       title = "gbm Variable Importance (median model)") +
  theme_bw()
dev.off()



pdf("Local-Level-Forecasting/figures/fourmodel_pop_pct_diagnostics.pdf", width = 5, height = 5)
coverage_hist_plot(df_pred_lm)
coverage_hist_plot(df_pred_gam)
plot(fit_gam, shade = TRUE, select = 1); abline(h = 0, col = "red", lty = 2)
plot(fit_gam, shade = TRUE, select = 2); abline(h = 0, col = "red", lty = 2)
dev.off()



### four model compare
set.seed(1)
K <- 5
n <- nrow(tmp)
fold <- sample(rep(1:K, length.out = n))
y <- tmp$diff_wis_overall



yhat_lm  <- yhat_gam <- yhat_bart <- yhat_gbm <- rep(NA_real_, n)

for (k in 1:K) {
  tr <- fold != k; te <- !tr
  train <- tmp[tr, ]; test <- tmp[te, ]
  
  ## 1) LM
  lm_fit  <- lm(diff_wis_overall ~ pct_urban + n_hsa, data = train) # 예: 포뮬러 교체 OK
  yhat_lm[te] <- predict(lm_fit, newdata = test)
  
  ## 2) GAM
  library(mgcv)
  gam_fit <- gam(diff_wis_overall ~ s(pct_urban) + s(n_hsa), data = train, method = "REML")
  yhat_gam[te] <- predict(gam_fit, newdata = test, type = "response")
  
  ## 3) BART
  library(dbarts)
  #vars <- c("pct_urban","n_hsa")  # 동일 피처 세트로 공정 비교
  #vars <- c("pop_ratio","pct_urban","population_state","density_state",
  #          "density_hsa","n_hsa","area_km2_hsa","area_km2_state")
  vars <- c("pct_urban", "pop_ratio","population_state","n_hsa", "density_state", "density_hsa")
  
  
  Xtr <- as.matrix(train[, vars, drop = FALSE]); colnames(Xtr) <- vars
  Xte <- as.matrix(test[,  vars, drop = FALSE]); colnames(Xte) <- vars
  bart_fit <- bart(x.train = Xtr, y.train = train$diff_wis_overall,
                   keeptrees = TRUE, verbose = FALSE)
  draw_te <- predict(bart_fit, newdata = Xte, combineChains = TRUE)
  yhat_bart[te] <- colMeans(draw_te)                # posperior mean
  
  ## 4) GBM
  fit_gbm_q <- function(alpha) {
    gbm(
      formula = form,
      data = train,
      distribution = list(name = "quantile", alpha = alpha),
      n.trees = 5000,
      interaction.depth = 3,
      shrinkage = 0.01,
      n.minobsinnode = 10,
      bag.fraction = 0.8,
      cv.folds = 5,
      keep.data = TRUE,
      verbose = FALSE
    )
  }
  
  gbm50  <- fit_gbm_q(0.5)
  best50  <- gbm.perf(gbm50,  method = "cv", plot.it = FALSE)
  yhat_gbm[te]  <- predict(gbm50,  newdata = test, n.trees = best50)
  
      if(2==3){
        ## 4) GBM  (설명 목적이면 quantile 대신 L2 회귀 권장)
        library(lightgbm)
        dtr <- lgb.Dataset(data = as.matrix(train[, vars]), label = train$diff_wis_overall)
        params <- list(objective = "regression", metric = "l2",
                       learning_rate = 0.05, num_leaves = 63,
                       min_data_in_leaf = 3, feature_fraction = 1.0,
                       bagging_fraction = 1.0, verbose = -1)
        cv <- lgb.cv(params, dtr, nrounds = 5000, nfold = 5,
                     early_stopping_rounds = 200, verbose = -1)
        mdl <- lgb.train(params, dtr, nrounds = cv$best_iter)
        yhat_gbm[te] <- predict(mdl, as.matrix(test[, vars]))
      }
}

# --- 공통 메트릭
rmse <- function(a,b) sqrt(mean((a-b)^2))
mae  <- function(a,b) mean(abs(a-b))
R2cv <- function(y, yhat) 1 - sum((y - yhat)^2)/sum( (y - mean(y))^2 )

data.frame(
  model = c("LM","GAM","BART","GBM(L2)"),
  R2_CV = c(R2cv(y, yhat_lm), R2cv(y, yhat_gam),
            R2cv(y, yhat_bart), R2cv(y, yhat_gbm)),
  RMSE  = c(rmse(y, yhat_lm), rmse(y, yhat_gam),
            rmse(y, yhat_bart), rmse(y, yhat_gbm)),
  MAE   = c(mae(y, yhat_lm), mae(y, yhat_gam),
            mae(y, yhat_bart), mae(y, yhat_gbm))
)


library(pdp)
library(ggplot2)
library(patchwork)   

pred_fun <- function(object, newdata) {
  colMeans(predict(object, newdata = as.matrix(newdata), combineChains = TRUE))
}

vars_to_plot <- c( "pct_urban", "density_hsa", "n_hsa", "density_state", 
#                   "area_km2_state", "density_hsa")
                   "population_state", "pop_ratio")

bart_fit <- bart(
  x.train = tmp[, vars_to_plot],
  y.train = tmp$diff_wis_overall,
  verbose = FALSE, keeptrees = TRUE,
)

plots <- lapply(vars_to_plot, function(v) {
  pd <- partial(
    object = bart_fit,
    pred.var = v,
    train = as.data.frame(tmp[, vars_to_plot, drop = FALSE]),
    pred.fun = pred_fun,
    ice = TRUE, center = TRUE,            # c-ICE로 중심화
    frac_to_plot = 0.2,                   # ICE 20%만 표시(겹침 완화)
    grid.resolution = 30
  )
  autoplot(pd, alpha = 0.15) +
    labs(title = paste("BART PD/ICE:", v), x = v, y = "f(x)") +
    theme_bw()
})


pdf("Local-Level-Forecasting/figures/bart_pd_ice.pdf", width = 7, height = 5)
wrap_plots(plots, ncol = 2)
dev.off()


