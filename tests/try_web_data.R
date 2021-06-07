library(caret)
library(pROC)
library(randomForest)
library(ggplot2)
stock <- read.csv("../../../HyVIC/Resources/Data/stock_ord.csv")
stock <- cbind(stock[, -1], label = ordered(stock$response)) # Not necessary

Out_split <- AutoScoreOrdinal::split_data(data = stock, ratio = c(7, 1, 2))
TrainSet <- Out_split$train_set
ValidationSet <- Out_split$validation_set
TestSet <- Out_split$test_set

head(TrainSet)
head(ValidationSet)
head(TestSet)

var_ranking <- AutoScoreOrdinal::AutoScore_rank(train_set = Out_split$train_set)
score_list <- lapply(1:9, function(i) {
  variable_list <- names(var_ranking)[1:i]
  train_set_1 <- Out_split$train_set[, c(variable_list, "label")]
  validation_set_1 <- Out_split$validation_set[, c(variable_list, "label")]
  df_transformed <- AutoScoreOrdinal:::transform_df(df = train_set_1, df_new = validation_set_1,
                                                    split = "quantile",
                                                    quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1))
  train_set_2 <- df_transformed[[1]]
  validation_set_2 <- df_transformed[[2]]

  # AutoScore Module 3 : Variable Weighting
  score_list <- AutoScoreOrdinal:::compute_score_table(train_set_2 = train_set_2,
                                                       validation_set_2 = validation_set_2,
                                                       variable_list = variable_list)

  # Using "auto_test" to generate score based on new dataset and Scoring table "score_table"
  validation_set_3 <- AutoScoreOrdinal:::auto_test(df = validation_set_2,
                                                   score_table = score_list$score_table)
  rowSums(subset(validation_set_3, select = -label))
})
compute_perc_improve <- function(x, digits = 2) {
  x_current <- x[1:(length(x) - 1)]
  x_next <- x[-1]
  x_imp <- (x_next - x_current) / x_current * 100
  names(x_imp) <- seq_along(x_imp)
  round(x_imp, digits = digits)
}
plot_metric <- function(n_vars, metrics, perc_imp_cutoff = 1, main = "") {
  plot(x = n_vars, y = metrics, main = main, xlab = "", ylab = "", type = "b")
  perf_imp <- compute_perc_improve(x = metrics)
  abline(v = which(perf_imp < perc_imp_cutoff & perf_imp > 0), lty = 3)
}
vus <- unlist(lapply(score_list, function(s) {
  VUROCS::VUS(y = as.numeric(Out_split$validation_set$label), fx = s)$val
}))
plot_metric(n_vars = 1:9, metrics = vus, main = "Parsimony: VUS")
#'
#' **Average AUC**
#'
J <- length(unique(Out_split$validation_set$label))
cumm_auc <- do.call("rbind", lapply(1:length(score_list), function(n_var) {
  cbind(n_var = n_var, do.call("rbind", lapply(1:(J - 1), function(j) {
    model_roc <- pROC::roc(response = as.numeric(Out_split$validation_set$label) <= j,
                           predictor = score_list[[n_var]], quiet = TRUE)
    data.frame(j = j, auc = pROC::auc(model_roc))
  })))
}))
par(mfrow = c(2, 2))
lapply(1:(J - 1), function(j) {
  plot_metric(n_vars = 1:9, metrics = cumm_auc$auc[cumm_auc$j == j],
              main = sprintf("Parsimony: AUC for <=%d vs >%d", j, j))
})
par(mfrow = c(1, 1))
library(dplyr)
auc_mean <- cumm_auc %>% group_by(n_var) %>% summarise(auc_mean = mean(auc))
plot_metric(n_vars = 1:9, metrics = auc_mean$auc_mean,
            main = "Parsimony: Average AUC")
# auc_mean2 <- cumm_auc %>% group_by(n_var) %>% summarise(auc_mean = exp(mean(log(auc))))
# plot_metric(n_vars = 1:15, metrics = auc_mean2$auc_mean,
#             main = "Parsimony: Geometric mean of AUC") # does not help
#'
#' **Averaging AUC: alternative approach**
#'
# cumm_auc2 <- unlist(lapply(1:length(score_list), function(n_var) {
#   roc_list <- lapply(1:(J - 1), function(j) {
#     pROC::roc(response = as.numeric(Out_split$validation_set$label) <= j,
#               predictor = score_list[[n_var]], quiet = TRUE)
#   })
#   th_vec <- unlist(lapply(roc_list, function(r) {
#     pROC::coords(r, "best", ret = "threshold", transpose = TRUE)[1]
#     # Not a good idea: cannot proceed if th is Inf
#   }))
#   y_all <- unlist(lapply(1:(J - 1), function(j) {
#     as.numeric(Out_split$validation_set$label) <= j
#   }))
#   z_all <- unlist(lapply(th_vec, function(th) score_list[[n_var]] - th))
#   model_roc <- pROC::roc(response = y_all, predictor = z_all, quiet = TRUE)
#   pROC::auc(model_roc)
# }))
# plot_metric(n_vars = 1:15, metrics = cumm_auc2,
#             main = "Parsimony: Averaging AUC, alternative approach")




m <- ordinal::clm(label ~ ., data = stock, link = "logit")
summary(m)
y_p <- predict(m, type = "class")$fit
table(pred = y_p, obs = stock$label)


m$beta
m$alpha

z <- as.vector(model.matrix(~ ., data = stock[, 1:9])[, -1] %*% m$beta)
boxplot(z ~ stock$label)

roc_list <- lapply(1:4, function(j) {
  pROC::roc(response = as.numeric(stock$label) <= j, predictor = z, quiet = TRUE)
})
th_vec <- unlist(lapply(roc_list, function(r) {
  pROC::coords(r, "best", ret = "threshold", transpose = TRUE)
}))
plot(NA, xlim = c(1, 0), ylim = c(0, 1))
lapply(1:length(roc_list), function(j) plot(roc_list[[j]], add = TRUE, lty = j))
unlist(lapply(roc_list, pROC::auc))
# 0.9739723 0.9514631 0.9712469 0.9941995
mean(unlist(lapply(roc_list, pROC::auc))) # 0.9727204
VUROCS::VUS(y = stock$label, fx = z) # 0.6652381
y_all <- unlist(lapply(1:4, function(j) {
  as.numeric(stock$label) <= j
}))
roc_all <- pROC::roc(response = y_all, predictor = rep(z, 4), quiet = TRUE)
plot(roc_all, add = TRUE, col = "red") # wrong
z_all <- unlist(lapply(th_vec, function(th) z - th))
roc_all2 <- pROC::roc(response = y_all, predictor = z_all, quiet = TRUE)
plot(roc_all2, add = TRUE, col = "blue")
pROC::auc(roc_all2) # 0.9734

model_roc1 <- pROC::roc(response = as.numeric(Out_split$validation_set$label) <= 1,
                        predictor = score_list[[n_var]], quiet = TRUE)
plot(model_roc1)
pROC::ci.auc(model_roc1)
th1 <- pROC::coords(model_roc1, "best", ret = "threshold", transpose = TRUE)
model_roc2 <- pROC::roc(response = as.numeric(Out_split$validation_set$label) <= 2,
                        predictor = score_list[[n_var]], quiet = TRUE)
plot(model_roc2, add = TRUE, lty = 2)
pROC::ci.auc(model_roc2)
th2 <- pROC::coords(model_roc2, "best", ret = "threshold", transpose = TRUE)

model_all <- pROC::roc(
  response = c(as.numeric(Out_split$validation_set$label) <= 1,
               as.numeric(Out_split$validation_set$label) <= 2),
  predictor = c(score_list[[n_var]] - th1, score_list[[n_var]] - th2),
  quiet = TRUE
)
plot(model_all, add = TRUE, lty = 3)
pROC::ci.auc(model_all)
pROC::coords(model_all, "best", ret = "threshold", transpose = TRUE)

Ranking <- AutoScoreOrdinal::AutoScore_rank(TrainSet, ntree = 100)
mae <- AutoScore_parsimony_clm(TrainSet, ValidationSet, rank=Ranking,
                               nmin=1, nmax=9,
                               probs=c(0, 0.05, 0.2, 0.8, 0.95, 1))
# May still be preferrable to use some modified ROC here
num_var <- 4
FinalVariable <- names(Ranking[1:num_var])
CutVec <- AutoScore::AutoScore_weighting(TrainSet, ValidationSet, FinalVariable, MaxScore=100, probs=c(0, 0.05, 0.2, 0.8, 0.95, 1)) # Need to change
ScoringTable <- AutoScore::AutoScore_fine_tuning(TrainSet, ValidationSet, FinalVariable, CutVec, MaxScore=100)
AutoScore::AutoScore_testing(TestSet, FinalVariable, CutVec, ScoringTable)

# -----

dat <- read.csv("~/Downloads/9753173/cumRoc3/data/cork/cork_SI2.csv")
dat <- data.frame(label = ordered(dat$quality + 1), x = dat$dArea)

m <- ordinal::clm(label ~ x, link = "logit", data = dat)
theta <- coef(m)[1:2]
beta <- coef(m)[3]
z_cut <- compute_cutoffs_logit(theta = theta)
z <- beta * dat$x
y_z <- as.numeric(cut(z, breaks = c(-Inf, z_cut, Inf), labels = 1:3))
table(y = dat$label, y_z = y_z)

library(dplyr)
roc_df <- do.call("rbind", lapply(1:2, function(j) {
  data.frame(j = j,
             tp = sum(z <= z_cut[j] & as.numeric(dat$label <= j)),
             tn = sum(z > z_cut[j] & as.numeric(dat$label > j)),
             fp = sum(z <= z_cut[j] & as.numeric(dat$label > j)),
             fn = sum(z > z_cut[j] & as.numeric(dat$label <= j)))
})) %>%
  mutate(fpr = 1 - tn / (tn + fp), tpr = tp / (tp + fn))

Modelroc <- roc(response = as.numeric(dat$label <= 1), predictor = z, quiet = T)
coords(Modelroc, "best", ret = "threshold", transpose = TRUE)
plot(Modelroc)

Modelroc2 <- roc(response = as.numeric(dat$label <= 2), predictor = z, quiet = T)
coords(Modelroc2, "best", ret = "threshold", transpose = TRUE)
plot(Modelroc2)

# ------

#' Compute the false positive and false negative rates for predicting each level
#' in an ordinal outcome
#' @param response An (ordered) factor for the ordinal outcome.
#' @param predictor A numeric predictor for the outcome.
#' @param predictor_cutoff Cut-off values to apply to \code{predictor}. Length
#'   of \code{predictor_cutoff} must be \code{J - 1} for \code{response} with
#'   \code{J} distinct values.
#' @return Returns a \code{data.frame} with \code{J - 1} rows.
compute_roc <- function(response, predictor, predictor_cutoff) {
  response <- as.numeric(response)
  predictor_cutoff <- sort(predictor_cutoff)
  do.call("rbind", lapply(1:length(predictor_cutoff), function(j) {
    data.frame(
      j = j,
      tp = sum(predictor <= predictor_cutoff[j] & response <= j),
      tn = sum(predictor > predictor_cutoff[j] & response > j),
      fp = sum(predictor <= predictor_cutoff[j] & response > j),
      fn = sum(predictor > predictor_cutoff[j] & response <= j)
    )
  })) %>%
    mutate(fpr = 1 - tn / (tn + fp), tpr = tp / (tp + fn))
}

dat <- read.csv("~/Downloads/9753173/cumRoc3/data/nnal/nhanes_SI3.csv")
dat <- data.frame(label = ordered(dat$shsX3 + 1), x1 = dat$URXNALln, x2 = factor(dat$SDDSRVYR))
m <- ordinal::clm(label ~ x1 + x2, link = "logit", data = dat)
theta <- coef(m)[1:2]
beta <- coef(m)[-(1:2)]
z_cut <- compute_cutoffs_logit(theta = theta)
design_mat <- model.matrix(~ x1 + x2, data = dat)[, -1]
z <- as.numeric(design_mat %*% beta)
y_z <- as.numeric(cut(z, breaks = c(-Inf, z_cut, Inf), labels = 1:3))
table(y = dat$label, y_z = y_z)

roc_df <- compute_roc(response = dat$label, predictor = z, predictor_cutoff = z_cut)

Modelroc <- roc(response = as.numeric(dat$label <= 1), predictor = z, quiet = T)
coords(Modelroc, "best", ret = "threshold", transpose = TRUE)

Modelroc2 <- roc(response = as.numeric(dat$label <= 2), predictor = z, quiet = T)
coords(Modelroc2, "best", ret = "threshold", transpose = TRUE)
plot(Modelroc2)

z_cut_roc <- c(coords(Modelroc, "best", ret = "threshold", transpose = TRUE),
               coords(Modelroc2, "best", ret = "threshold", transpose = TRUE))
roc2_df <- compute_roc(response = dat$label, predictor = z, predictor_cutoff = z_cut_roc)

plot(Modelroc)
points(x = 1 - roc_df$fpr[1], y = roc_df$tpr[1], pch = 20, col = "red")
points(x = 1 - roc2_df$fpr[1], y = roc2_df$tpr[1], pch = 20)

plot(Modelroc2)
points(x = 1 - roc_df$fpr[2], y = roc_df$tpr[2], pch = 20, col = "red")
points(x = 1 - roc2_df$fpr[2], y = roc2_df$tpr[2], pch = 20)

# -----

data("WVS", package = "carData")
head(WVS)
m <- ordinal::clm(poverty ~ religion + degree + country + age + gender,
                  data = WVS, link = "logit")
theta <- coef(m)[1:2]
beta <- coef(m)[-(1:2)]
design_mat <- model.matrix(~ religion + degree + country + age + gender, data = WVS)[, -1]
z <- as.numeric(design_mat %*% beta)
z_cut <- compute_cutoffs(theta = theta,
                         z = seq(from = -1, to = 10, length.out = 2000),
                         link = "logit")
y_z <- as.numeric(cut(z, breaks = c(-Inf, z_cut, Inf), labels = 1:3))
table(y_z, predict(m, type = "class")$fit)
y <- as.numeric(WVS$poverty)
table(y = y, y_z = y_z)
roc_df <- compute_roc(response = y, predictor = z, predictor_cutoff = z_cut)

Modelroc <- roc(response = as.numeric(y <= 1), predictor = z, quiet = T)
coords(Modelroc, "best", ret = "threshold", transpose = TRUE)

Modelroc2 <- roc(response = as.numeric(y <= 2), predictor = z, quiet = T)
coords(Modelroc2, "best", ret = "threshold", transpose = TRUE)

Modelroc3 <- roc(response = as.numeric(y > 2), predictor = z, quiet = T)
coords(Modelroc3, "best", ret = "threshold", transpose = TRUE)

z_cut_roc <- c(coords(Modelroc, "best", ret = "threshold", transpose = TRUE),
               coords(Modelroc2, "best", ret = "threshold", transpose = TRUE))
roc2_df <- compute_roc(response = y, predictor = z, predictor_cutoff = z_cut_roc)

plot(Modelroc)
points(x = 1 - roc_df$fpr[1], y = roc_df$tpr[1], pch = 20, col = "red")
points(x = 1 - roc2_df$fpr[1], y = roc2_df$tpr[1], pch = 20)

plot(Modelroc2)
points(x = 1 - roc_df$fpr[2], y = roc_df$tpr[2], pch = 20, col = "red")
points(x = 1 - roc2_df$fpr[2], y = roc2_df$tpr[2], pch = 20)

y_z_roc <- as.numeric(cut(z, breaks = c(-Inf, z_cut_roc, Inf), labels = 1:3))
table(y_z_roc, y)
table(y_z, y)
boxplot(z ~ y_z)
boxplot(z ~ y_z_roc)
mean(y_z == y)
mean(y_z_roc == y)
mean(abs(y_z - y))
mean(abs(y_z_roc - y))

devtools::install_github("andrewheiss/reconPlots")

p_mat <- estimate_p_mat(theta = theta, z = z_vec, link = "logit")
reconPlots::curve_intersect(curve1 = data.frame(x = z_vec, y = p_mat[, 5]),
                            curve2 = data.frame(x = z_vec, y = p_mat[, 6]),
                            empirical = TRUE)$x
compute_cutoffs_logit(theta = theta)
compute_cutoffs(theta = theta, z = seq(from = -10, to = 10, length.out = 2000), link = "logit")
