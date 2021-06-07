stock <- read.csv("../../../HyVIC/Resources/Data/stock_ord.csv")
stock <- cbind(stock[, -1], label = ordered(stock$response)) # Not necessary
model <- ordinal::clm(label ~ ., data = stock, link = "logit")
coef_vec <- model$beta
ci_mat <- as.matrix(confint.default(model), ncol = 2)
confint.default(model)[names(coef_vec), ]

AutoScoreOrdinal::check_data(stock)
Out_split <- AutoScoreOrdinal::split_data(data = stock, ratio = c(7, 1, 2))

AutoScoreOrdinal::compute_uni_variable_table(df = Out_split$train_set)
AutoScoreOrdinal::compute_multi_variable_table(df = Out_split$train_set)

var_ranking <- AutoScoreOrdinal::AutoScoreOrdinal_rank(train_set = Out_split$train_set)

i <- 31
train_set_1 <- Out_split$train_set[, c(names(var_ranking)[1:i], "label")]
validation_set_1 <- Out_split$validation_set[, c(names(var_ranking)[1:i], "label")]
cut_vec <- AutoScoreOrdinal:::get_cut_vec(df = train_set_1, categorize = "quantile",
                                          quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1))
train_set_2 <- AutoScoreOrdinal:::transform_df_fixed(df = train_set_1, cut_vec = cut_vec)
validation_set_2 <- AutoScoreOrdinal:::transform_df_fixed(df = validation_set_1,
                                                          cut_vec = cut_vec)
AutoScoreOrdinal:::compute_score_table(train_set_2 = train_set_2,
                                       validation_set_2 = validation_set_2,
                                       variable_list = names(var_ranking)[1:i],
                                       max_score = 100)

AutoScoreOrdinal:::compute_auc_val(train_set_1 = train_set_1,
                                   validation_set_1 = validation_set_1,
                                   variable_list = names(var_ranking)[1:27],
                                   max_score = 100,
                                   categorize = "quantile",
                                   quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1))

train_set_2_v2 <- AutoScoreOrdinal:::change_reference(df = train_set_2,
                                                      coef_vec = coef_vec)

AutoScoreOrdinal::AutoScoreOrdinal_parsimony(train_set = Out_split$train_set,
                                             validation_set = Out_split$validation_set,
                                             rank = var_ranking,
                                             max_score = 100,
                                             n_min = 1, n_max = 20)
i <- 1
rank <- var_ranking
perform_stat <- do.call("rbind", lapply(1:9, function(i) {
  cat("Select", i, "variables:  ")
  variable_list <- names(var_ranking)[1:i]
  train_set_1 <- Out_split$train_set[, c(variable_list, "label")]
  validation_set_1 <- Out_split$validation_set[, c(variable_list, "label")]
  model_auc <- AutoScoreOrdinal:::compute_auc_val(
    train_set_1 = Out_split$train_set[, c(variable_list, "label")],
    validation_set_1 = Out_split$validation_set[, c(variable_list, "label")],
    variable_list = names(var_ranking)[1:i],
    max_score = 100,
    categorize = "quantile",
    quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
  )
  cumm_auc_i <- model_auc$auc[is.na(model_auc$j)]
  cat(cumm_auc_i, "\n")
  cbind(m = i, model_auc)
}))
# names(vus) <- n_min:n_max
plot(x = 1:9, y = perform_stat$auc[is.na(perform_stat$j)],
     main = "Parsimony plot on the Validation Set",
     xlab = "Number of Variables", ylab = "Mean Area Under the Curve",
     col = "steelblue", lwd = 2, type = "o")
perform_stat





i <- 4
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
