load("../HyVIC/Resources/Data/testdf6_mimic_20000.Rdata")
names(testdf6_mimic_20000)[names(testdf6_mimic_20000) == "Mortality_inpatient"] <- "label"
testdf6_mimic_20000$label <- sample(x = letters[1:3], size = nrow(testdf6_mimic_20000), replace = TRUE)
testdf6_mimic_20000$label <- ordered(testdf6_mimic_20000$label)
set.seed(1234)
Out_split <- AutoScoreOrdinal::split_data(data = testdf6_mimic_20000, ratio = c(7, 1, 2))

train_set <- Out_split$train_set
validation_set <- Out_split$validation_set
variable_list <- names(testdf6_mimic_20000)[1:5]
train_set_1 <- train_set[, c(variable_list, "label")]
validation_set_1 <- validation_set[, c(variable_list, "label")]
model_vus <- AutoScoreOrdinal:::compute_vus_val(train_set_1 = train_set_1,
                                                validation_set_1 = validation_set_1,
                                                variable_list = variable_list,
                                                split = "quantile",
                                                quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
                                                max_score = 100)
model_somers_d <- AutoScoreOrdinal:::compute_somersd_val(train_set_1 = train_set_1,
                                                         validation_set_1 = validation_set_1,
                                                         variable_list = variable_list,
                                                         split = "quantile",
                                                         quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
                                                         max_score = 100)
cumm_auc <- AutoScoreOrdinal:::compute_cumm_auc(train_set_1 = train_set_1,
                                                validation_set_1 = validation_set_1,
                                                variable_list = variable_list,
                                                split = "quantile",
                                                quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
                                                max_score = 100)

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
validation_set_3$total_score <- rowSums(subset(validation_set_3, select = -label))

y_levels <- levels(validation_set_3$label)
J <- length(y_levels)
do.call("rbind", lapply(1:(J - 1), function(j) {
  model_roc <- pROC::roc(response = as.numeric(validation_set_3$label) <= j,
                         predictor = validation_set_3$total_score, quiet = TRUE)
  auc <- pROC::auc(model_roc)
  v <- c(auc = auc, pROC::coords(model_roc, "best", ret = "threshold", transpose = TRUE))
  df <- as.data.frame(matrix(v, nrow = 1))
  names(df) <- names(v)
  cbind(j = j, df)
}))



cut_vec <- AutoScoreOrdinal:::get_cut_vec(df = train_set_1, categorize = categorize,
                       quantiles = quantiles, max_cluster = max_cluster)
train_set_2 <- AutoScoreOrdinal:::transform_df_fixed(df = train_set_1, cut_vec = cut_vec)

# AutoScore Module 3 : Score weighting
score_table <- AutoScoreOrdinal:::compute_score_table(train_set_2 = train_set_2,
                                   validation_set_2 = validation_set_2,
                                   max_score = max_score,
                                   variable_list = final_variables)
AutoScoreOrdinal:::print_scoring_table(scoring_table = score_table, final_variable = final_variables)
validation_set$total_score <- AutoScoreOrdinal:::compute_final_score(
  data = validation_set, final_variables = final_variables,
  cut_vec = cut_vec, score_table = score_table
)
perf_list <- AutoScoreOrdinal:::evaluate_model(data = validation_set,
               final_variables = final_variables,
               score_table = score_table,
               cut_vec = cut_vec, n_boot = 100,
               report_cindex = TRUE)

data$total_score <- AutoScoreOrdinal:::compute_final_score(
  data = data, final_variables = final_variables,
  cut_vec = cut_vec, score_table = score_table
)
model_auc <- AutoScoreOrdinal:::compute_mean_auc(y = data$label, fx = data$total_score)
# Compute bootstrap CI:
data_list <- lapply(1:100, function(iter) {
  rows <- sample(x = 1:nrow(data), size = nrow(data), replace = TRUE)
  data_boot <- data[rows, ]
  data_boot$total_score <- AutoScoreOrdinal:::compute_final_score(
    data = data_boot, final_variables = final_variables,
    cut_vec = cut_vec, score_table = score_table
  )
  data_boot
})
model_auc_boot <- unlist(lapply(data_list, function(data_boot) {
  AutoScoreOrdinal:::compute_mean_auc(y = data_boot$label, fx = data_boot$pred_score)
}))
model_auc_ci <- coxed::bca(model_auc_boot, conf.level = 0.95)
if (report_cindex) {
  model_cindex <- Hmisc::rcorrcens(
    Surv(label, status) ~ pred_score,
    data = data.frame(pred_score = data$total_score,
                      label = as.numeric(data$label),
                      status = 1)
  )
  model_cindex_boot <- unlist(lapply(data_list, function(data_boot) {
    Hmisc::rcorrcens(
      Surv(label, status) ~ pred_score,
      data = data.frame(pred_score = data_boot$total_score,
                        label = as.numeric(data_boot$label),
                        status = 1)
    )
  }))
  model_cindex_ci <- coxed::bca(model_cindex_boot, conf.level = 0.95)


  data <- out_split$test_set
  data_1 <- data[, c(final_variables, "label")]
  data_2 <- AutoScoreOrdinal:::transform_df_fixed(df = data_1, cut_vec = cut_vec)
  data_3 <- AutoScoreOrdinal:::assign_score(df = data_2, score_table = score_table)
  data_3$total_score <- rowSums(subset(data_3, select = final_variables))

  data$total_score <- AutoScoreOrdinal:::compute_final_score(
    data = data, final_variables = final_variables,
    cut_vec = cut_vec, score_table = score_table
  )
  model_auc <- compute_mean_auc(y = data$label, fx = data$total_score)
  # Compute bootstrap CI:
  data_list <- lapply(1:100, function(iter) {
    rows <- sample(x = 1:nrow(data), size = nrow(data), replace = TRUE)
    data_boot <- data[rows, ]
    data_boot$total_score <- AutoScoreOrdinal:::compute_final_score(
      data = data_boot, final_variables = final_variables,
      cut_vec = cut_vec, score_table = score_table
    )
    data_boot
  })
  model_auc_boot <- unlist(lapply(data_list, function(data_boot) {
    compute_mean_auc(y = data_boot$label, fx = data_boot$total_score)
  }))
  model_auc_ci <- coxed::bca(model_auc_boot, conf.level = 0.95)
  if (report_cindex) {
    model_cindex <- Hmisc::rcorrcens(
      Surv(label, status) ~ pred_score,
      data = data.frame(pred_score = data$total_score,
                        label = as.numeric(data$label),
                        status = 1)
    )[, "C"]
    model_cindex_boot <- unlist(lapply(data_list, function(data_boot) {
      Hmisc::rcorrcens(
        Surv(label, status) ~ pred_score,
        data = data.frame(pred_score = data_boot$total_score,
                          label = as.numeric(data_boot$label),
                          status = 1)
      )[, "C"]
    }))
    model_cindex_ci <- coxed::bca(model_cindex_boot, conf.level = 0.95)
  } else {
    model_cindex <- NULL
    model_cindex_ci <- NULL
  }
