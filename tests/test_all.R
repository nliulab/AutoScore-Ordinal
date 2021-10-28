data("WVS", package = "carData")
head(WVS)
names(WVS)[1] <- "label"
set.seed(1234)
out_split <- AutoScoreOrdinal::split_data(data = WVS, ratio = c(7, 1, 2))
var_ranking <- AutoScoreOrdinal::AutoScoreOrdinal_rank(train_set = out_split$train_set)
cumm_auc <- AutoScoreOrdinal::AutoScoreOrdinal_parsimony(
  train_set = out_split$train_set,
  validation_set = out_split$validation_set,
  rank = var_ranking, max_score = 100, n_min = 1, n_max = length(var_ranking)
)
var_final <- names(var_ranking[1:3])
var_cut_vec <- AutoScoreOrdinal::AutoScoreOrdinal_weighting(
  train_set = out_split$train_set, validation_set = out_split$validation_set,
  final_variables = var_final
) # indifferent to max_score
var_cut_vec_adj <- var_cut_vec
var_cut_vec_adj$age <- c(20, 30, 60, 75)
score_list_adj <- AutoScoreOrdinal::AutoScoreOrdinal_fine_tuning(
  train_set = out_split$train_set, validation_set = out_split$validation_set,
  final_variables = var_final, cut_vec = var_cut_vec_adj, max_score = NULL
)
AutoScoreOrdinal::AutoScoreOrdinal_testing(
  test_set = out_split$test_set, final_variables = var_final,
  cut_vec = var_cut_vec_adj, score_table = score_list_adj$score_table,
  score_thresholds = score_list_adj$score_thresholds
)
