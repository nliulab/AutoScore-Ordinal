set.seed(1234)
Out_split0 <- FDS_adult_composite %>%
  # Have to dichotomise Total_hd_count_last1yr and Total_icu_count_last1yr 
  # beforehand because >95% are 0. AutoScore cannot handle this
  mutate(LOS_prev = ifelse(is.na(LOS_prev), 0, LOS_prev)) %>%
  select(-last_stay, -diff_days, -LOS) %>%
  as.data.frame() %>%
  AutoScoreOrdinal::split_data(data = ., ratio = c(7, 1, 2))
lapply(Out_split0, function(dat) summary(dat$LOS_prev))

# Divide each laboratory test into 6 categories (including missing):
train_lab_test <- Out_split0$train_set %>% select(Pulse:SODIUM)
summary(train_lab_test)
(cut_lab_test <- lapply(train_lab_test, function(x) {
  cut_off_tmp <- quantile(x, c(0, 0.05, 0.2, 0.8, 0.95, 1), na.rm = TRUE)
  cut_off_tmp <- unique(cut_off_tmp)
  cut_off <- signif(cut_off_tmp, 3) 
  if (length(cut_off) >= 3) {
    cut_off[2:(length(cut_off) - 1)]
  } else {
    cut_off
  }
})) # Can fine-tune later
Out_split <- lapply(Out_split0, function(dat) {
  dat[, names(train_lab_test)] <- transform_df_fixed_new(
    df = dat[, names(train_lab_test)], 
    cut_vec = cut_lab_test
  )
  dat
})
lapply(Out_split, summary)

x_names <- setdiff(names(Out_split$train_set), "label")
train_set_1 <- Out_split$train_set[, c(x_names, "label")]
validation_set_1 <- Out_split$validation_set[, c(x_names, "label")]
cut_vec <- get_cut_vec(df = train_set_1, categorize = "quantile",
                       quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1))
train_set_2 <- transform_df_fixed(df = train_set_1, cut_vec = cut_vec)
validation_set_2 <- transform_df_fixed(df = validation_set_1, cut_vec = cut_vec)
summary(train_set_2)
train_set_2$label <- factor(as.numeric(train_set_2$label == 1))
scr <- compute_score_table(train_set_2 = train_set_2,
                           variable_list = x_names,
                           max_score = 100)
print_scoring_table(scoring_table = scr, final_variable = x_names)

train_set_1$Total_hd_count_last1yr <- -train_set_1$Total_hd_count_last1yr


scr <- c(DM1 = 0, DM0 = 0, DMcx0 = 0, DMcx1 = 0)
print_scoring_table(scoring_table = scr, final_variable = c("DM", "DMcx"))
