# install.packages("devtools")
library(devtools)
install_github(repo = "nliulab/AutoScore")

# library(AutoScore)
library(caret)
library(pROC)
library(randomForest)
library(ggplot2)

data("Sample_Data", package = "AutoScore")
df_AutoScore <- Sample_Data
names(df_AutoScore)[names(df_AutoScore)=="Mortality_inpatient"]<-"label"

Out_split <- AutoScore::split_data(data = df_AutoScore, ratio = c(7, 1, 2))
TrainSet <- Out_split$TrainSet
ValidationSet <- Out_split$ValidationSet
TestSet <- Out_split$TestSet

head(TrainSet)
head(ValidationSet)
head(TestSet)

Ranking <- AutoScore::AutoScore_rank(TrainSet, ntree=100)

AUC <- AutoScore::AutoScore_parsimony(TrainSet, ValidationSet, rank=Ranking, nmin=1, nmax=20, probs=c(0, 0.05, 0.2, 0.8, 0.95, 1))

num_var <- 6
FinalVariable <- names(Ranking[1:num_var])

CutVec <- AutoScore::AutoScore_weighting(TrainSet, ValidationSet, FinalVariable, MaxScore=100, probs=c(0, 0.05, 0.2, 0.8, 0.95, 1))

CutVec$tempc_mean <- c(36, 36.5, 37.3, 38)
CutVec$platelet_min <- c(60, 120, 280, 400)
CutVec$lactate_max <- c(1, 1.7, 2.8, 5.7)
CutVec$resprate_mean <- c(13, 16, 21, 26)
CutVec$spo2_mean <- c(95, 99)
ScoringTable <- AutoScore::AutoScore_fine_tuning(TrainSet, ValidationSet, FinalVariable, CutVec, MaxScore=100)

AutoScore::AutoScore_testing(TestSet, FinalVariable, CutVec, ScoringTable)

# df_AutoScore <- Preprocess(Sample_Data, outcome="Mortality_inpatient")

AutoScore::Descriptive(df_AutoScore)

UniTable <- AutoScore::UniVariable(df_AutoScore)

MultiTable <- AutoScore::MultiVariable(df_AutoScore)
