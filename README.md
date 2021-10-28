# **AutoScore-Ordinal Introduction**

### Description

<!--
AutoScore is a novel machine learning framework to automate the development of
interpretable clinical scoring models. AutoScore consists of six modules: 1)
variable ranking with machine learning, 2) variable transformation, 3) score
derivation, 4) model selection, 5) domain knowledge-based score fine-tuning, and
6) performance evaluation. The AutoScore is elaborated in the article
(<http://dx.doi.org/10.2196/21798>) and its flowchart is shown in the following
figure. AutoScore could seamlessly generate risk scores using a parsimonious set
of variables, which can be easily implemented and validated in clinical
practice. Moreover, it enables users to build transparent and interpretable
clinical scores quickly in a straightforward manner.
-->

### Functions and pipeline

The five pipeline functions: `AutoScore_Ordinal_rank()`,
`AutoScore_Ordinal_parsimony()`, `AutoScore_Ordinal_weighting()`,
`AutoScore_fine_Ordinal_tuning()` and `AutoScore_Ordinal_testing()`
constitute the 5-step AutoScore-based process for generating point-based
clinical scores. This 5-step process gives users the flexibility of
customization (e.g., determining the final list of variables according
to the parsimony plot, and fine-tuning the cutoffs in variable
transformation). Please follow the step-by-step instructions (in Demos)
to build your own scores.

-   STEP(i): `AutoScore_Ordinal_rank()` - Rank variables with random
    forest for multiclass classification (AutoScore-Ordinal Module 1)
-   STEP(ii): `AutoScore_Ordinal_parsimony()` - Select the best model
    with parsimony plot (AutoScore-Ordinal Modules 2+3+4)
-   STEP(iii): `AutoScore_Ordinal_weighting()` - Generate the initial
    score with the final list of variables (Re-run AutoScore-Ordinal
    Modules 2+3)
-   STEP(iv): `AutoScore_Ordinal_fine_tuning()` - Fine-tune the score by
    revising `cut_vec` with domain knowledge (AutoScore-Ordinal
    Module 5)
-   STEP(v): `AutoScore_Ordinal_testing()` - Evaluate the final score
    with multiclass ROC analysis (AutoScore-Ordinal Module 6)

Note: This is just the initial version of the AutoScore-Ordinal. Further
version will be developed and updated.

### Citation

<!--
Xie F, Chakraborty B, Ong MEH, Goldstein BA, Liu N. AutoScore: A Machine Learning-Based Automatic Clinical Score Generator and Its Application to Mortality Prediction Using Electronic Health Records. JMIR Medical Informatics 2020;8(10):e21798 (<http://dx.doi.org/10.2196/21798>)
-->

### Contact

-   Yilin Ning (Email: <yilin.ning@duke-nus.edu.sg>)
-   Nan Liu (Email: <liu.nan@duke-nus.edu.sg>)

# **AutoScore-Ordinal Demonstration**

<h id="Demo0">

## **Install the package and prepare data**

### Install from GitHub:

``` r
# From Github
install.packages("devtools")
library(devtools)
install_github(repo = "nliulab/AutoScoreOrdinal")
```

### Load R package

``` r
library(AutoScoreOrdinal)
```

### Load data

-   Read data from CSV or Excel files.
-   For this demo, use the integrated `sample_data_ordinal` in the
    package.
-   `sample_data_ordinal` has 20000 simulated samples, with similar
    distribution to the data used in the paper.

``` r
load("../data/sample_data_ordinal.rda")
head(sample_data_ordinal)
##           ED_LOS Age Gender    Race Triage_Class_Code EDBoardingTime
## 339445 3.5933333  27   MALE Chinese                P2           3.33
## 152316 3.6288889  70 FEMALE   Malay                P2           8.15
## 434455 2.6502778  67   MALE Chinese                P2           6.58
## 422608 4.9711111  79 FEMALE Chinese         P3 and P4           5.53
## 180459 0.5352778  84   MALE Chinese                P2           1.67
## 357148 4.4008333  65 FEMALE   Malay         P3 and P4           5.22
##        ConsultationWaitingTime DayofWeek VENTILATION resuscitation
## 339445                    0.96         4           0             0
## 152316                    0.00         4           0             0
## 434455                    0.76         2           0             0
## 422608                    1.29         4           0             0
## 180459                    0.30         4           0             0
## 357148                    3.11         2           0             0
##        Num_visit_last_1yr Total_Num_Surgery_last1yr Total_icu_count_last1yr
## 339445                  0                         0                       0
## 152316                  0                         0                       0
## 434455                  0                         0                       0
## 422608                  0                         0                       0
## 180459                  0                         0                       0
## 357148                  0                         0                       0
##        Total_hd_count_last1yr label Admission_Type_4cat LOS_prev Pulse
## 339445                      0     1                  B2        0    92
## 152316                      0     1                   C        0    65
## 434455                      0     1                   C        0    78
## 422608                      0     1                   C        0    81
## 180459                      0     1                   C        0    63
## 357148                      0     1                   C        0   107
##        Respiration SPO2 BP_Diastolic BP_Systolic BICARBONATE CREATININE
## 339445          19  100           75         128        23.0         83
## 152316          18   97           86         183        24.3         47
## 434455          19   98           76         140        19.3        269
## 422608          19   99          138         220        21.9        116
## 180459          18   97           62         115        21.3        234
## 357148          18   98           79         133        23.2         55
##        POTASSIUM SODIUM MI CHF PVD Stroke Dementia Pulmonary Rheumatic PUD
## 339445       4.1    136  0   0   0      0        0         0         0   0
## 152316       3.5    137  0   0   0      0        0         0         0   0
## 434455       5.0    137  0   0   0      0        0         0         0   0
## 422608       4.0    139  0   0   0      0        0         0         0   0
## 180459       4.2    133  0   0   0      0        0         0         0   0
## 357148       3.8    133  0   0   0      0        0         0         0   0
##        LiverMild DM_disease DMcx Paralysis Renal Cancer LiverSevere Mets
## 339445         0          0    0         0     0      0           0    0
## 152316         0          0    1         0     0      0           0    0
## 434455         0          0    0         0     1      0           0    0
## 422608         0          0    0         0     0      0           0    0
## 180459         0          1    0         0     1      0           0    0
## 357148         0          0    1         0     0      0           0    0
```

### Data preprocessing (Users to check the following)

-   Handle missing values (AutoScore requires a complete dataset).
-   Remove special characters from variable names, e.g., `[`, `]`, `(`,
    `)`,`,`. (Suggest using `_` to replace them if needed)
-   Name of the variable should be unique and not entirely included by
    other variable names.
-   Ensure that the dependent variable is named “label” (make sure no
    variables using the same name).
-   Independent variables should be numeric (class: num/int) or
    categorical (class: factor/logic).
-   Handle outliers (optional).
-   Check variable distribution (optional).

### AutoScore preprocessing (Users to check the following)

-   Check if data fulfil the basic requirement by AutoScore.
-   Fix the problem if you see any warnings.

``` r
check_data(sample_data_ordinal)
## 
##  missing value check passed.
```

-   Modify your data to ensure no warning messages.

<h id="Demo">

## **AutoScore-Ordinal Demo**

In this Demo, we demonstrate the use of AutoScore-Ordinal on a
comparably large dataset where separate training and validation sets are
available. Please note that it is just a demo using simulated data, and
thus, the result might not be clinically meaningful.

### Prepare training, validation, and test datasets

-   Option 1: Prepare three separate datasets to train, validate, and
    test models.
-   Option 2: Use demo codes below to randomly split your dataset into
    training, validation, and test datasets (70%, 10%, 20%,
    respectively).

``` r
set.seed(1234)
out_split <- split_data(data = sample_data_ordinal, ratio = c(0.7, 0.1, 0.2), 
                        strat_by_label = TRUE)
train_set <- out_split$train_set
validation_set <- out_split$validation_set
test_set <- out_split$test_set
```

### STEP(i): Generate variable ranking list (AutoScore Module 1)

-   `ntree`: Number of trees in the random forest algorithm (Default:
    100).

``` r
ranking <- AutoScore_Ordinal_rank(train_set = train_set, ntree = 100)
## The ranking based on variable importance was shown below for each variable: 
##                    ED_LOS                       Age                CREATININE 
##                281.026964                277.826873                274.249058 
##               BICARBONATE                  LOS_prev        Num_visit_last_1yr 
##                268.176957                267.573845                266.465760 
##               BP_Systolic            EDBoardingTime                     Pulse 
##                259.809568                258.183854                250.924184 
##              BP_Diastolic   ConsultationWaitingTime                    SODIUM 
##                234.197849                227.823404                217.524249 
##                 POTASSIUM                      Mets               Respiration 
##                211.646130                145.067351                135.346953 
##                      SPO2                 DayofWeek         Triage_Class_Code 
##                134.369792                112.036086                 71.949647 
##       Admission_Type_4cat                      Race                     Renal 
##                 71.637172                 70.277671                 49.440464 
## Total_Num_Surgery_last1yr                    Gender                        MI 
##                 48.980815                 41.232014                 39.129263 
##                    Cancer                      DMcx                       CHF 
##                 38.252953                 34.371165                 32.148785 
##    Total_hd_count_last1yr                DM_disease                    Stroke 
##                 30.331055                 28.710326                 28.351551 
##                 Pulmonary                 LiverMild                       PVD 
##                 27.033979                 25.335346                 25.308722 
##               LiverSevere                 Paralysis                  Dementia 
##                 21.945582                 21.458072                 16.993113 
##                       PUD             resuscitation               VENTILATION 
##                 16.028545                 13.720929                 11.927190 
##   Total_icu_count_last1yr                 Rheumatic 
##                  9.475186                  7.955148
```

### STEP(ii): Select the best model with parsimony plot (AutoScore Modules 2+3+4)

-   `nmin`: Minimum number of selected variables (Default: 1).
-   `nmax`: Maximum number of selected variables (Default: 20).
-   `categorize`: Methods for categorizing continuous variables. Options
    include `"quantile"` or `"kmeans"` (Default: `"quantile"`).
-   `quantiles`: Predefined quantiles to convert continuous variables to
    categorical ones. (Default: `c(0, 0.05, 0.2, 0.8, 0.95, 1)`)
    Available if `categorize = "quantile"`.
-   `max_cluster`: The max number of cluster (Default: 5). Available if
    `categorize = "kmeans"`.
-   `max_score`: Maximum total score (Default: 100).
-   `auc_lim_min`: y-axis limits (min) of the parsimony plot (Default:
    0.5)
-   `auc_lim_max`: y-axis limits (max) of the parsimony plot (Default:
    “adaptive”)

``` r
mAUC <- AutoScore_Ordinal_parsimony(
  train_set,
  validation_set,
  rank = ranking,
  max_score = 100,
  n_min = 1,
  n_max = 20,
  categorize = "quantile",
  quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1), 
  auc_lim_min = 0
)
## Select 1 variables:  0.4007599 
## Select 2 variables:  0.5293802 
## Select 3 variables:  0.6810808 
## Select 4 variables:  0.6928276 
## Select 5 variables:  0.7049731 
## Select 6 variables:  0.7302284 
## Select 7 variables:  0.7329536 
## Select 8 variables:  0.7331184 
## Select 9 variables:  0.7581485 
## Select 10 variables:  0.7590899 
## Select 11 variables:  0.7592438 
## Select 12 variables:  0.7626986 
## Select 13 variables:  0.7627602 
## Select 14 variables:  0.7958155 
## Select 15 variables:  0.7950922 
## Select 16 variables:  0.796445 
## Select 17 variables:  0.7947679 
## Select 18 variables:  0.80253 
## Select 19 variables:  0.8033686 
## Select 20 variables:  0.8020225
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

-   Determine the optimal number of variables (`num_var`) based on the
    parsimony plot obtained in STEP(ii).
-   The final list of variables is the first `num_var` variables in the
    ranked list `ranking` obtained in STEP(i).
-   Optional: User can adjust the finally included variables
    `final_variables` based on the clinical preferences and knowledge.

``` r
# Example 1: Top 6 variables are selected
num_var <- 6
final_variables <- names(ranking[1:num_var])

# Example 2: Top 9 variables are selected
num_var <- 9
final_variables <- names(ranking[1:num_var])

# Example 3: Top 6 variables, the 9th and 14th variable are selected
num_var <- 6
final_variables <- names(ranking[c(1:num_var, 9, 14)])
```

### STEP(iii): Generate initial scores with the final list of variables (Re-run AutoScore Modules 2+3)

-   Generate `cut_vec` with current cutoffs of continuous variables,
    which can be fine-tuned in STEP(iv).

``` r
cut_vec <- AutoScore_Ordinal_weighting(
  train_set = train_set, validation_set = validation_set, 
  final_variables = final_variables, 
  max_score = 100,
  categorize = "quantile",
  quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1), n_boot = 100
)
## ****Included Variables: 
##        variable_name
## 1             ED_LOS
## 2                Age
## 3         CREATININE
## 4        BICARBONATE
## 5           LOS_prev
## 6 Num_visit_last_1yr
## ****Initial Scores: 
## 
## 
## ==================  ============  =====
## variable            interval      point
## ==================  ============  =====
## ED_LOS              <0.665         15  
##                     [0.665,1.33)   10  
##                     [1.33,3.97)     4  
##                     [3.97,6)        1  
##                     >=6             0  
##                                        
## Age                 <27             0  
##                     [27,46)         3  
##                     [46,78)        16  
##                     [78,88)        20  
##                     >=88           27  
##                                        
## CREATININE          <46            10  
##                     [46,61)         0  
##                     [61,133)        1  
##                     [133,580)       9  
##                     >=580           4  
##                                        
## BICARBONATE         <16.8          13  
##                     [16.8,20.5)     4  
##                     [20.5,25.3)     0  
##                     [25.3,28.1)     1  
##                     >=28.1          6  
##                                        
## LOS_prev            <4.94           0  
##                     [4.94,15.5)     4  
##                     >=15.5          5  
##                                        
## Num_visit_last_1yr  <1              0  
##                     [1,4)          16  
##                     >=4            29  
## ==================  ============  =====
## ***Performance (based on validation set):
## mAUC: 0.7302, 95% bootstrap CI: 0.6948-0.7527
## ***The cutoffs of each variables generated by the AutoScore-Ordinal are saved in cut_vec. You can decide whether to revise or fine-tune them
```

### STEP(iv): Fine-tune the initial score generated in STEP(iii) (AutoScore Module 5 & Re-run AutoScore Modules 2+3)

-   Revise `cut_vec` with domain knowledge to update the scoring table
    (AutoScore Module 5).
-   Re-run AutoScore Modules 2+3 to generate the updated scores.
-   Users can choose any cutoff values and/or any number of categories,
    but are suggested to choose numbers close to the automatically
    determined values.

``` r
## For example, we have current cutoffs of continuous variable: Age 
## ==============  ===========  =====
## variable        interval     point
## ==============  ===========  =====
## Age                 <27          0  
##                     [27,46)      3  
##                     [46,78)     16  
##                     [78,88)     20  
##                     >=88        27 
```

-   Current cutoffs:`c(46, 78, 88)`. We can fine tune the cutoffs as
    follows:

``` r
# Example 1: rounding to a nice number
cut_vec$Age <- c(25, 45, 75, 90)

# Example 2: changing cutoffs according to clinical knowledge or preference 
cut_vec$Age <- c(25, 50, 75, 90)

# Example 3: combining categories
cut_vec$Age <- c(45, 75, 90)
```

The mAUC and 95% bootstrap CI are reported after fine-tuning. The
default number of bootstrap samples is 100.

``` r
cut_vec$ED_LOS <- c(2 / 3, 4 / 3, 4)
cut_vec$Age <- c(25, 45, 75, 90)
cut_vec$CREATININE <- c(46, 135, 580)
cut_vec$BICARBONATE <- c(17, 20, 28)
cut_vec$LOS_prev <- c(5, 15)
scoring_table <- AutoScore_Ordinal_fine_tuning(train_set,
                                               validation_set,
                                               final_variables,
                                               cut_vec,
                                               max_score = 100, n_boot = 100)
## ***Fine-tuned Scores: 
## 
## 
## ==================  ============  =====
## variable            interval      point
## ==================  ============  =====
## ED_LOS              <0.667         13  
##                     [0.667,1.33)   10  
##                     [1.33,4)        3  
##                     >=4             0  
##                                        
## Age                 <25             0  
##                     [25,45)         6  
##                     [45,75)        19  
##                     [75,90)        23  
##                     >=90           32  
##                                        
## CREATININE          <46            10  
##                     [46,135)        0  
##                     [135,580)       6  
##                     >=580           3  
##                                        
## BICARBONATE         <17            13  
##                     [17,20)         3  
##                     [20,28)         0  
##                     >=28            6  
##                                        
## LOS_prev            <5              0  
##                     [5,15)          3  
##                     >=15            6  
##                                        
## Num_visit_last_1yr  <1              0  
##                     [1,4)          16  
##                     >=4            26  
## ==================  ============  =====
## ***Performance (based on Validation Set, after fine-tuning):
## mAUC: 0.7335, 95% bootstrap CI: 0.7046-0.7560
```

### STEP(v): Evaluate final risk scores on test dataset (AutoScore Module 6)

The mAUC and generalised c-index are reported for the test set, with 95%
bootstrap CI computed from 100 bootstrap samples (default).

``` r
pred_score <- AutoScore_Ordinal_testing(
  test_set = test_set, 
  final_variables = final_variables, 
  cut_vec = cut_vec, 
  score_table = scoring_table, 
  with_label = TRUE, n_boot = 100
)
## ***Performance using AutoScore-Ordinal (based on unseen test Set):
## mAUC: 0.7500, 95% bootstrap CI: 0.7312-0.7660
## Generalised c-index: 0.7357, 95% bootstrap CI: 0.7206-0.7539
head(pred_score)
##   pred_score Label
## 1         41     1
## 2         19     1
## 3         45     1
## 4          6     1
## 5         42     1
## 6         22     1
```

-   Users could use the `pred_score` for further analysis or export it
    as the CSV to other software.

``` r
write.csv(pred_score, file = "pred_score.csv")
```

<!---
To-do:
- [done] Add bootstrap CI for mAUC
- [done] Remove threshold from scoring table
- Give a function to produce mapping table from training set
- Give a function to make bar plot
-->
