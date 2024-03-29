% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoScoreOrdinal.R
\name{AutoScore_Ordinal_fine_tuning}
\alias{AutoScore_Ordinal_fine_tuning}
\title{AutoScore STEP(iv): Fine-tune the score by revising cut_vec with domain knowledge (AutoScore Module 5)}
\usage{
AutoScore_Ordinal_fine_tuning(
  train_set,
  validation_set,
  final_variables,
  cut_vec,
  max_score = 100,
  n_boot = 100
)
}
\arguments{
\item{train_set}{A processed \code{data.frame} that contains data to be analyzed, for training.}

\item{validation_set}{A processed \code{data.frame} that contains data for validation purpose.}

\item{final_variables}{A vector containing the list of selected variables, selected from Step(2). See Guidebook}

\item{cut_vec}{Generated from STEP(iii) \code{AutoScore_weighting()}.Please follow the guidebook}

\item{max_score}{Maximum total score (Default: 100).}
}
\value{
Generated final table of scoring model for downstream testing
}
\description{
Domain knowledge is essential in guiding risk model development.
 For continuous variables, the variable transformation is a data-driven process (based on "quantile" or "kmeans" ).
 In this step, the automatically generated cutoff values for each continuous variable can be fine-tuned
 by combining, rounding, and adjusting according to the standard clinical norm.  Revised \code{cut_vec} will be input  with domain knowledge to
update scoring table. User can choose any cut-off values/any number of categories. Then final Scoring table will be generated. See Guidebook.
}
\examples{
\dontrun{
scoring_table <- AutoScore_fine_tuning(train_set, validation_set,
 final_variables, cut_vec, max_score = 100)}
}
\references{
\itemize{
 \item{Xie F, Chakraborty B, Ong MEH, Goldstein BA, Liu N. AutoScore: A Machine Learning-Based Automatic Clinical Score Generator and
  Its Application to Mortality Prediction Using Electronic Health Records. JMIR Medical Informatics 2020;8(10):e21798}
}
}
\seealso{
\code{\link{AutoScore_Ordinal_rank}},
  \code{\link{AutoScore_Ordinal_parsimony}},
  \code{\link{AutoScore_Ordinal_weighting}},
  \code{\link{AutoScore_Ordinal_testing}}.
}
