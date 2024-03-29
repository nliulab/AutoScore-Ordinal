% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoScoreOrdinal.R
\name{compute_final_score}
\alias{compute_final_score}
\title{Compute risk scores for data given variables selected, cut-off values and scoring table}
\usage{
compute_final_score(data, final_variables, cut_vec, score_table)
}
\arguments{
\item{data}{A processed \code{data.frame} that contains data for validation
or testing purpose. This \code{data.frame} must have variable \code{label}
and should have same format as \code{train_set} (same variable names and
outcomes)}

\item{final_variables}{A vector containing the list of selected variables,
selected from Step(ii). See Guidebook}

\item{cut_vec}{Generated from STEP(iii) \code{AutoScore_weighting()}.Please
follow the guidebook}
}
\description{
Compute risk scores for data given variables selected, cut-off values and scoring table
}
