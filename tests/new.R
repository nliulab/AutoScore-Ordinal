#' Clean up levels of a factor
#' @param levels_vec A vector of levels in a factor.
#' @return Returns a vector of levels with the first and last levels cleaned up.
clean_levels <- function(levels_vec) {
  n_levels <- length(levels_vec)
  levels_vec[1] <- gsub(".*,", "(,", levels_vec[1])
  levels_vec[n_levels] <- gsub(",.*", ",)", levels_vec[n_levels])
  levels_vec
}
#' Categorise continuous covariates in training and validation sets
#' @param x (A subset of) Training set
#' @param testSet1 (A subset of) Validation set
#' @param probs Percentiles to cut continuous covariates at. This vector needs
#'   to include 0 and 1. Default is \code{c(0, 0.05, 0.2, 0.8, 0.95, 1)}.
#' @param Print_categories Whether to print the values of cutoff points for each
#'   covariate. Default is \code{FALSE}.
#' @return Returns a list of training and validation sets with continuous
#'   covariates categorised. If \code{Print_categories = TRUE}, the list of
#'   cut-off values for each continuous covariate used is also included in the
#'   output.
Dftransform <- function(x, testSet1, probs = c(0, 0.05, 0.2, 0.8, 0.95, 1),
                        Print_categories = FALSE) {
  CutVec <- list()
  for (i in 1:(length(x) - 1)) {
    if (class(x[, i]) == "factor") {
      if (length(levels(x[, i])) < 10)
        (next)() else stop("Error!! The number of categories should be less than 10")
    }
    # options(scipen = 20)
    a <- unique(quantile(x[, i], probs = probs))
    a1 <- signif(a, 3)  # remain 3 digits
    if (Print_categories == TRUE)
    {
      print(names(x)[i])
      # print(a1)
      l <- list(a1)
      names(l)[1] <- names(x)[i]
      CutVec <- append(CutVec, l)
    }  #update

    if (length(a1) <= 2) {
      x[, i] <- as.factor(x[, i])
      testSet1[, i] <- as.factor(testSet1[, i])
    } else {
      ##fix bug for roundings
      #a1 <- c(a1[a1 < max(a1)], max(a1)*1.1)
      #a1 <- c(a1[a1 > min(a1)], min(a1)*0.9)
      #a1 <- sort(a1)
      if (a1[5]==a1[6]) {a1[6]<-a1[6]+0.1}
      if (a1[1]==a1[2]) {a1[1]<-a1[1]-0.1}
      x[, i] <- cut(x[, i], breaks = a1, right = F, include.lowest = T, dig.lab = 3)
      # xmin<-unlist(strsplit(levels(x[,i])[1],','))[1] xmax<-unlist(strsplit(levels(x[,i])[length(levels(x[,i]))],','))[2]
      levels(x[, i]) <- clean_levels(levels_vec = levels(x[, i]))

      at <- a1
      at[1] <- floor(min(testSet1[, i]))
      at[length(at)] <- ceiling(max(testSet1[, i]))
      at1 <- unique(signif(at, 3))
      testSet1[, i] <- cut(testSet1[, i], breaks = at1, right = F,
                           include.lowest = T, dig.lab = 3)
      # xmin<-as.character(min(at1)) xmax<-as.character(max(at1))
      levels(testSet1[, i]) <- clean_levels(levels_vec = levels(testSet1[, i]))

    }
    # print(summary(x[,i]))update print(summary(testSet1[,i]))update

  }

  if (Print_categories == TRUE)
    return(list(x, testSet1, CutVec)) else return(list(x, testSet1))

}

#' Compute the cutoff values for X%*%beta from theta terms of a CLM
#' @param theta theta terms of a CLM. For an outcome with J categories, there
#'   should be J-1 elements in \code{\theta}.
#' @return Returns a numeric vector of length J-1.
compute_cutoffs_logit <- function(theta) {
  J <- length(theta) + 1
  cutoff <- NULL
  if (J > 3) {
    cutoff <- unlist(lapply(2:(J - 2), function(j) {
      log((2 * exp(theta[j - 1] + theta[j + 1]) -
             exp(theta[j - 1] + theta[j]) -
             exp(theta[j] + theta[j + 1])) /
            (2 * exp(theta[j]) - exp(theta[j - 1]) - exp(theta[j + 1])))
    }))
  }
  sort(c(theta[1] + theta[2] - log(exp(theta[2]) - 2 * exp(theta[1])),
         cutoff,
         cutoff[J - 1] <- log(exp(theta[J - 1]) - 2 * exp(theta[J - 2]))))
}


#' @param TrainSet Training set.
#' @param ValidationSet Validation set.
#' @param rank A named numeric vector indicating the importance of each
#'   covariate (larger value means more important).
#' @param link Link function of the CLM. Possible values are \code{"logit"}
#'   (default), \code{"probit"}, \code{"cloglog"} and \code{"loglog"}.
#' @param nmin Minimum number of covariate to include in a model. Default is 1.
#' @param nmax Maximum number of covariate to include in a model. Default is 20.
#' @inheritParams Dftransform
#' @import ordinal
#' @import pROC
#' @export
AutoScore_parsimony_clm <- function(TrainSet, ValidationSet, rank, link = "logit",
                                    nmin = 1, nmax = 20,
                                    probs = c(0, 0.05, 0.2, 0.8, 0.95, 1)) {
  J <- length(unique(TrainSet$label))
  s <- names(rank)
  # AUC <- c()

  mae_vec <- unlist(lapply(nmin:nmax, function(i) {
    print("Select the number of Variables")
    print(i)
    SD <- TrainSet[, c(s[1:i], "label")]
    ValidationSet1 <- ValidationSet[, c(s[1:i], "label")]

    # AutoScore Module 2 : cut numeric and transfer categories
    SDlist <- Dftransform(SD, ValidationSet1, probs = probs)
    SD2 <- SDlist[[1]]
    ValidationSet2 <- SDlist[[2]]
    # str(SD2) str(testSet2)

    # multivariable analysis after
    model <- ordinal::clm(label ~ ., link = link, data = SD2)
    y_validation <- ValidationSet2$label

    # AutoScore Module 3 : cut numeric and transfer categories
    coefVec <- coef(model)
    SD2 <- AutoScore::ChangeRef(SD2, coefVec)
    model <- ordinal::clm(label ~ ., link = link, data = SD2)
    # print(model) summary(model)
    coefVec <- coef(model)
    ValidationSet2_x <- ValidationSet2[, -which(names(ValidationSet2) == "label")]
    if (i == 1) {
      ValidationSet2_x <- data.frame(x = ValidationSet2_x)
      names(ValidationSet2_x) <- s[1]
    }
    design_mat <- model.matrix(~ ., data = ValidationSet2_x)[, -1]
    if (is.null(dim(design_mat))) design_mat <- matrix(design_mat, ncol = 1)
    theta_vec <- coefVec[1:(J - 1)]
    beta_vec <- coefVec[-(1:(J - 1))]
    beta_min <- min(beta_vec)
    z_star <- round(design_mat %*% beta_vec / beta_min)
    z_star_cutoffs <- round(compute_cutoffs_logit(theta = theta_vec) / beta_min)
    y_predicted <- cut(x = z_star, breaks = c(-Inf, z_star_cutoffs, Inf),
                       labels = 1:J)
    # PlotROCCurve(ValidationSet3$TotalScore,as.numeric(y_validation)-1)

    # Modelroc <- pROC::roc(y_validation, TotalScore, quiet = TRUE)
    # print(pROC::auc(Modelroc))
    # AUC <- c(AUC, pROC::auc(Modelroc))
    # Try using mean absolute error as performance indicator
    mean(abs(as.numeric(y_predicted) - as.numeric(y_validation)))
  }))

  names(mae_vec) <- nmin:nmax
  plot(mae_vec, main = "Parsimony plot on the Validation Set",
       xlab = "Number of Variables", ylab = "Mean absolute error", col = "red",
       lwd = 2, type = "o")
       # print("list of AUC values are shown below")
       # print(data.frame(AUC))
  # plot(AUC, main = "Parsimony plot on the Validation Set", xlab = "Number of Variables", ylab = "Area Under the Curve", col = "red",
  #      lwd = 2, type = "o")
  #
  # return(AUC)
  mae_vec
}

inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}
inv_cloglog <- function(x) {
  1 - exp(-exp(x))
}
inv_loglog <- function(x) {
  exp(-exp(-x))
}
inv_probit <- function(x) {
  pnorm(x)
}
estimate_p_mat <- function(theta, z, link) {
  n <- length(z)
  inv_link <- get(paste0("inv_", link))
  cump_mat <- inv_link(matrix(rep(theta, n), nrow = n, byrow = TRUE) - 
                         matrix(rep(z, length(theta)), nrow = n, byrow = FALSE))
  cump_mat <- cbind(0, cump_mat, 1)
  t(apply(cump_mat, 1, diff))
}
#' Compute the cutoff values for X%*%beta from theta terms of a CLM
#' @param theta theta terms of a CLM. For an outcome with J categories, there
#'   should be J-1 elements in \code{\theta}.
#' @param z A numeric vector for the predictor value. Make sure the range is 
#'   sufficient for all intersection between probability curves to intersect. 
#'   Default is \code{NULL} for \code{link = "logit"}.
#' @inheritParams AutoScore_parsimony_clm
#' @return Returns a numeric vector of length J-1.
#' @import reconPlots
#' @export
compute_cutoffs <- function(theta, z = NULL, link) {
  link <- match.arg(arg = link, choices = c("logit", "probit", "cloglog", "loglog"))
  if (link == "logit") {
    J <- length(theta) + 1
    cutoff <- NULL
    if (J > 3) {
      cutoff <- unlist(lapply(2:(J - 2), function(j) {
        log((2 * exp(theta[j - 1] + theta[j + 1]) -
               exp(theta[j - 1] + theta[j]) -
               exp(theta[j] + theta[j + 1])) /
              (2 * exp(theta[j]) - exp(theta[j - 1]) - exp(theta[j + 1])))
      }))
    }
    sort(c(theta[1] + theta[2] - log(exp(theta[2]) - 2 * exp(theta[1])),
           cutoff,
           cutoff[J - 1] <- log(exp(theta[J - 1]) - 2 * exp(theta[J - 2]))))
  } else {
    if (is.null(z)) stop(simpleError("z is required when link is not logit."))
    p_mat <- estimate_p_mat(theta = theta, z = z, link = link)
    unlist(lapply(1:(ncol(p_mat) - 1), function(j) {
      reconPlots::curve_intersect(curve1 = data.frame(x = z, y = p_mat[, j]), 
                                  curve2 = data.frame(x = z, y = p_mat[, j + 1]), 
                                  empirical = TRUE)$x
    }))
  }
}
