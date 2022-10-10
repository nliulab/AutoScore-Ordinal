AutoScore-Ordinal: An Interpretable Machine Learning Framework for
Generating Scoring Models for Ordinal Outcomes
================

## Important Information
-   **AutoScore-Ordinal has been merged with AutoScore package. Please refer to [**AutoScore**](https://github.com/nliulab/AutoScore/) for using AutoScore-Ordinal functions**
-   **Check out bookdown pages (https://nliulab.github.io/AutoScore/) for guidebook and tutorial**
-   **Check out [**AutoScore Related Published Papers**](https://github.com/nliulab/AutoScore/blob/master/README_Application.md)**

## AutoScore-Ordinal Description

AutoScore-Ordinal is a novel machine learning framework to automate the
development of interpretable clinical scoring models for ordinal
outcomes, which expands the [original AutoScore framework for binary
outcomes](https://github.com/nliulab/AutoScore). AutoScore-Ordinal
modifies the six modules of the AutoScore framework to handle ordinal
outcomes: 1) variable ranking with machine learning, 2) variable
transformation, 3) score derivation (now from the proportional odds
model), 4) model selection, 5) domain knowledge-based score fine-tuning,
and 6) performance evaluation (using the mean AUC (mAUC) across binary
classifications). The AutoScore-Ordinal is elaborated in the manuscript
*“AutoScore-Ordinal: An Interpretable Machine Learning Framework for
Generating Scoring Models for Ordinal Outcomes”* and its flowchart is
shown in the following figure, where blue shading indicate modifications
from the original AutoScore framework. AutoScore-Ordinal could
seamlessly generate risk scores using a parsimonious set of variables,
which can be easily implemented and validated in clinical practice.
Moreover, it enables users to build transparent and interpretable
clinical scores quickly in a straightforward manner.

## Citation

Xie F, Chakraborty B, Ong MEH, Goldstein BA, Liu N. AutoScore: A Machine Learning-Based Automatic Clinical Score Generator and Its Application to Mortality Prediction Using Electronic Health Records. JMIR Medical Informatics 2020;8(10):e21798 (<http://dx.doi.org/10.2196/21798>)
-->

### Contact

-   Yilin Ning (Email: <yilin.ning@duke-nus.edu.sg>)
-   Nan Liu (Email: <liu.nan@duke-nus.edu.sg>)

## AutoScore-Ordinal Installation

### Install the development version from GitHub or the stable version from CRAN (recommended):

``` r
# From Github
install.packages("devtools")
library(devtools)
install_github(repo = "nliulab/AutoScore", build_vignettes = TRUE)

# From CRAN (recommended)
install.packages("AutoScore")
```

### Load R package

``` r
library(AutoScore)
```

Please go to our bookdown page (<https://nliulab.github.io/AutoScore/>)
for looking at the full tutorial of using AutoScore package
