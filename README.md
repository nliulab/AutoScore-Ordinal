AutoScore-Ordinal: An Interpretable Machine Learning Framework for
Generating Scoring Models for Ordinal Outcomes
================

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

> ### AutoScore-Ordinal has been merged with the [AutoScore package](https://github.com/nliulab/AutoScore/). Please visit [AutoScore bookdown page](https://nliulab.github.io/AutoScore/) for a full tutorial.

## Citation

* Saffari SE, Ning Y, Xie F, Chakraborty B, Volovici V, Vaughan R, Ong MEH, Liu N, [AutoScore-Ordinal: An interpretable machine learning framework for generating scoring models for ordinal outcomes](https://doi.org/10.1186/s12874-022-01770-y), BMC Medical Research Methodology 2022; 22: 286.

* Xie F, Chakraborty B, Ong MEH, Goldstein BA, Liu N. [AutoScore: A machine learning-based automatic clinical score generator and its application to mortality prediction using electronic health records](http://dx.doi.org/10.2196/21798). JMIR Medical Informatics 2020; 8(10): e21798.

## Contact

-   Yilin Ning (Email: <yilin.ning@duke-nus.edu.sg>)
-   Nan Liu (Email: <liu.nan@duke-nus.edu.sg>)

## Package installation

Install from GitHub or CRAN:

``` r
# From Github
install.packages("devtools")
library(devtools)
install_github(repo = "nliulab/AutoScore", build_vignettes = TRUE)

# From CRAN (recommended)
install.packages("AutoScore")
```
[devtools]: https://github.com/hadley/devtools

Load AutoScore package: 

``` r
library(AutoScore)
```

