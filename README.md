# extraSuperpower
R package for two-way factorial design sample size calculation. This is performed in three steps. The package:
1. Calculates expected outcomes into a cell mean model.
2. Simulates the data
3. Estimates the power for a given sample size

These steps allow for independent and repeated measures experiments with balanced or unbalanced design. 

For the first step we provide a function to create mean values and standard deviation matrices. For repeated measures designs correlation and covariance matrices are also generated. For the second step separate functions are used to simulate independent and repeated measures experiments. Once the two-way factorial study is simulated under different sample sizes, the power under different statistical tests for these sample sizes can be estimated.

``extraSuperpower`` depends on packages ``stringr``,  ``ggplot2``, ``reshape2``, ``scales``, ``Matrix``, ``ggpubr``, ``ggthemes``, ``rlist``, ``fGarch``, ``truncnorm``, ``MASS``, ``sn``, ``tmvtnorm``, ``afex``, ``Rfit``, ``permuco`` and ``nparLD``. 

The package is available in CRAN.
To install in R.

``install.packages("extraSuperpower")``

First steps:

``library(extraSuperpower)``

``?calculate_mean_matrix    ## example of one between, one within (repeated measures) factorial design simulation``

``?test_power_overkn    ## example of independent measures sample size calculation with plot``
