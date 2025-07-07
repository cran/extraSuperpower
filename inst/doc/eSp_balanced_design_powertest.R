## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE, eval=FALSE-----------------------------------------
# library(extraSuperpower)

## ----package loading----------------------------------------------------------
library(extraSuperpower)

## -----------------------------------------------------------------------------
## outcome mean in reference group at baseline is 10
## a control group and an intervention group will be compared over 3 timepoints
## all measurements are independent
refmean <- 10
Alevs <- 2
Blevs <- 3
fAeff <- 1.5
fBeff <- 0.8

## if you do not provide a list with names of factors and levels, factor names are to "fA" and "fB" and level names are set to 'letters[1:nlfA]' and 'letters[1:nlfB]'.

Alevelnames <- c("control", "intervention")
Blevelnames <- 1:Blevs
nameslist <- list("Group" = Alevelnames, "Time" = Blevelnames)

simple_twoway <- calculate_mean_matrix(refmean = refmean, nlfA = Alevs, nlfB = Blevs,
                                       fAeffect = fAeff, fBeffect = fBeff,
                                       label_list = nameslist)

## ----fig.asp=0.8, fig.width=8-------------------------------------------------
##labelling factors and their levels is convenient
simple_twoway

## ----fig.asp=0.8, fig.width=8-------------------------------------------------
simple_twoway_sdadjusted <- calculate_mean_matrix(refmean = refmean, nlfA = Alevs, nlfB = Blevs,
                                                  fAeffect = fAeff, fBeffect = fBeff,
                                                  sdproportional = FALSE, sdratio = 0.1,
                                                  label_list = nameslist)
simple_twoway_sdadjusted

## ----fig.asp=0.8, fig.width=8-------------------------------------------------
#intervention group is the second row in the means matrix, times 2 and 3 the 2nd and 3rd columns.
cellsinteraction <- c(2, 2, 2, 3)
cellsinteraction <- matrix(cellsinteraction, 2, 2)

interaction_twoway <- calculate_mean_matrix(refmean = refmean, nlfA = Alevs, nlfB = Blevs, 
                                       fAeffect = fAeff, fBeffect = fBeff,
                                       groupswinteraction = cellsinteraction, interact = 0.7,
                                       label_list = nameslist)

interaction_twoway

## ----fig.asp=0.8, fig.width=8-------------------------------------------------
#Let's suppose within subject correlation is 0.7
rho <- 0.7
interaction_twoway_timewithin <- calculate_mean_matrix(refmean = refmean, nlfA = Alevs, nlfB = Blevs, 
                                       fAeffect = fAeff, fBeffect = fBeff,
                                       groupswinteraction = cellsinteraction, interact = 0.7,
                                       rho = rho, withinf = "fB",
                                       label_list = nameslist)

interaction_twoway_timewithin

## -----------------------------------------------------------------------------
iterations <- 50
set.seed(170824)
n <- seq(6, 12, 3)
indepmeasures_normal_sim <- simulate_twoway_nrange(matrices_obj = interaction_twoway,
                                            nset = n, distribution = "normal", nsims = iterations)
length(indepmeasures_normal_sim)
length(n)

## -----------------------------------------------------------------------------
indepmeasures_skewed_sim <- simulate_twoway_nrange(matrices_obj = interaction_twoway,
                                            nset = n, distribution = "skewed", skewness = 2,
                                            nsims = iterations)

## ----normally distributed repeated measures simulation------------------------
repmeasures_normal_sim <- simulate_twoway_nrange(matrices_obj = interaction_twoway_timewithin, 
                                                 nset = n, repeated_measurements = TRUE, 
                                                 nsims = iterations)

## ----skewed repeated measures simulation--------------------------------------
repmeasures_skewed_sim <- simulate_twoway_nrange(matrices_obj = interaction_twoway_timewithin, 
                                                 nset = n, repeated_measurements = TRUE,
                                                 distribution = "skewed", skewness=2,
                                                 nsims = iterations)

## ----fig.asp=0.8, fig.width=8-------------------------------------------------
test_power_overkn(indepmeasures_normal_sim)

## ----fig.asp=0.8, fig.width=8-------------------------------------------------
test_power_overkn(indepmeasures_skewed_sim)

## ----fig.asp=0.8, fig.width=8-------------------------------------------------
test_power_overkn(indepmeasures_skewed_sim, test = "rank")

## ----fig.asp=0.8, fig.width=8-------------------------------------------------
test_power_overkn(repmeasures_normal_sim)

## ----testing power repeated measures normally distributed simulation, fig.asp=0.8, fig.width=8, warning=FALSE, message=FALSE----
test_power_overkn(repmeasures_normal_sim, test = "rank")

## ----repeated measures skewed distribution power testing with ANOVA, fig.asp=0.8, fig.width=8----
test_power_overkn(repmeasures_skewed_sim)

## ----repeated measures skewed distribution power testing with rank, fig.asp=0.8, fig.width=8, warning=FALSE----
test_power_overkn(repmeasures_skewed_sim, test = "rank")

