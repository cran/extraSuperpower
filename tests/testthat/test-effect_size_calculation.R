test_that("independent matrix and plot are taken as input", {
  faeff <- 2
  fA <- 3
  fbeff <- 0.5
  fB <- 2
  meansd_mats <- calculate_mean_matrix(refmean = 10, nlfA = fA, nlfB = fB,
                                       fAeffect = faeff, fBeffect = fbeff)
  res <- effsize(meansd_mats)
  expect_type(res, "double")
})

test_that("names of matrices error", {
  nlevfA <- 2
  nlevfB <- 2
  group_size <- 5
  first.mat <- matrix(c(1, 0, 0, 0), nlevfA, nlevfB, dimnames = list(groups=LETTERS[1:nlevfA], treatment=letters[1:nlevfB]))
  second.mat <- 2
  matlist <- list(first.mat=first.mat, second.mat=second.mat)
  expect_error(effsize(matlist))

  matlist <- list(mean.mat=first.mat, sd.mat=second.mat)
  expect_type(effsize(matlist), "double")
})

# test_that("repeated measures matrix is not taken as input", {
#   faeff <- 2
#   fA <- 3
#   fbeff <- 0.5
#   fB <- 2
#   meansd_mats <- calculate_mean_matrix(refmean = 10, nlfA = fA, nlfB = fB,
#                                        fAeffect = faeff, fBeffect = fbeff, plot = FALSE)
#   res <- effsize(meansd_mats)
#   expect_type(res, "double")
# })

test_that("simpleeffectsizeestimates", {
  ##examples from Daniel Lakens' blog
  ##'http://daniellakens.blogspot.com/2020/03/effect-sizes-and-power-for-interactions.html'
  mean.mat <- matrix(c(1, 0, 0, 0), 2, 2)
  sd.mat <- 2
  matlist <- list(mean.mat=mean.mat, sd.mat=sd.mat)
  expect_equal(effsize(matlist)[1,], rep(0.125, 3))

  mean.mat <- matrix(c(1, 0, 0, 1), 2, 2)
  matlist <- list(mean.mat=mean.mat, sd.mat=sd.mat)
  expect_equal(effsize(matlist)[1,], c(0, 0, 0.25))
})

test_that("concordance with Superpower, no interaction effects", {
 mean_mats <- calculate_mean_matrix(refmean = 10, nlfA = 5, nlfB = 4,
                                     fAeffect = 2, fBeffect = 3,
                                     plot = FALSE)
  design <- Superpower::ANOVA_design(
    n = 100,
    design = "4b*5b",
    mu = as.vector(mean_mats$mean.mat),
    sd=as.vector(mean_mats$sd.mat), plot = FALSE
  )
  spres <- Superpower::ANOVA_exact(design, verbose = FALSE)
  expect_true(all(abs(spres$main_results$cohen_f[c(2,1,3)]/ effsize(mean_mats))-1 <0.01))

  sdcoef <- 0.1
  mean_mats <- calculate_mean_matrix(refmean = 10, nlfA = 5, nlfB = 4,
                                     fAeffect = 2, fBeffect = 3, sdratio = sdcoef,
                                     plot = FALSE)
  design <- Superpower::ANOVA_design(
    n = 100,
    design = "4b*5b",
    mu = as.vector(mean_mats$mean.mat),
    sd=as.vector(mean_mats$sd.mat), plot = FALSE
  )
  spres <- Superpower::ANOVA_exact(design, verbose = FALSE)
  expect_true(all(abs(spres$main_results$cohen_f[c(2,1,3)]/effsize(mean_mats))-1 <0.01))
})


test_that("concordance with Superpower, interaction effects", {
  faeff <- 2
  fA <- 5
  fbeff <- 3
  fB <- 4
  ginteract <- expand.grid(1:2,3:4)
  intereff <- 1.5
  int_mean_mat <- calculate_mean_matrix(refmean = 10, nlfA = fA, nlfB = fB,
                                      fAeffect = faeff, fBeffect = fbeff,
                                      groupswinteraction = ginteract, interact = intereff,
                                      plot = FALSE)
  design <- Superpower::ANOVA_design(
    n = 100,
    design = "4b*5b",
    mu = as.vector(int_mean_mat$mean.mat),
    sd=as.vector(int_mean_mat$sd.mat), plot = FALSE
  )
  spres <- Superpower::ANOVA_exact(design, verbose = FALSE)
  expect_true(all(abs(spres$main_results$cohen_f[c(2,1,3)]/effsize(int_mean_mat))-1 <0.01))
})

