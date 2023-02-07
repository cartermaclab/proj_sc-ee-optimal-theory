#> -------------------------------------------
#> SELF-CONTROLLED & ENHANCED EXPECTANCIES META-ANALYSIS PROJECT
#> -- McKay, Bacelar, Parma, Miller, and Carter
#>
#> Re-analysis of enhanced expectancies data
#>
#> Authors:
#>   Brad McKay
#>
#> Last update: Feb 05 2023
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------


#> SCRIPT SETUP ----
#>
#> Required libraries
source("scripts/libraries.R")

#> Load data file
eedat <- readr::read_csv("data/ee_data.csv")


#> WRANGLE ----
#>
#> Turn effect size data into escalc object
eedat <- metafor::escalc(
  measure = "SMD",
  yi = yi,
  vi = vi,
  n1i = n1i,
  n2i = n2i,
  data = eedat)

#> Calculate p-values for z-curve analysis
eedat$pval <- summary(eedat)$pval

#> Remove two outliers and save vector of effect sizes and standard errors
eedat_nol2 <- eedat[-c(28, 35),]
d2 <- eedat_nol2$yi
s2 <- sqrt(eedat_nol2$vi)


#> ANALYSIS ----
#>
#> Fit RoBMA-PSMA model
#> Commented out purposefully as resulting files are saved and available in
#> the data directory of the project repository

#> Fit RoBMA model to enhanced expectancies data with outliers removed
 #eepsma2 <- RoBMA(
  # d = d2,
  # se = s2,
  # seed = 1,
  # model_type = "PSMA",
  # parallel = TRUE
 #)

#> Sensitivity analysis with informed effect distribution
#>
#sens_eepsma2 <- RoBMA(
     #d = d2,
     #se = s2,
     #seed = 1,
     #model_type = "PSMA",
     #parallel = TRUE,
     #priors_effect = prior(distribution = "normal",
                           #parameters = list(mean = 0, sd = .36))
     #)

#summary(sens_eepsma2)

#> Save posterior distributions (post), model-averaged estimates (psma), and
#> individual model estimates (models)
# post_ee_rob_nol <- as.vector((eepsma2[[9]][[3]][[1]]))
# post_ee_rob_nol <- as_tibble(post_ee_rob_nol)
# write_csv(post_ee_rob_nol, file = "data/post_ee_rob_nol.csv")
# ee_psma_models2 <- summary(eepsma2, type = "models")
# saveRDS(ee_psma_models2, file = "data/eepsmamodels2.rds")
# ee_psma_fit2 <- summary(eepsma2)
# saveRDS(ee_psma_fit2, file = "data/eepmsa2.rds")

readRDS(file = "data/eepsmamodels2.rds")
readRDS(file = "data/eepmsa2.rds")

#> Fit with outliers included
d3 <- eedat$yi
s3 <- sqrt(eedat$vi)

#> Fit RoBMA model to enhanced expectancies data with outliers included
# eepsma3 <- RoBMA(
#   d = d3,
#   se = s3,
#   seed = 1,
#   model_type = "PSMA",
#   parallel = TRUE
# )
# eepmsa3_fit <- summary(eepsma3)
# eepmsa_models3 <- summary(eepsma3, type = "models")

#> Save posterior distributions (post), model-averaged estimates (psma), and
#> individual model estimates (models)
# post_ee_rob_ol <-as.vector((eepsma3[[9]][[3]][[1]]))
# post_ee_rob_ol <- as_tibble(post_ee_rob_ol)
# write_csv(post_ee_rob_ol, file = "data/post_ee_rob_ol.csv")
# saveRDS(eepmsa_models3, file = "data/eepsmamodels3.rds")
# saveRDS(eepmsa3_fit, file = "data/eepsma3.rds")

readRDS(file = "data/eepsmamodels3.rds")
readRDS(file = "data/eepsma3.rds")

#> Fit RoBMA model to subset of enhanced expectancies data
# ee_man_mod <- RoBMA(
#   d = d_man,
#   se = s_man,
#   seed = 1,
#   model_type = "PSMA",
#   parallel = TRUE
# )

#> Save summary of results for model-averaged analysis and individual models &
#> extract posterior distribution for figures
# ee_subset_psma <- summary(ee_man_mod)
# ee_subset_models <- summary(ee_man_mod, type = "models")
# post_ee_man_mod <-as.vector((ee_man_mod[[9]][[3]][[1]]))
# post_ee_man_mod <- as_tibble(post_ee_man_mod)
# write_csv(post_ee_man_mod, file = "data/post_ee_man_mod.csv")

# saveRDS(ee_subset_psma, file = "data/ee_subset_psma.rds")
# saveRDS(ee_subset_models, file = "data/ee_subset_models.rds")

readRDS(file = "data/ee_subset_psma.rds")
readRDS(file = "data/ee_subset_models.rds")
