#> -------------------------------------------
#> SELF-CONTROLLED & ENHANCED EXPECTANCIES META-ANALYSIS PROJECT
#> -- McKay, Bacelar, Parma, Miller, and Carter
#>
#> Re-analysis of self-controlled learning data
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
scdat <- readr::read_csv("data/sc_data.csv")


#> WRANGLE ----
#>
#> Create columns with average n per group
scdat$n1 <- scdat$N/2
scdat$n2 <- scdat$N/2

#> Turn effect size data into escalc object
scdat <- metafor::escalc(
  measure = "SMD",
  yi = ret_g,
  vi = ret_v,
  n1i = n1,
  n2i = n2,
  data = scdat)

#> Calculate p-values for z-curve analysis
scdat$pval <- summary(scdat)$pval
retp <- dplyr::filter(scdat, scdat$pval != "NA")

#> Remove two outliers and save vector of effect sizes and standard errors
scdat_nol <- scdat[-c(50, 56),]
d <- scdat_nol$ret_g
s <- scdat_nol$ret_se
d <- na.omit(d)
s <- na.omit(s)


#> ANALYSIS ----
#>
#> Fit RoBMA-PSMA model
#> Commented out purposefully as resulting files are saved and available in
#> the data directory of the project repository

#> Fit RoBMA to self-controlled practice data with outliers removed
#psma <- RoBMA(d = d, se = s, seed = 1, model_type = "PSMA", parallel = TRUE)



#> Sensitivity analysis with informed effect distribution
#>
#sens_psma <- RoBMA(d = d,
                   #se = s, seed = 1,
                   #model_type = "PSMA",
                   #parallel = TRUE,
                   #priors_effect = prior(distribution = "normal",
                                         #parameters = list(mean = 0, sd = .36)))


#summary(sens_psma)

#> Save posterior distributions (post), model-averaged estimates (psma), and
#> individual model estimates (models)
# post_sc_rob <-as.vector((psma[[9]][[3]][[1]]))
# post_sc_rob <- as_tibble(post_sc_rob)
# psma_fit <- summary(psma)
# sc_psma_models <- summary(psma, type = "models")
# write_csv(post_sc_rob, file = "data/post_sc_rob.csv")
# saveRDS(sc_psma_models, file = "data/psmamodels.rds")
# saveRDS(psma_fit, file = "data/psma.rds")

readRDS(file = "data/psma.rds")

#> Fit with outliers included
dsc <- scdat$ret_g
ssc <- scdat$ret_se
dsc <- na.omit(dsc)
ssc <- na.omit(ssc)

#> Fit RoBMA to self-controlled practice data with outliers included
##psmasc <- RoBMA(d = dsc, se = ssc, seed = 1, model_type = "PSMA", parallel = TRUE)

#> Save posterior distributions (post), model-averaged estimates (psma), and
#> individual model estimates (models)
# post_sc_rob_ol <-as.vector((psmasc[[9]][[3]][[1]]))
# post_sc_rob_ol <- as_tibble(post_sc_rob_ol)
# write_csv(post_sc_rob_ol, file = "data/post_sc_rob_ol.csv")
# psmasc_fit <-  summary(psmasc)
# psmasc_models <- summary(psmasc, type = "models")
# saveRDS(psmasc_fit, file = "data/psmasc.rds")
# saveRDS(psmasc_models, file = "data/psmasc_models.rds")

readRDS(file = "data/psmasc.rds")
readRDS(file = "data/psmasc_models.rds")
