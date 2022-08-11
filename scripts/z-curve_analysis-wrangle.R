#> -------------------------------------------
#> SELF-CONTROLLED & ENHANCED EXPECTANCIES META-ANALYSIS PROJECT
#> -- McKay, Bacelar, Parma, Miller, and Carter
#>
#> Z-curve analyses and wrangling for z-curve figures
#>
#> Authors:
#>   Brad McKay
#>
#> Last update: July 20 2022
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------


#> SCRIPT SETUP ----
#>
#> Required libraries and previous analysis scripts
source("scripts/sc_wrangle-analysis.R")
source("scripts/ee_wrangle-analysis.R")

#> Set seed for reproducible confidence intervals
set.seed(9293)


#> ANALYSIS ----
#>
#> Fit z-curve on EE data alone
ee_zed_nol2 <- zcurve(p = eedat_nol2$pval)
summary(ee_zed_nol2)

#> Sensitivity analysis with both outliers included
ee_zed <- zcurve(p = eedat$pval)
summary(ee_zed)

#> Fit z-curve to SC data alone
retp_nol <- filter(scdat_nol, scdat_nol$pval != "NA") #remove rows without pvals
sc_zed <- zcurve(p = retp_nol$pval)
summary(sc_zed)

#> Sensitivity analysis with both outliers included
retp_ol <- filter(scdat, scdat$pval != "NA") #remove rows without pvals
sc_zed_ol <- zcurve(p = retp_ol$pval)
summary(sc_zed_ol)

#> Fit z-curve to both motivational factors in OPTIMAL theory
ps_nol <- c(retp_nol$pval, eedat_nol2$pval) #combine p-values into single vector
all_zed_nol <- zcurve(p = ps_nol)
summary(all_zed_nol)

#> Sensitivity analysis with both outliers included
ps <- c(retp$pval, eedat$pval)
all_zed <- zcurve(p = ps)
summary(all_zed)


#> WRANGLE ----
#>
#> Enhanced expectancies (no outliers)
#>
#> Grab distributions for EDR and ERR estimate
EDR_dist <- ee_zed_nol2[[11]][[2]]
ERR_dist <- ee_zed_nol2[[11]][[1]]
est_power_nol <- as.data.frame(cbind(EDR_dist, ERR_dist))
est_power_nol <- est_power_nol %>% pivot_longer(
  cols = c("EDR_dist", "ERR_dist"),
  names_to = "ptype",
  values_to = "values")

#> Grab distribution of z-values
z_val_nol  <- ee_zed_nol2[[4]]
z_val_nol <- as_tibble(z_val_nol)

#> Self-control (no outliers)
#>
#> Grab distribtutions for EDR and ERR estimates
EDR_dist <- sc_zed[[11]][[2]]
ERR_dist <- sc_zed[[11]][[1]]
est_power_sc <- as.data.frame(cbind(EDR_dist, ERR_dist))
est_power_sc <- est_power_sc %>% pivot_longer(
  cols = c("EDR_dist", "ERR_dist"),
  names_to = "ptype",
  values_to = "values")

#> Grab distribution of z-values
z_val_sc  <- sc_zed[[4]]
z_val_sc <- as_tibble(z_val_sc)

#> Both factors combined (no outliers)
#>
#> Grab distribtutions for EDR and ERR estimates
EDR_dist <- all_zed_nol[[11]][[2]]
ERR_dist <- all_zed_nol[[11]][[1]]
est_power_all_nol <- as.data.frame(cbind(EDR_dist, ERR_dist))
est_power_all_nol <- est_power_all_nol %>% pivot_longer(
  cols = c("EDR_dist", "ERR_dist"),
  names_to = "ptype",
  values_to = "values")

#> Grab distribution of z-values
z_val_all_nol  <- all_zed_nol[[4]]
z_val_all_nol <- as_tibble(z_val_all_nol)
