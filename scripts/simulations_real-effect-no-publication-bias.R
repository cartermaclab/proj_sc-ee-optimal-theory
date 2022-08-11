#> -------------------------------------------
#> SELF-CONTROLLED & ENHANCED EXPECTANCIES META-ANALYSIS PROJECT
#> -- McKay, Bacelar, Parma, Miller, and Carter
#>
#> Simulations for true effects and no publication bias for figures
#>
#> Authors:
#>   Brad McKay
#>
#> Last update: August 11 2022
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------

#> SCRIPT SETUP ----
#>
#> Required libraries and previous analysis scripts
source("scripts/libraries.R")
source("scripts/carter-etal-2019_simulation-script.R")

#> Set seed for reproducible simulations
#> Data used for figures have been saved to the data directory for easy
#> access without the need to run the simulations
#> See sim_rob.csv and sim_z.csv
set.seed(9293)


#> SIMULATIONS ----
#>
#> RoBMA simulation of true effect and no publication bias
sim_real_effect <- simMA(
  49, delta = .54, tau = .35,
  qrpEnv = "none", censorFunc = "none",
  verbose = FALSE
)
d_sim <- sim_real_effect$g
se_sim <- sim_real_effect$g_se

rob_sim <- RoBMA(
  d = d_sim, se = se_sim,
  model_type = "PSMA",
  seed = 1, parallel = TRUE
)

sim_rob <- as.vector((rob_sim[[9]][[3]][[1]]))
sim_rob <- as_tibble(sim_rob)
# write_csv(sim_rob, file = "data/sim_rob.csv")


#> Z-curve simulation of true effect and no publication bias
sim_zed <- simMA(
  10000, delta = .48, tau = 0,
  qrpEnv = "none", censorFunc = "none",
  verbose = FALSE, fixed.n = 31
)

sim_zed <- metafor::escalc(
  measure = "SMD",
  yi = g,
  vi = g_v,
  n1i = n1,
  n2i = n2,
  data = sim_zed
)

ps <- summary(sim_zed)$pval

sim_zed_res <- zcurve(p = ps)
summary(sim_zed_res)

#> Grab distribution of z-values
z_val_sim  <- sim_zed_res[[4]]
z_val_sim <- as_tibble(z_val_sim)
# write_csv(z_val_sim, file = "data/sim_z.csv")
