#> -------------------------------------------
#> SELF-CONTROLLED & ENHANCED EXPECTANCIES META-ANALYSIS PROJECT
#> -- McKay, Bacelar, Parma, Miller, and Carter
#>
#> Figures for manuscript
#>
#> Authors:
#>   Brad McKay
#>   Mike Carter
#>
#> Last update: July 26 2022
#>
#> Website: https://www.cartermaclab.org
#> -------------------------------------------

#> SCRIPT SETUP ----
#>
#> Required libraries and previous analysis scripts
source("scripts/z-curve_analysis-wrangle.R")

#> Load data files for posteriors
post_sc_rob <- readr::read_csv("data/post_sc_rob.csv")
post_sc_rob_ol <- readr::read_csv("data/post_sc_rob_ol.csv")
post_ee_rob_nol <- readr::read_csv("data/post_ee_rob_nol.csv")
post_ee_rob_ol <- readr::read_csv("data/post_ee_rob_ol.csv")
post_ee_man_mod <- readr::read_csv("data/post_ee_man_mod.csv")
sim_rob <- readr::read_csv("data/sim_rob.csv")
sim_z <- readr::read_csv("data/sim_z.csv")

#> Default elements for all figures
theme_set(
  theme_minimal() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(vjust = -0.5),
      axis.title.x = element_text(vjust = -0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(size = 1),
      plot.margin = unit(c(.2,.75,.2,.2), "cm") #top, right, bottom, left
    )
)


#> CREATE FIGURES ----
#>
#> Figure 1 - multipanel figure of prior and posterior distributions
#> (A) Prior; (B) Self-controlled; (C) Enhanced expectancies; (D) Posterior
#> plot from simulated data with a true effect and no publication bias
#>
#> Spike and slab prior distribution and plot
spike <- rep(0, 5000)
slab <-  rnorm(5000)
prior <- dplyr::as_tibble(c(spike, slab))

fig1a <- ggplot(
  prior, aes(x = value)
) +
  geom_histogram(fill = "#c0b8da",
                 color = "black",
                 binwidth = .05) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = "Mu",
                     limits = c(-.5, 1)) +
  scale_y_continuous(name = "Count",
                     limits = c(0, 7900)) +
  ggtitle(expression(italic("Prior distribution"))
  )
fig1a

#> Self-controlled practice (no outliers)
fig1b <- ggplot(
  post_sc_rob, aes(x = value)
) +
  geom_histogram(fill = "#89edff",
                 color = "black",
                 binwidth = .05) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = "Mu",
                     limits = c(-.5, 1)) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 7900)) +
  geom_vline(xintercept = mean(post_sc_rob$value),
             linetype = "dashed",
             size = 1) +
  ggtitle(expression(italic("Self-controlled practice"))
  )
fig1b

#> Enhanced expectancies (no outliers)
fig1c <- ggplot(
  post_ee_rob_nol, aes(x = value)
) +
  geom_histogram(fill = "#ff89a1",
                 color = "black",
                 binwidth = .05) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = "Mu",
                     limits = c(-.5, 1)) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 7900)) +
  geom_vline(xintercept = mean(post_ee_rob_nol$value),
             linetype = "dashed",
             size = 1) +
  ggtitle(expression(italic("Enhanced expectancies"))
  )
fig1c

#> Simulation with real effect and no publication bias
fig1d <- ggplot(
  sim_rob, aes(x = value)) +
  geom_histogram(fill = "#b5d4cc",
                 color = "black",
                 binwidth = .05) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = "Mu",
                     limits = c(-.5, 1)) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 7900)) +
  geom_vline(xintercept = mean(sim_rob$value),
             linetype = "dashed",
             size = 1) +
  ggtitle(expression(italic("Simulation with a real effect"))
)
fig1d

#> Combine figure 1 subplots into multipanel Figure 1
fig1 <- fig1a + fig1b + fig1c + fig1d +
  plot_layout(nrow = 1)

fig1 +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 20, face = "bold"))


#> Figure 2 - multipanel figure of z-curve plots, and expected
#> discovery rate (EDR) and expected replication rate (ERR) uncertainty
#> (A) Simulation with real effect and no publication bias;
#> (B) Self-controlled; (C) Enhanced expectancies; (D) Combined
#> (Top) Z-curve plots; (Bottom) 95% confidence distributions for EDR and ERR
#>
#> Simulation with real effect and no publication bias
#>
#> Z-curve plot
fig2a <- ggplot(
  sim_z, aes(x = value)
) +
  geom_histogram(fill = "#b5d4cc",
                 color = "black") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 6)
  ) +
  scale_y_continuous(name = "Density",
                     limits = c(0, 1100),
                     breaks = seq(0, 1000, 250)
  ) +
  geom_vline(xintercept = 1.96,
             linetype = "solid",
             size = 1) +
  ggtitle(expression(italic("Simulation with a real effect"))
  ) +
  annotate(geom = "text",
           x = 2.05, y = 1000,
           label = "p < .05",
           hjust = "left") +
  annotate(geom = "segment",
           x = 2.9, xend = 4,
           y = 1000, yend = 1000,
           color = "black",
           size = 1,
           arrow = arrow(length = unit(3, "mm")))
fig2a

#> Self-controlled practice (no outliers)
#>
#> Z-curve plot
fig2b1 <- ggplot(
  z_val_sc, aes(x = value)
) +
  geom_histogram(fill = "#89edff",
                 color = "black") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 6)
                     ) +
  scale_y_continuous(name = "Density",
                     limits = c(0, 15)
                     ) +
  geom_vline(xintercept = 1.96,
             linetype = "solid",
             size = 1) +
  ggtitle(expression(italic("Self-controlled practice"))
  )
fig2b1

#> EDR and ERR uncertainty
fig2b2 <- ggplot(
  est_power_sc, aes(x = values, fill = ptype)
) +
  geom_density(alpha = 0.5,
               size = 1) +
  scale_fill_manual(name = "Power estimate",
                    labels = (c("EDR",
                                "ERR")),
                    values = c("EDR_dist" = "#04d9ff",
                               "ERR_dist" = "#c1f5ff")) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 1),
                     labels = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(name = "Density") +
  guides(fill = guide_legend(override.aes = list(size = 0))) +
  geom_vline(xintercept = 0.8,
             linetype = "dashed",
             size = 1) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )
fig2b2

#> Enhanced expectancies (no outliers)
#>
#> Z-curve plot
fig2c1 <- ggplot(
  z_val_nol, aes(x = value)
) +
  geom_histogram(fill = "#ff89a1",
                 color = "black") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 6)
  ) +
  scale_y_continuous(name = "Density",
                     limits = c(0, 10),
                     breaks = seq(0, 10, 5)
  ) +
  geom_vline(xintercept = 1.96,
             linetype = "solid",
             size = 1) +
  ggtitle(expression(italic("Enhanced expectancies"))
  )
fig2c1

#> EDR and ERR uncertainty
fig2c2 <- ggplot(
  est_power_nol, aes(x = values, fill = ptype)
) +
  geom_density(alpha = 0.5,
               size = 1) +
  scale_fill_manual(name = "Power estimate",
                    labels = (c("EDR",
                                "ERR")),
                    values = c("EDR_dist" = "#ff073a",
                               "ERR_dist" = "#ffc1cd")) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 1),
                     labels = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(name = "Density") +
  guides(fill = guide_legend(override.aes = list(size = 0))) +
  geom_vline(xintercept = 0.8,
             linetype = "dashed",
             size = 1) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )
fig2c2

#> Motivational factors combined (no outliers)
#>
#> Z-curve plot
fig2d1 <- ggplot(
  z_val_all_nol, aes(x = value)
) +
  geom_histogram(fill = "#9370f6",
                 color = "black") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = expression(bolditalic("Z") * bold("-value")),
                     limits = c(0, 6)
  ) +
  scale_y_continuous(name = "Density",
                     limits = c(0, 25)
  ) +
  geom_vline(xintercept = 1.96,
             linetype = "solid",
             size = 1) +
  ggtitle(expression(italic("Motivational factors combined"))
  )
fig2d1

#> EDR and ERR uncertainty
fig2d2 <- ggplot(
  est_power_all_nol, aes(x = values, fill = ptype)
) +
  geom_density(alpha = 0.5,
               size = 1) +
  scale_fill_manual(name = "Power estimate",
                    labels = (c("EDR",
                                "ERR")),
                    values = c("EDR_dist" = "#521bf0",
                               "ERR_dist" = "#C6A3FF")) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = "Power",
                     limits = c(0, 1),
                     labels = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(name = "Density") +
  guides(fill = guide_legend(override.aes = list(size = 0))) +
  geom_vline(xintercept = 0.8,
             linetype = "dashed",
             size = 1) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )
fig2d2

#> Combine figure 2 subplots into multipanel Figure 2
layout <- "
A#
BC
DE
FG
"

fig2 <- fig2a + fig2b1 + fig2b2 + fig2c1 + fig2c2 + fig2d1 + fig2d2 +
  plot_layout(design = layout)

fig2 +
  patchwork::plot_annotation(
    tag_levels = list(c("A", "B", "", "C", "", "D", ""))) &
  theme(plot.tag = element_text(size = 18, face = "bold"))

#> Old labelling before simulation that used a multi-level tagging system
#> fig2 <- (fig2a1 + fig2a2) / (fig2b1 + fig2b2) / (fig2c1 + fig2c2)
#> fig2[[1]] <- fig2[[1]] + plot_layout(tag_level = 'new')
#> fig2[[2]] <- fig2[[2]] + plot_layout(tag_level = 'new')
#> fig2[[3]] <- fig2[[3]] + plot_layout(tag_level = 'new')
#> fig2 +
#>   patchwork::plot_annotation(tag_levels = c("A", "1")) &
#>   theme(plot.tag = element_text(size = 18, face = "bold"))


#> Table 1 figures
#>
#> Sankey diagrams of selection models
#>
#> Model 1: Two-tailed selection based on significance
model1 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Significant (p < .05)",
              "Null (p > .05)", "Reported",
              "Not reported"),
    color = c("#5e81ac",
              "#d08770",
              "#5e81ac",
              "#d08770"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1),
    target = c(2,2,2,3),
    value =  c(10,0,2,8)
  )
)
model1 <- model1 %>% layout(
  font = list(
    size = 30,
    color = "black"
  )
)
model1

#> Model 2: Two-tailed selection based on significance
#> and non-significant trends
model2 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Significant (p < .05)",
              "Trending (p < .10)",
              "Null (p > .10)",
              "Reported",
              "Not reported"),
    color = c("#5e81ac",
              "#5e81ac",
              "#d08770",
              "#5e81ac",
              "#d08770"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1,2,2),
    target = c(3,4,3,4,3,4),
    value =  c(10,0,5,5,1,9)
  )
)
model2 <- model2 %>% layout(
  font = list(
    size = 30,
    color = "black"
  )
)
model2

#> Model 3: One-tailed selection based on non-significant trends
model3 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Trending (p < .05)",
              "Null (p > .05)",
              "Reported",
              "Not reported"),
    color = c("#5e81ac",
              "#d08770",
              "#5e81ac",
              "#d08770"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1),
    target = c(2,3,2,3),
    value =  c(10,0,2,8)
  )
)
model3 <- model3 %>% layout(
  font = list(
    size = 30,
    color = "black"
  )
)
model3

#> One-tailed selection based on significance and non-significant trends
model4 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Significant (p < .025)",
              "Trending (p < .05)",
              "Null (p > .05)",
              "Reported",
              "Not reported"),
    color = c("#5e81ac",
              "#5e81ac",
              "#d08770",
              "#5e81ac",
              "#d08770"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1,2,2),
    target = c(3,4,3,4,3,4),
    value =  c(10,0,5,5,1,9)
  )
)
model4 <- model4 %>% layout(
  font = list(
    size = 30,
    color = "black"
  )
)
model4

#> Model 5: One-tailed selection based on non-significant trends
#> and direction of outcome
model5 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Trending (p < .05)",
              "Predicted direction (p < .5)",
              "Opposite direction (p > .5)",
              "Reported",
              "Not reported"),
    color = c("#5e81ac",
              "#5e81ac",
              "#d08770",
              "#5e81ac",
              "#d08770"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1,2,2),
    target = c(3,4,3,4,3,4),
    value =  c(10,0,5,5,1,9)
  )
)
model5 <- model5 %>% layout(
  font = list(
    size = 30,
    color = "black"
  )
)
model5

#> Model 6: One-tailed full selection model
model6 <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Significant (p < .025)",
              "Trending (p < .05)",
              "Predicted direction (p < .5)",
              "Opposite direction (p > .5)",
              "Reported",
              "Not reported"),
    color = c("#5e81ac",
              "#5e81ac",
              "#5e81ac",
              "#d08770",
              "#5e81ac",
              "#d08770"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,0,1,1,2,2,3,3),
    target = c(4,5,4,5,4,5,4,5),
    value =  c(10,0,5,5,2,8,1,9)
  )
)
model6 <- model6 %>% layout(
  font = list(
    size = 30,
    color = "black"
  )
)
model6

#> Scatterplots of regression-based models
#>
#> Model 7: PET model
model7_dat <- as_tibble(
  faux::rnorm_multi(
    n = 200,
    mu = .6,
    sd = .2,
    r = .7,
    varnames = c("standard_error",
                 "effect_size")
  )
)

model7_pet <- model7_dat %>% ggplot(
  aes(x = standard_error,
      y = effect_size)
) +
  geom_point() +
  geom_smooth(method = lm,
              se = FALSE,
              color = "#a3be8c") +
  labs(x = "Standard Error",
       y = "Effect Size") +
  theme(rect = element_rect(fill = "transparent"))
model7_pet

#> Model 8: PEESE model

model8_dat <- as_tibble(
  faux::rnorm_multi(
    n = 200,
    mu = .6,
    sd = .2,
    r = .7,
    varnames = c("standard_error",
                 "effect_size")
  )
)
model8_dat$effect_size <- model8_dat$effect_size^2

model8_peese <- model8_dat %>% ggplot(
  aes(x = standard_error,
      y = effect_size)
) +
  geom_point() +
  geom_smooth(method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE,
              colour = "#a3be8c") +
  labs(x = "Standard Error",
       y = "Effect Size") +
  theme(rect = element_rect(fill = "transparent"))
model8_peese