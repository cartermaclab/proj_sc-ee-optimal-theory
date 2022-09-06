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
#> Last update: Sept 3 2022
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
#> Figure 1 - Description of McKay et al. and Bacelar et al. data sets.
#>
#> Make workspace
data <- tibble(x = 1:100, y = 1:100)

fig1 <- data %>%
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10))

#> Create arrows indicating the flow of each meta
fig1 +
  geom_segment(
    x = 20, xend = 12.5, y = 94, yend = 85.4,
    size = 0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 +
  geom_segment(
    x = 30, xend = 40, y = 94, yend = 87.4,
    size = 0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 +
  geom_segment(
    x = 80, xend = 90, y = 94, yend = 85.4,
    size = 0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 +
  geom_segment(
    x = 70, xend = 60, y = 94, yend = 87.4,
    size = 0.5, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 +
  geom_segment(
    x = 50.5, xend = 50.5, y = 70, yend = 60.4,
    size = 0.75, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 +
  geom_segment(
    x = 35.5, xend = 35.5, y = 40, yend = 27.9,
    size = 0.75, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 +
  geom_segment(
    x = 65.5, xend = 65.5, y = 40, yend = 27.9,
    size = 0.75, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 +
  geom_segment(
    x = 15, xend = 15, y = 70, yend = 27.9,
    size = 0.75, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 +
  geom_segment(
    x = 85, xend = 85, y = 70, yend = 27.9,
    size = 0.75, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

#> Create boxes for SC and EE meta-analyses
fig1 +
  geom_rect(xmin = 55, xmax = 96, ymin = 94, ymax = 100,
            color = '#89edff', fill = 'white', size = 0.85) +
  annotate('text', x = 75.5, y = 97,
           label = expression(bold(
             "Self-Controlled Practice (McKay et al., in press)")),
           size = 4) -> fig1

fig1 +
  geom_rect(xmin = 5.5, xmax = 45.5, ymin = 94, ymax = 100,
            color = '#ff89a1', fill = 'white', size = 0.85) +
  annotate('text', x = 25.5, y = 97,
           label = expression(bold(
             'Enhanced Expectancies (Bacelar et al., 2022)')),
           size = 4) -> fig1

#> Create box for common inclusion criteria
fig1 +
  geom_rect(xmin = 25.5, xmax = 75.5, ymin = 70, ymax = 87,
            color = '#9370f6', fill = 'white', size = 0.85) +
  annotate('text', x = 50.5, y = 84,
           label = expression(bold('Shared Inclusion Criteria:')), size = 4) +
  annotate('text', x = 50.5, y = 77,
           label = 'Experimental Design \nDelayed Retention Test \nIncluded an Objective Measure of Motor Behavior \nPublished in Peer-Reviewed Journal',
           size = 4) -> fig1

#> Create box for unique inclusion criteria for each meta
fig1 +
  geom_rect(xmin = 77, xmax = 104.5, ymin = 70, ymax = 85,
            color = '#89edff', fill = 'white', size = 0.85) +
  annotate('text', x = 90.75, y = 82,
           label = expression(bold('Unique Inclusion Criteria:')),
           size = 4) +
  annotate('text', x = 90.75, y = 77,
           label = 'Included a Self-Controlled Group \nIncluded a Yoked Group \nAccepted Thesis',
           size = 4) -> fig1

fig1 +
  geom_rect(xmin = -3.5, xmax = 23.5, ymin = 70, ymax = 85,
            color = '#ff89a1', fill = 'white', size = 0.85) +
  annotate('text', x = 10, y = 82,
           label = expression(bold('Unique Inclusion Criteria:')),
           size = 4) +
  annotate('text', x = 10, y = 77,
           label = 'Enhanced Expectancies Group \nControl or Reduced Expec Group \nLearnable, Goal-Directed Task',
           size = 4) -> fig1

#> Create box for dependent measure priority list
fig1 +
  geom_rect(xmin = 25.5, xmax = 75.5, ymin = 40, ymax = 60,
            color = '#9370f6', fill = 'white', size = 0.85) +
  annotate('text', x = 50.5, y = 57,
           label = expression(bold('Dependent Measure Selection:')),
           size = 4) +
  annotate('text', x = 30.5, y = 55,
           label = expression(bold('Preferred')),
           size = 4) +
  annotate('text', x = 51.6, y = 54.5,
           label = 'Accuracy (i.e., Radial Error)',
           size = 3.5) +
  annotate('text', x = 57.4, y = 44.5,
           label = 'Correlated with Objective (i.e., Variable Error)',
           size = 3.5) +
  annotate('text', x = 32.5, y = 43.5,
           label = expression(bold('Less Preferred')),
           size = 4) -> fig1

#> Create boxes for included studies
fig1 +
  geom_rect(xmin = 10.5, xmax = 40.5, ymin = 15, ymax = 27.5,
            color = '#ff89a1', fill = 'white', size = 0.85) +
  annotate('text', x = 25.5, y = 25,
           label = expression(bold('Studies Screened and Included')),
           size = 4) +
  annotate('text', x = 25.5, y = 20,
           label = 'Full Text Screened: 125\n Studies Included: 56',
           size = 4) -> fig1

fig1 +
  geom_rect(xmin = 60.5, xmax = 90.5, ymin = 15, ymax = 27.5,
            color = '#89edff', fill = 'white', size = 0.85) +
  annotate('text', x = 75.5, y = 25,
           label = expression(bold('Studies Screened and Included')),
           size = 4) +
  annotate('text', x = 75.5, y = 20,
           label = 'Full Text Screened: 160\n Outcomes Included: 54',
           size = 4) -> fig1

#> Add arrow within dependent measure selection box
fig1 +
  geom_segment(
    x = 31, xend = 31, y = 45, yend = 53,
    size = 0.75, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(2, "mm"), type = "closed")) -> fig1

fig1 + theme_void() -> fig1

#> Crop extra white space before saving as pdf
fig1 + theme(
  plot.margin = unit(c(-0.25, 0, -2.75, 0), "cm")
)

#> Read in fig1 as an magick image object to trim extra margin away


#> Figure 2 - multipanel figure of prior and posterior distributions
#> (A) Prior; (B) Self-controlled; (C) Enhanced expectancies; (D) Posterior
#> plot from simulated data with a true effect and no publication bias
#>
#> Spike and slab prior distribution and plot
spike <- rep(0, 5000)
slab <-  rnorm(5000)
prior <- dplyr::as_tibble(c(spike, slab))

fig2a <- ggplot(
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
fig2a

#> Self-controlled practice (no outliers)
fig2b <- ggplot(
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
  # geom_vline(xintercept = mean(post_sc_rob$value),
  #            linetype = "dashed",
  #            size = 1) +
  theme(axis.text.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  ggtitle(expression(italic("Self-controlled practice"))
  )
fig2b

#> Enhanced expectancies (no outliers)
fig2c <- ggplot(
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
  # geom_vline(xintercept = mean(post_ee_rob_nol$value),
  #            linetype = "dashed",
  #            size = 1) +
  theme(axis.text.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  ggtitle(expression(italic("Enhanced expectancies"))
  )
fig2c

#> Simulation with real effect and no publication bias
fig2d <- ggplot(
  sim_rob, aes(x = value)) +
  geom_histogram(fill = "#b5d4cc",
                 color = "black",
                 binwidth = .05) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = "Mu",
                     limits = c(-.5, 1)) +
  scale_y_continuous(name = NULL,
                     limits = c(0, 7900)) +
  theme(axis.text.y = element_blank()) +
  # geom_vline(xintercept = mean(sim_rob$value),
  #            linetype = "dashed",
  #            size = 1) +
  theme(axis.text.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  ggtitle(expression(italic("Simulation with a real effect"))
)
fig2d

#> Combine figure 1 subplots into multipanel Figure 1
fig2 <- fig2a + fig2b + fig2c + fig2d +
  plot_layout(nrow = 1)

fig2 +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 20, face = "bold"))


#> Figure 3 - multipanel figure of z-curve plots, and expected
#> discovery rate (EDR) and expected replication rate (ERR) uncertainty
#> (A) Simulation with real effect and no publication bias;
#> (B) Self-controlled; (C) Enhanced expectancies; (D) Combined
#> (Top) Z-curve plots; (Bottom) 95% confidence distributions for EDR and ERR
#>
#> Simulation with real effect and no publication bias
#>
#> Z-curve plot
fig3a <- ggplot(
  sim_z, aes(x = value)
) +
  geom_histogram(fill = "#b5d4cc",
                 color = "black") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 6)
  ) +
  scale_y_continuous(name = "Count",
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
fig3a

#> Self-controlled practice (no outliers)
#>
#> Z-curve plot
fig3b1 <- ggplot(
  z_val_sc, aes(x = value)
) +
  geom_histogram(fill = "#89edff",
                 color = "black") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 6)
                     ) +
  scale_y_continuous(name = "Count",
                     limits = c(0, 15)
                     ) +
  geom_vline(xintercept = 1.96,
             linetype = "solid",
             size = 1) +
  ggtitle(expression(italic("Self-controlled practice"))
  )
fig3b1

#> EDR and ERR uncertainty
fig3b2 <- ggplot(
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
  scale_y_continuous(name = "Count") +
  guides(fill = guide_legend(override.aes = list(size = 0))) +
  geom_vline(xintercept = 0.8,
             linetype = "dashed",
             size = 1) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )
fig3b2

#> Enhanced expectancies (no outliers)
#>
#> Z-curve plot
fig3c1 <- ggplot(
  z_val_nol, aes(x = value)
) +
  geom_histogram(fill = "#ff89a1",
                 color = "black") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = NULL,
                     limits = c(0, 6)
  ) +
  scale_y_continuous(name = "Count",
                     limits = c(0, 10),
                     breaks = seq(0, 10, 5)
  ) +
  geom_vline(xintercept = 1.96,
             linetype = "solid",
             size = 1) +
  ggtitle(expression(italic("Enhanced expectancies"))
  )
fig3c1

#> EDR and ERR uncertainty
fig3c2 <- ggplot(
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
  scale_y_continuous(name = "Count") +
  guides(fill = guide_legend(override.aes = list(size = 0))) +
  geom_vline(xintercept = 0.8,
             linetype = "dashed",
             size = 1) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )
fig3c2

#> Motivational factors combined (no outliers)
#>
#> Z-curve plot
fig3d1 <- ggplot(
  z_val_all_nol, aes(x = value)
) +
  geom_histogram(fill = "#9370f6",
                 color = "black") +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(name = expression(bolditalic("Z") * bold("-value")),
                     limits = c(0, 6)
  ) +
  scale_y_continuous(name = "Count",
                     limits = c(0, 25)
  ) +
  geom_vline(xintercept = 1.96,
             linetype = "solid",
             size = 1) +
  ggtitle(expression(italic("Motivational factors combined"))
  )
fig3d1

#> EDR and ERR uncertainty
fig3d2 <- ggplot(
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
  scale_y_continuous(name = "Count") +
  guides(fill = guide_legend(override.aes = list(size = 0))) +
  geom_vline(xintercept = 0.8,
             linetype = "dashed",
             size = 1) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )
fig3d2

#> Combine figure 2 subplots into multipanel Figure 2
layout <- "
A#
BC
DE
FG
"

fig3 <- fig3a + fig3b1 + fig3b2 + fig3c1 + fig3c2 + fig3d1 + fig3d2 +
  plot_layout(design = layout)

fig3 +
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




