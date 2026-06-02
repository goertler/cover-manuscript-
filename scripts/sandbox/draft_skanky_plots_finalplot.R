##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 5/08/2026
# Description: draft sankey plots for author discussion
##########################################################

# library
#install.packages("remotes")
#remotes::install_github("davidsjoberg/ggsankey")
# library
library(ggsankey)
library(ggplot2)
library(dplyr)
library(showtext) # AFS font
showtext_auto()

# data
df <- read.csv("outcome_ids.csv")
head(df)
str(df)

d <- df %>%
  make_long(fish.sampling, habitat, cover, conclusion, salmon.response)

# reduce to 3 variables, but always have cover and conclusion
d3_a <- df %>%
  make_long(fish.sampling, cover, conclusion)

d3_b <- df %>%
  make_long(habitat, cover, conclusion)

d3_c <- df %>%
  make_long(cover, salmon.response, conclusion)

# reduce to 4 variables, but always have cover and conclusion
d4_a <- df %>%
  make_long(fish.sampling, habitat, cover, conclusion)

d4_b <- df %>%
  make_long(fish.sampling, salmon.response, cover, conclusion)

d4_c <- df %>%
  make_long(habitat, salmon.response, cover, conclusion)

# plot

ggplot(d3_c, aes(x = x, next_x = next_x, node = node, next_node = next_node, label = node, fill = factor(node))) +
  geom_sankey(flow.alpha = 0.5
              , node.color = "black"
              ,show.legend = FALSE)+
  geom_sankey_label(size = 3, color = 1, fill = "white")+
  theme_bw()+
  scale_fill_brewer(palette = "Set2")
  #scale_fill_viridis_d(option = "plasma", alpha = 0.8)+
  theme_sankey(base_size = 16) +
  theme(legend.position = "none")

#custom colors
png("sankey_plot.png", width = 6, height = 5, units = "in", res = 300)

ggplot(d3_c, aes(x = x, next_x = next_x, node = node, next_node = next_node, label = node, fill = factor(node))) +
geom_sankey(flow.alpha = 0.6,          # Makes the linking flows semi-transparent
            node.color = "gray30") +   # Outlines the nodes cleanly
  scale_fill_manual(values = c(
    "addition" = "#CC79A7",
    "gradient" = "#CECECE",
    "instream" = "#7D7D7D",
    "structure" = "#E69F00",
    "substrate category" = "#009E73",
    "substrate size" = "#ABABAB",
    "visibility" = "#D55E00",
    "no cover" = "#EBEBEB",
    "density" = "#56B4E9",
    "growth" = "#999999",
    "survival " = "orchid3",
    "capacity" = "#F0E442",
    "preference" = "#0072B2",
    "not juvenile salmon" = "#EBEBEB",
    "abundance" = "#b3ebff",
    "not related"  = "#0072B2",
    "not included"  = "#EBEBEB",
    "not evaluated" = "#EBEBEB",
    "model assumptions" = "#CECECE",
    "positive" = "#56B4E9",
    "negative" = "#F0E442",
    "habitat extent" ="white",
    "temperature" ="white",
    "bed stability" = "white",
    "velocity" = "white"
  )) +
  theme_sankey(base_size = 16)+
  geom_sankey_label(size = 6, color = 1, family = "Helvetica") +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  labs(x = NULL)

dev.off()
