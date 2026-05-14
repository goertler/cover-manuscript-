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
  make_long(salmon.response, cover, conclusion)

# reduce to 4 variables, but always have cover and conclusion
d4_a <- df %>%
  make_long(fish.sampling, habitat, cover, conclusion)

d4_b <- df %>%
  make_long(fish.sampling, salmon.response, cover, conclusion)

d4_c <- df %>%
  make_long(habitat, salmon.response, cover, conclusion)

# plot

ggplot(d4_c, aes(x = x, next_x = next_x, node = node, next_node = next_node, label = node, fill = factor(node))) +
  geom_sankey(flow.alpha = 0.5
              , node.color = "black"
              ,show.legend = FALSE)+
  geom_sankey_label(size = 3)+  theme_bw()#+
  scale_fill_viridis_d(option = "inferno")
