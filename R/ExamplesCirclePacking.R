library(ggraph)
library(igraph)
library(viridis)
#library(tidyverse)
# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
edges <- flare$edges

# Usually we associate another dataset that give information about each node of the dataset:
vertices <- flare$vertices

# Then we have to make a 'graph' object using the igraph library:
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Make the plot (Hide the first level)
ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[1], "2" = viridis(4)[2], "3" = viridis(4)[3], "4"=viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE") 

# Hide the first 2 levels
ggraph(mygraph, layout = 'circlepack', weight=size) + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = "white", "2" = magma(4)[2], "3" = magma(4)[3], "4"=magma(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "white", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() + 
  theme(legend.position="FALSE")
