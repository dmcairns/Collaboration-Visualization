library(ggraph)
library(tidygraph)
library(igraph)

# 1. Define parent-child relationships (edges)
edges <- data.frame(
  from = c(
    rep("TAMU", 2), 
    rep("Engineering", 3), 
    rep("Arts & Sciences", 2)
  ),
  to = c(
    "Engineering", "Arts & Sciences", 
    "CSCE", "ECEN", "MEEN", 
    "BIOL", "CHEM"
  )
)

# 2. Define node properties (leaf node values)
nodes <- data.frame(
  name = c("TAMU", "Engineering", "Arts & Sciences", "CSCE", "ECEN", "MEEN", "BIOL", "CHEM"),
  value = c(0, 0, 0, 120, 85, 95, 60, 45) # Values for leaf nodes
)

# 3. Create a graph structure
graph <- tbl_graph(nodes = nodes, edges = edges)

# 4. Plot using ggraph with circlepack layout
ggraph(graph, layout = 'circlepack', weight = value) +
  # Draw nested circles
  geom_node_circle(aes(fill = factor(depth)), color = "white", linewidth = 0.5) +
  # Add text labels for leaf nodes
  geom_node_text(aes(label = name, filter = leaf), size = 3.5, fontface = "bold") +
  # Custom fills and formatting
  scale_fill_brewer(palette = "Set2", guide = "none") +
  theme_void() +
  coord_equal() +
  labs(title = "Hierarchical Circle Packing in R")