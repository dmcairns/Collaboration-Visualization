# 1. Load required packages
# Install if needed: install.packages(c("tidyverse", "tidygraph", "ggraph"))
library(tidyverse)
library(tidygraph)
library(ggraph)

# Set seed for reproducible network layout
set.seed(42)

# 2. Create sample college data (Nodes)
colleges <- tibble(
  id = 1:5,
  name = c("Engineering", "Arts & Sciences", "Medicine", "Business", "Education"),
  total_proposals = c(120, 95, 150, 45, 30) # Controls circle size
)

# 3. Create collaborative proposals data (Edges)
collaborations <- tibble(
  from = c(1, 1, 1, 2, 2, 3), # Matching college IDs
  to   = c(2, 3, 4, 3, 5, 5),
  co_proposals = c(18, 25, 6, 12, 4, 9) # Controls line width
)

# 4. Convert data to a graph object
graph_data <- tbl_graph(nodes = colleges, edges = collaborations, directed = FALSE)

# 5. Build the network visualization
ggraph(graph_data, layout = "stress") + 
  # Render edges (collaborations)
  geom_edge_link(
    aes(width = co_proposals),
    color = "gray60",
    alpha = 0.7
  ) +
  # Render nodes (colleges)
  geom_node_point(
    aes(size = total_proposals),
    color = "#2b5c8f",
    alpha = 0.9
  ) +
  # Add college labels
  geom_node_text(
    aes(label = name),
    repel = TRUE,          # Prevents text overlap
    fontface = "bold",
    size = 4
  ) +
  # Adjust width and size scales
  scale_edge_width_continuous(
    name = "Collaborative\nProposals",
    range = c(0.8, 4)      # Min and max line thickness
  ) +
  scale_size_continuous(
    name = "Total Proposals\nSubmitted",
    range = c(6, 20)       # Min and max circle radius
  ) +
  # Clean up plot aesthetics
  theme_graph() +
  labs(
    title = "Inter-College Grant Collaboration Network",
    subtitle = "Node size indicates total college proposals; line width indicates joint proposals."
  )