library(igraph)
library(tidygraph)
library(dplyr)
library(RColorBrewer)
library(classInt)


nodes <- read.table(url("https://github.com/cjbarrie/CS-ED/blob/main/data/all.nodes.txt?raw=true"))

edges <- read.table(url("https://github.com/cjbarrie/CS-ED/blob/main/data/all.edgelist.txt?raw=true"))


nodes %>% 
  group_by(V2) %>%
  count() 

library(tidylog)

colnames(nodes) <- c("node1", "lr")
colnames(edges) <- c("node1", "node2", "type", "n", "id")
rt_samp <- edges %>%
  left_join(nodes, by = "node1") %>%
  filter(type == "retweet")

rt_samp$node1 <- as.character(rt_samp$node1)
rt_samp$node2 <- as.character(rt_samp$node2)

igraph_rt_samp <- graph_from_edgelist(
  as.matrix(rt_samp[,c("node1","node2")]),
  directed = T
)

plot(simplify(igraph_rt_samp), 
     vertex.label = NA, 
     vertex.size = 2,
     edge.arrow.size = 0)

samp_attr <- data.frame(
  node = V(igraph_rt_samp)$name,
  node.seq = 1:length(V(igraph_rt_samp)$name)
  # ,
  # degree.in = degree(igraph_rt_samp, mode = "in"), #unhash this to also estimate indegree
  # between.dir = betweenness(igraph_rt_samp, directed = T,normalized = T), #unhash this to also estimate betweenness
  # between.undir = betweenness(igraph_rt_samp, directed = F, normalized = T) #unhash this to also estimate betweenness (undirected)
)

nodes$node <- as.character(nodes$node)
nodes <- nodes %>%
  mutate(lrcolor = recode(lr,
                          "right" = "#DE0100",
                          "left" = "#0015BC",
                          "-" = "#a6a49f")
  )

samp_attr_lr <- samp_attr %>%
  left_join(nodes, by = "node")

plot(simplify(igraph_rt_samp), 
     vertex.label = NA, 
     vertex.size = 4,
     vertex.color = samp_attr_lr$lrcolor,
     edge.arrow.size = 0,
     edge.width=seq(1,10),
     edge.arrow.width=1)

