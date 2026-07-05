pacman::p_load(xml2, dplyr, purrr, tibble, readr, stringr, tidyr)

source("R/parsers_functions.R", encoding = "UTF-8")

doc <- read_yed_graphml("resources/musiversum_07.graphml")
ns <- get_yed_namespaces(doc)

nodes <- extract_nodes(doc, ns)
edges <- extract_edges(doc, ns)
graph <- classify_graph_parts(nodes, edges)

legend <- extract_legend(graph)
relations <- extract_relations(graph)
properties <- extract_properties(graph = graph, property_gap_factor = 1.125)

assemble_outputs(
  graph = graph,
  relations = relations,
  properties = properties,
  legend = legend
)
