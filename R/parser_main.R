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

musiversum <- assemble_outputs(
  graph = graph,
  relations = relations,
  properties = properties,
  legend = legend
)

musiversum_relations <- musiversum$relations |> 
  select(
  source_label,
  source_origin = source_data_element_source,
  relation_label,
  target_label,
  target_origin = target_data_element_source
  ) |> 
  arrange(source_label, relation_label, target_label)

musiversum_properties <- musiversum$node_properties_long |> 
  select(
    owner_label,
    property_label,
    outward_facing
  ) 

write_delim(x = musiversum_relations, file = "resources/musiversum_relations.tsv", delim = "\t")
write_delim(x = musiversum_properties, file = "resources/musiversum_properties.tsv", delim = "\t")
