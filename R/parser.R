library(xml2)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)

extract_yed_relations <- function(file) {
  doc <- read_xml(file)
  
  # Register common GraphML / yEd namespaces
  ns <- xml_ns(doc)
  ns <- c(
    ns,
    d = "http://graphml.graphdrawing.org/xmlns",
    y = "http://www.yworks.com/xml/graphml"
  )
  
  # ---- Nodes ----
  nodes <- xml_find_all(doc, ".//d:node", ns)
  
  node_tbl <- tibble(
    node_id = xml_attr(nodes, "id"),
    node_label = map_chr(nodes, \(node) {
      label <- xml_find_first(node, ".//y:NodeLabel", ns) |> xml_text()
      
      if (is.na(label) || str_trim(label) == "") {
        xml_attr(node, "id")
      } else {
        str_squish(label)
      }
    })
  ) |>
    mutate(
      label_letters = str_replace_all(node_label, "[^[:alpha:]]", ""),
      is_label_node = label_letters != "" &
        label_letters == str_to_upper(label_letters)
    )
  
  # ---- Raw edges ----
  edges <- xml_find_all(doc, ".//d:edge", ns)
  
  edge_tbl <- tibble(
    edge_id = xml_attr(edges, "id"),
    xml_source_id = xml_attr(edges, "source"),
    xml_target_id = xml_attr(edges, "target"),
    
    arrow_source = map_chr(edges, \(edge) {
      arrows <- xml_find_first(edge, ".//y:Arrows", ns)
      value <- xml_attr(arrows, "source")
      if (is.na(value)) "" else value
    }),
    
    arrow_target = map_chr(edges, \(edge) {
      arrows <- xml_find_first(edge, ".//y:Arrows", ns)
      value <- xml_attr(arrows, "target")
      if (is.na(value)) "" else value
    })
  ) |>
    # Original rule: skip edges whose XML target end is white_circle
    filter(
      arrow_source != "white_circle",
      arrow_target != "white_circle"
    )
  
  label_nodes <- node_tbl |>
    filter(is_label_node) |>
    select(
      label_node_id = node_id,
      relation_label = node_label
    )
  
  # ---- Edges touching label nodes ----
  label_edges <- bind_rows(
    # Label node is the XML source
    edge_tbl |>
      inner_join(label_nodes, by = c("xml_source_id" = "label_node_id")) |>
      transmute(
        label_node_id = xml_source_id,
        other_node_id = xml_target_id,
        relation_label,
        edge_id,
        label_is_xml_source = TRUE,
        arrow_at_label_node = arrow_source,
        arrow_at_other_node = arrow_target,
        arrow_source,
        arrow_target
      ),
    
    # Label node is the XML target
    edge_tbl |>
      inner_join(label_nodes, by = c("xml_target_id" = "label_node_id")) |>
      transmute(
        label_node_id = xml_target_id,
        other_node_id = xml_source_id,
        relation_label,
        edge_id,
        label_is_xml_source = FALSE,
        arrow_at_label_node = arrow_target,
        arrow_at_other_node = arrow_source,
        arrow_source,
        arrow_target
      )
  )
  
  # ---- Semantic target side ----
  # The final target node is the non-label node whose side of the edge
  # has the incoming white_delta arrow.
  target_sides <- label_edges |>
    filter(arrow_at_other_node == "white_delta") |>
    transmute(
      label_node_id,
      relation_label,
      target_id = other_node_id,
      target_edge_id = edge_id
    )
  
  # ---- Semantic source side ----
  # The source node is the other non-label node touching the same label node.
  # This works regardless of whether the source-label edge was drawn
  # source -> label or label -> source.
  source_sides <- label_edges |>
    filter(arrow_at_other_node != "white_delta") |>
    anti_join(
      target_sides |> select(label_node_id, target_edge_id),
      by = c("label_node_id", "edge_id" = "target_edge_id")
    ) |>
    transmute(
      label_node_id,
      source_id = other_node_id,
      source_edge_id = edge_id
    )
  
  target_sides |>
    inner_join(source_sides, by = "label_node_id") |>
    left_join(
      node_tbl |> select(source_id = node_id, source_label = node_label),
      by = "source_id"
    ) |>
    left_join(
      node_tbl |> select(target_id = node_id, target_label = node_label),
      by = "target_id"
    ) |>
    select(
      source_id,
      source_label,
      relation_label,
      target_id,
      target_label,
      label_node_id,
      source_edge_id,
      target_edge_id
    )
}

# Example usage
relations <- extract_yed_relations("resources/musiversum_07.graphml")

# print(relations)

# write_csv(relations, "yed_relations.csv")
