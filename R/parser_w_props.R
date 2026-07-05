library(xml2)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)
library(tidyr)

extract_yed_graph <- function(
    file,
    property_distance_x = 250,
    property_distance_y = 120
) {
  doc <- read_xml(file)
  
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
    }),
    
    x = map_dbl(nodes, \(node) {
      geometry <- xml_find_first(node, ".//y:Geometry", ns)
      value <- xml_attr(geometry, "x")
      if (is.na(value)) NA_real_ else as.numeric(value)
    }),
    
    y = map_dbl(nodes, \(node) {
      geometry <- xml_find_first(node, ".//y:Geometry", ns)
      value <- xml_attr(geometry, "y")
      if (is.na(value)) NA_real_ else as.numeric(value)
    }),
    
    width = map_dbl(nodes, \(node) {
      geometry <- xml_find_first(node, ".//y:Geometry", ns)
      value <- xml_attr(geometry, "width")
      if (is.na(value)) NA_real_ else as.numeric(value)
    }),
    
    height = map_dbl(nodes, \(node) {
      geometry <- xml_find_first(node, ".//y:Geometry", ns)
      value <- xml_attr(geometry, "height")
      if (is.na(value)) NA_real_ else as.numeric(value)
    })
  ) |>
    mutate(
      center_x = x + width / 2,
      center_y = y + height / 2,
      
      label_letters = str_replace_all(node_label, "[^[:alpha:]]", ""),
      
      is_label_node = label_letters != "" &
        label_letters == str_to_upper(label_letters)
    )
  
  # ---- Edges ----
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
  )
  
  label_nodes <- node_tbl |>
    filter(is_label_node) |>
    select(
      label_node_id = node_id,
      relation_label = node_label
    )
  
  # ============================================================
  # 1. RELATIONS:
  #    A -- LABEL --> B
  #    where the final target side has white_delta.
  #    Ignore white_circle edges for relations.
  # ============================================================
  
  relation_edge_tbl <- edge_tbl |>
    filter(
      arrow_source != "white_circle",
      arrow_target != "white_circle"
    )
  
  label_edges <- bind_rows(
    relation_edge_tbl |>
      inner_join(label_nodes, by = c("xml_source_id" = "label_node_id")) |>
      transmute(
        label_node_id = xml_source_id,
        other_node_id = xml_target_id,
        relation_label,
        edge_id,
        label_is_xml_source = TRUE,
        arrow_at_label_node = arrow_source,
        arrow_at_other_node = arrow_target
      ),
    
    relation_edge_tbl |>
      inner_join(label_nodes, by = c("xml_target_id" = "label_node_id")) |>
      transmute(
        label_node_id = xml_target_id,
        other_node_id = xml_source_id,
        relation_label,
        edge_id,
        label_is_xml_source = FALSE,
        arrow_at_label_node = arrow_target,
        arrow_at_other_node = arrow_source
      )
  )
  
  target_sides <- label_edges |>
    filter(arrow_at_other_node == "white_delta") |>
    transmute(
      label_node_id,
      relation_label,
      target_id = other_node_id,
      target_edge_id = edge_id
    )
  
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
  
  relations <- target_sides |>
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
  
  # ============================================================
  # 2. EXPLICIT PROPERTY ANCHORS:
  #    A --white_circle-- PROPERTY_NODE
  #
  #    Drawing direction may be reversed, so we inspect which physical
  #    end has the white_circle arrow.
  # ============================================================
  
  white_circle_edges <- edge_tbl |>
    filter(
      arrow_source == "white_circle" |
        arrow_target == "white_circle"
    ) |>
    mutate(
      # The node at the white_circle end is the property node.
      property_node_id = case_when(
        arrow_source == "white_circle" ~ xml_source_id,
        arrow_target == "white_circle" ~ xml_target_id,
        TRUE ~ NA_character_
      ),
      
      owner_node_id = case_when(
        arrow_source == "white_circle" ~ xml_target_id,
        arrow_target == "white_circle" ~ xml_source_id,
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(property_node_id), !is.na(owner_node_id))
  
  explicit_property_anchors <- white_circle_edges |>
    left_join(
      node_tbl |>
        select(
          owner_node_id = node_id,
          owner_label = node_label
        ),
      by = "owner_node_id"
    ) |>
    left_join(
      node_tbl |>
        select(
          property_node_id = node_id,
          property_anchor_label = node_label,
          property_anchor_x = center_x,
          property_anchor_y = center_y
        ),
      by = "property_node_id"
    ) |>
    select(
      owner_node_id,
      owner_label,
      property_node_id,
      property_anchor_label,
      property_anchor_x,
      property_anchor_y,
      property_edge_id = edge_id
    )
  
  # ============================================================
  # 3. EDGELESS PROPERTY NODES:
  #    These are unconnected nodes near the explicit property anchor.
  # ============================================================
  
  connected_node_ids <- edge_tbl |>
    select(xml_source_id, xml_target_id) |>
    pivot_longer(
      cols = everything(),
      values_to = "node_id"
    ) |>
    distinct(node_id) |>
    pull(node_id)
  
  edgeless_nodes <- node_tbl |>
    filter(!node_id %in% connected_node_ids) |>
    filter(!is_label_node) |>
    select(
      edgeless_property_node_id = node_id,
      edgeless_property_label = node_label,
      edgeless_x = center_x,
      edgeless_y = center_y
    )
  
  nearby_edgeless_properties <- explicit_property_anchors |>
    crossing(edgeless_nodes) |>
    mutate(
      dx = abs(edgeless_x - property_anchor_x),
      dy = abs(edgeless_y - property_anchor_y),
      distance = sqrt(dx^2 + dy^2)
    ) |>
    filter(
      dx <= property_distance_x,
      dy <= property_distance_y
    ) |>
    group_by(edgeless_property_node_id) |>
    slice_min(distance, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(
      owner_node_id,
      owner_label,
      property_node_id = edgeless_property_node_id,
      property_label = edgeless_property_label,
      property_source = "nearby_edgeless_node",
      distance
    )
  
  explicit_properties <- explicit_property_anchors |>
    transmute(
      owner_node_id,
      owner_label,
      property_node_id,
      property_label = property_anchor_label,
      property_source = "white_circle_edge",
      distance = 0
    )
  
  node_properties_long <- bind_rows(
    explicit_properties,
    nearby_edgeless_properties
  ) |>
    arrange(owner_label, property_source, distance, property_label)
  
  # Optional wide version: property_1, property_2, ...
  node_properties_wide <- node_properties_long |>
    group_by(owner_node_id, owner_label) |>
    mutate(property_number = row_number()) |>
    ungroup() |>
    select(owner_node_id, owner_label, property_number, property_label) |>
    pivot_wider(
      names_from = property_number,
      values_from = property_label,
      names_prefix = "property_"
    )
  
  # Add source/target properties to relations as list-style strings
  relation_with_properties <- relations |>
    left_join(
      node_properties_long |>
        group_by(source_id = owner_node_id) |>
        summarise(
          source_properties = paste(property_label, collapse = " | "),
          .groups = "drop"
        ),
      by = "source_id"
    ) |>
    left_join(
      node_properties_long |>
        group_by(target_id = owner_node_id) |>
        summarise(
          target_properties = paste(property_label, collapse = " | "),
          .groups = "drop"
        ),
      by = "target_id"
    )
  
  list(
    nodes = node_tbl,
    edges = edge_tbl,
    relations = relations,
    node_properties_long = node_properties_long,
    node_properties_wide = node_properties_wide,
    relation_with_properties = relation_with_properties
  )
}

# Example usage
yed <- extract_yed_graph("resources/musiversum_07.graphml")

relations <- yed$relations |> arrange(source_label, relation_label)
node_properties <- yed$node_properties_long
relations_with_properties <- yed$relation_with_properties

print(relations_with_properties)

write_csv(relations, "yed_relations.csv")
write_csv(node_properties, "yed_node_properties.csv")
write_csv(relations_with_properties, "yed_relations_with_properties.csv")
