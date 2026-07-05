library(xml2)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)
library(tidyr)

extract_yed_graph <- function(
    file,
    property_max_dx = 450,
    property_max_dy = 320
) {
  doc <- read_xml(file)
  
  ns <- xml_ns(doc)
  ns <- c(
    ns,
    d = "http://graphml.graphdrawing.org/xmlns",
    y = "http://www.yworks.com/xml/graphml"
  )
  
  get_chr_attr <- function(x, attr) {
    value <- xml_attr(x, attr)
    ifelse(is.na(value), "", value)
  }
  
  is_uppercase_label <- function(x) {
    letters <- str_replace_all(x, "[^[:alpha:]]", "")
    letters != "" & letters == str_to_upper(letters)
  }
  
  # ---- Nodes ----
  nodes <- xml_find_all(doc, ".//d:node", ns)
  
  node_tbl <- tibble(
    node_id = xml_attr(nodes, "id"),
    
    node_type = map_chr(nodes, \(node) {
      typed_node <- xml_find_first(
        node,
        ".//y:ShapeNode | .//y:GenericNode | .//y:UMLNoteNode",
        ns
      )
      
      if (inherits(typed_node, "xml_missing")) {
        NA_character_
      } else {
        xml_name(typed_node)
      }
    }),
    
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
      is_comment_node = node_type == "UMLNoteNode",
      is_label_node = node_type == "GenericNode" & is_uppercase_label(node_label),
      is_property_node = node_type == "GenericNode" & !is_label_node
    )
  
  comment_node_ids <- node_tbl |>
    filter(is_comment_node) |>
    pull(node_id)
  
  # ---- Edges ----
  edges <- xml_find_all(doc, ".//d:edge", ns)
  
  edge_tbl <- tibble(
    edge_id = xml_attr(edges, "id"),
    xml_source_id = xml_attr(edges, "source"),
    xml_target_id = xml_attr(edges, "target"),
    
    arrow_source = map_chr(edges, \(edge) {
      arrows <- xml_find_first(edge, ".//y:Arrows", ns)
      get_chr_attr(arrows, "source")
    }),
    
    arrow_target = map_chr(edges, \(edge) {
      arrows <- xml_find_first(edge, ".//y:Arrows", ns)
      get_chr_attr(arrows, "target")
    })
  )
  
  # Ignore comment nodes and edges to/from comment nodes
  edge_tbl_no_comments <- edge_tbl |>
    filter(
      !xml_source_id %in% comment_node_ids,
      !xml_target_id %in% comment_node_ids
    )
  
  # 1. RELATIONS ----
  
  label_nodes <- node_tbl |>
    filter(is_label_node) |>
    select(
      label_node_id = node_id,
      relation_label = node_label
    )
  
  relation_edge_tbl <- edge_tbl_no_comments |>
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
  
  # 2. EXPLICIT PROPERTY ANCHORS ----
  
  white_circle_edges <- edge_tbl_no_comments |>
    filter(
      arrow_source == "white_circle" |
        arrow_target == "white_circle"
    ) |>
    mutate(
      property_anchor_id = case_when(
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
    filter(!is.na(property_anchor_id), !is.na(owner_node_id))
  
  property_anchors <- white_circle_edges |>
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
          property_anchor_id = node_id,
          property_anchor_label = node_label,
          anchor_x = center_x,
          anchor_y = center_y
        ),
      by = "property_anchor_id"
    ) |>
    select(
      owner_node_id,
      owner_label,
      property_anchor_id,
      property_anchor_label,
      anchor_x,
      anchor_y,
      property_edge_id = edge_id
    )
  
  # 3. PROPERTY GROUPING BY LOCAL BOX ADJACENCY ----
  
  property_candidates <- node_tbl |>
    filter(is_property_node) |>
    filter(!is_comment_node) |>
    select(
      property_node_id = node_id,
      property_label = node_label,
      property_x = center_x,
      property_y = center_y,
      property_width = width,
      property_height = height
    )
  
  # Helper: are two property boxes close enough to be in the same group?
  # Uses your rule of thumb:
  # - vertical stack gap <= 1.125 * typical height
  # - adjacent column gap <= 1.125 * typical width
  property_boxes_are_adjacent <- function(a, b, gap_factor = 1.125) {
    dx <- abs(a$property_x - b$property_x)
    dy <- abs(a$property_y - b$property_y)
    
    typical_width <- mean(c(a$property_width, b$property_width), na.rm = TRUE)
    typical_height <- mean(c(a$property_height, b$property_height), na.rm = TRUE)
    
    same_column <- dx <= gap_factor * typical_width &&
      dy <= gap_factor * typical_height * 1.5
    
    same_row_or_nearby_column <- dy <= gap_factor * typical_height &&
      dx <= gap_factor * typical_width * 1.5
    
    same_column | same_row_or_nearby_column
  }
  
  # Build property-property adjacency table
  property_adjacency <- property_candidates |>
    rename_with(\(x) paste0("a_", x)) |>
    crossing(
      property_candidates |>
        rename_with(\(x) paste0("b_", x))
    ) |>
    filter(a_property_node_id != b_property_node_id) |>
    rowwise() |>
    mutate(
      adjacent = property_boxes_are_adjacent(
        tibble(
          property_x = a_property_x,
          property_y = a_property_y,
          property_width = a_property_width,
          property_height = a_property_height
        ),
        tibble(
          property_x = b_property_x,
          property_y = b_property_y,
          property_width = b_property_width,
          property_height = b_property_height
        )
      )
    ) |>
    ungroup() |>
    filter(adjacent) |>
    transmute(
      from = a_property_node_id,
      to = b_property_node_id
    )
  
  # Small base-R flood-fill over the adjacency table
  find_property_component <- function(start_node_id, adjacency_tbl) {
    visited <- character()
    frontier <- start_node_id
    
    while (length(frontier) > 0) {
      current <- frontier[[1]]
      frontier <- frontier[-1]
      
      if (current %in% visited) {
        next
      }
      
      visited <- c(visited, current)
      
      neighbours <- adjacency_tbl |>
        filter(from == current) |>
        pull(to)
      
      frontier <- unique(c(frontier, setdiff(neighbours, visited)))
    }
    
    visited
  }
  
  # Attach each white-circle anchor to its local component of nearby properties.
  # Since the white-circle anchor itself is a property node, it is the seed.
  anchor_components <- property_anchors |>
    rowwise() |>
    mutate(
      property_node_ids = list(
        find_property_component(property_anchor_id, property_adjacency)
      )
    ) |>
    ungroup() |>
    select(
      owner_node_id,
      owner_label,
      property_anchor_id,
      property_anchor_label,
      property_node_ids
    ) |>
    unnest(property_node_ids) |>
    rename(property_node_id = property_node_ids)
  
  # If two anchors somehow reach the same property, keep the one whose anchor is closest.
  node_properties_long <- anchor_components |>
    left_join(
      property_candidates,
      by = "property_node_id"
    ) |>
    left_join(
      property_anchors |>
        select(
          property_anchor_id,
          anchor_x,
          anchor_y
        ),
      by = "property_anchor_id"
    ) |>
    mutate(
      dx = abs(property_x - anchor_x),
      dy = abs(property_y - anchor_y),
      distance = sqrt(dx^2 + dy^2)
    ) |>
    group_by(property_node_id) |>
    slice_min(distance, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      property_source = if_else(
        property_node_id == property_anchor_id,
        "white_circle_anchor",
        "adjacent_property_node"
      )
    ) |>
    arrange(owner_label, property_y, property_x) |>
    select(
      owner_node_id,
      owner_label,
      property_node_id,
      property_label,
      property_source,
      property_anchor_id,
      property_anchor_label,
      dx,
      dy,
      distance
    )
  
  unassigned_property_candidates <- property_candidates |>
    anti_join(node_properties_long, by = "property_node_id")
  
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
    edges_without_comments = edge_tbl_no_comments,
    relations = relations,
    node_properties_long = node_properties_long,
    node_properties_wide = node_properties_wide,
    relation_with_properties = relation_with_properties,
    unassigned_property_candidates = unassigned_property_candidates
  )
}

yed <- extract_yed_graph("resources/musiversum_07.graphml")

yed$relations
yed$node_properties_long
yed$relation_with_properties
# 
# write_csv(yed$relations, "yed_relations.csv")
# write_csv(yed$node_properties_long, "yed_node_properties.csv")
# write_csv(yed$relation_with_properties, "yed_relations_with_properties.csv")