# XML / parsing helpers ----
read_yed_graphml <- function(file) {
  read_xml(file)
}

get_yed_namespaces <- function(doc) {
  ns <- xml_ns(doc)
  c(ns, d = "http://graphml.graphdrawing.org/xmlns", y = "http://www.yworks.com/xml/graphml")
}

# Node extraction ----
get_chr_attr <- function(node, attr) {
  value <- xml_attr(node, attr)
  if_else(is.na(value), "", value)
}

is_uppercase_label <- function(x) {
  letters <- str_replace_all(x, "[^[:alpha:]]", "")
  letters != "" & letters == str_to_upper(letters)
}

extract_nodes <- function(doc, ns) {
  nodes <- xml_find_all(doc, ".//d:node", ns)
  
  tibble(
    node_id = xml_attr(nodes, "id"),
    
    node_type = map_chr(nodes, \(node) {
      typed_node <- xml_find_first(node,
                                   ".//y:ShapeNode | .//y:GenericNode | .//y:UMLNoteNode",
                                   ns)
      
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
      if (is.na(value))
        NA_real_
      else
        as.numeric(value)
    }),
    
    y = map_dbl(nodes, \(node) {
      geometry <- xml_find_first(node, ".//y:Geometry", ns)
      value <- xml_attr(geometry, "y")
      if (is.na(value))
        NA_real_
      else
        as.numeric(value)
    }),
    
    width = map_dbl(nodes, \(node) {
      geometry <- xml_find_first(node, ".//y:Geometry", ns)
      value <- xml_attr(geometry, "width")
      if (is.na(value))
        NA_real_
      else
        as.numeric(value)
    }),
    
    height = map_dbl(nodes, \(node) {
      geometry <- xml_find_first(node, ".//y:Geometry", ns)
      value <- xml_attr(geometry, "height")
      if (is.na(value))
        NA_real_
      else
        as.numeric(value)
    }),
    
    shape_type = map_chr(nodes, \(node) {
      shape <- xml_find_first(node, ".//y:Shape", ns)
      get_chr_attr(shape, "type")
    }),
    
    fill_color = map_chr(nodes, \(node) {
      fill <- xml_find_first(node, ".//y:Fill", ns)
      get_chr_attr(fill, "color")
    }),
    
    border_color = map_chr(nodes, \(node) {
      border <- xml_find_first(node, ".//y:BorderStyle", ns)
      get_chr_attr(border, "color")
    }),
    
    border_type = map_chr(nodes, \(node) {
      border <- xml_find_first(node, ".//y:BorderStyle", ns)
      get_chr_attr(border, "type")
    }),
    
    border_width = map_dbl(nodes, \(node) {
      border <- xml_find_first(node, ".//y:BorderStyle", ns)
      value <- xml_attr(border, "width")
      if (is.na(value))
        NA_real_
      else
        as.numeric(value)
    })
  ) |>
    mutate(
      center_x = x + width / 2,
      center_y = y + height / 2,
      fill_rgb = str_sub(fill_color, 1, 7),
      border_rgb = str_sub(border_color, 1, 7),
      
      is_comment_node = node_type == "UMLNoteNode",
      is_label_node = node_type == "GenericNode" &
        is_uppercase_label(node_label),
      is_property_node = node_type == "GenericNode" &
        !is_label_node,
      is_outward_facing = border_rgb == "#FF0000" &
        border_type == "line" &
        border_width == 2
    )
}

# Edge extraction ----
extract_edges <- function(doc, ns) {
  edges <- xml_find_all(doc, ".//d:edge", ns)
  
  tibble(
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
}

# Graph classification ----
classify_graph_parts <- function(nodes, edges) {
  comment_node_ids <- nodes |>
    filter(is_comment_node) |>
    pull(node_id)
  
  edges_no_comments <- edges |>
    filter(!xml_source_id %in% comment_node_ids,
           !xml_target_id %in% comment_node_ids)
  
  connected_node_ids <- edges_no_comments |>
    select(xml_source_id, xml_target_id) |>
    pivot_longer(cols = everything(), values_to = "node_id") |>
    distinct(node_id) |>
    pull(node_id)
  
  list(
    nodes = nodes,
    edges = edges,
    edges_no_comments = edges_no_comments,
    connected_node_ids = connected_node_ids,
    label_nodes = nodes |> filter(is_label_node),
    property_nodes = nodes |> filter(is_property_node),
    comment_nodes = nodes |> filter(is_comment_node)
  )
}

# Relation extraction ----
extract_relations <- function(graph) {
  nodes <- graph$nodes
  edges <- graph$edges_no_comments
  
  label_nodes <- graph$label_nodes |>
    select(label_node_id = node_id, relation_label = node_label)
  
  relation_edges <- edges |>
    filter(arrow_source != "white_circle",
           arrow_target != "white_circle")
  
  label_edges <- bind_rows(
    relation_edges |>
      inner_join(label_nodes, by = c("xml_source_id" = "label_node_id")) |>
      transmute(
        label_node_id = xml_source_id,
        other_node_id = xml_target_id,
        relation_label,
        edge_id,
        arrow_at_other_node = arrow_target
      ),
    
    relation_edges |>
      inner_join(label_nodes, by = c("xml_target_id" = "label_node_id")) |>
      transmute(
        label_node_id = xml_target_id,
        other_node_id = xml_source_id,
        relation_label,
        edge_id,
        arrow_at_other_node = arrow_source
      )
  )
  
  target_sides <- label_edges |>
    filter(arrow_at_other_node == "white_delta") |>
    transmute(label_node_id,
              relation_label,
              target_id = other_node_id,
              target_edge_id = edge_id)
  
  source_sides <- label_edges |>
    anti_join(
      target_sides |> select(label_node_id, target_edge_id),
      by = c("label_node_id", "edge_id" = "target_edge_id")
    ) |>
    transmute(label_node_id,
              source_id = other_node_id,
              source_edge_id = edge_id)
  
  target_sides |>
    inner_join(source_sides, by = "label_node_id") |>
    left_join(nodes |> select(source_id = node_id, source_label = node_label),
              by = "source_id") |>
    left_join(nodes |> select(target_id = node_id, target_label = node_label),
              by = "target_id")
}

# Legend extraction
extract_legend <- function(graph) {
  graph$nodes |>
    filter(
      node_type == "ShapeNode",
      shape_type == "rectangle",
      !node_id %in% graph$connected_node_ids,
      node_label != "",
      fill_rgb != ""
    ) |>
    transmute(fill_rgb, data_element_source = node_label) |>
    distinct()
}

# Property extraction ----
extract_properties <- function(graph, property_gap_factor = 1.125) {
  property_anchors <- extract_property_anchors(graph)
  
  property_candidates <- graph$nodes |>
    filter(is_property_node) |>
    filter(!is_comment_node) |>
    select(
      property_node_id = node_id,
      property_label = node_label,
      property_x = center_x,
      property_y = center_y,
      property_width = width,
      property_height = height,
      is_outward_facing
    )
  
  property_adjacency <- build_property_adjacency(property_candidates = property_candidates,
                                                 property_gap_factor = property_gap_factor)
  
  property_components <- find_property_components(property_anchors = property_anchors,
                                                  property_adjacency = property_adjacency)
  
  properties <- assign_properties_to_owners(
    property_components = property_components,
    property_anchors = property_anchors,
    property_candidates = property_candidates
  )
  
  list(
    property_anchors = property_anchors,
    property_candidates = property_candidates,
    property_adjacency = property_adjacency,
    property_components = property_components,
    node_properties_long = properties$node_properties_long,
    node_properties_wide = properties$node_properties_wide,
    unassigned_property_candidates = properties$unassigned_property_candidates
  )
}

extract_property_anchors <- function(graph) {
  nodes <- graph$nodes
  edges <- graph$edges_no_comments
  
  edges |>
    filter(arrow_source == "white_circle" |
             arrow_target == "white_circle") |>
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
    filter(!is.na(property_anchor_id), !is.na(owner_node_id)) |>
    left_join(nodes |>
                select(owner_node_id = node_id, owner_label = node_label),
              by = "owner_node_id") |>
    left_join(
      nodes |>
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
}

build_property_adjacency <- function(property_candidates,
                                     property_gap_factor = 1.125) {
  property_boxes_are_adjacent <- function(a, b) {
    dx <- abs(a$property_x - b$property_x)
    dy <- abs(a$property_y - b$property_y)
    
    typical_width <- mean(c(a$property_width, b$property_width), na.rm = TRUE)
    
    typical_height <- mean(c(a$property_height, b$property_height), na.rm = TRUE)
    
    same_column <- dx <= property_gap_factor * typical_width &&
      dy <= property_gap_factor * typical_height * 1.5
    
    same_row_or_nearby_column <- dy <= property_gap_factor * typical_height &&
      dx <= property_gap_factor * typical_width * 1.5
    
    same_column | same_row_or_nearby_column
  }
  
  property_candidates |>
    rename_with(\(x) paste0("a_", x)) |>
    crossing(property_candidates |>
               rename_with(\(x) paste0("b_", x))) |>
    filter(a_property_node_id != b_property_node_id) |>
    rowwise() |>
    mutate(adjacent = property_boxes_are_adjacent(
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
    )) |>
    ungroup() |>
    filter(adjacent) |>
    transmute(from = a_property_node_id, to = b_property_node_id)
}

find_property_components <- function(property_anchors, property_adjacency) {
  find_component <- function(start_node_id, adjacency_tbl) {
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
  
  property_anchors |>
    rowwise() |>
    mutate(property_node_ids = list(find_component(
      property_anchor_id, property_adjacency
    ))) |>
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
}

assign_properties_to_owners <- function(property_components,
                                        property_anchors,
                                        property_candidates) {
  node_properties_long <- property_components |>
    left_join(property_candidates, by = "property_node_id") |>
    left_join(property_anchors |>
                select(property_anchor_id, anchor_x, anchor_y),
              by = "property_anchor_id") |>
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
      ),
      outward_facing = if_else(
        property_node_id != property_anchor_id & is_outward_facing,
        "Y",
        "N"
      )
    ) |>
    arrange(owner_label, property_y, property_x) |>
    select(
      owner_node_id,
      owner_label,
      property_node_id,
      property_label,
      outward_facing,
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
  
  list(
    node_properties_long = node_properties_long,
    node_properties_wide = node_properties_wide,
    unassigned_property_candidates = unassigned_property_candidates
  )
}

# Final assembly ----
assemble_outputs <- function(graph, relations, properties, legend) {
  node_data_sources <- graph$nodes |>
    select(node_id, node_fill_rgb = fill_rgb) |>
    left_join(legend, by = c("node_fill_rgb" = "fill_rgb"))
  
  relations_final <- relations |>
    left_join(
      node_data_sources |>
        select(
          source_id = node_id,
          source_data_element_source = data_element_source
        ),
      by = "source_id"
    ) |>
    left_join(
      node_data_sources |>
        select(
          target_id = node_id,
          target_data_element_source = data_element_source
        ),
      by = "target_id"
    )
  
  relation_with_properties <- relations_final |>
    left_join(
      properties$node_properties_long |>
        group_by(source_id = owner_node_id) |>
        summarise(
          source_properties = paste(property_label, collapse = " | "),
          .groups = "drop"
        ),
      by = "source_id"
    ) |>
    left_join(
      properties$node_properties_long |>
        group_by(target_id = owner_node_id) |>
        summarise(
          target_properties = paste(property_label, collapse = " | "),
          .groups = "drop"
        ),
      by = "target_id"
    )
  
  list(
    nodes = graph$nodes,
    edges = graph$edges,
    edges_without_comments = graph$edges_no_comments,
    relations = relations_final,
    node_properties_long = properties$node_properties_long,
    node_properties_wide = properties$node_properties_wide,
    relation_with_properties = relation_with_properties,
    legend = legend,
    unassigned_property_candidates = properties$unassigned_property_candidates
  )
}

# extract_yed_graph <- function(file) {
#   doc <- read_yed_graphml(file)
#   ns <- get_yed_namespaces(doc)
#   
#   nodes <- extract_nodes(doc, ns)
#   edges <- extract_edges(doc, ns)
#   
#   graph <- classify_graph_parts(nodes, edges)
#   relations <- extract_relations(graph)
#   properties <- extract_properties(graph)
#   legend <- extract_legend(graph)
#   
#   assemble_outputs(
#     graph = graph,
#     relations = relations,
#     properties = properties,
#     legend = legend
#   )
# }
