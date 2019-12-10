
#' Visualize a causal loop diagramm
#'
#' @param concepts_df A dataframe with min. one column specifying all unique concepts in CLD
#' @param id_column Character string specifying based on which column in concepts_df concepts should be linked to links_df
#' @param label_column Optional character string specifying column name based on which to label concepts. Defaults to id_column.
#' @param links_df A dataframe with at least a sender and receiver column specifying links between concepts
#' @param signed If true, links are colored based on sign (+ or -). This means a column called "sign" needs to be provided in links_df
#' @param layout Choose layout algorithm (based on graphviz). Option are "custom_dot", "circo", "dot", "neato", "twopi" or "fdp".
#' If custom_dot is used, you need to specify at least one of each node types "Activity" and "Target" as source and sink nodes
#' @param remove_isolates Should isolates be removed?
#' @param label_size Choose label size. Defaults to 14.
#' @param color_mapping Optional: Provide a named list of color values based on values in type column. Example: list(type_value = color_value)
#' @param shape_mapping Optional: Provide a named list of shape values based on values in type column. Example: list(shape_value = color_value).
#' All graphviz shape can be chose. Defaults to "circle"
#' @param ranksep Rank separation for dot and custom_dot layouts
#' @param type_column Character string specifying the column in concepts_df upon which node type is based. Also used for custom color and shape.
#'
#' @return For custom_dot layout: A list with dot_code and a plot object. For all other layouts: A DiagrammeR graph object and plot object
#' @export
#'
#' @examples
visualize_cld_graph <- function(concepts_df,id_column,label_column, type_column,
                             links_df,
                             signed = TRUE,
                             layout = c("custom_dot","circo","dot","neato","twopi","fdp"),
                             remove_isolates = FALSE,
                             label_size = 14,
                             color_mapping = NULL, shape_mapping = NULL,
                             ranksep = 2){

  if(missing(layout)){
    layout <- "custom_dot"
  }
  if(remove_isolates == TRUE){
    concepts_df <- concepts_df[concepts_df$id %in% unique(unique(links_df$sender,links_df$receiver)),]
  }

  if(missing(label_column)){
    label_column <- as.character(id_column)
  }
  if(missing(type_column)){
    concepts_df$type <- "default"
    type_column <- "type"
  }

  graph <- DiagrammeR::create_graph()

  graph <- do.call(DiagrammeR::add_nodes_from_table, list(
    graph = graph,
    table = concepts_df,
    label_col = as.character(label_column),
    type_col = as.character(type_column))
  )

  graph <- do.call(DiagrammeR::add_edges_from_table,list(
    graph = graph,
    table = links_df,
    from_col = "sender",
    to_col = "receiver",
    from_to_map = as.character(id_column)
    )
  )


  # rendering options for the graph
  #node coloring
  graph_rendering <- graph

  if (is.null(color_mapping)){
    # this is currently custom to OS format for CLDs
    graph_rendering$nodes_df$fillcolor <- "#f44c4150"
  }
  else{
    if (!("type" %in% colnames(concepts_df))){
      stop("No type attribute column provided in concepts_df")
    }
    graph_rendering$nodes_df$fillcolor <- unlist(lapply(concepts_df$type,
                                                        get_entry_from_mapping,
                                                        mapping = color_mapping,
                                                        default = "#f4b24150"))
    if (!(is.null(shape_mapping))){
    graph_rendering$nodes_df$shape <- unlist(lapply(concepts_df$type,
                                                    get_entry_from_mapping,
                                                    mapping = shape_mapping,
                                                    default = "circle"))
    }
  }


  graph_rendering$nodes_df$fontcolor <- "black"

  if(!("sign" %in% colnames(graph_rendering$edges_df)) & signed == TRUE){
    warning("To color based on sign, provide a sign column in concepts_df")
  }

  if("sign" %in% colnames(graph_rendering$edges_df) & signed == TRUE){
    graph_rendering$edges_df$color <- ifelse(graph_rendering$edges_df$sign == "+","#809fff",
                                             ifelse(graph_rendering$edges_df$sign == "-",
                                                    "#ffb3b3","gray80"))
  }
  else{
    graph_rendering$edges_df$color <- "gray80"
  }

  graph_rendering$global_attrs <- rbind(graph_rendering$global_attrs,
                                        c("rankdir","LR","graph"),
                                        c("ranksep",paste(ranksep," equally"),"graph"),
                                        c("fontsize",as.character(label_size),"node"),
                                        c("arrowsize",0.5,"edge"),
                                        c("outputorder","edgesfirst","graph"),
                                        c("pad",3,"graph"))

  graph_rendering$edges_df$edgeURL <- graph_rendering$edges_df$description
  graph_rendering$edges_df$edgetooltip <- graph_rendering$edges_df$description

  graph_rendering$global_attrs[graph_rendering$global_attrs == "layout", 2] <- ifelse(layout != "custom_dot",
                                                                                      as.character(layout),"dot")

  #custom dot code ----

  if(layout == "custom_dot"){
    if(!(("Target" %in% concepts_df$type) &
         ("Activity" %in% concepts_df$type))){
      stop("to use custom_dot layout, specify Target and Activity nodes in concepts_df type column")
    }
    node_df <- DiagrammeR::get_node_df(graph_rendering)
    target_ids <- node_df$id[node_df$type == "Target"]
    activity_ids <- node_df$id[node_df$type == "Activity"]
    rest_ids <- node_df$id[node_df$id %in% c(target_ids,activity_ids)]

    target_ids_pasted <- paste(node_df$id[node_df$type == "Target"],collapse = ";")
    activity_ids_pasted <- paste(node_df$id[node_df$type == "Activity"],collapse = ";")
    rest_ids_pasted <- paste(node_df$id[node_df$id %in% c(target_ids,activity_ids)],collapse = ";")

    dot_out <- DiagrammeR::generate_dot(graph_rendering)

    #remove last closing bracket, paste subgraph info there
    clust_dot <-
      glue::glue(
        "subgraph cluster_0 {{
    label=\"targets\";
    style=\"invis\";
    {target_ids_pasted};
    rank=\"sink\";
    }}
  subgraph cluster_1 {{
    label=\"activities\";
    style=\"invis\";
    {activity_ids_pasted};
    rank=\"source\";
    }}
  subgraph cluster_2 {{
    style=\"invis\";
    {rest_ids_pasted};
    }}")

    dot_out <- gsub(pattern = "}$",
                    replacement = paste(clust_dot,"}"),
                    x = dot_out)
    print("using custom_dot - providing raw dot code output")
    return(list(dot_code = dot_out, plot = DiagrammeR::grViz(dot_out)))
  }
  else{
    rendering <- DiagrammeR::render_graph(graph_rendering)
    return(list(graph = graph_rendering, plot = rendering))
  }
}

#' Save dot code generated in integrater as .svg file
#'
#' @param dot_code Provide dot code generated through eg. visualize_cld_graph or any DiagrammeR function.
#' Use DiagrammeR::generate_dot to create dot output from a DiagrammeR graph object
#' @param file Filename - must end in .svg
#'
#' @return Plots dot code and writes .svg file
#' @export
#'
#' @examples
save_cld_graph_as_svg <- function(dot_code,file){
    dot_output <- gsub("\'","\"",dot_code) #because diagrammR does this in a way atom cannot handle
    DOT::dot(DOT = dot_output,
        file = file)
  }

# lower level function

#' Get a entry for a specifc mapping. Used in visualize_cld_graph mainly.
#'
#' @param entry Entry to get mapping from
#' @param mapping Named list, providing mappings
#' @param default Default value to choose if entry is not in mapping.
#'
#' @return
#' @export
#'
#' @examples
get_entry_from_mapping <- function(entry, mapping, default){
  if(as.character(entry) %in% names(mapping)){
    return(mapping[[as.character(entry)]])
  }
  else{
    warning(paste("No value provided for ",as.character(entry),"in mapping using ",default))
    return(default)
  }
}

# # small tests
#
# test <- visualize_cld_graph(concepts_df = cld_concepts, links_df = cld_el, id_column = "concept", label_column = "english",
#                  layout = "dot",color_mapping = list(Target = "gray"), shape_mapping = list(Activity = "square"))
# test
# save_cld_graph_as_svg(test$dot_code, file = "test.svg")
