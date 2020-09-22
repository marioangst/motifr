#' Count multi-level motifs
#'
#' @param net A network object with a node attribute specifying the
#'   level of each node
#' @param motifs a list of motif identifiers which shall be counted, e.g.
#'   \code{list("1,2[I.C]")}
#' @param lvl_attr character vector specifying the vertex attribute name where
#'   level information is stored in \code{net}
#' @param assume_sparse whether the network shall be assumed to be sparse (for
#'   optimization), default TRUE
#' @param omit_total_result whether total results shall be omitted, default
#'   FALSE
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (\code{NULL}), this is determined automatically using the structure
#'   of the provided network object
#'
#' @return data frame with a column containing motif identifier strings and one
#'   column containing motif counts
#' @export
#'
#' @examples
#' count_motifs(ml_net,
#'   lvl_attr = c("sesType"),
#'   motifs = list("1,2[I.C]", "1,2[II.C]", "2,1[I.C]", "2,1[II.C]"),
#'   directed = FALSE
#' )
count_motifs <- function(net,
                         motifs,
                         lvl_attr = c("sesType"),
                         assume_sparse = TRUE,
                         omit_total_result = TRUE,
                         directed = NULL) {
  # convert net to python object
  py_g <- motifr::to_py_graph(net,
    lvl_attr = lvl_attr,
    directed = directed
  )

  # make sure motifs is list
  motifs <- as.list(motifs)

  # call counter
  counted <- sma$countMotifsAutoR(py_g,
    motifs,
    assume_sparse = assume_sparse,
    omit_total_result = omit_total_result
  )
  df <- data.frame(motif = names(counted), count = unlist(counted))
  return(df)
}

#' Compute statistical properties (expectation and variance) of the distribution
#' of motifs in a random baseline
#'
#' This function supports the Erdős-Rényi Model (\code{erdos_renyi}) and the the
#' Actor’s Choice Model (\code{actors_choice}). The model can be specified using
#' the \code{model} parameter. The Erdős-Rényi Model can be used without
#' providing further parameters. In case of the Actor’s Choice Model a level of
#' the given network can be specified which is only level assumed to be
#' variable. All other levels are assumed to be fixed. Per default, \code{level
#' = -1}, the first level carrying two nodes in the signature of the motif is
#' selected as variable level. Set the \code{level} parameter to the value of
#' the \code{lvl_attr} of the nodes in the desired level to specify the level
#' manually.
#'
#' @param net network object
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in \code{net}.
#' @param motifs list of motif identifiers describing the motifs whose
#'   distribution shall be analysed
#' @param model model to be used
#' @param level Additional parameter to set the level to vary for the
#'   actors_choice model manually. All other levels are held fixed.
#' @param omit_total_result whether total results shall be omitted
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (\code{NULL}), this is determined automatically using the structure
#'   of the provided network object
#'
#' @return data frame with one column giving names of motif identifers and two
#'   column giving expectation and variances per motif. For other motifs,
#'   expectations are computed but variances are returned as NaN.
#' @export
#'
#' @examples
#' motifs_distribution(ml_net, motif = list("1,2[I.C]"), directed = FALSE)
motifs_distribution <- function(net,
                                motifs,
                                lvl_attr = "sesType",
                                model = "erdos_renyi",
                                level = -1,
                                omit_total_result = TRUE,
                                directed = NULL) {
  supported_models <- c("erdos_renyi", "actors_choice")
  if (!(model %in% supported_models)) {
    stop(paste(
      "Model", model, "is not supported. Choose one of",
      paste(supported_models, collapse = ", "),
      "or use simulate_baseline()."
    ))
  }

  # convert net to python object
  py_g <- motifr::to_py_graph(net,
    lvl_attr = lvl_attr,
    directed = directed
  )

  # make sure motifs is list
  motifs <- as.list(motifs)

  # call counter
  result <- sma$distributionMotifsAutoR(py_g,
    motifs,
    model = model,
    level = level,
    omit_total_result = omit_total_result
  )
  df <- data.frame(
    motif = names(result),
    expectation = unlist(result[1, ]),
    variance = unlist(result[2, ])
  )
  return(df)
}

#' Summary for motif counts and Erdős-Rényi distribution
#'
#' Returns a data frame with counts and statistical properties (expectation,
#' variances) of six selected motifs in the given network. Note that this
#' function implicitly assumes that the network is undirected, cf.
#' \code{motifr::to_py_graph}.
#'
#' @param net network object
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in \code{net}.
#'
#' @return dataframe with motif counts, expectations and variances for set of
#'   selected motifs
#' @export
#'
#' @examples
#' motif_summary(ml_net)
motif_summary <- function(net,
                          lvl_attr = c("sesType")) {
  # exquisite selection of motifs
  # all motifs are undirected, hence set directed = FALSE and suppose graph is
  # undirected
  motifs <- c(
    "1,2[I.C]", "1,2[II.C]",
    "2,1[I.C]", "2,1[II.C]",
    "2,2[III.C]", "2,2[III.D]"
  )

  # count and compute distribution parameters
  counts <- motifr::count_motifs(net,
    lvl_attr,
    motifs = motifs,
    omit_total_result = TRUE,
    directed = FALSE
  )
  distribution <- motifr::motifs_distribution(net,
    lvl_attr,
    motifs = motifs,
    omit_total_result = TRUE,
    directed = FALSE
  )

  # reformat data
  result <- merge(counts, distribution)
  return(result)
}

#' Returns an example for a motif found in a given network
#'
#' @param net network object
#' @param motif motif identifier string for the motif
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in \code{net}.
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (\code{NULL}), this is determined automatically using the structure
#'   of the provided network object
#'
#' @return vector of nodes in the motif
#' @seealso \code{motifr::show_motif}
#' @export
#'
#' @examples
#' exemplify_motif(ml_net, motif = "1,2[I.C]", directed = FALSE)
exemplify_motif <- function(net,
                            motif,
                            lvl_attr = "sesType",
                            directed = NULL) {
  # convert net to python object
  py_g <- motifr::to_py_graph(net, lvl_attr = lvl_attr, directed = directed)
  motif <- sma$exemplifyMotif(py_g, motif)
  return(purrr::simplify(motif))
}

#' Plots an example for a motif with given motif identifier string taken from
#' the given graph.
#'
#' If no network is provided, a motif in a dummy network
#' (\code{motifr::dummy_net} or \code{motifr::directed_dummy_net}) will be
#' shown.
#'
#' @param motif motif identifier string for the motif
#' @param net network object
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in \code{net}.
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (\code{NULL}), this is determined automatically using the structure
#'   of the provided network object
#' @param ... additional arguments to be passed to plotting function (eg.
#'   \code{label = TRUE})
#' @return plot
#' @seealso \code{motifr::exemplify_motif}
#' @export
#'
#' @examples
#' show_motif("1,2[I.C]", net = ml_net, directed = FALSE, label = TRUE)
show_motif <- function(motif,
                       net = NULL,
                       lvl_attr = c("sesType"),
                       directed = NULL,
                       ...) {
  if (is.null(net)) {
    if (is.null(directed) || directed == FALSE) {
      net <- motifr::dummy_net
    } else {
      net <- motifr::directed_dummy_net
    }
  }
  if(igraph::is.igraph(net)){
    net <- intergraph::asNetwork(net)
  }
  motif_names <- motifr::exemplify_motif(
    net = net, motif = motif,
    lvl_attr = lvl_attr,
    directed = directed
  )
  vertices <- network::get.vertex.attribute(net, "vertex.names")
  indices <- match(motif_names, vertices)
  subgraph <- network::get.inducedSubgraph(net, indices)
  p <- motifr::plot_mnet(subgraph,
    lvl_attr = lvl_attr, directed = directed,
    ...
  )
  return(p)
}

#' Simulate a random baseline
#'
#' A specified number of random networks using a modified Erdős-Rényi model is
#' computed. In each of the random networks motifs are counted. A dataframe with
#' these counts is returned. Optionally, other models than the modified
#' Erdős-Rényi model can be used.
#'
#' Note that when using the Actor's Choice model this function does not choose
#' the variable level automatically. Use the \code{level} parameter to provide a
#' valid level.
#'
#' When using ERGM the parameter \code{net} is not used. Random networks are
#' sampled in R using the \code{ergm_model} parameter.
#'
#' @param net network object
#' @param motifs list of motif identifier strings
#' @param n number of random graphs
#' @param lvl_attr character string specifying the attribute name where level
#'   information is stored in \code{net}.
#' @param assume_sparse whether the random graphs shall be assumed to be sparse.
#'   used to find ideal counting function. defaults to TRUE.
#' @param model baseline model to be used. Options are 'erdos_renyi',
#'   'fixed_densities'. See \code{vignette("random_baselines")} for more
#'   details. Defaults to 'erdos_renyi'.
#' @param level lvl_attr of the variable level for the Actor's Choice model
#' @param ergm_model ergm model as for example fitted by calling
#'   \code{ergm::ergm()}. Used when model is set to ergm to sample random
#'   networks.
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (\code{NULL}), this is determined automatically using the structure
#'   of the provided network object
#'
#' @return data frame with one column for each motif identifier string and one
#'   row for every computed random graph
#' @export
#'
#' @examples
#' simulate_baseline(ml_net, list("1,2[I.C]"), n = 10, directed = FALSE)
simulate_baseline <- function(net,
                              motifs,
                              n = 10,
                              lvl_attr = "sesType",
                              assume_sparse = TRUE,
                              model = "erdos_renyi",
                              level = -1,
                              ergm_model = NULL,
                              directed = NULL) {
  # make sure motifs is list
  motifs <- as.list(motifs)

  supported_models <- c(
    "erdos_renyi", "fixed_densities", "actors_choice",
    "ergm"
  )
  if (!(model %in% supported_models)) {
    stop(paste(
      "Model", model, "is not supported. Choose one of",
      paste(supported_models, collapse = ", ")
    ))
  }
  if (model == "ergm") {
    if (!requireNamespace("ergm", quietly = TRUE)) {
      stop("Package \"ergm\" needed for this function to work. Please install it with install.packages(ergm).",
        call. = FALSE
      )
    }
    if (!ergm::is.ergm(ergm_model)) {
      stop("Please provde a valid ergm model when using ERGM.")
    }
    # let's do the job ourselves
    result <- data.frame()
    for (i in 1:n) {
      sample <- stats::simulate(ergm_model)
      counts <- motifr::count_motifs(sample,
        motifs = motifs,
        lvl_attr = lvl_attr,
        assume_sparse = assume_sparse,
        directed = directed
      )
      result <- rbind(result, counts$count)
      if (i == 1) {
        colnames(result) <- counts$motif
      }
    }
    return(result)
  } else {
    # let sma do the job
    if (model == "actors_choice") {
      if (level < 0) {
        stop("Please provide a valid level when using an Actor's Choice model")
      }
    }
    py_g <- motifr::to_py_graph(net,
      lvl_attr = lvl_attr,
      directed = directed
    )

    result <- sma$simulateBaselineAutoR(py_g,
      motifs,
      n = n,
      assume_sparse = assume_sparse,
      model = model,
      level = level
    )
    df <- data.frame(result, check.names = FALSE)
    return(df)
  }
}

#' Compare empirical network to random baseline
#'
#' This function compares the motif counts in a given network with the motif
#' counts in a random baseline of motifs.
#'
#' Note that when using the Actor's Choice model this function does not choose
#' the variable level automatically. Use the \code{level} parameter to provide a
#' valid level.
#'
#' When using ERGM the parameter \code{net} is not used. Random networks are sampled
#' in R using the \code{ergm_model} parameter.
#'
#' @param net network object
#' @param motifs list of motif identifier strings
#' @param n number of random graphs
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in \code{net}.
#' @param assume_sparse whether the random graphs shall be assumed to be sparse.
#'   used to find ideal counting function
#' @param model baseline model to be used. Options are 'erdos_renyi' and
#'   'fixed_densities'. See \code{vignette("random_baselines")} for more details.
#'   Defaults to 'erdos_renyi'.
#' @param level lvl_attr of the variable level for the Actor's Choice model
#' @param ergm_model ergm model as for example fitted by calling
#'   \code{ergm::ergm()}. Used when model is set to ergm to sample random network
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (\code{NULL}), this is determined automatically using the structure
#'   of the provided network object
#'
#' @return data frame with one row for each motif identifier string and one row
#'   for every computed random graph
#' @export
#'
#' @examples
#' compare_to_baseline(ml_net, list("1,2[I.C]", "1,2[II.C]"), directed = FALSE)
compare_to_baseline <- function(net,
                                motifs,
                                n = 10,
                                lvl_attr = "sesType",
                                assume_sparse = TRUE,
                                model = "erdos_renyi",
                                level = -1,
                                ergm_model = NULL,
                                directed = NULL) {
  simulation <- motifr::simulate_baseline(net,
    motifs,
    n = n,
    lvl_attr = lvl_attr,
    assume_sparse = assume_sparse,
    model = model,
    level = level,
    ergm_model = ergm_model,
    directed = directed
  )
  count <- motifr::count_motifs(net,
    motifs,
    lvl_attr = lvl_attr,
    assume_sparse = assume_sparse,
    omit_total_result = TRUE,
    directed = directed
  )

  plot_df <- suppressMessages(reshape2::melt(simulation, variable.name = "motif"))
  # plot_df_count <- suppressMessages(reshape2::melt(count))

  p <-
    ggplot2::ggplot(plot_df, ggplot2::aes_(x = ~value)) +
    ggplot2::facet_wrap(~motif, scales = "free") +
    ggplot2::geom_histogram(fill = "gray", bins = 50) +
    ggplot2::geom_vline(data = count, ggplot2::aes_(xintercept = ~count)) +
    ggplot2::theme_minimal() +
    ggplot2::xlab(paste(
      "Simulated (gray histogram) versus actual (solid line) motif counts,",
      sprintf("n = %d iterations, model %s", n, model)
    ))

  return(p)
}

#' Lists motifs of a given class or all motifs with a given signature
#'
#' Returns a dataframe with one row for each instance of the motif specified by
#' the given motif identifier string. If the identifier string specifies a motif
#' class, e.g. \code{1,2[I.A]}, then only motifs of the given class are returned.
#' If the identifier string specifies a signature, e.g. \code{1,2}, then a full
#' list of all motifs of this signature is returned. In the latter case, the
#' dataframe contains an additional column stating the classes of the motifs.

#' The naming scheme of the columns is as follows: Each column is called
#' \code{levelA_nodeB} where \code{A} is the \code{lvl_attr} of the nodes in the column
#' and \code{B} the index of the nodes among the nodes on the same level. This
#' index stems from the internal order of the nodes and does not carry any
#' specific meaning.
#'
#' @param net network object
#' @param identifier motif identifier string (with or without class, see above)
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in \code{net}.
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (\code{NULL}), this is determined automatically using the structure
#'   of the provided network object
#'
#' @return data frame with one row for each motif
#' @export
#'
#' @examples
#' head(list_motifs(ml_net, "1,2[I.C]", directed = FALSE))
list_motifs <- function(net,
                        identifier,
                        lvl_attr = "sesType",
                        directed = NULL) {
  py_g <- to_py_graph(net, lvl_attr = lvl_attr, directed = directed)
  df <- sma$motifTable(py_g, identifier)
  return(df)
}
