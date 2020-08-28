
#' Translate multi-level statnet network object to networkx (python) object
#'
#' @param g statnet network object
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in statnet object.
#' @param relabel should nodes be relabeled with statnet vertex.names? (defaults
#'   to TRUE)
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (NULL) this is determined automatically using network::is.directed.
#'
#' @return python graph object
#' @export
#'
#' @examples
#' toPyGraph(motifr::dummy_net, lvl_attr = "sesType")
toPyGraph <- function(g, lvl_attr, relabel = TRUE, directed = NULL) {

  # function for translating a statnet network object into a Python compatible
  # networkx object
  adjacencyMatrix <- network::as.matrix.network(g)
  attributeNames <- network::list.vertex.attributes(g)
  attributeValues <- lapply(
    network::list.vertex.attributes(g),
    function(x) network::get.vertex.attribute(g, x)
  )

  if(is.null(directed)){
    directed = network::is.directed(g)
  } else if(directed == TRUE && ! network::is.directed(g)) {
    warning("Attempting to treat an undirected network as a directed network. This might lead to unintended results.")
  }

  py_g <- sma$translateGraph(adjacencyMatrix,
                             attributeNames,
                             attributeValues,
                             lvl_attr,
                             directed = directed)

  if (relabel == TRUE) {
    # JS: renaming in here right now, but will suggest update to rbridge.py to do in Python
    node_names <-
      reticulate::py_dict(
        keys = as.integer(0:(py_g$number_of_nodes() - 1)),
        values = network::get.vertex.attribute(g, "vertex.names")
      )
    py_g <- nx$relabel_nodes(py_g, mapping = node_names)
  }
  return(py_g)
}

#' Display all supported motifs with signature ``1,2``.
#'
#' @return Opens image
#' @export
#'
#' @examples
#' show_3_motifs()
show_3_motifs <- function() {
  magick::image_read(path = system.file("motif_reference",
    "motif_reference_3motifs.png",
    package = utils::packageName()
  ))
}

#' Display all supported motifs with signature ``2,2``.
#'
#' @return Opens figure from Ö. Bodin, M. Tengö: Disentangling intangible
#'   social–ecological systems in Global Environmental Change 22 (2012) 430–439
#'   http://dx.doi.org/10.1016/j.gloenvcha.2012.01.005
#' @export
#'
#' @examples
#' show_4_motifs()
show_4_motifs <- function() {
  magick::image_read(path = system.file("motif_reference",
    "motif_reference_4motifs.jpg",
    package = utils::packageName()
  ))
}

#' Count multi-level motifs
#'
#' @param net A statnet network object with a node attribute specifying the
#'   level of each node
#' @param motifs a list of motif identifiers which shall be counted, e.g.
#'   list("1,2[I.C]")
#' @param lvl_attr character vector specifying the vertex attribute name where
#'   level information is stored in statnet object
#' @param assume_sparse whether the network shall be assumed to be sparse (for
#'   optimization), default TRUE
#' @param omit_total_result whether total results shall be omitted, default
#'   FALSE
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (NULL) this is determined automatically using network::is.directed.
#'
#' @return data frame with a column containing motif identifier strings and one
#'   column containing motif counts
#' @export
#'
#' @examples
#' count_motifs(ml_net, lvl_attr = c("sesType"), motifs = list("1,2[I.C]", "1,2[II.C]", "2,1[I.C]", "2,1[II.C]"), directed = FALSE)
count_motifs <- function(net,
                         motifs,
                         lvl_attr = c("sesType"),
                         assume_sparse = TRUE,
                         omit_total_result = TRUE,
                         directed = NULL) {
  # convert net to python object
  py_g <- motifr::toPyGraph(net,
                            lvl_attr = lvl_attr,
                            directed = directed)

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
#' Warning: Variances can only be computed for 1,2 motifs at the moment.
#'
#' This function supports the Erdős-Rényi Model (``erdos_renyi``) and the the
#' Actor’s Choice Model (``actors_choice``). The model can be specified using
#' the ``model`` parameter. The Erdős-Rényi Model can be used without providing
#' further parameters. In case of the Actor’s Choice Model a level of the given
#' network can be specified which is only level assumed to be variable. All
#' other levels are assumed to be fixed. Per default, ``level = -1``, the first
#' level carrying two nodes in the signature of the motif is selected as
#' variable level. Set the ``level`` paramter to the value of the ``lvl_attr``
#' of the nodes in the desired level to specify the level manually.
#'
#' @param net statnet network object
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in statnet object.
#' @param motifs list of motif identifiers describing the motifs whose
#'   distribution shall be analysed
#' @param model model to be used. Options are 'erdos_renyi', or 'actors_choice'.
#'   See vignette "random_baselines" for more details. Defaults to
#'   'erdos_renyi'.
#' @param level Additional parameter to set the level to vary for the
#'   actors_choice model manually. All other levels are held fixed.
#' @param omit_total_result whether total results shall be omitted
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (NULL) this is determined automatically using network::is.directed.
#'
#' @return data frame with one column giving names of motif identifers and two
#'   column giving expectation and variances per motif. At the moment, variances
#'   are only computed for 1,2 motifs. For other motifs, expectations are
#'   computed but variances are returned as NaN.
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
  if (!(model %in% c("erdos_renyi", "actors_choice"))) {
    stop(paste(model, " is not supported. Choose one of 'erdos_renyi' or 'fixed_densities'.
               To use the fixed densities model, for the moment see simulate_baseline()."))
  }

  # convert net to python object
  py_g <- motifr::toPyGraph(net,
                            lvl_attr = lvl_attr,
                            directed = directed)

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

#' Summary for motif counts and distribution (Erdos-Renyi)
#'
#' @param net statnet network object
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in statnet object.
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
  motifs <- c("1,2[I.C]", "1,2[II.C]", "2,1[I.C]", "2,1[II.C]", "2,2[III.C]", "2,2[III.D]")
  # all motifs are undirected, hence set directed = FALSE and consider graph as undirected

  # count and compute distribution parameters
  counts <- motifr::count_motifs(net,
                                 lvl_attr,
                                 motifs = motifs,
                                 omit_total_result = TRUE,
                                 directed = FALSE)
  distribution <- motifr::motifs_distribution(net,
                                              lvl_attr,
                                              motifs = motifs,
                                              omit_total_result = TRUE,
                                              directed = FALSE)

  # reformat data
  result <- merge(counts, distribution)
  return(result)
}

#' Returns an example for a motif found in a given network
#'
#' @param net statnet network object
#' @param motif motif identifier string for the motif
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in statnet object.
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (NULL) this is determined automatically using network::is.directed.
#'
#' @return vector of nodes in the motif
#' @seealso show_motif
#' @export
#'
#' @examples
#' exemplify_motif(ml_net, motif = "1,2[I.C]", directed = FALSE)
exemplify_motif <- function(net,
                            motif,
                            lvl_attr = "sesType",
                            directed = NULL) {
  # convert net to python object
  py_g <- motifr::toPyGraph(net, lvl_attr = lvl_attr, directed = directed)
  motif <- sma$exemplifyMotif(py_g, motif)
  return(purrr::simplify(motif))
}

#' Plots an example for a motif with given motif identifier string taken from
#' the given graph.
#'
#' If no network is provided, a motif in a dummy network will be shown.
#'
#' @param motif motif identifier string for the motif
#' @param net statnet network object
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in statnet object.
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (NULL) this is determined automatically using network::is.directed.
#' @param ... additional arguments to be passed to plotting function (eg. label
#'   = TRUE)
#' @return plot
#' @seealso exemplify_motif
#' @export
#'
#' @examples
#' show_motif("1,2[I.C]", net = ml_net, directed = FALSE)
show_motif <- function(motif,
                       net = motifr::dummy_net,
                       lvl_attr = c("sesType"),
                       directed = NULL,
                       ...) {
  motif_names <- motifr::exemplify_motif(
    net = net, motif = motif,
    lvl_attr = lvl_attr,
    directed = directed
  )
  vertices <- network::get.vertex.attribute(net, "vertex.names")
  indices <- sapply(motif_names, function(x) {
    match(x, vertices)
  })
  subgraph <- network::get.inducedSubgraph(net, indices)
  p <- motifr::plot_mnet(subgraph, lvl_attr = lvl_attr, ...)
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
#' the variable level automatically. Use the ``level`` parameter to provide a
#' valid level.
#'
#' When using ERGM the paramter ``net`` is not used. Random networks are sampled
#' in R using the ``ergm_model`` parameter.
#'
#' @param net statnet network object
#' @param motifs list of motif identifier strings
#' @param n number of random graphs
#' @param lvl_attr character string specifying the attribute name where level
#'   information is stored in statnet object.
#' @param assume_sparse whether the random graphs shall be assumed to be sparse.
#'   used to find ideal counting function. defaults to TRUE.
#' @param model baseline model to be used. Options are 'erdos_renyi',
#'   'fixed_densities'. See vignette "random_baselines" for more details.
#'   Defaults to 'erdos_renyi'.
#' @param level lvl_attr of the variable level for the Actor's Choice model
#' @param ergm_model ergm model as for example fitted by calling
#'   ``ergm::ergm()``. Used when model is set to ergm to sample random networks.
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (NULL) this is determined automatically using network::is.directed.
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
  if (!(model %in% c("erdos_renyi", "fixed_densities", "actors_choice", "ergm"))) {
    stop(paste(model, " is not supported. Choose one of 'erdos_renyi' or 'fixed_densities'.
               To use the actors_choice model, for the moment see motifs_distribution to
               calculate expected mean and variances analytically."))
  }
  if (model == "ergm") {
    if (!requireNamespace("ergm", quietly = TRUE)) {
      stop("Package \"ergm\" needed for this function to work. Please install it.",
        call. = FALSE
      )
    }
    if (!ergm::is.ergm(ergm_model)) {
      stop("Please provde a valid ergm model when using ERGM")
    }
    # let's do the job ourselves
    result <- data.frame()
    for (i in 0:n) {
      sample <- stats::simulate(ergm_model)
      counts <- motifr::count_motifs(sample,
                                     motifs = motifs,
                                     lvl_attr = lvl_attr,
                                     assume_sparse = assume_sparse,
                                     directed = directed)
      result <- rbind(result, counts$count)
      if (i == 0) {
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
    py_g <- motifr::toPyGraph(net,
                              lvl_attr = lvl_attr,
                              directed = directed)

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
#' the variable level automatically. Use the ``level`` parameter to provide a
#' valid level.
#'
#' When using ERGM the paramter ``net`` is not used. Random networks are sampled
#' in R using the ``ergm_model`` parameter.
#'
#' @param net statnet network object
#' @param motifs list of motif identifier strings
#' @param n number of random graphs
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in statnet object.
#' @param assume_sparse whether the random graphs shall be assumed to be sparse.
#'   used to find ideal counting function
#' @param model baseline model to be used. Options are 'erdos_renyi' and
#'   'fixed_densities'. See vignette "random_baselines" for more details.
#'   Defaults to 'erdos_renyi'.
#' @param level lvl_attr of the variable level for the Actor's Choice model
#' @param ergm_model ergm model as for example fitted by calling
#'   ``ergm::ergm()``. Used when model is set to ergm to sample random network
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (NULL) this is determined automatically using network::is.directed.
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
    ggplot2::ggplot(plot_df, ggplot2::aes(value)) +
    ggplot2::facet_wrap(~motif, scales = "free") +
    ggplot2::geom_histogram(fill = "gray", bins = 50) +
    ggplot2::geom_vline(data = count, ggplot2::aes(xintercept = count)) +
    ggplot2::theme_minimal() +
    ggplot2::xlab(sprintf("Simulated (gray histogram) versus actual (solid line) motif counts, n = %d iterations, model %s", n, model))

  return(p)
}

#' Returns a dataframe with one row for each instance of the motif specified by
#' the given motif identifier string. If the identifier string specifies a motif
#' class, e.g. ``1,2[I.A]`` , then only motifs of the given class are returned.
#' If the identifier string specifies a signature, e.g. ``1,2``, then a full
#' list of all motifs of this signature is returned. In the latter case, the
#' dataframe contains an additional column stating the classes of the motifs.

#' The naming scheme of the columns is as follows: Each column is called
#' ``levelA_nodeB`` where ``A`` is the ``sesType`` of the nodes in the column
#' and ``B`` the index of the nodes amoung the nodes on the same level. This
#' index stems from the internal order of the nodes and does not carry any
#' specific meaning. in R using the ``ergm_model`` parameter.
#'
#' @param net statnet network object
#' @param identifier motif identifier string (with or without class, see above)
#' @param lvl_attr character vector specifying the attribute name where level
#'   information is stored in statnet object.
#' @param directed whether the graph shall be treated as a directed graph. Per
#'   default (NULL) this is determined automatically using network::is.directed.
#'
#' @return data frame with one row for each motif
#' @export
#'
#' @examples head(list_motifs(ml_net, "1,2[I.C]", directed = FALSE))
list_motifs <- function(net,
                        identifier,
                        lvl_attr = "sesType",
                        directed = NULL) {
  py_g <- toPyGraph(net, lvl_attr = lvl_attr, directed = directed)
  df <- sma$motifTable(py_g, identifier)
  return(df)
}
