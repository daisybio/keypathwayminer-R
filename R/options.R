# Variable, global to package's namespace.  This function is not exported to user space and does not need to be documented.
MYPKGoptions <- settings::options_manager(execution = "Remote",
                                          async = TRUE,
                                          quest_id = NULL,
                                          url = "https://exbio.wzw.tum.de/keypathwayminer/",
                                          algorithm = "Greedy",
                                          graph_id = 1,
                                          strategy = "GLONE",
                                          remove_bens = FALSE,
                                          use_range_k = FALSE, k_min = 1, k_max = 3, k_step = 1,
                                          use_range_l = FALSE, l_min =0, l_max = 0, l_step = 1,
                                          l_same_percentage = FALSE, same_percentage = 0,
                                          computed_pathways = 20,
                                          perturbation_start = 10,
                                          perturbation_step = 10,
                                          perturbation_max = 20,
                                          graphs_per_step = 1,
                                          with_perturbation = FALSE,
                                          perturbation_technique = "nodeswap",
                                          unmapped_nodes = "Add to negative list",
                                          link_type = "OR",
                                          resource_folder = "/extdata",
                                          properties_file = "kpm.properties",
                           .allowed = list(execution = settings::inlist("Remote", "Local"),
                                          async = settings::inlist(TRUE, FALSE),
                                          algorithm = settings::inlist("Greedy", "ACO", "Optimal"),
                                          strategy = settings::inlist("INES", "GLONE"),
                                          unmapped_nodes = settings::inlist("Add to negative list", "Add to positive list"),
                                          remove_bens = settings::inlist(TRUE, FALSE),
                                          use_range_k = settings::inlist(TRUE, FALSE),
                                          use_range_l = settings::inlist(TRUE, FALSE),
                                          l_same_percentage = settings::inlist(TRUE, FALSE),
                                          same_percentage = settings:: inrange(0, 100),
                                          perturbation_start = settings:: inrange(0, 100),
                                          perturbation_step = settings:: inrange(0, 100),
                                          perturbation_max = settings:: inrange(0, 100),
                                          with_perturbation = settings::inlist(TRUE, FALSE),
                                          perturbation_technique = settings::inlist('edgeremove', 'edgerewire', 'nodeswap'),
                                          link_type = settings::inlist("OR", "AND")))

#' Set or get options for my package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{a}}{(\code{numeric};1) The value of a }
#'  \item{\code{b}}{(\code{numeric};2) The value of b }
#' }
#'
#' @export
kpm_options <- function(...) {

    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    MYPKGoptions(...)
}
