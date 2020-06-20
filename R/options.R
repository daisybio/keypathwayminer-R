# Variable, global to package's namespace.  This function is not exported to user space and does not need to be documented.
MYPKGoptions <- settings::options_manager(execution = "Remote",
                                          async = TRUE,
                                          quest_id = NULL,
                                          url = "https://exbio.wzw.tum.de/keypathwayminer/",
                                          algorithm = "Greedy",
                                          graph_id = 1,
                                          strategy = "GLONE",
                                          remove_bens = FALSE,
                                          use_range = FALSE, Kmin = 0, Lmin = 0, Kmax = 0, Lmax = 0, Kstep = 1, Lstep = 1,
                                          l_same_percentage = FALSE, same_percentage = 0, computed_pathways = 10,
                                          with_perturbation = FALSE, unmapped_nodes = "Add to negative list",
                                          linkType = "OR",
                     .allowed = list(execution = settings::inlist("Remote", "Local"),
                                     async = settings::inlist(TRUE, FALSE),
                                     algorithm = settings::inlist("Greedy", "ACO", "Optimal"),
                                     strategy = settings::inlist("INES", "GLONE"),
                                     remove_bens = settings::inlist(TRUE, FALSE),
                                     use_range = settings::inlist(TRUE, FALSE),
                                     l_same_percentage = settings::inlist(TRUE, FALSE),
                                     with_perturbation = settings::inlist(TRUE, FALSE),
                                     linkType = settings::inlist("OR", "AND", "Custom")))


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
