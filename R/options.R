# Variable, global to package's namespace.
# This function is not exported to user space and
# does not need to be documented.
MYPKGoptions <- settings::options_manager(execution = "Remote",
                                          async = TRUE,
                                          session_id = NULL,
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
                                          link_type = settings::inlist("OR", "AND","CUSTOM")))

#' Set or get options for KeyPathwayMineR
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section I. Options for remote and local use:
#' \itemize{
#'  \item{\code{execution}}{ (\code{"Local" or "Remote"}) Execution type of KeyPathwayMineR. Either via RestfulAPI or standalobe jar}
#'  \item{\code{algorithm}}{ (\code{"Greedy", "ACO", "Optimal"}) The algorithm that will be used to extract the pathways}
#'  \item{\code{strategy}}{ (\code{"GLONE", "INES"}) The strategy that will be used to extract pathways}
#'  \item{\code{remove_bens}}{ (\code{Boolean}) If TRUE border exception nodes will be removed}
#'  \item{\code{computed_pathways}}{ (\code{Integer}) The number of solutions that should be computed}
#'  \item{\code{link_type}}{ (\code{"OR", "AND","CUSTOM"}) Define how multiple datasets should be combined in the analysis}
#'  }
#' \strong{Gene exceptions K (only used for INES):}
#' \itemize{
#'  \item{\code{use_range_k}}{ (\code{Boolean}) If TRUE k_values will be ranged (batch run). If false the user only needs to set k_min.}
#'  \item{\code{k_min}}{ (\code{Integer}) Starting value of k range or k value if k is not ranged}
#'  \item{\code{k_step}}{ (\code{Integer}) How k should be increased within the range}
#'  \item{\code{k_max}}{ (\code{Integer}) The maximum k value, i.e. the upper limit of the range}
#'  }
#' \strong{The case (L) exceptions for the n-th matrix:}
#' \itemize{
#'  \item{\code{use_range_l}}{ (\code{Boolean}) If TRUE l_values will be ranged (batch run). If false the user only needs to set l_min}
#'  \item{\code{l_min}}{ (\code{Integer or Vector})  Starting value of l range or l value if l is not ranged}
#'  \item{\code{l_step}}{ (\code{Integer or Vector}) How l should be increased within the range}
#'  \item{\code{l_max}}{ (\code{Integer or Vector}) The maximum l value, i.e. the upper limit of the range}
#'}
#'
#' @section II. Parameters only for remote execution:
#' \itemize{
#'  \item{\code{async}}{ (\code{Boolean}) \cr \code{TRUE =} Submit job asynchronously (returns immediately)
#'                                        \cr \code{FALSE =} Submit a new job am (returns when job is complete)}
#'  \item{\code{session_id}}{ (\code{String}) The id of the job to be queried}
#'  \item{\code{url}}{ (\code{String}) URL for HTTP request}
#'  \item{\code{graph_id}}{ (\code{Integer}) The id of one of the networks that are available when you call get_networks()}
#'  \item{\code{unmapped_nodes}}{ (\code{"Add to negative list", "Add to positive list"}) String What should be done with unmapped nodes, i.e. should they be added
#'   to the positive list (always included in solutions) or to the negative list (categorically ignored)}
#'   \item{\code{l_same_percentage}}{ (\code{Boolean}) If multiple datasets are defined, should the same l value be used for all of them}
#'   \item{\code{same_percentage}}{ (\code{Percentage 0:100}) The l percentage value that should be used for multiple datasets}
#' }
#'
#' @section III. Parameters only for local execution:
#' \itemize{
#'   \item{\code{resource_folder}}{ (\code{String}) Path to folder with example datasets}
#'   \item{\code{properties_file}}{ (\code{String}) Path to properties file}
#'   }
#' \strong{Options for network robustness analysis:}
#' \itemize{
#'   \item{\code{with_perturbation}}{ (\code{Boolean}) Set \code{TRUE} to perform a robustness analysis}
#'   \item{\code{perturbation_technique}}{ (\code{'edgeremove', 'edgerewire', 'nodeswap'}) Perturbation technique to use for the analysis}
#'   \item{\code{perturbation_start}}{ (\code{Integer})  Perturbation percentage range lower value}
#'   \item{\code{perturbation_step}}{ (\code{Integer}) Perturbation percentage step size}
#'   \item{\code{perturbation_max}}{ (\code{Integer}) Perturbation percentage range upper value}
#'   \item{\code{graphs_per_step}}{ (\code{Integer}) Number of random graphs to be created (permutations)}
#' }
#'
#' @details \strong{Important note:} in the \strong{local} execution the user can specify for every
#' matrix a specific or ranged l-value.\cr\cr
#'  \strong{Example 1: } One matrix and not ranged:\cr\code{kpm_options(l_min=4)}\cr\cr
#'  \strong{Example 2: } Two matrices and not ranged:\cr\code{kpm_options(l_min=c(1,2))}\cr\cr
#'  \strong{Example 3: } Three matrices and ranged:\cr\code{kpm_options(l_min=c(1,2,4), l_step=c(1,1,2), l_max=c(2,3,8))}\cr\cr
#' The \emph{n-th} position in each vector corresponds to the \emph{n-th} matrix.\cr\cr
#' Note: The \strong{web service} does not allow individual fixed parameters to be set for each dataset at the moment.
#' @export
kpm_options <- function(...) {
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    MYPKGoptions(...)
}
