% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_DA.R
\name{profile_pathway}
\alias{profile_pathway}
\title{Run gprofiler2's gost function on pathway of interesst}
\usage{
profile_pathway(result, configuration, pathway = NULL, ...)
}
\arguments{
\item{result}{result object of kpm run}

\item{configuration}{configuration of interest}

\item{pathway}{pathway of interesst}

\item{...}{additional parameters which can be passed to gost function}
}
\value{
data.frame with all results from gost run
}
\description{
Run gprofiler2's gost function on pathway of interesst
}
\examples{
gost_pathway(result = local_example_1, configuration= "K-5-L1-2", pathway= "Pathway-1", organism = "hsapiens", numeric_ns = "ENTREZGENE_ACC")
}
