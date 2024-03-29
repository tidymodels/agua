% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tune.R
\name{agua_backend_options}
\alias{agua_backend_options}
\title{Control model tuning via \code{\link[h2o:h2o.grid]{h2o::h2o.grid()}}}
\usage{
agua_backend_options(parallelism = 1)
}
\arguments{
\item{parallelism}{Level of Parallelism during grid model building. 1 = sequential building (default).
Use the value of 0 for adaptive parallelism - decided by H2O. Any number > 1 sets the exact number of models built in parallel.}
}
\description{
Control model tuning via \code{\link[h2o:h2o.grid]{h2o::h2o.grid()}}
}
