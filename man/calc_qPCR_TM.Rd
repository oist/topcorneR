% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qPCR.R
\name{calc_qPCR_TM}
\alias{calc_qPCR_TM}
\title{Compute melting temperature from raw data}
\usage{
calc_qPCR_TM(mlt, cut.Area = 1)
}
\arguments{
\item{mlt}{A table providing the raw data loaded by \code{\link[=load_qPCR_meltcurve]{load_qPCR_meltcurve()}}.}

\item{cut.Area}{Parameter passed directly to \code{\link[qpcR:meltcurve]{qpcR::meltcurve()}}.}
}
\value{
A vector of melting temperatures in the same order as the wells of
the \code{mlt} table.
}
\description{
Although the qPCR machines output melting temperatures in their report files,
it is also possible to compute them using exported raw data.  This function
automates running \code{\link[qpcR:meltcurve]{qpcR::meltcurve()}} for the computation in a quiet way.
}
\seealso{
Other qPCR functions: 
\code{\link{calc_qPCR_mod}()},
\code{\link{load_qPCR_results}()}
}
\concept{qPCR functions}
