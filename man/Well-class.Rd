% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plates.R
\name{as.character,Well-method}
\alias{as.character,Well-method}
\alias{show,Well-method}
\alias{Row}
\alias{Row,Well-method}
\alias{Column}
\alias{Column,Well-method}
\title{The row of a well}
\usage{
\S4method{as.character}{Well}(x)

\S4method{show}{Well}(object)

Row(object)

\S4method{Row}{Well}(object)

Column(object)

\S4method{Column}{Well}(object)
}
\arguments{
\item{x}{a \code{\link{Well}} object}

\item{object}{The \code{\link{Well}} object from which column coordinates are extracted.}
}
\value{
A row name in character format.

A column name in character format
}
\description{
Given a well object, tells in which row the well is.

Given a well object, tells in which column the well is.
}
\examples{
well <- Well(well = "A01", plateFormat = "384")
Row(well)

well <- Well("A01")
Column(well)

}
\seealso{
Other Plate functions: 
\code{\link{PlateTypeToWellNames}()},
\code{\link{nextWell}()},
\code{\link{plateWellVolume}()},
\code{\link{seekReagent}()},
\code{\link{setWell}()},
\code{\link{set_block}()},
\code{\link{sourceReagent}()}

Other Plate functions: 
\code{\link{PlateTypeToWellNames}()},
\code{\link{nextWell}()},
\code{\link{plateWellVolume}()},
\code{\link{seekReagent}()},
\code{\link{setWell}()},
\code{\link{set_block}()},
\code{\link{sourceReagent}()}
}
\concept{Plate functions}
