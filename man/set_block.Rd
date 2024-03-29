% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plates.R
\name{set_block}
\alias{set_block}
\alias{set_block,Plate,character,character,numeric-method}
\title{Set values in rectangular areas of a plate}
\usage{
set_block(plate, block, what, value, add = FALSE)

\S4method{set_block}{Plate,character,character,numeric}(plate, block, what, value, add = FALSE)
}
\arguments{
\item{plate}{A \code{\link{Plate}} object representing a multiwell plate.}

\item{block}{Coordinates of a rectangular block (such as \dQuote{A01~B02}),
or a vector of coordinates.}

\item{what}{A reagent name.}

\item{value}{The value to set.}

\item{add}{Add to the volume instead of overriding it.}
}
\value{
Returns the \code{Plate} object, where the values for the wells indicated
in the blocks have been updated.
}
\description{
Updates a \code{\link{Plate}} object representing a multiwell plate, by setting a given
value for all wells in a block or a list of blocks defined by the well
coordinates of their upper-left and bottom-right corners.
}
\details{
This function wraps \code{\link[platetools:set_block]{platetools::set_block()}}.
}
\examples{
p <- Plate("96")
head(p)

p <- p |>
  set_block(c("A01~B02", "A05~D05"), "dNTP", 0.25) |>
  set_block(    "A03",               "dNTP", 0.50)
head(p)

# Be careful with the column names
p <- set_block(p, "A01~H12", "Mg2+", 3.0)
head(p)

p <- set_block(p, "A02~A03", "dNTP", 1, add = TRUE)
head(p)

}
\seealso{
Other Plate functions: 
\code{\link{Plate-class}},
\code{\link{PlateTypeToWellNames}()},
\code{\link{as.character,Well-method}},
\code{\link{nextWell}()},
\code{\link{plateMap}()},
\code{\link{plateWellVolume}()},
\code{\link{randomise}()},
\code{\link{seekReagent}()},
\code{\link{setWell}()},
\code{\link{sourceReagent}()}

Other Dispensing functions: 
\code{\link{dispense_pairs}()}
}
\concept{Dispensing functions}
\concept{Plate functions}
