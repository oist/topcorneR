% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plates.R
\name{setWell}
\alias{setWell}
\alias{setWell,Plate,Well,character,numeric-method}
\alias{setWell,Plate,Well,character,logical-method}
\title{Set the contents of a well}
\usage{
setWell(plate, well, what, volume)

\S4method{setWell}{Plate,Well,character,numeric}(plate, well, what, volume)

\S4method{setWell}{Plate,Well,character,logical}(plate, well, what, volume)
}
\arguments{
\item{plate}{A \code{\link{Plate}} object.}

\item{well}{A \code{\link{Well}} object.}

\item{what}{A reagent name (character value).}

\item{volume}{A volume in nanoliters (numeric value).}
}
\value{
Returns a \code{\link{Plate}} object.
}
\description{
Set the contents of a well
}
\examples{
sourcePlate <- Plate("384", deadVolume = 1e4, maxVolume = 1e5)
A01 <- Well("A01")
sourcePlate <- setWell(sourcePlate, A01, "dNTP", 100000)
sourcePlate <- setWell(sourcePlate, Well("A02"), "dNTP",   100000)
sourcePlate <- setWell(sourcePlate, Well("A03"), "buffer", 100000)
head(sourcePlate)
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
\code{\link{set_block}()},
\code{\link{sourceReagent}()}
}
\concept{Plate functions}
