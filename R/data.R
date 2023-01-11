#' Example `Plate` object
#'
#' This [`Plate`] represents a 384-well plate with a dead volume of
#' 10 microliters and a maximum volume of 100 microliters.  It is filled with
#' reagents in its three first wells.
#'
#' @family example data

"examplePlate"

#' Example `Transducer` object
#'
#' This [`Transducer`] object is about to transfer from well `A01` of a source
#' plate in 384-well format to well `A01` of a destination plate in 96-well
#' format.
#'
#' @family example data

"exampleTransducer"

#' Example `Echo` object
#'
#' This [`Echo`] object represents an Echo 525 machine loaded with a 384-well
#' source plate from the [`examplePlate`] object and a 96-well destination
#' plate, with its transducer ready to transfer between from source well `A01`
#' to destination well `A01`.
#'
#' @family example data

"exampleEcho"
