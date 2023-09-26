#' Dispense pair combinations
#'
#' Functions to dispense combinations of all the pairs of two sets of reagents,
#' for instance forward and reverse PCR primers.
#'
#' The names of the reagents must match the names that will be used in the
#' source plate.
#'
#' @param plate A [`Plate`] object.
#' @param set1 First set of reagents.
#' @param set2 Second set of reagents.
#' @param volume Fixed volume to dispense.
#' @param start Starting well.
#'
#' @returns Returns a `Plate` object with columns documenting the reagents to
#' be dispensed.
#'
#' @family Dispensing functions
#' @seealso [titration_curve()]
#'
#' @importFrom platetools well_to_num
#'
#' @examples
#' examplePlate |>
#'   dispense_pairs(c("F01", "F02", "F03"), c("R01", "R02"), volume = 20) |>
#'   DataFrame() |> head(6)
#' examplePlate |>
#'   dispense_pairs(c("F01", "F02"), volume = 10) |>
#'   DataFrame() |> head(6)
#'
#' @export

dispense_pairs <- function (plate, set1, set2 = set1, volume, start = "A01") {
  set1_N     <- length(set1)
  set2_N     <- length(set2)
  plateType  <- plate@metadata$type
  n_per_pair <- length(volume)

  # Lots of technological debt in this function, sorry...

  dispense_pairs_first <- function(n) {
    consecutive_of_wells_for_one_primer <- n_per_pair * set2_N
    Range <- 1 : consecutive_of_wells_for_one_primer +   # Consecutive wells for one primer
             (n-1) * n_per_pair * set2_N                     # Start at nth primer
    PlateTypeToWellNames(plateType)[Range + well_to_num(start, plate = plateType) - 1]
  }

  dispense_pairs_second <- function(n) {
    consecutive_of_wells_for_one_primer <- n_per_pair
    Range <- rep(1:consecutive_of_wells_for_one_primer, set1_N)  + # One stretch per forward primer
      (rep(0:(set1_N -1) * n_per_pair * set2_N, n_per_pair) |> sort()) +
      (n-1) * consecutive_of_wells_for_one_primer
    PlateTypeToWellNames(plateType)[Range + well_to_num(start, plate = plateType) - 1]
  }

  for (Nth in seq_along(set1)) {
    plate <- set_block(plate, dispense_pairs_first(Nth),  set1[Nth], volume)
  }

  for (Nth in seq_along(set2)) {
    plate <- set_block(plate, dispense_pairs_second(Nth), set2[Nth], volume)
  }

  plate
}
