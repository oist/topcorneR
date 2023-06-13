#' Avogadro's constant
#'
#' Number of molecules in a mole.  Not exported, but made available through
#' the `:::` operator for convenience.
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Avogadro_constant}
#'
#' @family Accessory functions
#'
#' @examples
#' topcorneR:::N
#' prettyNum(topcorneR:::N, digits = 9)

N <- 6.02214076e23

#' Titration curve
#'
#' Given a number of _steps_ and a target _maximal volume_, this function will
#' return an increasing serie of volumes that cover the range in an
#' approximately logarithmic progression.  Each volume can be dispensed, as they
#' are guaranteed to be multiple of the _droplet's volume_.  The range can be
#' forced to start at zero if a negative control is needed.
#'
#' @param steps The number of points in the titration curve.
#' @param maxvol The maximum volume to reach.
#' @param dropvolume The volume of a droplet, in nanoliters.
#' @param zero Start with a negative control with a volume of zero.
#'
#' @return A numeric vector of length `steps` indicating the volumes to transfer
#' in order to cover a volume range in a logarithmic manner.
#'
#' @family Accessory functions
#'
#' @author Charles Plessy
#'
#' @examples
#' titration_curve(12)
#' titration_curve(12, zero = FALSE)
#' titration_curve(12, dropvolume = 25)
#' titration_curve(12, dropvolume = 25, maxvol = 300)
#'
#' @export

titration_curve <- function(steps = 12, maxvol = 1000, dropvolume = 2.5, zero = TRUE) {
  # First, let's define an internal function with a private addsteps argument
  # for recursive runs.
  internal_loop <- function(steps, maxvol, dropvolume, zero, addsteps) {
    realsteps <- steps + addsteps
    if (isTRUE(zero)) realsteps <- realsteps -1
    scale <- 2 ^ (log2(maxvol) * (1 / realsteps) * (1:realsteps))
    scale <- dropvolume * round(scale / dropvolume)
    if (isTRUE(zero)) scale <- c(0, scale)
    scale <- unique(scale)
    if (length(scale) < steps) {
      scale <- internal_loop(steps = steps, maxvol = maxvol, dropvolume = dropvolume, zero = zero, addsteps = addsteps + 1)
    }
    scale
  }
  # Then, let's run it
  internal_loop(steps = steps, maxvol = maxvol, dropvolume = dropvolume, zero = zero, addsteps = 0)
}
