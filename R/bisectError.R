#' Private function to search for defective wells
#'
#' Sometimes defective wells cause some functions to crash.  Given a list
#' of wells, this function will bisect the list until it converges to the
#' well(s) that cause a given function to crash.
#'
#' @param f A function that crashes on some wells
#' @param l A list of wells
#' @param min The smaller index to start with (defaults to 1)
#' @param max The larger index to stop with (defaults to list length)
#' @param prevMin,prevMax Arguments used to pass previous state in recursion
#'
#' @note This function can surely be used in a broader way.  Let me know if
#' you would like to move it to another package.
#'
#' @return A numeric vector of two integers representing the smallest range of
#' indices necessary to crash the function.
#'
#' @author Charles Plessy
#'
#' @family Private functions

bisectError <- function(f, l, min = 1L, max = length(l), prevMin = NA, prevMax = NA) {
  # Base case: When the range has been narrowed down to a single element
  if (min == max)
    return(c(min, max))
  if (!is.na(prevMin) && !is.na(prevMax))
    if (min == prevMin && max == prevMax)
      return(c(min, max))
  message(paste0("Trying with min = ", min, " and max = ", max, "." ))

  mid <- as.integer((min + max) / 2)

  # Try applying the function to the first half of the current segment
  tryCatch({
    lapply(l[min:mid], f)
    # If no error occurs, the problematic element is in the second half
    bisectError(f, l, mid + 1, max)
  }, error = function(e) {
    # If an error occurs, the problematic element is in the first half
    bisectError(f, l, min, mid)
  })
}
