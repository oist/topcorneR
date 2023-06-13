#' Load CT values from qPCR results file
#'
#' Loads CT values from the results file produced by a real-time quantitative
#' PCR (RT-qPCR) machine.
#'
#' @return Returns a [tibble::tibble()] object reporting well coordinates, CT
#' value and melting temperature as computed by the machine, and optionally
#' the name of the experiment.
#'
#' @param file Path to the file to open.
#' @param expName The name of the experiment.
#' @param machine The type of the machine that produced the results file.
#' @param format The format of the file.  At the moment only _Excel_ is supported.
#' @param skip The number of lines to skip before reading the data.
#' @param n_max The number of lines to read.
#' @param cycles The number of PCR cycles.
#'
#' @note This function needs the _readxl_ package.
#'
#' @author Charles Plessy
#'
#' @family qPCR functions
#'
#' @rdname load_qPCR
#' @export
load_qPCR_results <- function(
    file,
    expName=NA,
    machine=c(NA, "VIA", "QS5"),
    format='Excel',
    skip=43,
    n_max=384) {

  if(isFALSE(requireNamespace('readxl'))) stop("Please install the readxl package to use this function.")

  machine <- match.arg(machine)
  if(isTRUE(machine == "QS5")) skip = 45

  res <- readxl::read_excel(file, sheet = "Results", skip = skip, n_max=n_max)
  res <- res[ , c("Well Position", "CT", "Tm1")]

  res$CT <- res$CT |> sub(pat = "Undetermined", rep = NA) |> as.numeric()

  res$well <- paste0(sort(rep(LETTERS[1:16], 24)), formatC(1:24, width = 2, flag = "0"))
  res$row <- factor(sort(rep(LETTERS[1:16], 24)))
  res$col <- factor(rep(1:24, 16))

  if(is.null(expName)) expName <- NA
  res$expName <- expName

  res[,c("well", "CT", "Tm1", "expName", "row", "col", "Well Position")]
}

#' @rdname load_qPCR
#' @family qPCR functions
#' @export

load_qPCR_meltcurve <- function(
    file,
    expName=NA,
    machine=c(NA, "VIA", "QS5"),
    format='Excel',
    skip=42) {

  if(isFALSE(requireNamespace('readxl'))) stop("Please install the readxl package to use this function.")

  machine <- match.arg(machine)

  mlt <- readxl::read_excel(file, sheet = "Melt Curve Raw Data", skip = skip)
  mlt$well <- ifelse (nchar(mlt$`Well Position`) == 2,
                      mlt$`Well Position` |> sub(pat = "(^[A-Z])", rep = "\\10"),
                      mlt$`Well Position`)

  if(is.null(expName)) expName <- NA
  mlt$expName <- expName

  mlt[,c("well", "expName", "Reading", "Temperature", "Fluorescence",
         "Derivative", "Well Position")]
}

#' @rdname load_qPCR
#' @family qPCR functions
#' @export

load_qPCR_raw <- function(
    file,
    expName=NA,
    machine=c(NA, "VIA", "QS5"),
    format='Excel',
    skip=42,
    cycles=40) {

  if(isFALSE(requireNamespace('readxl'))) stop("Please install the readxl package to use this function.")

  machine <- match.arg(machine)

  fluo <- readxl::read_excel(file, sheet = "Multicomponent Data", skip = skip)
  fluo$SYBR <- fluo$SYBR |> gsub(pat=",", rep="") |> as.numeric()
  fluo$ROX  <- fluo$ROX  |> gsub(pat=",", rep="") |> as.numeric()
  fluo$NORM <- fluo$SYBR / fluo$ROX
  fluo$`Well Position` <- fluo$`Well Position` |> gsub(pat = " ", rep = "")
  fluo$well <- ifelse (nchar(fluo$`Well Position`) == 2,
                            fluo$`Well Position` |> sub(pat = "(^[A-Z])", rep = "\\10"),
                            fluo$`Well Position`)
  fluo[fluo$Cycle <= cycles, c("well", "Cycle", "SYBR", "ROX", "NORM", "Well Position")]
}

#' Compute melting temperature from raw data
#'
#' Although the qPCR machines output melting temperatures in their report files,
#' it is also possible to compute them using exported raw data.  This function
#' automates running [qpcR::meltcurve()] for the computation in a quiet way.
#'
#' @param mlt A table providing the raw data loaded by [load_qPCR_meltcurve()].
#' @param cut.Area Parameter passed directly to [qpcR::meltcurve()].
#'
#' @return A vector of melting temperatures in the same order as the wells of
#' the `mlt` table.
#'
#' @family qPCR functions
#' @export

calc_qPCR_TM <- function(mlt, cut.Area = 1) {
  if(isFALSE(requireNamespace('qpcR'))) stop("Please install the qpcR package to use this function.")

  {sink("/dev/null")  # Work around verboseness

    TMs <- sapply(unique(mlt$well), \(well) {
      x <- mlt[mlt$well == well, c("Temperature", "Fluorescence")] |> as.data.frame() |>
        qpcR::meltcurve(cut.Area = cut.Area, plot = FALSE)
      x <- x[[1]] # Deconstruct the object
      x[order(x$Area, decreasing = TRUE),][1,"Tm"] # Return the peak with the largest area.
    })

  sink()}

  TMs
}

#' Compute CT and amplification efficiency from raw data
#'
#' @param fluo A table providing the raw data loaded by [`load_qPCR_raw`].
#'
#' @return A `tibble` providing `Eff` (efficiency), `cpD1` (CT), and `cpD2`
#' another computation of CT) calculated by the [`qpcR::efficiency`] function.
#'
#'
#' @family qPCR functions
#' @export

calc_qPCR_mod <- function(fluo) {

  # Same as qpcR::modlist, but quiet, and with well name as title.
  modlist2 <- function(x, ...) {
    {sink("/dev/null")  # Work around verboseness
      mod <- qpcR::modlist(x, verbose = F, ...)
      sink()}
    mod[[1]]$Title <- unique(x$well)
    mod
  }

  # It seems that qpcR::modlist returns a different object if qpcR is not loaded.
  library('qpcR') |> suppressPackageStartupMessages()
  fluo.l.mod <- sapply(split(fluo, fluo$well),
                       modlist2,
                       fluo = "NORM",
                       cyc = "Cycle")
  detach("package:qpcR", unload=TRUE)

  fluo.l.eff <- sapply(fluo.l.mod, qpcR::efficiency, plot = FALSE)

  tibble::tibble(
    well = names(fluo.l.mod),
    Eff  = fluo.l.eff["eff", ]  |> unlist(),
    cpD1 = fluo.l.eff["cpD1", ] |> unlist(),
    cpD2 = fluo.l.eff["cpD2", ] |> unlist()
  )
}
