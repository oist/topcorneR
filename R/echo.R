#' @include plates.R
NULL

#' Transducer state
#'
#' Objects of this class represent the state of the transduction system in the
#' Echo machine: the position of the transducer in the coordinate system of the
#' source plate, and the position of the target plate on top of the transducer.
#'
#' @slot source A [`Well`] in the source plate.
#' @slot destination A [`Well`] in the destination plate.
#'
#' @examples
#' Transducer(
#'   source      = Well("A01"),
#'   destination = Well("A01", plateFormat = "96"))
#'
#' @rdname Transducer
#' @export

Transducer <- setClass("Transducer",
  slots = c(
    source      = "Well",
    destination = "Well"))

#' @rdname Transducer
#' @param x A [`Transducer`] object
#' @export

setMethod("as.character", "Transducer", function(x)
  paste0("Transducer at the following coordinates:\n",
      "  Source:      ", as.character(x@source), "\n",
      "  Destination: ", as.character(x@destination)))

#' @rdname Transducer
#' @param object An [`Transducer`] object.
#' @export

setMethod("show", "Transducer", function(object) cat(as.character(object)))


#' Echo state
#'
#' Objects of this class represent the state of an Echo machine loaded with
#' a source and a destination plate, and with its transducer at a given
#' position.
#'
#' The model of the echo machine determines the resolution of the liquid
#' transfers: 2.5 nL increments for the \dQuote{555} and 25 nL increments for
#' the \dQuote{525}.
#'
#' @slot source A source [`Plate`].
#' @slot destination A destination [`Plate`].
#' @slot transducer A [`Transducer`] state.
#' @slot model The model of the Echo machine.
#' @slot log A log of the transfers
#'
#' @examples
#' plate <- Plate( type       =   "384"
#'               , deadVolume =  10000
#'               , maxVolume  = 100000)
#'
#' transducer <- Transducer( source      = Well("A01")
#'                         , destination = Well("A01", plateFormat = "96"))
#'
#' echo <- Echo( source      = plate
#'             , destination = plate
#'             , transducer  = transducer
#'             , model       = "525")
#' echo
#'
#' @export

Echo <- setClass("Echo",
  slots = c(
    source      = "Plate",
    destination = "Plate",
    transducer  = "Transducer",
    model       = "character",
    log         = "list"
  ))

#' @rdname Echo-class
#' @param x An [`Echo`] object.
#' @export

setMethod("as.character", "Echo", function(x) {
  paste0("A ", x@model, " Echo machine:\n",
      "  Source plate:      ", as.character(x@source), "\n",
      "  Destination plate: ", as.character(x@destination), "\n",
      as.character(x@transducer), "\n",
      length(x@log), " elements in the log.", sep = "")
})

#' @rdname Echo-class
#' @param object An [`Echo`] object.
#' @export

setMethod("show", "Echo", function(object) cat(as.character(object)))


#' Change a well position
#'
#' Makes sure that the updated well position is in the same plate format as the
#' original one.
#'
#' @param from A [`Well`] object.
#' @param to A `Well` object.
#'
#' @return Returns a `Well` object.
#'
#' @examples
#'
#' changeWell( Well("A01")
#'           , Well("B03", plateFormat = "undefined"))
#'
#' @export

setGeneric("changeWell", function(from, to) standardGeneric("changeWell"))

#' @rdname changeWell
#' @export

setMethod("changeWell", c("Well", "Well"), function (from, to) {
  plateFormat <- from@plateFormat
  if (    plateFormat != "undefined" &
       to@plateFormat != "undefined" &
       to@plateFormat != plateFormat)
    stop( "Plate formats not compatible (from: ", sQuote(plateFormat)
        , ", to: ", sQuote(to@plateFormat), ").")
  to@plateFormat <- plateFormat
  if (validObject(to)) to
})

#' Change the state of the transducer
#'
#' Moves the tranducer to a new source position, or moves the destination plate,
#' or both
#'
#' @param transducer A [`Transducer`] object.
#' @param source A [`Well`] object.
#' @param destination A `Well` object.
#'
#' @return Returns a transducer object in the new state.
#'
#' @examples
#' moveTransducer(exampleTransducer, Well("A02"))
#' moveTransducer(exampleTransducer, destination = Well("A02", plateFormat = "96"))
#' moveTransducer( exampleTransducer
#'               , source      = Well("A02")
#'               , destination = Well("A02", plateFormat = "96"))
#'
#' moveTransducer(exampleEcho, source = Well("A02"))
#'
#' moveTransducer(exampleEcho, destination = Well("A02", plateFormat = "96"))
#'
#' moveTransducer( exampleEcho
#'               , source      = Well("A02")
#'               , destination = Well("A02", plateFormat = "96"))
#'
#' @export

setGeneric( "moveTransducer"
          , function(transducer, source, destination) standardGeneric("moveTransducer"))

#' @rdname moveTransducer
#' @export

setMethod( "moveTransducer"
        , c("Transducer", "Well", "missing")
        , function(transducer, source, destination) {
  transducer@source <- changeWell(transducer@source, source)
  if (validObject(transducer)) transducer
})

#' @rdname moveTransducer
#' @export

setMethod( "moveTransducer"
        , c("Transducer", "missing", "Well")
        , function(transducer, source, destination) {
  transducer@destination <- changeWell(transducer@destination, destination)
  if (validObject(transducer)) transducer
})

#' @rdname moveTransducer
#' @export

setMethod( "moveTransducer"
        , c("Transducer", "Well", "Well")
        , function(transducer, source, destination) {
  transducer <- moveTransducer(transducer, source = source)
  transducer <- moveTransducer(transducer, destination = destination)
  if (validObject(transducer)) transducer
})

#' @rdname moveTransducer
#' @export

setMethod( "moveTransducer"
        , c("Echo", "Well", "missing")
        , function(transducer, source, destination) {
  transducer@transducer <- moveTransducer(transducer@transducer, source)
  if (validObject(transducer)) transducer
})

#' @rdname moveTransducer
#' @export

setMethod( "moveTransducer"
        , c("Echo", "missing", "Well")
        , function(transducer, source, destination) {
  transducer@transducer <- moveTransducer(transducer@transducer, destination = destination)
  if (validObject(transducer)) transducer
})

#' @rdname moveTransducer
#' @export

setMethod( "moveTransducer"
        , c("Echo", "Well", "Well")
        , function(transducer, source, destination) {
  transducer@transducer <- moveTransducer(transducer@transducer, source, destination)
  if (validObject(transducer)) transducer
})


#' Show or export transfer logs
#'
#' Extract the logs from an `Echo` object and formats them as a
#' table.  The `exportLogs` function sets the column names expected by the
#' _Echo Plate Reformat_ software and discards the `what` column.
#'
#' @param echo An `Echo` object.
#'
#' @examples
#' showLogs(exampleEcho)
#' exportLogs(exampleEcho)
#'
#' @export

setGeneric("showLogs", function(echo) standardGeneric("showLogs"))

#' @rdname showLogs
#' @export

setMethod("showLogs", "Echo", function(echo) {
  df <- as.data.frame(t(as.data.frame(lapply(echo@log, unlist))))
  rownames(df) <- NULL
  df
})

#' @rdname showLogs
#' @export

setGeneric("exportLogs", function(echo) standardGeneric("exportLogs"))

#' @rdname showLogs
#' @export

setMethod("exportLogs", "Echo", function(echo) {
  df <- showLogs(echo)[,1:3]
  colnames(df) <- c("Source well", "Destination well", "Transfer volume")
  df
})

#' Transfer volume from a source to a destination plate
#'
#' Reduce the volume of a reagent in the source plate at the coordinates of
#' the transducer, add that volume to the destination plate,
#' and register the transfer in the log.
#'
#' @param echo An [`Echo`] object.
#' @param volume Volume to transfer.
#'
#' The `what` parameter must correspond to a column name in the source
#' plate.
#'
#' @return Returns An [`Echo`] object with the `source`, `destination` and `log`
#' slots updated.
#'
#' @examples
#'
#' sourcePlate <- examplePlate
#' head(sourcePlate)
#'
#' destinationPlate <- Plate("96", deadVolume = 1e4, maxVolume = 1e5)
#'
#' transducer <- Transducer( source      = Well("A01")
#'                         , destination = Well("A01", plateFormat = "96"))
#'
#' echo <- Echo( source      = sourcePlate
#'             , destination = destinationPlate
#'             , transducer  = transducer
#'             , model       = "525")
#'
#' echo |> showLogs()
#'
#' echo <- echo |>
#'   transfer(10) |>
#'   transfer(10) |>
#'   moveTransducer(destination = Well("A02", plateFormat = "96")) |>
#'   transfer(5)
#'
#' echo |> showLogs()
#'
#' @export

setGeneric("transfer", function(echo, volume) setGeneric("transfer"))

.transfer <- function (echo, volume) {
  what            <-   sourceReagent(echo@source, echo@transducer@source)
  availableVolume <- plateWellVolume(echo@source, echo@transducer@source)
  if (!enoughVolume(echo, volume))
   stop( "Not enough ", what, " remaining (want: ", volume
         , ", has: ", availableVolume
         , ", min. well volume: ", metadata(echo@source)$deadVolume, ".")
  destinationVolume        <- plateWellVolume(echo@destination, echo@transducer@destination)
  destinationReagentVolume <- plateWellVolume(echo@destination, echo@transducer@destination, what)
  if (destinationVolume + volume > metadata(echo@destination)$maxVolume)
   stop( "Overflow in destination well", echo@transducer@destination@well
         , " (transfer: ", volume
         , ", current total volume: ", destinationVolume
         , ", max. well volume: ", metadata(echo@destination)$maxVolume, ".")
  # Take the volume from the source
  echo@source <- setWell(echo@source, echo@transducer@source, what, availableVolume - volume)
  # Shoot it to the destination
  echo@destination <- setWell(echo@destination, echo@transducer@destination, what, destinationReagentVolume + volume)
  # Log the transfer
  echo@log[[length(echo@log) + 1]] <-
   list( from = echo@transducer@source@well
         , to   = echo@transducer@destination@well
         , vol  = volume
         , what = what)
  if (validObject(echo)) echo
}

#' @rdname transfer
#' @export

setMethod( "transfer", c("Echo", "numeric"), .transfer)


#' Plan Echo transfers
#'
#' Given a source plate, a target plate and the representation of a filled
#' target plate, calculate the liquid transfers necessary perform the filling.
#'
#' Each reagent of the plan plate is transferred one after the other.  Once
#' a reagent is selected, a source well is sought for the transfer and the
#' transducer is moved to it.
#'
#' @param source A source [`Plate`].
#' @param destination A destination `Plate`.
#' @param plan A `Plate` representing the additions to the
#'       destination plate.
#' @param model Which Echo model to use.
#'
#' @return A transfer log
#'
#' @seealso \code{\link{showLogs}}
#'
#' @examples
#' sourcePlate <- examplePlate
#' head(sourcePlate)
#'
#' planPlate <- Plate("384") |>
#'   setWell(Well("A01"), "dNTP",   50) |>
#'   setWell(Well("A02"), "dNTP",  100) |>
#'   setWell(Well("A01"), "buffer", 50)
#' head(planPlate)
#'
#' destinationPlate <- Plate("384", deadVolume = 1e4, maxVolume = 1e5)
#'
#' echo <- planTransfers(sourcePlate, destinationPlate, planPlate)
#' echo
#' echo |> showLogs()
#'
#' @export

planTransfers <- function(source, destination, plan, model = "525") {
  echo <- Echo( source      = source
              , destination = destination
              , transducer  = Transducer( source =      Well("A01", plateFormat = "384")
                                        , destination = Well("A01", plateFormat = "384"))
              , model       = model)

# To do: better support plate formats.  Plate objects should have a plateFormat
  # slot; in most cases it should not be retreived from the transducer.
# To do: reform the sourceReagent function (the name is misleading, since now
# it can also be run on destination plates)

# To do: accept plan plates that contain non-numerical variables.

  transferLoop <- function (echo, plan, reagent) {
    # Where is the next transfer ?
    nextTrans <- seekReagent(plan, reagent, echo@transducer@destination)
    # If no next transfer, end the loop
    if(suppressWarnings(is.na(nextTrans))) return(echo)
    # Otherwise, move the tranducer
    echo <- moveTransducer(echo, destination = nextTrans)
    # Read the volume to be transferred
    volume <- plateWellVolume(plate = plan, nextTrans, reagent)
    # Check if there is enough reagent
    while (! enoughVolume(echo, volume))
      # If not, move the transducer to the next source well
      echo <- seekReagent(echo, reagent, nextWell(echo@transducer@source))
    # Transfer
    echo <- transfer(echo, volume)
    # Register the transfer in the plan plate
    plan <- setWell(plan, nextTrans, reagent, NA)
    # Recursively call the loop
    transferLoop(echo, plan, reagent)
  }

  for (reagent in sourceReagent(plan)) {
    # Move transducer in upper-left corner
    echo <- echo |> moveTransducer( source      = Well(well = "A01", plateFormat = echo@transducer@source@plateFormat)

                            , destination = Well(well = "A01", plateFormat = echo@transducer@destination@plateFormat))
    # Find reagent in source plate
    echo <- seekReagent(echo, reagent)
    # Enter transfer loop
    echo <- transferLoop(echo, plan, reagent)
  }
  echo
}

#' @rdname seekReagent
#' @export

setMethod("seekReagent", c("Echo", "character", "Well"), function(object, reagent, start) {
  newPos <- seekReagent(object@source, reagent, start)
  if (suppressWarnings(is.na(newPos))) stop(paste0("Reagent ", reagent," not found !"))
  object |> moveTransducer(source = newPos)
})

#' @rdname seekReagent
#' @export
#'
setMethod("seekReagent", c("Echo", "character", "missing"), function(object, reagent, start) {
  object |> seekReagent(reagent, object@transducer@source)
})

enoughVolume <- function(echo, volume) {
  plateWellVolume(echo@source, echo@transducer@source) > volume + metadata(echo@source)$deadVolume
}
