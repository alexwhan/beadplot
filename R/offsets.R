#' Calculate offsets
#'
#' @param segments A data.frame describing max and min of each segment
#' @param segment_id A variable to describe the segments for which to calculate offsets. Converted to factor. Need to be the same as the number of rows.
#' @param offsets_min A variable in offsets describing the minimum of each segment
#' @param offsets_max A variable in offsets describing the maximum of each segment
#'
#' @return A data.frame
#' @export
#'
#' @examples
#'
#' segments <- data.frame(
#'   segment = LETTERS[1:4],
#'   min = c(0, 2, 0, 0),
#'   max = c(3,4,3,2)
#' )
#' calc_offsets(segments, "segment", "min", "max")

calc_offsets <- function(segments, segment_id, segment_min = "min", segment_max = "max") {
  if(!inherits(segments[[segment_id]], "factor")) {
    message(paste("Converting", segment_id, "to factor, this may affect expected ordering"))
    segments[[segment_id]] <- factor(segments[[segment_id]])
  }

  nsegments <- length(levels(segments[[segment_id]]))
  segments <- segments[order(segments[[segment_id]]),]
  segments$diff <- segments[[segment_max]] - segments[[segment_min]]
  segments$offset <- cumsum(c(segments[[segment_min]][1], segments[["diff"]][1:(nsegments - 1)]))
  names(segments)[[which(names(segments) == segment_id)]] <- "segment_id"
  return(segments)
}

#' Offset continuous data by segment
#'
#' @inheritParams calc_offsets
#' @param data A data.frame for which to calculate offsets
#' @param segment_id A commmon variable identifying segments between data and offsets.
#' @param disp A continuous variable in data for which offsets will be added
#'
#' @return A data.frame
#' @export
#'
#' @examples
#'
#' segments <- data.frame(
#'   segment = LETTERS[1:4],
#'   min = c(0, 2, 0, 0),
#'   max = c(3,4,3,2)
#' )
#' dat <- data.frame(
#'   segment = c("A", "C", "D" ),
#'   dist = c(1, 2, 1))
#'   offset_data(dat, segments, "segment", "dist", "min", "max")

offset_data <- function(data, segments, segment_id, disp, segment_min = "min", segment_max = "max") {

  if(!inherits(data[[segment_id]], "factor")) {
    message(paste("Converting", segment_id, "to factor"))
    data[[segment_id]] <- factor(data[[segment_id]])
  }
  if(!inherits(offsets[[segment_id]], "factor")) {
    message(paste("Converting", segment_id, "to factor, this may affect expected ordering"))
    offsets[[segment_id]] <- factor(offsets[[segment_id]])
  }

  #Check the levels of segment in data are a subset of segment in offsets
  stopifnot(all(levels(data[[segment_id]]) %in% levels(offsets[[segment_id]])))

  offsets <- calc_offsets(offsets, segment_id, segment_min, segment_max)

  data_offset <- merge(data, segments, by = segment_id)

  data_offset$offset_disp <- data_offset[[disp]] + data_offset$offset
  return(data_offset)
}
