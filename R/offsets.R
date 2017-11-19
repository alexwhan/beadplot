#' Calculate offsets
#'
#' @param offsets A data.frame describing max and min of each group
#' @param group A grouping variable to describe the groups for which to calculate offsets. Converted to factor. Need to be the same as the number of rows.
#' @param offsets_min A variable in offsets describing the minimum of each group
#' @param offsets_max A variable in offsets describing the maximum of each group
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
#' calc_offsets(segments, "segment", "min", "max)

calc_offsets <- function(offsets, group, offsets_min = "min", offsets_max = "max") {
  if(!inherits(offsets[[group]], "factor")) {
    message(paste("Converting", group, "to factor, this may affect expected ordering"))
    offsets[[group]] <- factor(offsets[[group]])
  }

  ngroups <- length(levels(offsets[[group]]))
  offsets <- offsets[order(offsets[[group]]),]
  offsets$diff <- offsets[[offsets_max]] - offsets[[offsets_min]]
  offsets$offset <- cumsum(c(offsets[[offsets_min]][1], offsets[["diff"]][1:(ngroups - 1)]))
  return(offsets)
}

#' Title Offset continuous data by group
#'
#' @inheritParams calc_offsets
#' @param data A data.frame for which to calculate offsets
#' @param group A commmon grouping variable between data and offsets.
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
#'   offset_data(dat, segments, "segment", "dist', "min", "max")

offset_data <- function(data, offsets, group, disp, offsets_min = "min", offsets_max = "max") {

  if(!inherits(data[[group]], "factor")) {
    message(paste("Converting", group, "to factor"))
    data[[group]] <- factor(data[[group]])
  }
  if(!inherits(offsets[[group]], "factor")) {
    message(paste("Converting", group, "to factor, this may affect expected ordering"))
    offsets[[group]] <- factor(offsets[[group]])
  }

  #Check the levels of group in data are a subset of group in offsets
  stopifnot(all(levels(data[[group]]) %in% levels(offsets[[group]])))

  offsets <- calc_offsets(offsets, group, offsets_min, offsets_max)

  data_offset <- merge(data, offsets, by = group)

  data_offset$offset_disp <- data_offset[[disp]] + data_offset$offset
  return(data_offset)
}
