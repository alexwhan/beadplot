library(ggplot2)
basic_data <- data.frame(
  y = factor(c(1, 2, 2, 3, 3)),
  segment_id = c("A", "B", "C", "A", "D" ),
  dist = c(1, 2, 1, 2.5, 1))
basic_segments <- data.frame(
  segment_id = LETTERS[1:4],
  min = c(0, 2, 0, 0),
  max = c(3, 4, 3, 2)
)
basic_offsets <- calc_offsets(basic_segments, "segment_id", "min", "max")
#devtools::use_data(basic_data, basic_segments, basic_offsets, overwrite = TRUE)