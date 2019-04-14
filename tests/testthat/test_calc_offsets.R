context("calc_offsets")

data(basic_data, basic_segments, basic_offsets)
test_that("calc_offsets the correct data", {
  expect_equal(calc_offsets(basic_segments, "segment_id", "min", "max"), basic_offsets)
})

test_that("calc_offsets returns error for wrong segment_id name", {
  expect_error(calc_offsets(basic_segments, "group"))
})

# ggplot2::ggplot(data, ggplot2::aes(dist, y, segment_id = segment_id)) +
#   stat_string(segments = segments) +
#   stat_bead(segments = segments)
