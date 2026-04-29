test_that("ild_prepare converts strict wide numeric waves to canonical ILD", {
  d <- data.frame(
    id = c(1, 2),
    age = c(30, 40),
    mood_t1 = c(10, 20),
    mood_t2 = c(11, 21),
    stress_t1 = c(5, 6),
    stress_t2 = c(7, 8)
  )

  x <- ild_prepare(
    d,
    id = "id",
    input_format = "wide",
    wide_keep_cols = "age"
  )

  expect_true(is_ild(x))
  expect_equal(nrow(x), 4L)
  expect_true(all(c("id", "age", ".ild_wide_time", "mood", "stress") %in% names(x)))
  expect_equal(x[[".ild_seq"]], c(1, 2, 1, 2))
  expect_equal(x[[".ild_time_num"]], c(1, 2, 1, 2))
})

test_that("ild_prepare wide mode errors on unmatched non-keep columns when wide_cols is NULL", {
  d <- data.frame(
    id = c(1, 2),
    baseline = c("a", "b"),
    mood_t1 = c(1, 2),
    mood_t2 = c(3, 4)
  )

  expect_error(
    ild_prepare(d, id = "id", input_format = "wide"),
    "all non-id/non-keep columns must match wide_names_pattern"
  )
})

test_that("ild_prepare wide mode parses date tokens with explicit pattern", {
  d <- data.frame(
    id = c(1, 2),
    y_2024_01_01 = c(1, 2),
    y_2024_01_02 = c(3, 4)
  )

  x <- ild_prepare(
    d,
    id = "id",
    time = "date",
    input_format = "wide",
    wide_names_pattern = "^(.+)_(\\d{4}_\\d{2}_\\d{2})$",
    wide_time_parser = "date",
    wide_time_format = "%Y_%m_%d"
  )

  expect_true(inherits(x$date, "Date"))
  expect_equal(sort(unique(x$date)), as.Date(c("2024-01-01", "2024-01-02")))
})

test_that("ild_prepare wide mode errors on duplicate parsed measure-time slots", {
  d <- data.frame(
    id = c(1, 2),
    xA_t1 = c(1, 2),
    xB_t1 = c(3, 4)
  )

  expect_error(
    ild_prepare(
      d,
      id = "id",
      input_format = "wide",
      wide_names_pattern = "^(x).+_t(\\d+)$"
    ),
    "Duplicate measure-time slots"
  )
})

test_that("ild_prepare wide conversion matches manually-long preparation", {
  wide <- data.frame(
    id = c(1, 2),
    mood_t1 = c(10, 20),
    mood_t2 = c(15, 25),
    stress_t1 = c(3, 4),
    stress_t2 = c(5, 6)
  )

  long <- data.frame(
    id = rep(c(1, 2), each = 2),
    time = rep(c(1, 2), times = 2),
    mood = c(10, 15, 20, 25),
    stress = c(3, 5, 4, 6)
  )

  x_wide <- ild_prepare(wide, id = "id", time = "time", input_format = "wide")
  x_long <- ild_prepare(long, id = "id", time = "time")

  keep <- c("id", "time", "mood", "stress", ".ild_id", ".ild_time_num", ".ild_seq", ".ild_dt", ".ild_gap")
  for (nm in keep) {
    expect_equal(x_wide[[nm]], x_long[[nm]], ignore_attr = TRUE)
  }
})
