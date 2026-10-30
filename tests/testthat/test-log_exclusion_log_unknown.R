source(file.path("..", "..", "R", "utils.R"))

test_that("log_exclusion() with no log_path still writes to outputs/exclusion_log.csv", {
  old_wd <- getwd()
  tmp_dir <- tempfile("log_exclusion_test_")
  dir.create(tmp_dir)
  setwd(tmp_dir)
  on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

  path <- log_exclusion(
    site_id = "US-Ha1", variable = "ALL", timestamp = "2020",
    reason = "test", threshold = "QC_THRESHOLD_YY=0.50", excluded_by = "test"
  )

  expect_identical(path, file.path("outputs", "exclusion_log.csv"))
  expect_true(file.exists(file.path("outputs", "exclusion_log.csv")))
  rows <- read.csv(file.path("outputs", "exclusion_log.csv"))
  expect_equal(nrow(rows), 1)
  expect_equal(rows$site_id, "US-Ha1")
})

test_that("log_exclusion() with a custom log_path writes there instead", {
  old_wd <- getwd()
  tmp_dir <- tempfile("log_exclusion_test_")
  dir.create(tmp_dir)
  setwd(tmp_dir)
  on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

  custom_path <- file.path("GSL_definition", "logs", "exclusion_log.csv")
  path <- log_exclusion(
    site_id = "US-Ha1", variable = "ALL", timestamp = "2020",
    reason = "test", threshold = "QC_THRESHOLD_DD=0.50", excluded_by = "test",
    log_path = custom_path
  )

  expect_identical(path, custom_path)
  expect_true(file.exists(custom_path))
  expect_false(dir.exists("outputs"))
})

test_that("log_unknown() with no log_path still writes to outputs/unknown_log.csv", {
  old_wd <- getwd()
  tmp_dir <- tempfile("log_unknown_test_")
  dir.create(tmp_dir)
  setwd(tmp_dir)
  on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

  path <- log_unknown(record_id = "US-Ha1", reason = "test", logged_by = "test")

  expect_identical(path, file.path("outputs", "unknown_log.csv"))
  expect_true(file.exists(file.path("outputs", "unknown_log.csv")))
})

test_that("log_unknown() with a custom log_path writes there instead", {
  old_wd <- getwd()
  tmp_dir <- tempfile("log_unknown_test_")
  dir.create(tmp_dir)
  setwd(tmp_dir)
  on.exit({ setwd(old_wd); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

  custom_path <- file.path("GSL_definition", "logs", "unknown_log.csv")
  path <- log_unknown(
    record_id = "US-Ha1", reason = "test", logged_by = "test",
    log_path = custom_path
  )

  expect_identical(path, custom_path)
  expect_true(file.exists(custom_path))
  expect_false(dir.exists("outputs"))
})
