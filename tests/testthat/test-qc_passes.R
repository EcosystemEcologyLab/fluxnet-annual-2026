source(file.path("..", "..", "R", "qc.R"))

test_that("qc_passes keeps QC >= threshold at 0.50", {
  qc <- c(0.24, 0.25, 0.49, 0.50, 0.74, 0.75, 1.00)
  expect_identical(
    qc_passes(qc, 0.50),
    c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("qc_passes keeps QC >= threshold at 0.75 (stricter than 0.50)", {
  qc <- c(0.24, 0.25, 0.49, 0.50, 0.74, 0.75, 1.00)
  expect_identical(
    qc_passes(qc, 0.75),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
})

test_that("qc_passes returns NA for NA input, at any threshold", {
  expect_identical(qc_passes(NA_real_, 0.50), NA)
  expect_identical(qc_passes(NA_real_, 0.75), NA)
  expect_identical(
    qc_passes(c(0.60, NA, 0.80), 0.75),
    c(FALSE, NA, TRUE)
  )
})
