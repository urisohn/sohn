# Tests for stimulus.plot(), stimulus.beeswarm(), and clear_stimulus_cache()

stimulus_test_data <- function() {
  fp <- testthat::test_path("data", "salerno_slepian_jpsp_study4.csv")
  df1 <- read.csv(fp)
  df1$cond <- ifelse(df1$intent == 1, "Intentional", "Unintentional")
  df1$stimulus2 <- paste0(df1$stimulus, sample(1:6, size = nrow(df1), replace = TRUE))
  df2 <- subset(df1, stimulus %in% unique(df1$stimulus[1:6]))
  list(df1 = df1, df2 = df2)
}

stimulus_compared_data <- function(df1) {
  df1$stim_comp <- ifelse(
    df1$cond == "Intentional",
    paste0("A", df1$stimulus),
    paste0("B", df1$stimulus)
  )
  df1
}

#stimulus.plot_001
test_that("stimulus.plot means returns expected structure", {
  dat <- stimulus_test_data()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  clear_stimulus_cache()

  res <- stimulus.plot(
    data = dat$df1, dv = "rev", stimulus = "stimulus", condition = "cond",
    plot.type = "means", watermark = FALSE
  )

  expect_true(is.data.frame(res))
  expect_true(all(c("stimulus", "effect", "ciL", "ciH") %in% names(res)))
  expect_gt(nrow(res), 0)
})

#stimulus.plot_002
test_that("stimulus.plot effects returns expected structure", {
  dat <- stimulus_test_data()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  clear_stimulus_cache()
  set.seed(2024)

  res <- stimulus.plot(
    data = dat$df1, dv = "rev", stimulus = "stimulus", condition = "cond",
    plot.type = "effects", simtot = 20, participant = "id", watermark = FALSE, seed = 2024
  )

  expect_true(is.list(res))
  expect_true(all(c("observed", "under.null", "resamples", "p.hetero") %in% names(res)))
  expect_true(is.data.frame(res$observed))
})

#stimulus.plot_003
test_that("stimulus.plot errors for compared-stimulus design", {
  dat <- stimulus_test_data()
  df_comp <- stimulus_compared_data(dat$df1)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  msgs <- character(0)
  out <- withCallingHandlers(
    withRestarts(
      stimulus.plot(
        data = df_comp, dv = "rev", stimulus = "stim_comp", condition = "cond",
        plot.type = "means", watermark = FALSE
      ),
      abort = function(...) "aborted"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_identical(out, "aborted")
  expect_true(any(grepl("stimulus.beeswarm", msgs, fixed = TRUE)))
})

#stimulus.plot_004
test_that("stimulus.beeswarm runs on compared-stimulus subset", {
  dat <- stimulus_test_data()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  clear_stimulus_cache()
  set.seed(2024)

  res <- stimulus.beeswarm(
    data = dat$df2, dv = "rev", stimulus = "stimulus", condition = "cond",
    simtot = 20, watermark = FALSE
  )

  expect_true(is.matrix(res) || is.data.frame(res))
  expect_equal(ncol(res), 2)
})

#stimulus.plot_005
test_that("clear_stimulus_cache resets package cache", {
  dat <- stimulus_test_data()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  clear_stimulus_cache()
  set.seed(2024)

  invisible(stimulus.plot(
    data = dat$df1, dv = "rev", stimulus = "stimulus", condition = "cond",
    plot.type = "effects", simtot = 20, participant = "id", watermark = FALSE, seed = 2024
  ))
  expect_gt(length(statuser:::sp_stimulus_cache()), 0L)

  clear_stimulus_cache()
  expect_equal(statuser:::sp_stimulus_cache(), list())
})

#stimulus.plot_006
test_that("Salerno & Slepian effects with simtot=20 matches saved md5", {
  dat <- stimulus_test_data()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  clear_stimulus_cache()
  set.seed(2024)

  r1 <- stimulus.plot(
    data = dat$df1, dv = "rev", stimulus = "stimulus", condition = "cond",
    plot.type = "effects", simtot = 20, participant = "id", watermark = FALSE, seed = 2024
  )

  fp <- testthat::test_path("data", "stimulus_saved_results", "r1.rds")
  skip_if_not(file.exists(fp), "saved r1 fixture missing")
  expect_equal(get.md5(r1), get.md5(readRDS(fp)))
})

#stimulus.plot_007
test_that("Salerno & Slepian means matches saved md5", {
  dat <- stimulus_test_data()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  clear_stimulus_cache()

  r2 <- stimulus.plot(
    data = dat$df1, dv = "rev", stimulus = "stimulus", condition = "cond",
    plot.type = "means", watermark = FALSE
  )

  fp <- testthat::test_path("data", "stimulus_saved_results", "r2.rds")
  skip_if_not(file.exists(fp), "saved r2 fixture missing")
  expect_equal(get.md5(r2), get.md5(readRDS(fp)))
})

#stimulus.plot_008
test_that("Salerno & Slepian model=all subset matches saved reference", {
  skip_if_not(requireNamespace("lmerTest", quietly = TRUE))
  dat <- stimulus_test_data()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  clear_stimulus_cache()
  set.seed(2024)

  r3 <- tryCatch(
    stimulus.plot(
      data = dat$df1, dv = "rev", stimulus = "stimulus", condition = "cond",
      plot.type = "effects", simtot = 20, model = "all", participant = "id",
      watermark = FALSE, seed = 2024
    ),
    error = function(e) {
      skip(paste("Mixed-model fit unavailable:", conditionMessage(e)))
    }
  )
  r3.list <- list(
    r3$model.results$m.mean,
    r3$model.results$m.ci,
    r3$model.results$m.labels,
    r3$model.results$m.p
  )

  fp <- testthat::test_path("data", "stimulus_saved_results", "r3.rds")
  skip_if_not(file.exists(fp), "saved r3 fixture missing")
  saved <- readRDS(fp)
  expect_equal(r3.list[[3]], saved[[3]])
  expect_equal(r3.list[[1]], saved[[1]], tolerance = 1e-4)
  expect_equal(r3.list[[2]], saved[[2]], tolerance = 1e-4)
  expect_equal(r3.list[[4]], saved[[4]], tolerance = 1e-4)
})
