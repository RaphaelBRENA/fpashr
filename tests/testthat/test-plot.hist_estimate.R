library(testthat)
library(MASS)

data(geyser)
wait_data <- geyser$waiting

test_that("test that plot.hist_estimate returns a ggplot histogram for
          average shifted histograms",{

            ash <- compute_ash(wait_data, h = 9, m = 3)
            class(ash) <- c("ash_density", "hist_estimate")
            p <- plot.hist_estimate(ash)

            expect_s3_class(p, "ggplot")
          })

test_that("test that plot.hist_estimate returns a ggplot histogram for
          frequency polygons",{

            fp <- compute_fp(wait_data, bins = 10)
            class(fp) <- c("frequency_polygon", "hist_estimate")
            p <- plot.hist_estimate(fp)

            expect_s3_class(p, "ggplot")
          })

test_that("test that plot.hist_estimate gives error for invalid input",{

  expect_error(plot.hist_estimate(1:10))

})

