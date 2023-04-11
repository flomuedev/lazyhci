test_that("the plot looks okay (mixed)", {
  demo1  <- read.csv(lazy_demo_data("demo1.csv"))
  demo1.model <- lazy_model(demo1, participant = "id", within.vars = "time", between.vars = "group", make_factor = TRUE)

  p <- lazy_plot(demo1.model, dv = "pulse")

  expect_doppelganger("simple_plot", p)
  })
