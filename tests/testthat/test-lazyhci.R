test_that("we can load a lazy_model (mixed)", {
  demo1  <- read.csv("https://stats.idre.ucla.edu/stat/data/demo1.csv")
  demo1.model <- lazy_model(demo1, participant = "id", within.vars = "time", between.vars = "group", make_factor = TRUE)
  })
