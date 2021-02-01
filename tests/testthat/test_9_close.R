context("closing session")

modello.close()

test_that("'.modello' is not init", expect_false(modello.is.init()))
