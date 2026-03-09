test_that("LaborMarket object has correct class", {
  lm_obj <- simlabormarket(nk = 2, nl = 2, nt = 2, ni = 50, lambda = 0.3)
  expect_s4_class(lm_obj, "LaborMarket")
})

test_that("show() runs without error", {
  lm_obj <- simlabormarket(nk = 2, nl = 2, nt = 2, ni = 50, lambda = 0.3)
  expect_output(show(lm_obj), "LaborMarket simulation")
})

test_that("all slots are accessible", {
  lm_obj <- simlabormarket(nk = 2, nl = 2, nt = 2, ni = 50, lambda = 0.3)

  expect_true(is.data.frame(lm_obj@panel))
  expect_true(is.list(lm_obj@init.params))
  expect_true(is.array(lm_obj@tranM))
  expect_true(is.array(lm_obj@steadyM))
  expect_true(is.numeric(lm_obj@alpha_mean))
  expect_true(is.numeric(lm_obj@psi_mean))
  expect_true(is.numeric(lm_obj@psi_sd))
  expect_true(is.numeric(lm_obj@alpha_sd))
  expect_true(is.numeric(lm_obj@csort))
  expect_true(is.numeric(lm_obj@cnetw))
  expect_true(is.numeric(lm_obj@csig))
  expect_true(is.numeric(lm_obj@fsize))
  expect_true(is.numeric(lm_obj@w_sigma))
  expect_true(is.numeric(lm_obj@neduc))
  expect_true(is.numeric(lm_obj@sort_gap))
  expect_true(is.numeric(lm_obj@shocks))
})
