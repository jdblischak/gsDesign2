test_that("The IA nominal p-value is the same as the IA alpha spending.", {
  x <- gs_design_ahr(
    upper = gs_spending_bound,
    analysis_time = c(18, 30),
    upar = list(
      sf = gsDesign::sfLDOF,
      total_spend = 0.025,
      param = NULL,
      timing = c(18, 30) / 30
    ),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  ) |> to_integer()

  expect_equal(
    x$bound$`nominal p`[1],
    gsDesign::sfLDOF(alpha = 0.025, t = 18 / 30)$spend[1]
  )
})

test_that("Consistent results for to_integer() for sequential designs", {

  # ahr
  x_ahr <- gs_design_ahr(
    analysis_time = c(18, 30),
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  )
  observed <- to_integer(x_ahr)$analysis$n
  expected <- c(592, 592)
  expect_equal(observed, expected)
  observed <- to_integer(x_ahr)$analysis$event
  expected <- c(205, 321)
  expect_equal(observed, expected)

  # rd
  x_rd <- gs_design_rd(
    p_c = tibble::tibble(stratum = c("A", "B"), rate = c(.2, .3)),
    p_e = tibble::tibble(stratum = c("A", "B"), rate = c(.15, .27)),
    weight = "ss",
    stratum_prev = tibble::tibble(stratum = c("A", "B"), prevalence = c(.4, .6)),
    info_frac = c(0.7, 1),
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  )
  observed <- to_integer(x_rd)$analysis$n
  expected <- c(3722, 5320)
  expect_equal(observed, expected)

  # wlr
  x_wlr <- gs_design_wlr(
    analysis_time = c(18, 30),
    upper = gs_spending_bound,
    upar = list(sf = gsDesign::sfLDOF, total_spend = 0.025, param = NULL),
    lower = gs_b,
    lpar = c(-Inf, -Inf)
  )
  observed <- to_integer(x_wlr)$analysis$n
  expected <- c(600, 600)
  expect_equal(observed, expected)
  observed <- to_integer(x_wlr)$analysis$event
  expected <- c(208, 325)
  expect_equal(observed, expected)
})
