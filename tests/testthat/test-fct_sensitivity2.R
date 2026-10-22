files <- c("mocaredd-templatev2-simple.xlsx", "mocaredd-templatev2-4pools.xlsx", "example1-4pools.xlsx")

test_that("means match fct_arithmetic_mean3() and shares add up", {
  for (f in files) {
    checked <- fct_checkinput(.path = system.file("extdata", f, package = "mocaredd.dev2"))
    sa <- fct_sensitivity2(checked)
    r3 <- fct_arithmetic_mean3(checked)

    expect_equal(sa$outputs$E, r3$ER$E[match(sa$outputs$output, r3$ER$period_type)])

    ## group shares sum to 100 per output
    tot <- tapply(sa$variance$contribution_pct, sa$variance$output, sum)
    expect_equal(as.numeric(tot), rep(100, length(tot)))

    ## AD tornado shares sum to the AD group share
    ad_var <- tapply(sa$variance$contribution_pct[sa$variance$input_type == "AD"],
                     sa$variance$output[sa$variance$input_type == "AD"], sum)
    ad_tor <- tapply(sa$ad_tornado$contribution_pct, sa$ad_tornado$output, sum)
    expect_equal(as.numeric(ad_tor[names(ad_var)]), as.numeric(ad_var))
  }
})

test_that("AD only: delta method equals IPCC Approach 1", {
  checked <- fct_checkinput(.path = system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2"))
  checked$data$carbon$c_se <- 0
  sa <- fct_sensitivity2(checked)
  r3 <- fct_arithmetic_mean3(checked)

  expect_equal(sa$outputs$E_se, r3$ER$E_se[match(sa$outputs$output, r3$ER$period_type)])
  expect_true(all(sa$variance$group == "Activity data"))
})

test_that("tornado swing is linear in AD", {
  checked <- fct_checkinput(.path = system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2"))
  sa  <- fct_sensitivity2(checked)
  tor <- sa$ad_tornado
  expect_equal(tor$value_high - tor$value, tor$value - tor$value_low)
  expect_equal(abs(tor$value_high - tor$value_low), tor$swing)
  expect_s3_class(fct_tornado(tor, .output = tor$output[1], .top_n = 5), "ggplot")
})
