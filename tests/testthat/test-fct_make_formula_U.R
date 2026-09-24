test_that("formulas", {
  expect_equal(fct_make_formula_U(c("AGB", "RS"), "DM"),
               "AGB * (1 + RS) * CF * sqrt((AGB_se / AGB)^2 + (RS_se / (1 + RS))^2 + (CF_se / CF)^2)")
  expect_equal(fct_make_formula_U("ALL", "C"), "ALL_se")
  expect_equal(fct_make_formula_U(c("DW", "SOC"), c("C", "C"), .version = 2), "sqrt((DW_se)^2 + (SOC_se)^2)")
  expect_equal(fct_make_formula_U("DG_ratio", NA, .version = 2), "0")
})

test_that("IPCC rules equal first-order (delta method) se for independent inputs", {
  env <- list(AGB = 250, AGB_se = 25, RS = 0.24, RS_se = 0.05, BGB = 60, BGB_se = 9,
              DW = 12, DW_se = 3, LI = 3, LI_se = 1, SOC = 80, SOC_se = 15, CF = 0.47, CF_se = 0.02)
  cases <- list(
    list(c("AGB", "RS", "DW"), "DM", 1),
    list(c("AGB", "BGB", "LI", "SOC"), "DM", 1),
    list(c("AGB", "RS", "DW"), c("DM", NA, "C"), 2),
    list(c("AGB", "BGB", "DW"), c("DM", "C", "C"), 2)   # CF once: shared CF across terms breaks independence
  )
  for (x in cases) {
    f  <- fct_make_formula2(x[[1]], x[[2]], .version = x[[3]])
    fu <- fct_make_formula_U(x[[1]], x[[2]], .version = x[[3]])
    inputs <- intersect(c(x[[1]], "CF"), all.vars(parse(text = f)))
    var <- 0
    for (v in inputs) {
      h <- env[[v]] * 1e-6
      up <- env; up[[v]] <- env[[v]] + h
      dn <- env; dn[[v]] <- env[[v]] - h
      d  <- (eval(parse(text = f), up) - eval(parse(text = f), dn)) / (2 * h)
      var <- var + (d * env[[paste0(v, "_se")]])^2
    }
    expect_equal(eval(parse(text = fu), env), sqrt(var), tolerance = 1e-6)
  }
})
