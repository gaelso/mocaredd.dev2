test_that("means match fct_arithmetic_mean2() on the extdata templates", {
  for (f in c("mocaredd-templatev2-simple.xlsx", "mocaredd-templatev2-4pools.xlsx", "example1-4pools.xlsx")) {
    checked <- fct_checkinput(.path = system.file("extdata", f, package = "mocaredd.dev2"))
    r2 <- fct_arithmetic_mean2(checked)
    r3 <- fct_arithmetic_mean3(checked)
    expect_equal(r3$ER$E, r2$ER$E[match(r3$ER$period_type, r2$ER$period_type)])
  }
})

test_that("IPCC rules: AGB * (1 + RS) * CF and ER = FREL - MON", {
  checked <- fct_checkinput(.path = system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2"))
  r3 <- fct_arithmetic_mean3(checked)

  ## carbon stock of one land use by hand: product rule on AGB, (1 + RS), CF
  cb  <- checked$data$carbon
  lu  <- r3$c_stock$c_lu_id[r3$c_stock$c_form == "AGB * (1 + RS) * CF"][1]
  agb <- cb[cb$c_lu_id == lu & cb$c_element == "AGB", ]
  rs  <- cb[cb$c_lu_id == lu & cb$c_element == "RS", ]
  cf  <- cb[cb$c_element == "CF", ]
  C    <- agb$c_value * (1 + rs$c_value) * cf$c_value
  C_se <- C * sqrt((agb$c_se / agb$c_value)^2 + (rs$c_se / (1 + rs$c_value))^2 + (cf$c_se / cf$c_value)^2)
  expect_equal(r3$c_stock$C[r3$c_stock$c_lu_id == lu], C)
  expect_equal(r3$c_stock$C_se[r3$c_stock$c_lu_id == lu], C_se)

  ## sum rule for ER
  er  <- r3$ER
  ref <- er[!grepl("^E", er$period_type), ][1, ]
  mon <- er[grepl("^E-", er$period_type), ][1, ]
  erm <- er[er$period_type == sub("^E-", "ER-", mon$period_type), ]
  expect_equal(erm$E_se, sqrt(ref$E_se^2 + mon$E_se^2))
})
