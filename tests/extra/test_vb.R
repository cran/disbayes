source("data.r")

test_that("standard disbayes model, variational Bayes",{
  dbres <- disbayes(dat = ihdbristol,
                    inc_num = "inc_num", inc_denom = "inc_denom",
                    prev_num = "prev_num", prev_denom = "prev_denom",
                    mort_num = "mort_num", mort_denom = "mort_denom",
                    eqage = 40, loo=FALSE, method="vb", algorithm="meanfield")
  expect_s3_class(dbres, "disbayes")
  ## Needs more experience to interpret diagnostic values, e.g. khat
})
