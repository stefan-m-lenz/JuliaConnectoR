test_that("Installation of unknown version is reported", {
   expect_error(installJulia("1.2.3.4"), regexp = "Julia version \"1.2.3.4\" cannot be identified")
})

test_that("Installation of unknown version is reported", {
   expect_error(installJulia("1.4.0"), regexp = "only supported as archive")
})

