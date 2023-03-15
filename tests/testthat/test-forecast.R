test_that("check_forecast is working", {
  file <- system.file(
    package = "ss3sim",
    "extdata", "models", "cod-em", "forecast.ss"
  )
  forecast <- r4ss::SS_readforecast(file, verbose = FALSE)
  expect_silent(check_forecast(forecast))
  forecastnew <- check_forecast(forecast)
  expect_equal(forecastnew$Forecast, 2,
    label = "check_forecast didn't change F to F_msy, i.e., "
  )
  forecast$Fcast_years[3] <- 10
  expect_error(check_forecast(forecast),
    label = "Positive Fcast year in check_forecast"
  )
  forecast$Bmark_years[5] <- 10
  expect_error(check_forecast(forecast),
    label = "Positive Bmark year in check_forecast"
  )
})
