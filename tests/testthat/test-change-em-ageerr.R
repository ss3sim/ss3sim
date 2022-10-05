wd.old <- getwd()
temp_path <- file.path(tempdir(), "pars")
dir.create(temp_path, showWarnings = FALSE)
setwd(temp_path)
d <- system.file("extdata", package = "ss3sim")
# use OM because has a datafile, unlike codEM< and is a 1 sex model.
file.copy(file.path(d, "models", "cod-om"), ".", recursive = TRUE)
setwd("cod-om")
on.exit(setwd(wd.old), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)
datalist <- r4ss::SS_readdat("codOM.dat", verbose = F)

test_that("change_em_err works as expected", {
  fleets <- 1
  definition <- 2
  output <- suppressWarnings(change_em_ageerr(
    dat_list = datalist,
    outfile = NULL,
    fleets = fleets,
    definition = definition
  ))
  
  expect_equal(
    output$agecomp$Ageerr[min(which((output$agecomp$FltSvy==fleets) == TRUE)):
                            max(which((output$agecomp$FltSvy==fleets) == TRUE))], 
    rep(definition,((max(which((output$agecomp$FltSvy==fleets) == TRUE))+1)
                    -min(which((output$agecomp$FltSvy==fleets) == TRUE))))
  )
})
test_that("change_em_err stops on error when expected", {
  expect_error(
    change_em_ageerr(
      dat_list = datalist,
      outfile = NULL,
      fleets = 1,
      definition = 3 #definition does not exist
    ),
    "Supplied ageing error definition is not in your dat file."
  )
})
test_that("change_em_err stops on error when expected", {
  expect_error(
    change_em_ageerr(
      dat_list = datalist,
      outfile = NULL,
      fleets = 3, #fleet does not exist
      definition = 1
    ),
    "Supplied fleet is not in your dat file. Ageing error 
          definition cannot be changed."
  )
})
