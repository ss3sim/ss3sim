context("External growth estimation")

test_that("check correct name given to mean_outfile", {
    wd.orig <- getwd()
    on.exit(setwd(wd.orig))
    d <- system.file("extdata", package = "ss3sim")
    om <- paste0(d, "/models/cod-om")
    em <- paste0(d, "/models/cod-em")
    #Write case files here
    temppath <- file.path(tempdir(), "ss3sim")
    dir.create(temppath, showWarnings = FALSE)
    setwd(temppath)
    file.copy(paste0(d, "/eg-cases/"), ".", recursive = TRUE)

    writeX <- function(fleets, years, Nsamp, species, case,
                       mean_outfile = NULL) {
      a <- c(paste("fleets;", fleets),
             paste("years;", years),
             paste("Nsamp;", Nsamp),
             paste("mean_outfile;", mean_outfile))
      writeLines(a, paste0("mlacomp", case, "-", species, ".txt"))
    }

    writeX(fleets = NULL, years = NULL, Nsamp = NULL, "cod",
           case = 0, mean_outfile = NULL)
    writeX(fleets = "c(1)", years = "list(c(20))",
         Nsamp = "list(50)", "cod", case = 1, mean_outfile = "vbgf")
    writeX(fleets = "c(1)", years = "list(c(20))",
         Nsamp = "list(50)", "cod", case = 1, mean_outfile = "vbgf_remove")

    test <- c("D0-E0-F0-R0-M0-cod", "D0-E0-F0-R1-M0-cod")
    casestructure <- list(D = c("agecomp", "lcomp"),
                          E = "E",
                          F = "F",
                          I = "index",
                          R = "R",
                          X = "mlacomp")
    run_ss3sim(iterations = 1, scenarios = test[1],
           case_folder = case_folder, case_files = casestructure,
           om_dir = om, em_dir = em, bias_adjust = FALSE,
           ignore.stdout = TRUE, show.output.on.console = FALSE)
    sample_mlacomp(om, ".", recursive = TRUE)
    file.remove("cod-om/codOM.dat")
  expect_error({
    verify_input(model_dir = "cod_om", type = "om")
  }
  )
    unlink("cod-om", recursive = TRUE)
})