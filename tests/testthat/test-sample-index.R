d <- system.file("extdata", package = "ss3sim")
a <- list(
  "index" = list(
    "fleets" = 2,
    "years" = list(seq(76, 100, by = 2)),
    "sds_obs" = list(0.2)
  )
)

exp_vals <- r4ss::SS_readdat(
  file = file.path(d, "example-om", "ss3_expected_values.dat"),
  verbose = FALSE
)

test_that("sample_index() works for 1 fleet", {
  set.seed(123)
  sampled_index <- with(
    a[["index"]],
    sample_index(
      dat_list = exp_vals,
      fleets = fleets,
      years = years,
      seas = list(unique(
        exp_vals[["CPUE"]][, "seas"]
      )),
      sds_obs = sds_obs
    )
  )
  # check SD filled in correctly
  expect_equal(
    sampled_index[["CPUE"]][, "se_log"],
    rep(
      a[["index"]][["sds_obs"]][[1]],
      length.out = nrow(sampled_index[["CPUE"]])
    )
  )
  # check only correct fleets used.
  expect_equal(
    order(unique(sampled_index[["CPUE"]][, "index"])),
    order(unique(a[["index"]][["fleets"]]))
  )
  # check input values sampled correctly (not generalizable to other cases)
  set.seed(123)
  samples <- exp_vals[["CPUE"]][, "obs"] *
    exp(rnorm(length(exp_vals[["CPUE"]][, "obs"]),
      mean = 0,
      sd = a[["index"]][["sds_obs"]][[1]]
    ) - a[["index"]][["sds_obs"]][[1]]^2 / 2)
  expect_equal(sampled_index[["CPUE"]][, "obs"], samples)
  # check that no CPUE are returned if fleets are NULL
  no_index <- with(
    a[["index"]],
    sample_index(
      dat_list = exp_vals,
      fleets = NULL,
      years = years,
      seas = list(unique(
        exp_vals[["CPUE"]][, "seas"]
      )),
      sds_obs = sds_obs
    )
  )
  expect_equal(NROW(no_index[["CPUE"]]), 0)
  expect_equal(no_index[["N_cpue"]], 0)
  # check that the function exits early if dat_list is not an r4ss list
  expect_error(sample_index(dat_list = list(happy = 1:3), fleets = 1))
})
