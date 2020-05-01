library("testthat")
library("ss3sim")
# broke up tests to allow tests to run longer than 10 min in travis
# see https://github.com/travis-ci/travis-ci/issues/3849#issuecomment-345686242
# note that if tests in test that script do not start with "test.", they will not
# run
test_check("ss3sim", filter = "^[r-v]")
