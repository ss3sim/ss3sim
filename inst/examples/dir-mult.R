
compare_dists <- function(nsamp = 100, cpar = 1, nyears = 1000, nsamp_same = FALSE) {

  probs <- rbeta(nyears, 3, 2)
  d <- gtools::rdirichlet(1, probs * (nsamp / cpar ^ 2 - 1))

  if (!nsamp_same) {
    m <- rmultinom(1, size = nsamp, prob = probs)
  } else {
    m <- rmultinom(1, size = nsamp / cpar ^ 2, prob = probs)
  }

  par(mfrow = c(2, 1))
  hist(d)
  hist(m / sum(m))

  print(mean(d))
  print(mean(m / sum(m)))
  print(sd(d))
  print(sd(m / sum(m)))
}

compare_dists(nsamp = 100)
compare_dists(nsamp = 1000000)

compare_dists(nsamp = 100, cpar = 2)
compare_dists(nsamp = 1000000, cpar = 2)

compare_dists(nsamp = 100, cpar = 2, nsamp_same = TRUE)
compare_dists(nsamp = 1000000, cpar = 2, nsamp_same = TRUE)
