#' Fill in matrix across rows of weight-at-age data by interpolation
#'
#' Function that fills in matrix across rows of wtatage data by interpolation
#' Missing Rows are then backfilled
#'
#' @param mat A matrix
#' @param minYear Minimum year
#' @param maxYear Maximum year
#'
#' @author Peter Kuriyama and Allan Hicks
#'
#' @seealso
#' * [sample_lcomp()]
#' * [sample_agecomp()]
#' * [sample_wtatage()]
#' @export
#'
fill_across <- function(mat, minYear, maxYear) {
  ## Initial Checks
  mat$Yr <- abs(mat$Yr)
  mat$index <- seq_len(nrow(mat))

  # check.mat <- mat

  # input matrix must have value for year 1
  if (length(unique(mat$Fleet)) != 1) stop("Too Many Fleets")

  # Interpolate Values across Rows
  for (ii in seq_len(max(mat$index))) {
    temp <- mat[ii, ]
    na.index <- which(is.na(temp))

    for (jj in na.index)
    {
      if (is.na(temp[jj])) {
        start.index <- jj - 1
        start <- temp[start.index]

        find.end <- jj

        # Find the end of the NA string
        while (is.na(temp[find.end])) {
          if (find.end == ncol(temp)) break
          find.end <- find.end + 1
        }

        # Fill across if end value missing
        if (find.end == ncol(temp)) {
          end.index <- ncol(temp)
          temp[(start.index + 1):end.index] <- start
        }

        end.index <- find.end
        end <- temp[end.index]

        # Linearly Interpolate Between Start and End
        for (fill in (start.index + 1):(end.index - 1))
        {
          val <- start + (end - start) * (fill - start.index) / (end.index - start.index)
          temp[fill] <- val
          # print(fill)
        }
      }
    }
    mat[ii, ] <- temp
  }

  mat$index <- NULL

  # Create Temporary Data frame
  temp.df <- as.data.frame(matrix(nrow = length(seq(minYear, maxYear)), ncol = ncol(mat)))
  temp.df[match(mat$Yr,seq(minYear,maxYear)), ] <- mat
  names(temp.df) <- names(mat)
  temp.df$Yr <- seq(minYear, maxYear)

  # Back Fill Rows
  fill.index <- c(1, which(is.na(temp.df$`0`) == FALSE), NROW(temp.df))
  if (length(which(fill.index == 1)) != 1) stop("Did you really have wtatage data in the first year?")
  # Remove Duplicates, occurs when input matrix has values in mat[maxYear, ]

  if (sum(duplicated(fill.index)) > 0) {
    fill.index <- fill.index[-which(duplicated(fill.index))]
  }

  diffs <- diff(fill.index)

  for (ii in seq_along(fill.index))
  {
    curr <- fill.index[ii]

    if (curr == 1) next

    if (diffs[ii - 1] == 1 & ii != 99) next

    prev <- fill.index[ii - 1]

    if (ii == 2) {
      temp.df[prev:(curr - 1), -1] <- temp.df[curr, -1]
    } else {
      temp.df[(prev + 1):(curr - 1), -1] <- temp.df[curr, -1]
    }

    # If Last Row is Missing, fill Forwards
    if (ii == length(fill.index) & is.na(temp.df[fill.index[ii], "0"])) {
      temp.df[(prev + 1):curr, -1] <- temp.df[prev, -1]
    }
  }

  # check to make sure that first year is filled
  if (is.na(temp.df[1, "0"])) {
    temp.df[1, -1] <- temp.df[2, -1]
    temp.df[1, "Yr"] <- 1
  }

  # check to make sure that last year is filled
  if (is.na(temp.df[100, "0"])) {
    temp.df[100, -1] <- temp.df[99, -1]
    temp.df[100, "Yr"] <- 100
  }

  return(temp.df)
}
