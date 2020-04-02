#' Fill in matrix across rows of weight-at-age data by interpolation
#'
#' Function that fills in matrix across rows of wtatage data by interpolation
#' Missing Rows are then backfilled
#'
#' @param data A matrix
#'
#' @author Peter Kuriyama and Allan Hicks
#'
#' @seealso \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}},
#'   \code{\link{fill_across}}
#' @export

#For Debugging
# mat1 <- wtatage.new.list[[1]]
# mat <- wtatage.new.list[[1]]
# mat <- mat[seq(1, 99, 3), ]
# mat1 <- mat

fill_header <- function(data) {
  inputcolnames <- colnames(data)
  colnames(data) <- tolower(inputcolnames)
  data$yr <- abs(data$yr)
  data <- data[order(data$yr), ]
  prefix.true <- !grepl("^a[ge]*[0-9]+$", colnames(data))
  prefix <- data[, prefix.true]
  data <- data[, !prefix.true]
  return(list("prefix" = prefix, "data" = data, "cols" = inputcolnames))
}

fill_tail <- function(begin, new) {
  out <- cbind(begin[["prefix"]], new)
  colnames(out) <- begin[["cols"]]
  return(out)
}

fill_checks <- function(data) {
  for (coli in c("seas", "sex", "bio_pattern", "birthseas", "fleet")) {
    if (length(unique(data[, coli])) > 1) {
      stop("There is more than one ", coli, " in the data to be filled.")
    }
  }
  invisible(TRUE)
}

#' Fill rows in a matrix
#' @template mat
fill_across <- function(mat) {
  #Interpolate Values across Rows
  for(ii in 1:NROW(mat)) {
    temp <- mat[ii, ]
    if(all(is.na(temp))) next
    if(all(!is.na(temp))) next
    na.index <- which(is.na(temp))
    if (1 %in% na.index) {
      temp[1] <- temp[!(seq_along(temp) %in% na.index)][1]
    }
    for(jj in na.index) {
      if(is.na(temp[jj])) {
        start.index <- jj - 1
        start <- temp[start.index]

        find.end <- jj

        #Find the end of the NA string
        while(is.na(temp[find.end])) {
          if(find.end == ncol(temp)) break
          find.end <- find.end + 1
        }

        # Fill across if end value missing
        if(find.end == ncol(temp)) {
          end.index <- ncol(temp)
          temp[(start.index + 1):end.index] <- start
        }

        end.index <- find.end
        end <- temp[end.index]

        #Linearly Interpolate Between Start and End
        for(fill in (start.index+1):(end.index-1)) {
          val <- start + (end - start) * (fill - start.index) / (end.index - start.index)
          temp[fill] <- val
        }
      }
    }
    mat[ii, ] <- temp
  }
  return(mat)
}

#' Fill a matrix using columns.
#' 
#' @template mat
#' @export
#' @importFrom zoo na.approx
#' @importFrom utils head tail
#' @examples
#' data <- matrix(rnorm(50), ncol = 5)
#' data[seq(1,50,by = 3)] <- NA
#' data <- data.frame("Yr" = seq_along(NROW(data)),
#' "seas" = 1, "sex" = 1, "bio_pattern" = 1,
#' "birthseas" = 1, "fleet" = 1, data)
#' colnames(data) <- gsub("^x|^X", "age", colnames(data))
#' datafill <- fill_down(data)
#' testthat::expect_equal(datafill[4, 7], c("age1" = mean(data[c(3,5), 7])))
#' rm(data, datafill)
fill_down <- function(mat) {
  # Fill top and bottom rows that are all NA
  top <- apply(mat, 2, function(x) utils::head(which(!is.na(x)), 1))
  for (ii in seq_along(top)) {
    mat[1, ii] <- ifelse(length(top[[ii]]) == 0, NA, mat[top[[ii]],ii])
  }
  top <- apply(mat, 2, function(x) utils::tail(which(!is.na(x)), 1))
  for (ii in seq_along(top)) {
    mat[NROW(mat), ii] <- ifelse(length(top[[ii]]) == 0, NA, mat[top[[ii]],ii])
  }
  # Fill middle rows with average
  new <- zoo::na.approx(mat, na.rm = TRUE)
  return(new)
}

fill_wtatage <- function(data,
  type = c("across_down", "down_across", "across", "down")) {
  type <- match.arg(type, several.ok = FALSE)
  initialdata <- fill_header(data)
  prefix <- initialdata[["prefix"]]
  manip <- initialdata[["data"]]
  fill_checks(prefix)

  # Round 1
  if (grepl("^across", type)) {
    manip <- fill_across(manip)
  } else {
  if (grepl("^down", type)) {
    manip <- fill_down(manip)
  }}
  # Round 2
  if (grepl("_across$", type)) {
    manip <- fill_across(manip)
  } else {
    if (grepl("_down$", type)) {
      manip <- fill_down(manip)
    }
  }
  firstage <- grep("^a[ge]*[0-9]+$", colnames(manip))[1]
  colid <- firstage + 1
  while (all(is.na(manip[, firstage]))) {
    manip[, firstage:(colid - 1)] <- manip[, colid]
    colid <- colid + 1
    if (colid == NCOL(manip)) break
  }
  return(fill_tail(initialdata, manip))
}
