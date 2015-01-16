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
#' @seealso \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}},
#'   \code{\link{fill_across}}
#' @export

#For Debugging
# mat1 <- wtatage.new.list[[1]]
# mat <- wtatage.new.list[[1]]
# mat <- mat[seq(1, 99, 3), ]
# mat1 <- mat

# minYear <- datfile$styr
# maxYear <- datfile$endyr

fill_across <- function(mat, minYear, maxYear) {
  ##Initial Checks
  mat$yr <- abs(mat$yr)
  mat$index <- 1:nrow(mat)

  # check.mat <- mat

  #input matrix must have value for year 1
  if(length(unique(mat$fleet)) != 1) stop('Too Many Fleets')

  #Interpolate Values across Rows
  for(ii in 1:max(mat$index))
  {
    temp <- mat[ii, ]
    na.index <- which(is.na(temp))

    for(jj in na.index)
    {
      if(is.na(temp[jj]))
      {
        start.index <- jj - 1
        start <- temp[start.index]

        find.end <- jj

        #Find the end of the NA string
        while(is.na(temp[find.end]))
        {
          if(find.end == ncol(temp)) break
          find.end <- find.end + 1
        }

        # Fill across if end value missing
        if(find.end == ncol(temp))
        {
          end.index <- ncol(temp)
          temp[(start.index + 1):end.index] <- start
        }

        end.index <- find.end
        end <- temp[end.index]

        #Linearly Interpolate Between Start and End
        for(fill in (start.index+1):(end.index-1))
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

  #Create Temporary Data frame
  temp.df <- as.data.frame(matrix(nrow = length(seq(minYear, maxYear)), ncol = ncol(mat) ))
  temp.df[mat$yr, ] <- mat
  names(temp.df) <- names(mat)
  temp.df$yr <- seq(minYear, maxYear)

  #Back Fill Rows
  fill.index <- c(minYear, which(is.na(temp.df$age0) == FALSE), maxYear)
  if(length(which(fill.index == 1)) != 1) stop('Did you really have wtatage data in the first year?')
  #Remove Duplicates, occurs when input matrix has values in mat[maxYear, ]

  if(sum(duplicated(fill.index)) > 0){
    fill.index <- fill.index[-which(duplicated(fill.index))]
  }

  diffs <- diff(fill.index)

  for(ii in 1:length(fill.index))
  {
    curr <- fill.index[ii]

    if(curr == 1) next

    if(diffs[ii - 1] == 1 & ii != 99) next

    prev <- fill.index[ii - 1]

    if(ii == 2)
    {
      temp.df[prev:(curr - 1), -1] <- temp.df[curr, -1]
    } else {
      temp.df[(prev + 1):(curr -1), -1] <- temp.df[curr, -1]
    }

    #If Last Row is Missing, fill Forwards
    if(ii == length(fill.index) & is.na(temp.df[fill.index[ii], 'age0']))
    {
     temp.df[(prev + 1):curr, -1] <- temp.df[prev, -1]
    }
  }

  #check to make sure that first year is filled
  if(is.na(temp.df[1, 'age0']))
  {
    temp.df[1, -1] <- temp.df[2, -1]
    temp.df[1, 'yr'] <- 1
  }

  #check to make sure that last year is filled
  if(is.na(temp.df[100, 'age0']))
  {
    temp.df[100, -1] <- temp.df[99, -1]
    temp.df[100, 'yr'] <- 100
  }

  return(temp.df)
}


# tt <- fill_across(mat = mat1, minYear = minYear, maxYear = maxYear)
# # mat1 <- temp.df
# # mat2 <- as.data.frame(matrix(nrow = length(seq(minYear, maxYear)), ncol = ncol(mat) ))
# # mat2[mat$yr, ] <- mat
# # names(mat2) <- names(mat1)

# write.csv(mat1, file = 'mat1.csv')
# write.csv(tt, file = 'temp_df.csv')
# write.csv(mat2, file = 'mat2.csv')


# #Used For Testing
# setwd('/Users/peterkuriyama/School/Research/capam_growth/Empirical/test')
# test.dat <- read.csv('preFillMat.csv')
# load('test_list.Rdata')
# test.list <- temp.list

# mat <- test.list[[1]]
# minYear <- 1
# maxYear <- 100
# fill_across(test.list[[2]], 1, 100)

# #Test values in last row
# mat <- test.list[[1]]
# mat <- rbind(mat, mat[3, ])
# mat[4, 'yr'] <- 100
# mat[4, 'age0'] <- .555555

# fill_across(mat, 1, 100)

# #Test values in first row
# mat <- test.list[[1]]
# mat <- rbind(mat, mat[3, ])
# mat[4, 'yr'] <- 1
# mat[4, 'age0'] <- .555555

# mat <- mat[order(mat$yr), ]

# order(mat['yr', ])


# mat <- test.dat
# mat$X <- NULL

# minYear <- 1
# maxYear <- 100

# xx <- fill_across(mat=mat, minYear = 1, maxYear = 100)
# write.csv(xx, file = 'check_fill_across.csv')
