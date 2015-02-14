#' Change population and observed length composition bins in an SS estimation
#' model
#'
#' \code{change_em_binning} alters the bin structure for the population and
#' length composition data in an SS estimation model. It is done by taking the
#' original length composition info from the EM \code{ss3.dat} then changing
#' according to the user's specification.
#'
#' @param file_in A character value giving the location of an SS \code{ss3.dat}
#'   file to input.
#' @param file_out A character value giving the location of an SS \code{ss3.dat}
#'   file to output.
#' @param bin_vector A numeric vector of bins to substitute into the
#'   \code{ss3.dat} file.
#' @param lbin_method A numeric value of either \code{"NULL","1","2","3"} to
#'   change the lbin_method for the population bin. Only supports either
#'   \code{"NULL","1","2"} at the moment. NULL means to keep it unchanged
#' @param write_file Should the \code{.dat} file be written? The new \code{.dat}
#'   file will always be returned invisibly by the function. Setting
#'   \code{write_file = FALSE} can be useful for testing. Note that you must
#'   supply a value to the argument \code{file_out}, but this argument can be
#'   set to any arbitrary value (such as \code{NULL}) if \code{write_file =
#'   FALSE}.
#' @importFrom r4ss SS_readdat SS_writedat
#' @export
#' @seealso \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}}
#' @family change functions
#' @author Kotaro Ono
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' l <- change_em_binning(f_in, file_out = NULL, lbin_method=1,
#'   bin_vector = seq(8,30,by=1), write_file = FALSE)
#' print(l$lbin_vector)
#' print(head(l$lencomp))
#'
#' l <- change_em_binning(f_in, file_out = NULL, lbin_method=1,
#'   bin_vector = seq(10,27,by=2), write_file = FALSE)
#' print(l$lbin_vector)
#' print(head(l$lencomp))

change_em_binning <- function(file_in, file_out, lbin_method=NULL, bin_vector,
  write_file = TRUE) {

    ## If lbin_method is NULL then don't do anything
    if(is.null(lbin_method)) return(NULL)

# error checking
  if(!is.numeric(bin_vector)) {
    stop("bin_vector must be numeric")
  }
  if(length(bin_vector) == 1) {
    warning(paste("length(bin_vector) == 1; are you sure you",
       "input a full numeric vector of bins and not a bin size?"))
  }
  if(!is.null(lbin_method)) {
	  if (lbin_method >2) stop("lbin_method method should be either NULL, 1 or 2")
  }

# load the datfile and other check
  datfile <- SS_readdat(file = file_in, verbose = FALSE)
  datfile <- change_fltname(datfile)
  if(is.null(datfile$lencomp)) {
    stop("no lcomp data. Verify your case argument files")
  }
  if(datfile$Ngenders > 1) {
    stop(paste("_Ngenders is greater than 1 in the model.",
        "change_em_binning only works with single-gender models."))
  }
  if(max(bin_vector)>max(datfile$lbin_vector)) {
    stop(paste("the maximum value in the bin_vector is above the original one",
	"this column will be filled with zero observation so it is meaningless"))
  }
  if(min(bin_vector)<min(datfile$lbin_vector)) {
    stop(paste("the minimum value in the bin_vector is below the original one",
	"this column will be filled with zero observation so it is meaningless"))
  }

# Find ID columns and data columns to replace:
  old_len_columns <- grep("^l[0-9.]+$", names(datfile$lencomp))
  id_columns <- seq_along(names(datfile$lencomp))[-old_len_columns]
  newdummy <- datfile$lencomp[, old_len_columns]

# change population length bin width
  lcomp_new <- as.data.frame(matrix(0, nrow=nrow(newdummy), ncol=length(bin_vector)))
  names(lcomp_new) <- paste0("l", bin_vector)
  for (i in 1:length(bin_vector)) {
  	if (i==1) {
  		select_col <- which(datfile$lbin_vector < bin_vector[i+1])
  		if(length(select_col)>1) lcomp_new[,i] = apply(newdummy[,select_col],1,sum, na.rm=TRUE)
  		if(length(select_col)==1) lcomp_new[,i] = newdummy[,select_col]
  	}
  	if (i>1 & i<length(bin_vector)) {
  		select_col <- which(datfile$lbin_vector >= bin_vector[i] & datfile$lbin_vector < bin_vector[i+1])
  		if(length(select_col)>1) lcomp_new[,i] = apply(newdummy[,select_col],1,sum, na.rm=TRUE)
  		if(length(select_col)==1) lcomp_new[,i] = newdummy[,select_col]
  	}
  	if (i == length(bin_vector)) {
  		select_col <- which(datfile$lbin_vector >= bin_vector[i])
  		if(length(select_col)>1) lcomp_new[,i] = apply(newdummy[,select_col],1,sum, na.rm=TRUE)
  		if(length(select_col)==1) lcomp_new[,i] = newdummy[,select_col]
  	}
  }

# Substitute new bins:
  datfile$lencomp <- data.frame(datfile$lencomp[, id_columns], lcomp_new)
  datfile$lbin_vector <- bin_vector
  datfile$N_lbins <- length(datfile$lbin_vector)

# change the lbin_method
  if (!is.null(lbin_method))
  {
 	 if(lbin_method == 1)
 	 {
 		 datfile$lbin_method = lbin_method
 		 datfile$binwidth = NULL
 		 datfile$minimum_size = NULL
 		 datfile$maximum_size = NULL
 	 }
  }

  if(write_file) {
    SS_writedat(datlist = datfile, outfile = file_out, overwrite=TRUE,
                verbose=FALSE)
  }

  invisible(datfile)
}
