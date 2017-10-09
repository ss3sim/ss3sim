#' Change the effective sample sizes in length and age data in
#' the EM.
#'
#' @details reads in the case files information for sample_lencomp,
#' sample_agecomp or sample_calcomp and changes ESS according to the arguments
#' in the case files. For now the only option is to use the number of hauls
#'
#'
#' @author Gwladys Lambert
#'
#' @template dat_list
#' @param ess_len lcomp_params
#' @param ess_ages acomp_params
#' @family changing functions
#' @export


change_ESS <- function(dat_list, ess_len=NULL,  ess_ages = NULL # , ess_mla = mla_params
){
  
  if (!is.null(ess_len$change_ess_now)) {
    for (idx_flt in 1:length(ess_len$fleets)) {
      idx_flt_temp <- ess_len$fleets[[idx_flt]]
      if(ess_len$ESS[[idx_flt]]=="hauls" & !ess_len$change_ess_now) {
        dat_list$lencomp$Nsamp[dat_list$lencomp$FltSvy == idx_flt_temp] <- ess_len$Nhauls[[idx_flt]]
      }
      if(is.numeric(ess_len$ESS[[idx_flt]])) {
        dat_list$lencomp$Nsamp[dat_list$lencomp$FltSvy == idx_flt_temp] <- ess_len$ESS[[idx_flt]]
      }
    }
   }
    
    if (!is.null(ess_ages$change_ess_now)) {
      
      for (idx_flt in 1:length(ess_ages$fleets)){
        idx_flt_temp <- ess_ages$fleets[[idx_flt]]
        if(ess_ages$ESS[[idx_flt]]=="hauls" & !ess_ages$change_ess_now) {
          sub <- dat_list$agecomp[dat_list$agecomp$FltSvy == idx_flt_temp,]
          for (idx_yrs in unique(sub$Yr)){
            sub1 <- sub[sub$Yr == idx_yrs,]
            if (length(sub1$Nsamp[sub1$Lbin_lo!=-1]) >0 ){
              dat_list$agecomp$Nsamp[dat_list$agecomp$FltSvy == idx_flt_temp & dat_list$agecomp$Yr == idx_yrs & dat_list$agecomp$Lbin_lo!=-1] <- 
                round(sub1$Nsamp/sum(sub1$Nsamp)*ess_ages$Nhauls[[idx_flt]],1)
            } 
            if (length(sub1$Nsamp[sub1$Lbin_lo==-1]) >0 ){
              dat_list$agecomp$Nsamp[dat_list$agecomp$FltSvy == idx_flt_temp & dat_list$agecomp$Yr == idx_yrs & dat_list$agecomp$Lbin_l==-1] <- 
                ess_ages$Nhauls[[idx_flt]]
            }
          }
        }
      }
      
    }
    
    #  WILL HAVE TO ADD OPTIONS HERE
    #
    # if(ess_mla$ESS=="hauls") {
    #   for (idx_flt in ess_mla$fleets)
    # }
    # 
  
    invisible(return(dat_list))
    
  }
  
  
  