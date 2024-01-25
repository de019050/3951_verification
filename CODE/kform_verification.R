library(readr)
ALLREFLEX <- read_csv("DATA/ALLREFLEX.csv")



# New k-form function 
# bases on tibble containing all neccessary informations
# input only one tibble

k_form<-function(data){
  
  
  # itime
  k_u_itime=ifelse((data$itime_usl-data$mitime)/data$sitime-data$kfac<0,"FALSE","TRUE")
  k_u_itime_val=(data$itime_usl-data$mitime)/data$sitime-data$kfac
  # dose
  k_l_dose=ifelse((data$mdose-data$dose_lsl)/data$sdose-data$kfac<0,"FALSE","TRUE")
  k_l_dose_val=(data$mdose-data$dose_lsl)/data$sdose-data$kfac
  # npos
  k_l_npos=ifelse((data$mnpos-data$npos_usl)/data$snpos-data$kfac<0,"FALSE","TRUE")
  k_l_npos_val=(data$mnpos-data$npos_usl)/data$snpos-data$kfac
  
  
  #return(cbind(k_u_itime,k_u_itime_val+k, k,k_l_dose,k_l_dose_val+k,k,k_l_npos,k_l_npos_val+k, k))
  return(cbind(data$charge,data$mat_bez,k_u_itime,k_u_itime_val,k_l_dose,k_l_dose_val,k_l_npos,k_l_npos_val, data$kfac))
  
}



