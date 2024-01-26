library(readr)
ALLREFLEX <- read_csv("DATA/ALLREFLEX.csv")



# New MSSD function 
# bases on tibble containing all neccessary informations
# input only one tibble

MSSD <-function(data){
  
  
  # actuation
  mssd_act=ifelse((data$act_usl-data$act_lsl)*data$fsfac-data$sact<0,"FALSE","TRUE")
  mssd_act_val=(data$act_usl-data$act_lsl)*data$fsfac-data$sact
  # itime
  #mssd_itime=
  # idepth
  mssd_idepth=ifelse((data$idepth_usl-data$idepth_lsl)*data$fsfac-data$sidepth<0,"FALSE","TRUE")
  mssd_idepth_val=(data$idepth_usl-data$idepth_lsl)*data$fsfac-data$sidepth
  # dose
  #mssd_dose=
  # npos
  #mssd_npos=
  
  #return=cbind(mssd_act,mssd_itime,mssd_idepth,mssd_dose,mssd_npos)
  return=cbind(data$charge,data$mat_bez,mssd_act,mssd_act_val,mssd_idepth,mssd_idepth_val,data$fsfac)
  
}



p_hat <- function(data){ 
  n<-data$newn
  # actuation
  q_l_act = (data$mact-data$act_lsl)/data$sact
  x_l_act = 0.5*(1-(q_l_act*sqrt(n))/(n-1))  
  p_hat_l_act = pbeta(as.numeric(x_l_act),(n-2)/2,(n-2)/2)
  
  q_u_act=(data$act_usl-data$mact)/data$sact
  x_u_act=0.5*(1-(as.numeric(q_u_act)*sqrt(n))/(n-1))  
  p_hat_u_act=pbeta(x_u_act,(n-2)/2,(n-2)/2)
  p_hat_act_val=p_hat_l_act+p_hat_u_act
  p_hat_act=ifelse(p_hat_act_val-data$pfac>0,"FALSE","TRUE")
  # itime
  # q_l_itime=(data$m_itime-data_2[1,4])/data$sd_itime
  # x_l_itime=0.5*(1-(q_l_itime*sqrt(n))/(n-1))  
  # p_hat_l_itime=pbeta(x_l_itime,(n-2)/2,(n-2)/2)
  # q_u_itime=(data_2[1,5]-data$m_itime)/data$sd_itime
  # x_u_itime=0.5*(1-(q_u_itime*sqrt(n))/(n-1))  
  # p_hat_u_itime=pbeta(x_u_itime,(n-2)/2,(n-2)/2)
  # p_hat_itime=p_hat_l_itime+p_hat_u_itime
  # idepth
  q_l_idepth=(data$midepth-data$idepth_lsl)/data$sidepth
  x_l_idepth=0.5*(1-(as.numeric(q_l_idepth)*sqrt(n))/(n-1))  
  p_hat_l_idepth=pbeta(x_l_idepth,(n-2)/2,(n-2)/2)
  q_u_idepth=(data$idepth_usl-data$midepth)/data$sidepth
  x_u_idepth=0.5*(1-(as.numeric(q_u_idepth)*sqrt(n))/(n-1))  
  p_hat_u_idepth=pbeta(x_u_idepth,(n-2)/2,(n-2)/2)
  p_hat_idepth_val=p_hat_l_idepth+p_hat_u_idepth
  p_hat_idepth=ifelse(p_hat_idepth_val-data$pfac>0,"FALSE","TRUE")
  
  # dose
  # q_l_dose=(data$m_dose-data_2[1,8])/data$sd_dose
  # x_l_dose=0.5*(1-(q_l_dose*sqrt(n))/(n-1))  
  # p_hat_l_dose=pbeta(x_l_dose,(n-2)/2,(n-2)/2)
  # q_u_dose=(data_2[1,9]-data$m_dose)/data$sd_dose
  # x_u_dose=0.5*(1-(q_u_dose*sqrt(n))/(n-1))  
  # p_hat_u_dose=pbeta(x_u_dose,(n-2)/2,(n-2)/2)
  # p_hat_dose=p_hat_l_dose+p_hat_u_dose
  
  # npos
  # q_l_npos=(data$m_npos-data_2[1,10])/data$sd_npos
  # x_l_npos=0.5*(1-(q_l_npos*sqrt(n))/(n-1))  
  # p_hat_l_npos=pbeta(x_l_npos,(n-2)/2,(n-2)/2)
  # q_u_npos=(data_2[1,11]-data$m_npos)/data$sd_npos
  # x_u_npos=0.5*(1-(q_u_npos*sqrt(n))/(n-1))  
  # p_hat_u_npos=pbeta(x_u_npos,(n-2)/2,(n-2)/2)
  # p_hat_npos=p_hat_l_npos+p_hat_u_npos
  
  
  #return=cbind(p_hat_act,p_hat_itime,p_hat_idepth,p_hat_dose,p_hat_npos)
  return=cbind(data$charge,data$mat_bez,p_hat_act_val,p_hat_act,p_hat_idepth_val,p_hat_idepth,data$pfac)
  
  
  
}