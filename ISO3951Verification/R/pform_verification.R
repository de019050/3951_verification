library(readr)
library(dplyr)

getwd()
setwd(getwd())

# Allreflex Data until 25012024
ALLREFLEX <- read_csv("DATA/ALLREFLEX.csv")

# Kform Examples from Chapter 16.2 of ISO 3951-2
ISO_PFORMTest <- read_csv("DATA/ISO_PFORMTest.csv")


#
# Create Test Input Data
#
ISO_PFORM_Input<-ISO_PFORMTest %>%
  group_by(Charge,LSL,USL,pValue,fsFac) %>%
  summarise(n=n(),value=mean(Value,na.rm=T), std=sd(Value,na.rm=T))



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





# My new Verification Pform function
# Define the function with seven parameters
myPFunction <- function(input1, input2, input3, input4, input5, input6, input7) {
  MeanValue<-input1
  STD<-input2
  LSL<-input3
  USL<-input4
  pFac<-input5
  fsFac<-input6
  n<- input7

  # Check if any inputs are missing, NA, or not numeric
  inputs <- list(input1, input2, input3, input4, input5, input6, input7)
  if (any(sapply(inputs, is.na)) || any(sapply(inputs, is.null)) || !all(sapply(inputs, is.numeric))) {
    stop("All inputs must be provided, non-NA, and numeric.")
  }

  # Check if input4 is larger than input3
  if (USL <= LSL) {
    stop("USL must be larger than LSL.")
  }

  # Perform the operation
  # Example operation: sum of all inputs
  if (STD>(USL-LSL)*fsFac){
    cat ('Lot can already be rejected,  STD = ',STD,' is > than SMAX =',round((USL-LSL)*fsFac,digits=3),' But check 2. calculation!\n')
  #}
  result <- sum(unlist(inputs))
  #return(result)

  q_l = (MeanValue-LSL)/STD
  x_l = 0.5*(1-(q_l*sqrt(n))/(n-1))
  p_hat_l = pbeta(as.numeric(x_l),(n-2)/2,(n-2)/2)

  cat ('q_l,x_l, p_hat_l',q_l, x_l, p_hat_l, '\n')
  q_u=(USL-MeanValue)/STD
  x_u=0.5*(1-(as.numeric(q_u)*sqrt(n))/(n-1))
  p_hat_u=pbeta(x_u,(n-2)/2,(n-2)/2)
  cat ('q_u,x_u, p_hat_u',q_u, x_u, p_hat_u, '\n')


  p_hat_val=p_hat_l+p_hat_u

  cat ('Lot shall be rejected,  p^ = ',round(p_hat_val,digit=6),' is > than p* =',pFac,'!')
  }
}

# Example usage
#myPFunction(MeanValue,STD,LSL,USL,pFac,fsFac,n)
myPFunction(3.5, 7.436,-10 , 10, 0.1925, 0.475, 3)
myPFunction(1,2,3,4,5,6,7)

