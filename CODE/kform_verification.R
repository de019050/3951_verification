library(readr)
library(dplyr)

getwd()
setwd("/home/de019050/R/3951_verification/CODE")

# Allreflex Data until 25012024
ALLREFLEX <- read_csv("../DATA/ALLREFLEX.csv")

# Kform Examples from Chapter 16.2 of ISO 3951-2
ISO_KFORMTest <- read_csv("../DATA/ISO_KFORMTest.csv")


#
# Create Test Input Data
#
ISO_KFORM_Input<-ISO_KFORMTest %>%
  group_by(Charge,LSL,USL,kValue) %>% 
  summarise(n=n(),value=mean(Value,na.rm=T), std=sd(Value,na.rm=T))


# New k-form function 
# bases on tibble containing all neccessary informations
# input only one tibble

# k_form<-function(Data){
#   
#   
#   # itime
#   k_u_itime=ifelse((data$itime_usl-data$mitime)/data$sitime-data$kfac<0,"FALSE","TRUE")
#   k_u_itime_val=(data$itime_usl-data$mitime)/data$sitime-data$kfac
#   # dose
#   k_l_dose=ifelse((data$mdose-data$dose_lsl)/data$sdose-data$kfac<0,"FALSE","TRUE")
#   k_l_dose_val=(data$mdose-data$dose_lsl)/data$sdose-data$kfac
#   # npos
#   k_l_npos=ifelse((data$mnpos-data$npos_usl)/data$snpos-data$kfac<0,"FALSE","TRUE")
#   k_l_npos_val=(data$mnpos-data$npos_usl)/data$snpos-data$kfac
#   
#   
#   #return(cbind(k_u_itime,k_u_itime_val+k, k,k_l_dose,k_l_dose_val+k,k,k_l_npos,k_l_npos_val+k, k))
#   return(cbind(data$charge,data$mat_bez,k_u_itime,k_u_itime_val,k_l_dose,k_l_dose_val,k_l_npos,k_l_npos_val, data$kfac))
#   
# }



k_form_ISO<-function(MeanValue,STD,LSL,USL,kFac){
# Function to calculate k-form acceptance criteria
# for one-sided values
    if (LSL == 'NA') { 
      QU= (USL-MeanValue)/STD
      #print(round(QU,digits=3))
      if (QU >= kFac){
        cat('The Lot meets the acceptability criterion.criterion. QU =',round(QU,digits=3),'is geq than kfac =',kFac)  
      } else if (QU < kFac){
        cat('The Lot does not meet the acceptability criterion. QU =',round(QU,digits=3),'is less than kfac =',kFac)  
      } else {
        cat('Check inputs')  
      }
      
      } else if (USL == 'NA') {
      QL= (MeanValue-LSL)/STD
      #print(round(QL,digits = 3))
      if (QL >= kFac){
        cat('The Lot meets the acceptability criterion. QL =',round(QL,digits=3),'is geq than kfac =',kFac)  
      } else if (QL < kFac){
        cat('The Lot does not meet the acceptability criterion. QL =',round(QL,digits=3),'is less than kfac =',kFac)  
      } else {
        cat('Check inputs')  
      }
      } else if  (USL!= 'NA' ||LSL!= 'NA') {
      print("Two limits! Use p-form!")
    } else {
    print("Wrong inputs")
    }      
  
}

# Input Data from both examples in Chapter16.2
ISO_KFORM_Input
# Example1
k_form_ISO(54.615385,3.3301267,'NA',60,1.426)
# Example2
k_form_ISO(6.551,0.3251,4.0,'NA',2.580)
