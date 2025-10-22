
#' Not used in Package
#'
#' @return
#' @export
#'
#' @examples
#'
myHelperFunction <- function() {
library(readr)
library(dplyr)
library(tidyr)
library(moments)
library(ggplot2)
library(plotly)
library(stringr)


# Kform Examples from Chapter 16.2 of ISO 3951-2
ISO_KFORMTest <- read_csv("DATA/ISO_KFORMTest.csv")


#
# Create Test Input Data
#
ISO_KFORM_Input<-ISO_KFORMTest %>%
  group_by(Charge,LSL,USL,kValue) %>%
  summarise(n=n(),value=mean(Value,na.rm=T), std=sd(Value,na.rm=T))



# pform Examples from Chapter 16.3.2 of ISO 3951-2
ISO_PFORMTest <- read_csv("DATA/ISO_PFORMTest.csv")


#
# Create Test Input Data
#
ISO_PFORM_Input<-ISO_PFORMTest %>%
  group_by(Charge,LSL,USL,pValue,fsFac) %>%
  summarise(n=n(),value=mean(Value,na.rm=T), std=sd(Value,na.rm=T))


}
