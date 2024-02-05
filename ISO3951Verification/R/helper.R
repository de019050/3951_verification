
#' Title
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

getwd()
setwd(getwd())

# Allreflex Data until 25012024
ALLREFLEX <- read_csv("DATA/ALLREFLEX.csv",
                      col_types = cols(charge = col_character()))

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

simstart_ALL_Reflex_Num<-ALLREFLEX %>%
  group_by(mat_bez,charge) %>%
  # calculate aggregate values per batch
  summarize(n=n(),
            mdose=mean(`R-P001`,na.rm=T), sdose=sd(`R-P001`,na.rm=T), mindose=min(`R-P001`,na.rm=T), maxdose=max(`R-P001`,na.rm=T),skdose=skewness(`R-P001`,na.rm=T),kdose=kurtosis(`R-P001`,na.rm=T),
            mitime=mean(`R-P027`,na.rm=T),sitime=sd(`R-P027`,na.rm=T), minitime=min(`R-P027`,na.rm=T), maxitime=max(`R-P027`,na.rm=T),skitime=skewness(`R-P027`,na.rm=T),kitime=kurtosis(`R-P027`,na.rm=T),
            midepth=mean(`R-P028`,na.rm=T),sidepth=sd(`R-P028`,na.rm=T), minidepth=min(`R-P028`,na.rm=T), maxidepth=max(`R-P028`,na.rm=T),skidepth=skewness(`R-P028`,na.rm=T),kidepth=kurtosis(`R-P028`,na.rm=T),
            mact=mean(`R-P026`,na.rm=T),sact=sd(`R-P026`,na.rm=T), minact=min(`R-P026`,na.rm=T), maxact=max(`R-P026`,na.rm=T),skact=skewness(`R-P026`,na.rm=T),kact=kurtosis(`R-P026`,na.rm=T),
            mnpos=mean(`R-P029`,na.rm=T),snpos=sd(`R-P029`,na.rm=T), minnpos=min(`R-P029`,na.rm=T), maxnpos=max(`R-P029`,na.rm=T),sknpos=skewness(`R-P029`,na.rm=T),knpos=kurtosis(`R-P029`,na.rm=T))|>
            mutate(newn = ifelse(n>=497,110,ifelse(n<=310 & n<=320,58,60)))%>%mutate(NewN20=ceiling(newn*(1+0.2^2)))|>#round(newn*(1+0.2^2),digits=0))  |>
            #added constnts from ISO
            mutate(kfac = ifelse(newn == 58, 2.592, ifelse(newn == 60, 2.5730, 2.449)))%>%
            mutate(pfac = ifelse(newn == 58, 0.003872, ifelse(newn == 60, 0.004150, 0.006602)))%>%
            mutate(fsfac = ifelse(newn == 58, 0.179, ifelse(newn == 60, 0.180, 0.180)))%>%
            # added speclimits from EDOs
            #first  one sided
            mutate(dose_lsl= ifelse(mat_bez == "FITUSIRAN", 0.5, ifelse(mat_bez == "MARS", 1.14, 2.0)))%>%
            mutate(itime_usl= ifelse(mat_bez == "FITUSIRAN", 15, ifelse(mat_bez == "MARS", 15, 15))) %>%
            mutate(npos_usl= ifelse(mat_bez == "FITUSIRAN", 4.5, ifelse(mat_bez == "MARS", 4.5, 4.6)))%>%
            # now 2-sided
            mutate(act_lsl= ifelse(mat_bez == "FITUSIRAN", 3, ifelse(mat_bez == "MARS", 3, 3)))%>%
            mutate(act_usl= ifelse(mat_bez == "FITUSIRAN", 16, ifelse(mat_bez == "MARS", 16, 24)))%>%
            mutate(idepth_lsl= ifelse(mat_bez == "FITUSIRAN", 4, ifelse(mat_bez == "MARS", 4, 4)))%>%
            mutate(idepth_usl= ifelse(mat_bez == "FITUSIRAN", 8, ifelse(mat_bez == "MARS", 8, 8)))






# PREPARING DATAFOR tRENDINGAND CHARTING

mt<-as_tibble(simstart_ALL_Reflex_Num)
head(mt)


mt_long<-pivot_longer(mt,cols=c(mdose,mact,mitime,midepth,mnpos), names_to="EDO",values_to="Results")









# creating equidistant data
# Equidistant Sampling function
# Funktion sie Index ausgeben soll basierend auf Input-Size
# Create sample dataframe
# Define function to perform equidistant sampling

# e.g. like
# split_func(row_number(my_df%>%filter(batch=="2F170")),max(row_number(my_df%>%filter(batch=="2F170")))/38)
# num [1:39] 1 14 27 40 53 66 79 93 106 119 ...
# 1 14 27 40 53 66 79 93 106 119 132 145 158 172 185 198 211 224 237 250 264 277 290 303 316 329 343 356 369 382 395 408 422 435 448 461 474 487 500

split_func <- function(x, by) {
  r <- diff(range(x))
  #out <- if(any(is.infinite(c(x,by)))) c(10, 10) else seq(0, r - by - 1, by = by)
  out <-  seq(0, r - by - 1, by = by)

  #out <- seq(0, r - by - 1, by = by)
  #as_tibble(c(round(min(x) + c(0, out - 0.51 + (max(x) - max(out)) / 2), 0), max(x)))
  (c(round(min(x) + c(0, out - 0.51 + (max(x) - max(out)) / 2), 0), max(x)))
}
#not working du to infinite values??
# equidistant  sampling using split_func()
# col 15 is sample size changed for gamma-factor=0.2
newdata<-tibble()
newdatatemp1<-tibble()
newdatatemp2<-tibble()
ALL_REFLEX<-ALLREFLEX
k<-1
for(i in unique(simstart_ALL_Reflex_Num$charge) ) {
  a1<-split_func(row_number(ALL_REFLEX%>%filter(charge==i)%>%dplyr::select(charge)),max(row_number(ALL_REFLEX%>%filter(charge==i)%>%dplyr::select(charge)))/unlist(simstart_ALL_Reflex_Num[k,35]))
  newdata<-ALL_REFLEX%>%filter(charge==i)%>%slice(a1-1)#%>%mutate(order=row_number())
  newdatatemp2<-rbind(newdatatemp1,newdata)
  newdatatemp1<-newdatatemp2
  k = k+1
}

equidata<-newdatatemp1 %>%
  group_by(mat_bez,charge) %>%
  # calculate aggregate values per batch
  summarize(n=n(),
            mdose=mean(`R-P001`,na.rm=T), sdose=sd(`R-P001`,na.rm=T), mindose=min(`R-P001`,na.rm=T), maxdose=max(`R-P001`,na.rm=T),skdose=skewness(`R-P001`,na.rm=T),kdose=kurtosis(`R-P001`,na.rm=T),
            mitime=mean(`R-P027`,na.rm=T),sitime=sd(`R-P027`,na.rm=T), minitime=min(`R-P027`,na.rm=T), maxitime=max(`R-P027`,na.rm=T),skitime=skewness(`R-P027`,na.rm=T),kitime=kurtosis(`R-P027`,na.rm=T),
            midepth=mean(`R-P028`,na.rm=T),sidepth=sd(`R-P028`,na.rm=T), minidepth=min(`R-P028`,na.rm=T), maxidepth=max(`R-P028`,na.rm=T),skidepth=skewness(`R-P028`,na.rm=T),kidepth=kurtosis(`R-P028`,na.rm=T),
            mact=mean(`R-P026`,na.rm=T),sact=sd(`R-P026`,na.rm=T), minact=min(`R-P026`,na.rm=T), maxact=max(`R-P026`,na.rm=T),skact=skewness(`R-P026`,na.rm=T),kact=kurtosis(`R-P026`,na.rm=T),
            mnpos=mean(`R-P029`,na.rm=T),snpos=sd(`R-P029`,na.rm=T), minnpos=min(`R-P029`,na.rm=T), maxnpos=max(`R-P029`,na.rm=T),sknpos=skewness(`R-P029`,na.rm=T),knpos=kurtosis(`R-P029`,na.rm=T))|>
  mutate(newn = ifelse(n>=497,110,ifelse(n<=310 & n<=320,58,60)))%>%mutate(NewN20=ceiling(newn*(1+0.2^2)))  |>
  #added constnts from ISO
  mutate(kfac = ifelse(newn == 58, 2.592, ifelse(newn == 60, 2.5730, 2.449)))%>%
  mutate(pfac = ifelse(newn == 58, 0.003872, ifelse(newn == 60, 0.004150, 0.006602)))%>%
  mutate(fsfac = ifelse(newn == 58, 0.179, ifelse(newn == 60, 0.180, 0.180)))%>%
  # added speclimits from EDOs
  #first  one sided
  mutate(dose_lsl= ifelse(mat_bez == "FITUSIRAN", 0.5, ifelse(mat_bez == "MARS", 1.14, 2.0)))%>%
  mutate(itime_usl= ifelse(mat_bez == "FITUSIRAN", 15, ifelse(mat_bez == "MARS", 15, 15))) %>%
  mutate(npos_usl= ifelse(mat_bez == "FITUSIRAN", 4.5, ifelse(mat_bez == "MARS", 4.5, 4.6)))%>%
  # now 2-sided
  mutate(act_lsl= ifelse(mat_bez == "FITUSIRAN", 3, ifelse(mat_bez == "MARS", 3, 3)))%>%
  mutate(act_usl= ifelse(mat_bez == "FITUSIRAN", 16, ifelse(mat_bez == "MARS", 16, 24)))%>%
  mutate(idepth_lsl= ifelse(mat_bez == "FITUSIRAN", 4, ifelse(mat_bez == "MARS", 4, 4)))%>%
  mutate(idepth_usl= ifelse(mat_bez == "FITUSIRAN", 8, ifelse(mat_bez == "MARS", 8, 8)))










# Test plotter
plotdataset <<- mt_long %>% filter(EDO == "mact")

gg_plot1 <- ggplot(plotdataset,
                   aes(x = charge,color = mat_bez))+
  geom_point(aes( y = Results)) +
  geom_point(aes( y = maxact)) +
  geom_point(aes( y = minact)) +
  geom_point(aes( y = sact)) +
  #geom_point(aes( y = ((maxact-Results)-(Results-minact))/(maxact-minact)*100)) +
  xlab("Batches") +
  ylab("Dose Accuracy") +
  #geom_hline(yintercept = ifelse(plotdataset$mat_bez=="JUPITER",2,1.14), color = "red", linetype = "dashed", alpha = 0.5) +
  #ylim(c(ifelse(plotdataset$mat_bez=="JUPITER",1.5,0.5), ifelse(plotdataset$mat_bez=="JUPITER",2.5,1.5))) +
  #geom_hline(yintercept = ifelse(plotdataset$mat_bez=="JUPITER",2,1.14), color = "red", linetype = "dashed", alpha = 0.5) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="None")
gg_plot1

ggplotly(gg_plot1)
