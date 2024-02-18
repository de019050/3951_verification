library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(moments)
library(ggplot2)
library(plotly)
library(stringr)
library(moments)
library(nortest)
library(sacolors)
library(gridExtra)
library(readxl)
library(tidyr)

SA_clr_defaults()


ZWICK_ROHDATA_A <- read_excel("C:/Users/De019050/Downloads/ZWICK_ROHDATA_A.xlsx",
                              col_types = c("text", "numeric", "numeric",
                                            "numeric", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric"))


ZWICK_ROHDATA_B <- read_excel("C:/Users/De019050/Downloads/ZWICK_ROHDATA_B.xlsx",
                              col_types = c("text", "numeric", "numeric",
                                            "numeric", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric"))





a<-ZWICK_ROHDATA_A|>
  filter(if_all(everything(), ~ !is.na(.x)))|>
  #filter(!is.na(`Device-ID`))|>
  pivot_longer(cols=2:10,names_to="EDO",values_to="Werte")|>
  mutate(AI="JUPITER")|>
  mutate(MTE="ZWICK")|>
  mutate(RUN="A")



b<-ZWICK_ROHDATA_B|>
  filter(if_all(everything(), ~ !is.na(.x)))|>
  #filter(!is.na(`Device-ID`))|>
  pivot_longer(cols=2:10,names_to="EDO",values_to="Werte")|>
  mutate(AI="MARS")|>
  mutate(MTE="ZWICK")|>
  mutate(RUN="B")


c <- rbind(a,b)

# Parameter e.g. for nortest::ad.test
paramter_for_dist <-c|>reframe(Mean = mean(if_any(Werte)),STD=sd(if_any(Werte)),Min=min(if_any(Werte)),Max=max(if_any(Werte)), STE= STD/sqrt(n()), .by = c(AI,EDO))

#plots for Jupiter
c|>filter(AI=='JUPITER')
# FKap, Cap removal Force
params_kap <- as.numeric(paramter_for_dist|> filter(AI=='JUPITER')|>filter(EDO=='FKap')|>dplyr::select(c(Mean,STD)))
FKap <- c|>filter(AI=='JUPITER')|>filter(EDO=='FKap')|>dplyr::select(Werte)
nortest::ad.test(FKap$Werte)
j1 <- SA_clr_plotdist(FKap$Werte, distr = "norm", params = params_kap, binwidth = 0.5, grid = TRUE)
#Fakt, actuation Force
params_akt <- as.numeric(paramter_for_dist|> filter(AI=='JUPITER')|>filter(EDO=='Fakt')|>dplyr::select(c(Mean,STD)))
Fakt <- c|>filter(AI=='JUPITER')|>filter(EDO=='Fakt')|>dplyr::select(Werte)
nortest::ad.test(Fakt$Werte)
j2 <- SA_clr_plotdist(Fakt$Werte, distr = "norm", params = params_akt, binwidth = 0.25, grid = TRUE)
#tinj, Injection Time
params_tinj <- as.numeric(paramter_for_dist|> filter(AI=='JUPITER')|>filter(EDO=='tinj')|>dplyr::select(c(Mean,STD)))
tinj <- c|>filter(AI=='JUPITER')|>filter(EDO=='tinj')|>dplyr::select(Werte)
nortest::ad.test(tinj$Werte)
j3 <- SA_clr_plotdist(tinj$Werte, distr = "norm", params = params_tinj, binwidth = 0.04, grid = TRUE)
#m, Mass of Dosage Volume
params_m <- as.numeric(paramter_for_dist|> filter(AI=='JUPITER')|>filter(EDO=='m')|>dplyr::select(c(Mean,STD)))
m <- c|>filter(AI=='JUPITER')|>filter(EDO=='m')|>dplyr::select(Werte)
nortest::ad.test(m$Werte)
j4 <- SA_clr_plotdist(m$Werte, distr = "norm", params = params_m, binwidth = 0.01, grid = TRUE)
#Linj, Length of Injection or Needle Length
params_Linj <- as.numeric(paramter_for_dist|> filter(AI=='JUPITER')|>filter(EDO=='Linj')|>dplyr::select(c(Mean,STD)))
Linj <- c|>filter(AI=='JUPITER')|>filter(EDO=='Linj')|>dplyr::select(Werte)
nortest::ad.test(Linj$Werte)
j5 <- SA_clr_plotdist(Linj$Werte, distr = "norm", params = params_Linj, binwidth = 0.1, grid = TRUE)
#Lnc, Length of Needle Covefr or Needle Cover Lock override Force
params_Lnc <- as.numeric(paramter_for_dist|> filter(AI=='JUPITER')|>filter(EDO=='Lnc')|>dplyr::select(c(Mean,STD)))
Lnc <- c|>filter(AI=='JUPITER')|>filter(EDO=='Lnc')|>dplyr::select(Werte)
nortest::ad.test(Lnc$Werte)
j6 <- SA_clr_plotdist(Lnc$Werte, distr = "norm", params = params_Lnc, binwidth = 0.1, grid = TRUE)


grid.arrange( j1,j2,j3,j4,j5,j6,ncol=2,nrow=3)


#plots for MARS
c|>filter(AI=='MARS')
# FKap, Cap removal Force
params_kap <- as.numeric(paramter_for_dist|> filter(AI=='MARS')|>filter(EDO=='FKap')|>dplyr::select(c(Mean,STD)))
FKap <- c|>filter(AI=='MARS')|>filter(EDO=='FKap')|>dplyr::select(Werte)
nortest::ad.test(FKap$Werte)
m1 <- SA_clr_plotdist(FKap$Werte, distr = "norm", params = params_kap, binwidth = 0.5, grid = TRUE)
#Fakt, actuation Force
params_akt <- as.numeric(paramter_for_dist|> filter(AI=='MARS')|>filter(EDO=='Fakt')|>dplyr::select(c(Mean,STD)))
Fakt <- c|>filter(AI=='MARS')|>filter(EDO=='Fakt')|>dplyr::select(Werte)
nortest::ad.test(Fakt$Werte)
m2 <- SA_clr_plotdist(Fakt$Werte, distr = "norm", params = params_akt, binwidth = 0.25, grid = TRUE)
#tinj, Injection Time
params_tinj <- as.numeric(paramter_for_dist|> filter(AI=='MARS')|>filter(EDO=='tinj')|>dplyr::select(c(Mean,STD)))
tinj <- c|>filter(AI=='MARS')|>filter(EDO=='tinj')|>dplyr::select(Werte)
nortest::ad.test(tinj$Werte)
m3 <- SA_clr_plotdist(tinj$Werte, distr = "norm", params = params_tinj, binwidth = 0.05, grid = TRUE)
#m, Mass of Dosage Volume
params_m <- as.numeric(paramter_for_dist|> filter(AI=='MARS')|>filter(EDO=='m')|>dplyr::select(c(Mean,STD)))
m <- c|>filter(AI=='MARS')|>filter(EDO=='m')|>dplyr::select(Werte)
nortest::ad.test(m$Werte)
m4 <- SA_clr_plotdist(m$Werte, distr = "norm", params = params_m, binwidth = 0.01, grid = TRUE)
#Linj, Length of Injection or Needle Length
params_Linj <- as.numeric(paramter_for_dist|> filter(AI=='MARS')|>filter(EDO=='Linj')|>dplyr::select(c(Mean,STD)))
Linj <- c|>filter(AI=='MARS')|>filter(EDO=='Linj')|>dplyr::select(Werte)
nortest::ad.test(Linj$Werte)
m5 <- SA_clr_plotdist(Linj$Werte, distr = "norm", params = params_Linj, binwidth = 0.1, grid = TRUE)
#Lnc, Length of Needle Covefr or Needle Cover Lock override Force
params_Lnc <- as.numeric(paramter_for_dist|> filter(AI=='MARS')|>filter(EDO=='Lnc')|>dplyr::select(c(Mean,STD)))
Lnc <- c|>filter(AI=='MARS')|>filter(EDO=='Lnc')|>dplyr::select(Werte)
nortest::ad.test(Lnc$Werte)
m6 <- SA_clr_plotdist(Lnc$Werte, distr = "norm", params = params_Lnc, binwidth = 0.1, grid = TRUE)



grid.arrange( m1,m2,m3,m4,m5,m6,ncol=2,nrow=3)








FKap <- ggplot(a)+
  geom_bar(aes(),color = AI)
FKap


Fakt <- ggplot(a)+
  geom_boxplot(aes(Fakt),outlier.colour = "red", outlier.shape = 1)

tinj <- ggplot(a)+
  geom_boxplot(aes(tinj),outlier.colour = "red", outlier.shape = 1)

m <- ggplot(a)+
  geom_boxplot(aes(m),outlier.colour = "red", outlier.shape = 1)

Linj <- ggplot(a)+
  geom_boxplot(aes(Linj),outlier.colour = "red", outlier.shape = 1)

Lnc <- ggplot(a)+
  geom_boxplot(aes(Lnc),outlier.colour = "red", outlier.shape = 1)


myplot <- function(i){
  p <- ggplot(a) + geom_boxplot(aes(y=i),outlier.colour = "red", outlier.shape = 1)
  return(p)
}

p <- lapply(c("FKap","Fakt","tinj","m","Linj","Lnc"), myplot)

do.call(grid.arrange, c(p))


grid.arrange( FKap,Fakt,tinj,m,Linj,Lnc,ncol=2,nrow=3)

grid.arrange(p,ncol=2,nrow=3)




Linj <- ggplot(a|>filter(EDO!="Device-ID")|>filter(EDO!="Lstartinj"),aes(x=EDO,y=Werte))+
  geom_boxplot()+
  facet_wrap(~EDO, scale="free")
Linj

Linj <- ggplot(a|>filter(EDO!="Device-ID")|>filter(EDO!="Lstartinj"),aes(x=Werte))+
  geom_histogram(alpha=0.6)+
  #geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  facet_wrap(~EDO, scale="free")
ggplotly(Linj)


Linj <- ggplot(a|>filter(EDO!="Device-ID")|>filter(EDO!="Lstartinj"),aes(x=order(`...1`,decreasing=FALSE),y=Werte))+
  geom_point()+
  facet_wrap(~EDO, scale="free")
ggplotly(Linj)

