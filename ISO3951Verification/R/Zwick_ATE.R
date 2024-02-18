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


# ZWICK_ROHDATA_A <- read_excel("C:/Users/De019050/Downloads/ZWICK_ROHDATA_A.xlsx",
#                               col_types = c("text", "numeric", "numeric",
#                                             "numeric", "numeric", "numeric", "numeric",
#                                             "numeric", "numeric", "numeric"))
#
#
# ZWICK_ROHDATA_B <- read_excel("C:/Users/De019050/Downloads/ZWICK_ROHDATA_B.xlsx",
#                               col_types = c("text", "numeric", "numeric",
#                                             "numeric", "numeric", "numeric", "numeric",
#                                             "numeric", "numeric", "numeric"))
#
#
#
#
#
# a<-ZWICK_ROHDATA_A|>
#   filter(if_all(everything(), ~ !is.na(.x)))|>
#   #filter(!is.na(`Device-ID`))|>
#   pivot_longer(cols=2:10,names_to="EDO",values_to="Werte")|>
#   mutate(AI="JUPITER")|>
#   mutate(MTE="ZWICK")|>
#   mutate(RUN="A")
#
#
#
# b<-ZWICK_ROHDATA_B|>
#   filter(if_all(everything(), ~ !is.na(.x)))|>
#   #filter(!is.na(`Device-ID`))|>
#   pivot_longer(cols=2:10,names_to="EDO",values_to="Werte")|>
#   mutate(AI="MARS")|>
#   mutate(MTE="ZWICK")|>
#   mutate(RUN="B")
#
#
#
# c <- rbind(a,b)
c <- read_csv("DATA/c.csv")

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














# Load the dataset
data <- c

# Create a boxplot
ggplot(data, aes(x=EDO, y=Werte, color=AI)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Adjusting the x-axis labels for better readability




ggplot(data, aes(x=AI, y=Werte, color=AI)) +
  geom_boxplot() +
  facet_wrap(~EDO, scales = "free_y") + # This will create a separate plot for each EDO value
  labs(title = "Zwick Data",
       subtitle = "First Comparability Study",  # Add title and subtitle
        x = "AI Type", # Custom x-axis label
        y = "Values") + # Custom y-axis label + # Add title and subtitle

    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), strip.text.x = element_text(angle = 0))


# Create a scatter plot with separate plots for each EDO and free y-axis scaling
ggplot(data, aes(x=c$...2, y=Werte, color=AI)) +  # Adjust 'x=MTE' if another column is intended for the x-axis
  geom_point(size=3) + # Increase the point size
  geom_line() + # Connect the dots
  facet_wrap(~EDO, scales = "free_y") + # Allows for free y-axis scaling within each facet
  labs(title = "Zwick Data",
       subtitle = "First Comparability Study",
       x = "Testnumber", # Custom x-axis label
       y = "Values") + # Custom y-axis label + # Add title and subtitle
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(angle = 0))
# ,
#         plot.title = element_text(hjust = 0.5), # Center the title
#         plot.subtitle = element_text(hjust = 0.5)) # Center the subtitle



# Create histograms with separate plots for each EDO, including a title and subtitle
ggplot(data, aes(x=Werte, fill=AI)) +  # We're using 'Werte' for the histogram's x-axis variable
  geom_histogram(bins=30, alpha=0.7, position="identity") + # Adjust bins for finer or coarser histogram
  facet_wrap(~EDO, scales = "free") + # Facet by EDO with free y-axis scaling
  labs(title = "Zwick Data",
       subtitle = "First Comparability Study",
       x = "Values", # Custom x-axis label
       y = "Frequency") + # Custom y-axis label, typically 'Frequency' for histograms
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(angle = 0))




