# This is used to plot env conditions
# rm(list=ls())
library(tidyverse)
library(ggplot2)

source(here::here("code","functions", "get_env_plot_functions.R"))

env_data <- read_csv(here::here("data","Environmental_Data_Plot.csv"))

env_data <- env_data %>%
            mutate(eisst_a = eisst-mean(eisst),
                   pos = eisst_a >=0)

ggplot(env_data,aes(x = yr, y = eisst)) + geom_line()

ggplot(env_data, aes(x = yr, y = eisst_a, fill = pos)) +
  geom_col(position = "identity", colour = "black", size = 0.25) +
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE) +
  theme_bw()

for (i in 1:length(y)){

ylab<-switch(y[i],"eisst"="Entrance Island SST",
  "pisst"="Pine Island SST",
  "frdmn"="Fraser River Discharge (mean)",
  "peak"="Fraser River Discharge (peak)",
  "pdo"="Pacific Decadal Osciallation",
  "GOA.SST.Sum"="GOA SST Summer","NPGO.Sum"="NPGO Summer",
  "GOA.SST.Win"="GOA SST Winter","NPGO.Win"="NPGO Winter",
  "GOA.SST.Ann"="GOA SST Annual","NPGO.Ann"="NPGO Annual",
  "Pink"="North Pacific Pink","Sockeye"="North Pacific Sockeye",
  "Chum"="North Pacific Chum","Salmon_Total"="North Pacific Total Salmon",
  "Shu_fry_len"="Shuswap Fry Mean Length",
  "Shu_fry_wt"="Shuswap Fry Mean Weight",
  "Que_fry_len"="Quesnel Fry Mean Length",
  "Que_fry_wt"="Quesnel Fry Mean Weight"#,
  #"GOA.Chl.Sum"="GOA Chla Summer"
  )
}


png("plot_env.png",width = 1000,height = 500)
par(mfrow = c(1,2))
#pdf("2022_Sockeye_Forecast_Env.png",width = 11,height = 7)
i = 1 
Plot.ev(ev=getev(ev.list=y[i],ev.mat=df,avg.last.yr = NA),
        main=ylab,yr.offset=0,main.brood=as.numeric(fcyr)-3,cyc=F,
        axis.label="") 
i = 2 
Plot.ev(ev=getev(ev.list=y[i],ev.mat=df,avg.last.yr = NA),
        main=ylab,yr.offset=0,main.brood=as.numeric(fcyr)-3,cyc=F,
        axis.label="") 

dev.off()


