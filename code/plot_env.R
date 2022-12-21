# This is used to plot env conditions

library(tidyverse)
df<-read.csv(here::here("data","Environmental_Data.csv"))
df$eisst<-rowMeans(df[,c("apesst","maesst","jnesst")])
df$pisst<-rowMeans(df[,c("appsst","mapsst","jnpsst","jlpsst")])
df$frdmn<-rowMeans(df[,c("aflow","mflow","jflow")])
colname_df<-colnames(df)

source("myfunction.R")
y<-colname_df[c(32,33,34,5,13,16:29)]

fcyr<-2022
#pdf("2022_Sockeye_Forecast_Env.png",width = 11,height = 7)
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
png(paste0("env_",ylab,".png"),width = 1000,height = 500)
if(grepl("fry",ylab))
Plot.ev(ev=getev(ev.list=y[i],ev.mat=df,avg.last.yr = NA),
        main=ylab,yr.offset=2,main.brood=as.numeric(fcyr)-3,cyc=T,
        axis.label="") 
else
Plot.ev(ev=getev(ev.list=y[i],ev.mat=df,avg.last.yr = NA),
          main=ylab,yr.offset=2,main.brood=as.numeric(fcyr)-2,cyc=T,
          axis.label="") 

dev.off()
}

