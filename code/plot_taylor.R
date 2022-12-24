## This program is used to make taylor plots
## questions about this code contract Yi Xu, yi.xu2@dfo-mpo.gc.ca
rm(list=ls())
#setwd("C:/DFO-MPO/Work/Sockeye_paper/code") #set up your own directory that include all the updated env files

library(tidyverse)
#library(plotrix)
source(here::here("code","mytaylor.R"))
fc <- read_csv(here::here("data","historical_fc.csv"),show_col_types = FALSE)
retro <- read_csv(here::here("data","retro_all_newmodels.csv"))

retro <- retro %>%
  mutate(model = replace(model, model=="RickerPi","RickerPi.SST"),
         model = replace(model, model=="RickerEi","RickerEi.SST"),
         model = replace(model, model=="RickerGOA.SST.Ann","RickerGOA.SST"),
         model = replace(model, model=="LarkinBasicCycAge","LarkinCyc"),
         model = replace(model, model=="PowerBasicCycAge","PowerCyc")) %>%
  filter(!model %in% c("RickerGOA.SST.Sum",
                       "RickerGOA.SST.Win",
                       "RickerNPGO.Ann","RickerNPGO.Sum","RickerNPGO.Win"))

oldricker<-c("RickerBasic","RickerCyc","RickerEi.SST","RickerFRDMean","RickerFRDpeak",
             "RickerPDO","RickerPi.SST")
oldpower<-c("PowerBasic","PowerCyc","PowerJuv","PowerJuvPi","PowerJuvEi",
            "PowerFRDpeak","PowerPi")
pname<-unique(fc$Stock)

filename=paste0("Summer_Late_taylor.png")

#filename=paste0("Early_Summer_taylor.png")

myseries <-c(3,4,0,1,2,5,6,7,8,9)

n_model <- c("RickerBasic","RickerCyc",
             "RickerEi.SST","RickerPi.SST","RickerFRDMean","RickerFRDpeak","RickerPDO",
             "RickerGOA.SST.Ann","RickerSockeye","RickerChum","RickerPink","RickerSalmon_Total",
             "LarkinBasic", "LarkinCyc","PowerBasic","PowerCyc","PowerPi",
             "PowerSockeye","PowerChum" ,"PowerPink","PowerSalmon_Total",
             "LLY","R1C","R2C","RAC","TSA","RS1","RS2","RSC","MRS","RS4yr","RS8yr")


png(here::here("plot",filename),h=1500,w=1500)
par(mfrow=c(3,3),omi = c(1, 1, 1, 4),xpd = NA) 
#for (j in c(4,14,16,17,18,15,8,1,11)){
for (j in c(7,2,6,3,5,9,12,13,10)){
  if(j == 1) mytitlecolor <- "black"
  if(j %in% c(11,9,12,13,10)) mytitlecolor <- "purple"
  if(j %in% c(4,14,16,17,18,15,8)) mytitlecolor <- "darkgreen"
  if(j %in% c(7,2,6,3,5)) mytitlecolor <- "blue"
  print(paste("Pop=",j))
  pop <- retro %>% filter(popID==j&age==99) 
  #n_model <- unique(pop$model)

  tot_n_model <- pop %>% distinct(model) 
  
  idx_larkin<-grep("Larkin",n_model)
  idx_power<-grep("Power",n_model)
  idx_power_old<-idx_power[n_model[idx_power]%in%oldpower]
  idx_power_new<-idx_power[!n_model[idx_power]%in%oldpower]
  idx_ricker<-grep("Ricker",n_model)
  idx_ricker_old<-idx_ricker[n_model[idx_ricker]%in%oldricker]
  idx_ricker_new<-idx_ricker[!n_model[idx_ricker]%in%oldricker]
  idx_sib<-grep("sibling5",n_model)
  idx_jack<-grep("Jacksib",n_model)
  
  idx_naive<-which(n_model%in%n_model[-c(idx_power,idx_ricker,idx_larkin,idx_sib)])
  mypch<-mycolor<-rep(NA,length(n_model))
  mycolor[idx_larkin]<-"gold"
  mypch[idx_larkin]<-(1:length(idx_larkin))+2
  mycolor[idx_power_old]<-"#61D04F" #green
  mypch[idx_power_old]<-myseries[1:length(idx_power_old)]
  mycolor[idx_power_new]<-"blue"
  mypch[idx_power_new]<-myseries[3:(length(idx_power_new)+2)]
  mycolor[idx_ricker_old]<-"pink"
  mypch[idx_ricker_old]<-myseries[1:length(idx_ricker_old)]
  mycolor[idx_ricker_new]<-"red"
  mypch[idx_ricker_new]<-myseries[3:(length(idx_ricker_new)+2)]
  mycolor[idx_naive]<-"mediumpurple1"
  mypch[idx_naive]<-0:(length(idx_naive)-1)
  mycolor[idx_sib]<-"grey"
  mypch[idx_sib]<-15
  mycolor[idx_jack]<-"grey"
  mypch[idx_jack]<-16
  
  
  for (k in 1:length(tot_n_model$model)){
    want <- pop %>% filter(model==tot_n_model$model[k]&age==99) %>% 
      mutate(diff=p50-obs,p50,obs=obs/1e6) %>% filter(!is.na(diff)) %>%
      select(retyr,p50,obs) %>%
      left_join(fc %>% filter(popID==j) %>% select(retyr,Stock,fc),by="retyr") %>% mutate(fc=fc/1e6)
    if(k==1) taylor.diagram(want$obs,want$obs,normalize=T,pch=16,pcex = 4,main=pname[j],col="black",cex=2,col.main = mytitlecolor,cex.main=3,ref.sd = T,xlab = "")
    #if(k==1) taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
    idx<-which(tot_n_model$model[k]==n_model)
    taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2)
  }
  taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 3,add=T,col="black")
  
  mtext("Taylor Diagram",side=3,line=0.5,cex=3.5,outer = T)
  mtext("Standard deviation",side=1,line=2,cex=2,outer = T)
  mtext("Standard deviation",side=2,line=2,cex=2,outer = T)
if(j == 15|j == 9) legend("right",c("Observation","Forecast",n_model),
       pch=c(16,15,mypch),
       col=c("black","black",mycolor),
       cex = 3,inset = c(-0.9,1),xpd = NA,
       bg=NA,bty = "n",ncol=1,pt.lwd = 2) #else
#taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
}

dev.off()