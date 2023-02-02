## This program is used to make taylor plots
## questions about this code contract Yi Xu, yi.xu2@dfo-mpo.gc.ca
rm(list=ls())
setwd("C:/DFO-MPO/Work/Sockeye_paper/code") #set up your own directory that include all the updated env files

library(tidyverse)
library(plotrix)
source("mytaylor.R")
fc <- read_csv("../data/historical_fc.csv",show_col_types = FALSE)
retro <- read_csv("../data/retro_all_newmodels_2022.csv")

retro <- retro %>%
  mutate(pop = popID) %>%
  mutate(model = replace(model, model=="RickerBasic","Ricker"),
         model = replace(model, model=="PowerBasic","Power"),
         model = replace(model, model=="LarkinBasic","Larkin"),
         model = replace(model, model=="RickerPi","RickerPi.SST"),
         model = replace(model, model=="RickerEi","RickerEi.SST"),
         model = replace(model, model=="PowerPi","PowerPi.SST"),
         model = replace(model, model=="PowerEi","PowerEi.SST"),
         model = replace(model, model=="RickerFRDMean","RickerFRD.mean"),
         model = replace(model, model=="RickerFRDpeak","RickerFRD.peak"),
         model = replace(model, model=="RickerSalmon_Total","RickerSalmon.Total"),
         model = replace(model, model=="PowerSalmon_Total","PowerSalmon.Total"),
         model = replace(model, model=="PowerGOA.SST.Ann","PowerGOA.SST"),
         model = replace(model, model=="RickerGOA.SST.Ann","RickerGOA.SST"),
         model = replace(model, model=="LarkinBasicCycAge","LarkinCyc"),
         model = replace(model, model=="PowerBasicCycAge","PowerCyc")) %>%
  mutate(model = replace(model, model=="PowerJuvEi"&pop==11,"PowerEi.SST"),
         model = replace(model, model=="PowerJuvPi"&pop==11,"PowerPi.SST"),
         model = replace(model, model=="PowerJuvPDO"&pop==11,"PowerPDO"),
         model = replace(model, model=="PowerJuvFRD.mean"&pop==11,"PowerFRD.mean"),
         model = replace(model, model=="PowerJuvFRD.peak"&pop==11,"PowerFRD.peak"),
         model = replace(model, model=="PowerJuvGOA.SST.Ann"&pop==11,"PowerGOA.SST"),
         model = replace(model, model=="PowerJuvChum"&pop==11,"PowerChum"),
         model = replace(model, model=="PowerJuvSockeye"&pop==11,"PowerSockeye"),
         model = replace(model, model=="PowerJuvPink"&pop==11,"PowerPink"),
         model = replace(model, model=="PowerJuvSalmon_Total"&pop==11,"PowerSalmon.Total"),
         model = replace(model, model=="PowerJuv"&pop==11,"PowerBasic"),
         model = replace(model, model=="RJ1"&pop==11,"R1C"),
         model = replace(model, model=="RJ2"&pop==11,"R2C"),
         model = replace(model, model=="RJC"&pop==11,"RAC")) %>%
  filter(!model %in% c("RickerGOA.SST.Sum",
                       "RickerGOA.SST.Win",
                       "RickerNPGO.Ann","RickerNPGO.Sum","RickerNPGO.Win","PowerPi"))

oldricker<-c("Ricker","RickerCyc","RickerEi.SST","RickerFRD.mean","RickerFRD.peak",
             "RickerPDO","RickerPi.SST")
oldpower<-c("Power","PowerCyc","PowerEi.SST","PowerFRD.mean","PowerFRD.peak",#"PowerJuv","PowerJuvPi","PowerJuvEi",
            "PowerPDO","PowerPi.SST")
pname<-unique(fc$Stock)

pname[pname=="Upper Barriere(Fennell)"] <- "Fennell (Upper Barriere)"

myseries <-c(3,4,0,1,2,5,6,7,8,9)

n_model <- c("Ricker","RickerCyc",
             "RickerEi.SST","RickerPi.SST","RickerFRD.mean","RickerFRD.peak","RickerPDO",
             "RickerGOA.SST","RickerSockeye","RickerChum","RickerPink","RickerSalmon.Total",
             "Power","PowerCyc",#"PowerPi",
             "PowerEi.SST","PowerPi.SST","PowerFRD.mean","PowerFRD.peak","PowerPDO",
             "PowerGOA.SST","PowerSockeye","PowerChum" ,"PowerPink","PowerSalmon.Total",
             "Larkin", "LarkinCyc","LLY","R1C","R2C","RAC","TSA","RS1","RS2","RSC","MRS","RS4yr","RS8yr")
##########make taylor plot first 9 stocks Figure 3a############
# filename=paste0("../plot/plot_taylor_Summer_Late.png")
filename=paste0("../plot/plot_taylor_Early_Summer.png")
png(filename,h=1500,w=1500)
par(mfrow=c(3,3),omi = c(1, 1, 1, 4),xpd = NA) 
for (j in c(1,4,14,15,17,18,8,16,7)){
#for (j in c(6,2,3,5,9,11,12,13,10)){

  if(j == 1) mytitlecolor <- "red3" #Early Stuart
  if(j %in% c(11,9,12,13,10)) mytitlecolor <- "purple" #Late
  if(j %in% c(4,14,16,17,18,15,8)) mytitlecolor <- "forestgreen" #green #Early Summer
  if(j %in% c(7,2,6,3,5)) mytitlecolor <- "blue" #Summer
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
  mycolor[idx_power_old]<-"springgreen3"
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
    if(k==1) taylor.diagram(want$obs,want$obs,normalize=T,pch=16,pcex = 4,col="black",cex=2,cex.main=3,ref.sd = T,xlab = "",ylab = "", main = "")
    #if(k==1) taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
    idx<-which(tot_n_model$model[k]==n_model)
    #if(j==5|j==16)taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2,xpd=NA)
    #else 
    taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2)#,xpd=NA)
  }
  taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 3,add=T,col="black")
  
  mtext("Taylor Diagram",side=3,line=0.5,cex=3.5,outer = T)
  mtext("Standard deviation",side=1,line=2,cex=2,outer = T)
  mtext("Standard deviation",side=2,line=2,cex=2,outer = T)
if(j == 18|j == 11) legend("right",c("Observation","Forecast",n_model),
       pch=c(16,15,mypch),
       col=c("black","black",mycolor),
       cex = 3,inset = c(-0.9,1),xpd = NA,
       bg=NA,bty = "n",ncol=1,pt.lwd = 2) #else
#taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
  title(main=pname[j],col.main = mytitlecolor,cex.main = 4)
}
legend("bottomright", c("Early Stuart","Early Summer","Summer","Late"),
       text.col = c("red3","forestgreen","blue","purple"),
       inset = c(-0.8,-0.25),bty = "n",xpd = NA, cex = 4)
dev.off()



##########make taylor plot first 9 stocks Figure 3b############
filename=paste0("../plot/plot_taylor_Summer_Late.png")
#filename=paste0("../plot/plot_taylor_Early_Summer.png")
png(filename,h=1500,w=1500)
par(mfrow=c(3,3),omi = c(1, 1, 1, 4),xpd = NA) 
#for (j in c(1,4,14,15,17,18,8,16,7)){
for (j in c(6,2,3,5,9,11,12,13,10)){
  
  if(j == 1) mytitlecolor <- "red3" #Early Stuart
  if(j %in% c(11,9,12,13,10)) mytitlecolor <- "purple" #Late
  if(j %in% c(4,14,16,17,18,15,8)) mytitlecolor <- "forestgreen" #green #Early Summer
  if(j %in% c(7,2,6,3,5)) mytitlecolor <- "blue" #Summer
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
  mycolor[idx_power_old]<-"springgreen3"
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
    if(k==1) taylor.diagram(want$obs,want$obs,normalize=T,pch=16,pcex = 4,col="black",cex=2,cex.main=3,ref.sd = T,xlab = "",ylab = "", main = "")
    #if(k==1) taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
    idx<-which(tot_n_model$model[k]==n_model)
    #if(j==5|j==16)taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2,xpd=NA)
    #else 
    taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2)#,xpd=NA)
  }
  taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 3,add=T,col="black")
  
  mtext("Taylor Diagram",side=3,line=0.5,cex=3.5,outer = T)
  mtext("Standard deviation",side=1,line=2,cex=2,outer = T)
  mtext("Standard deviation",side=2,line=2,cex=2,outer = T)
  if(j == 18|j == 11) legend("right",c("Observation","Forecast",n_model),
                             pch=c(16,15,mypch),
                             col=c("black","black",mycolor),
                             cex = 3,inset = c(-0.9,1),xpd = NA,
                             bg=NA,bty = "n",ncol=1,pt.lwd = 2) #else
  #taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
  title(main=pname[j],col.main = mytitlecolor,cex.main = 4)
}
legend("bottomright", c("Early Stuart","Early Summer","Summer","Late"),
       text.col = c("red3","forestgreen","blue","purple"),
       inset = c(-0.8,-0.25),bty = "n",xpd = NA, cex = 4)
dev.off()

# ## plot outside icons for references only 
# 
# ##########make taylor plot first 9 stocks Figure 3a############
# # filename=paste0("../plot/plot_taylor_Summer_Late.png")
# filename=paste0("../plot/plot_taylor_Early_Summer_outside.png")
# png(filename,h=1500,w=1500)
# par(mfrow=c(3,3),omi = c(1, 1, 1, 4),xpd = NA) 
# for (j in c(1,4,14,15,17,18,8,16,7)){
#   #for (j in c(6,2,3,5,9,11,12,13,10)){
#   
#   if(j == 1) mytitlecolor <- "red3" #Early Stuart
#   if(j %in% c(11,9,12,13,10)) mytitlecolor <- "purple" #Late
#   if(j %in% c(4,14,16,17,18,15,8)) mytitlecolor <- "forestgreen" #green #Early Summer
#   if(j %in% c(7,2,6,3,5)) mytitlecolor <- "blue" #Summer
#   print(paste("Pop=",j))
#   pop <- retro %>% filter(popID==j&age==99) 
#   #n_model <- unique(pop$model)
#   
#   tot_n_model <- pop %>% distinct(model) 
#   
#   idx_larkin<-grep("Larkin",n_model)
#   idx_power<-grep("Power",n_model)
#   idx_power_old<-idx_power[n_model[idx_power]%in%oldpower]
#   idx_power_new<-idx_power[!n_model[idx_power]%in%oldpower]
#   idx_ricker<-grep("Ricker",n_model)
#   idx_ricker_old<-idx_ricker[n_model[idx_ricker]%in%oldricker]
#   idx_ricker_new<-idx_ricker[!n_model[idx_ricker]%in%oldricker]
#   idx_sib<-grep("sibling5",n_model)
#   idx_jack<-grep("Jacksib",n_model)
#   
#   idx_naive<-which(n_model%in%n_model[-c(idx_power,idx_ricker,idx_larkin,idx_sib)])
#   mypch<-mycolor<-rep(NA,length(n_model))
#   mycolor[idx_larkin]<-"gold"
#   mypch[idx_larkin]<-(1:length(idx_larkin))+2
#   mycolor[idx_power_old]<-"springgreen3"
#   mypch[idx_power_old]<-myseries[1:length(idx_power_old)]
#   mycolor[idx_power_new]<-"blue"
#   mypch[idx_power_new]<-myseries[3:(length(idx_power_new)+2)]
#   mycolor[idx_ricker_old]<-"pink"
#   mypch[idx_ricker_old]<-myseries[1:length(idx_ricker_old)]
#   mycolor[idx_ricker_new]<-"red"
#   mypch[idx_ricker_new]<-myseries[3:(length(idx_ricker_new)+2)]
#   mycolor[idx_naive]<-"mediumpurple1"
#   mypch[idx_naive]<-0:(length(idx_naive)-1)
#   mycolor[idx_sib]<-"grey"
#   mypch[idx_sib]<-15
#   mycolor[idx_jack]<-"grey"
#   mypch[idx_jack]<-16
#   
#   
#   for (k in 1:length(tot_n_model$model)){
#     want <- pop %>% filter(model==tot_n_model$model[k]&age==99) %>% 
#       mutate(diff=p50-obs,p50,obs=obs/1e6) %>% filter(!is.na(diff)) %>%
#       select(retyr,p50,obs) %>%
#       left_join(fc %>% filter(popID==j) %>% select(retyr,Stock,fc),by="retyr") %>% mutate(fc=fc/1e6)
#     if(k==1) taylor.diagram(want$obs,want$obs,normalize=T,pch=16,pcex = 4,col="black",cex=2,cex.main=3,ref.sd = T,xlab = "",ylab = "", main = "")
#     #if(k==1) taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
#     idx<-which(tot_n_model$model[k]==n_model)
#     #if(j==5|j==16)taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2,xpd=NA)
#     #else 
#     taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2,xpd=NA)
#   }
#   taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 3,add=T,col="black")
#   
#   mtext("Taylor Diagram",side=3,line=0.5,cex=3.5,outer = T)
#   mtext("Standard deviation",side=1,line=2,cex=2,outer = T)
#   mtext("Standard deviation",side=2,line=2,cex=2,outer = T)
#   if(j == 18|j == 11) legend("right",c("Observation","Forecast",n_model),
#                              pch=c(16,15,mypch),
#                              col=c("black","black",mycolor),
#                              cex = 3,inset = c(-0.9,1),xpd = NA,
#                              bg=NA,bty = "n",ncol=1,pt.lwd = 2) #else
#   #taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
#   title(main=pname[j],col.main = mytitlecolor,cex.main = 4)
# }
# legend("bottomright", c("Early Stuart","Early Summer","Summer","Late"),
#        text.col = c("red3","forestgreen","blue","purple"),
#        inset = c(-0.8,-0.25),bty = "n",xpd = NA, cex = 4)
# dev.off()
# 
# 
# 
# ##########make taylor plot first 9 stocks Figure 3b outside############
# filename=paste0("../plot/plot_taylor_Summer_Late_outside.png")
# #filename=paste0("../plot/plot_taylor_Early_Summer.png")
# png(filename,h=1500,w=1500)
# par(mfrow=c(3,3),omi = c(1, 1, 1, 4),xpd = NA) 
# #for (j in c(1,4,14,15,17,18,8,16,7)){
# for (j in c(6,2,3,5,9,11,12,13,10)){
#   
#   if(j == 1) mytitlecolor <- "red3" #Early Stuart
#   if(j %in% c(11,9,12,13,10)) mytitlecolor <- "purple" #Late
#   if(j %in% c(4,14,16,17,18,15,8)) mytitlecolor <- "forestgreen" #green #Early Summer
#   if(j %in% c(7,2,6,3,5)) mytitlecolor <- "blue" #Summer
#   print(paste("Pop=",j))
#   pop <- retro %>% filter(popID==j&age==99) 
#   #n_model <- unique(pop$model)
#   
#   tot_n_model <- pop %>% distinct(model) 
#   
#   idx_larkin<-grep("Larkin",n_model)
#   idx_power<-grep("Power",n_model)
#   idx_power_old<-idx_power[n_model[idx_power]%in%oldpower]
#   idx_power_new<-idx_power[!n_model[idx_power]%in%oldpower]
#   idx_ricker<-grep("Ricker",n_model)
#   idx_ricker_old<-idx_ricker[n_model[idx_ricker]%in%oldricker]
#   idx_ricker_new<-idx_ricker[!n_model[idx_ricker]%in%oldricker]
#   idx_sib<-grep("sibling5",n_model)
#   idx_jack<-grep("Jacksib",n_model)
#   
#   idx_naive<-which(n_model%in%n_model[-c(idx_power,idx_ricker,idx_larkin,idx_sib)])
#   mypch<-mycolor<-rep(NA,length(n_model))
#   mycolor[idx_larkin]<-"gold"
#   mypch[idx_larkin]<-(1:length(idx_larkin))+2
#   mycolor[idx_power_old]<-"springgreen3"
#   mypch[idx_power_old]<-myseries[1:length(idx_power_old)]
#   mycolor[idx_power_new]<-"blue"
#   mypch[idx_power_new]<-myseries[3:(length(idx_power_new)+2)]
#   mycolor[idx_ricker_old]<-"pink"
#   mypch[idx_ricker_old]<-myseries[1:length(idx_ricker_old)]
#   mycolor[idx_ricker_new]<-"red"
#   mypch[idx_ricker_new]<-myseries[3:(length(idx_ricker_new)+2)]
#   mycolor[idx_naive]<-"mediumpurple1"
#   mypch[idx_naive]<-0:(length(idx_naive)-1)
#   mycolor[idx_sib]<-"grey"
#   mypch[idx_sib]<-15
#   mycolor[idx_jack]<-"grey"
#   mypch[idx_jack]<-16
#   
#   
#   for (k in 1:length(tot_n_model$model)){
#     want <- pop %>% filter(model==tot_n_model$model[k]&age==99) %>% 
#       mutate(diff=p50-obs,p50,obs=obs/1e6) %>% filter(!is.na(diff)) %>%
#       select(retyr,p50,obs) %>%
#       left_join(fc %>% filter(popID==j) %>% select(retyr,Stock,fc),by="retyr") %>% mutate(fc=fc/1e6)
#     if(k==1) taylor.diagram(want$obs,want$obs,normalize=T,pch=16,pcex = 4,col="black",cex=2,cex.main=3,ref.sd = T,xlab = "",ylab = "", main = "")
#     #if(k==1) taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
#     idx<-which(tot_n_model$model[k]==n_model)
#     #if(j==5|j==16)taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2,xpd=NA)
#     #else 
#     taylor.diagram(want$obs,want$p50,add = T,pch=mypch[idx],col=mycolor[idx],normalize=T,pcex = 3,lwd = 2,xpd=NA)
#   }
#   taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 3,add=T,col="black")
#   
#   mtext("Taylor Diagram",side=3,line=0.5,cex=3.5,outer = T)
#   mtext("Standard deviation",side=1,line=2,cex=2,outer = T)
#   mtext("Standard deviation",side=2,line=2,cex=2,outer = T)
#   if(j == 18|j == 11) legend("right",c("Observation","Forecast",n_model),
#                              pch=c(16,15,mypch),
#                              col=c("black","black",mycolor),
#                              cex = 3,inset = c(-0.9,1),xpd = NA,
#                              bg=NA,bty = "n",ncol=1,pt.lwd = 2) #else
#   #taylor.diagram(want$obs,want$fc,normalize=T,pch=15,pcex = 4,add=T,col="black")
#   title(main=pname[j],col.main = mytitlecolor,cex.main = 4)
# }
# legend("bottomright", c("Early Stuart","Early Summer","Summer","Late"),
#        text.col = c("red3","forestgreen","blue","purple"),
#        inset = c(-0.8,-0.25),bty = "n",xpd = NA, cex = 4)
# dev.off()
