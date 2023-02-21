## This program is used to create Ranking tables##
## and output to retro_all.csv
## questions about this code contract Yi Xu, yi.xu2@dfo-mpo.gc.ca
rm(list=ls())

library(tidyverse)
setwd("C:/DFO-MPO/Work/Sockeye_paper/code")
#retro <-read_csv(here::here("data","retro_all_newmodels.csv"),show_col_types = FALSE)
retro <-read_csv("../data/retro_all_newmodels_2022.csv",show_col_types = FALSE)
my_error <- function(obs,est,dev.type="all"){ #adpated from Gottfried code retro.pm.fun
  
  devs <- est-obs
  dev.logs <- log(est) - log(obs)
  n.devs <- sum(!is.na(devs)) 
  
  mre <- sum(devs,na.rm=TRUE)/n.devs ; mre.log <- sum(dev.logs,na.rm=TRUE)/n.devs
  mae <- sum(abs(devs),na.rm=TRUE)/n.devs ; mae.log <- sum(abs(dev.logs),na.rm=TRUE)/n.devs 
  mpe <- sum(devs/obs,na.rm=TRUE)/n.devs ; mpe.log <- sum(dev.logs/log(obs),na.rm=TRUE)/n.devs
  rmse <- sqrt(sum(devs^2,na.rm=TRUE)/n.devs); rmse.log <- sqrt(sum(dev.logs^2,na.rm=TRUE)/n.devs)
  
  retro.pm.out <- matrix(NA,nrow=2,ncol=4,dimnames=list(c("RawDev","DevLog"),c("MRE","MAE","MPE","RMSE")))
  
  retro.pm.out["RawDev",] <-  c(mre,mae,mpe,rmse)
  retro.pm.out["DevLog",] <-  c(mre.log,mae.log,mpe.log,rmse.log)
  
  if(dev.type != "all"){ retro.pm.out <- retro.pm.out[dev.type,] }
  
  return(retro.pm.out[1,])
  
}

# pname<-c( "Early Stuart", "Late Stuart","Stellako",
#           "Bowron","Raft","Quesnel", "Chilko","Seymour",
#           "Late Shuswap", "Birkenhead","Cultus","Portage",
#           "Weaver" ,"Upper Barriere(Fennell)","Scotch",
#           "Gates","Nadina","Pitt")#,"Harrison")
pname<-c( "Early\nStuart", "Late\nStuart","Stellako",
          "Bowron","Raft","Quesnel", "\nChilko","\nSeymour",
          "Late\nShuswap", "\nBirkenhead","Cultus","\nPortage",
          "Weaver" ,"Fennell\nUpper Barriere","Scotch",
          "Gates","\nNadina","Pitt")#,"Harrison")
pname_tb <-tibble(pname = pname, pop = 1: 18)
age_spec <-c(4,5,99)
tb <- tibble()
for (ipop in 1:18){
  for (j in 3:3){ #age
    pop <- retro %>% filter(popID==ipop&retyr%in%2009:2020) %>% mutate(obs=obs/1e6)
    if(ipop==19) { 
      pop$age[pop$model=="sibling5"]<-4
      pop$model[pop$model=="sibling5"]<-"sibling4"}
    
    uniq_models <-unique(pop$model)
    
    
    for (k in 1:1){
      if(k==1) yr_select<-2009:2020
      if(k==2) yr_select<-seq(2018,2009,-4)
      if(k==3) yr_select<-c(2009,2011:2013,2015:2017,2019:2020)
      
      yrname<-case_when(k==1 ~ "Allyrs",
                        k==2 ~ "cycyrs",
                        k==3 ~ "otheryrs")
      pop_select <- pop %>% filter(retyr%in%yr_select)
      out<-NULL
          for (i in 1:length(uniq_models)){
            tmp <- pop_select %>%
              filter(age==age_spec[j]&model==uniq_models[i])
            if(nrow(tmp)>=1) out <- bind_rows(out,as_tibble_row(my_error(tmp$obs,tmp$p50)) %>% mutate(model=uniq_models[i]))
          }
    
          pop_rank <- out %>%
            select(model,MRE,MAE,MPE,RMSE) %>%
            # mutate(MRE=round(MRE*1e3)/1e3,
            #        MAE=round(MAE*1e3)/1e3,
            #        MPE=round(MPE*1e3)/1e3,
            #        RMSE=round(RMSE*1e3)/1e3) %>%
            mutate(MRE_rank=rank(abs(MRE)),
                   MAE_rank=rank(abs(MAE)),
                   MPE_rank=rank(abs(MPE)),
                   RMSE_rank=rank(RMSE),
                   average=(MRE_rank+MAE_rank+MPE_rank+RMSE_rank)/4,
                   Overall_rank=rank(average,ties.method="first")) %>%
            mutate(pop = ipop, age = age_spec[j]) %>%
            mutate(rel_rank_RMSE = RMSE_rank/n(),
                   rel_rank_all = Overall_rank/n()) %>%
            arrange(Overall_rank)
          tb <- bind_rows(tb,pop_rank)
          #fn<-paste0("retro_",pname[ipop],"_age_",age_spec[j],"newmodels_",yrname,".csv")
          
          #fn<-paste0("ranking_tables/retro_",pname[ipop],"_age_",age_spec[j],".csv")
          #write_csv(pop_rank,file = fn)
    } #end k of select years
  } # end j with age
} # end ipop population ID


#write_csv(tb,file = "../data/organize_retro.csv")

# new <- tb %>%
#        select(pop, model, rel_rank_all) %>%
#        pivot_wider(names_from = pop, values_from = rel_rank_all)
library(ggplot2)
library(RColorBrewer)
cols <- rev(brewer.pal(9, 'PiYG'))#'RdYlBu'))
# ggplot(tb,aes(x = pop, y = model, fill = rel_rank_all)) +
#   geom_tile(colour = "white", size = 0.2) +
#   scale_fill_gradientn(colours = cols) +
#   theme_bw()
# ggsave("raw_rank_all.png", h = 11, w = 7)

model_level <- c("Ricker","RickerCyc",
                 "RickerEi.SST","RickerPi.SST","RickerFRD.mean","RickerFRD.peak","RickerPDO",
                 "RickerGOA.SST","RickerSockeye","RickerChum","RickerPink","RickerSalmon.Total",
                 "Power","PowerCyc",
                 "PowerEi.SST","PowerPi.SST","PowerFRD.mean","PowerFRD.peak","PowerPDO",
                 "PowerGOA.SST","PowerSockeye","PowerChum" ,"PowerPink","PowerSalmon.Total",
                 "Larkin","LarkinCyc",
                 "LLY","R1C","R2C","RAC","TSA","RS1","RS2","RSC","MRS","RS4yr","RS8yr")
pop_level <-pname[c(1,4,14,15,17,18,8,16,7,6,2,3,5,9,11,12,13,10)]


new <- tb %>%
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
                       "RickerNPGO.Ann","RickerNPGO.Sum","RickerNPGO.Win")) %>%

       filter(model %in% model_level) %>%
       mutate(model=factor(model,levels=model_level)) %>%
       left_join(pname_tb, by = "pop")

# ggplot(new,aes(x = pname, y = model, fill = rel_rank_all)) +
#   geom_tile(colour = "white", size = 0.2) +
#   scale_fill_gradientn(colours = cols) +
#   theme_bw()


ggplot(new,aes(x = pname, y = model, fill = rel_rank_RMSE)) +
  geom_tile(colour = "white", size = 0.2) +
  scale_fill_gradientn(colours = cols,name = "RMSE\nRelative Rank",
                       breaks = c(min(new$rel_rank_all),0.5,1),labels = c("0 Best Model","0.5","1.0 Worst Model")) +
  theme_bw() +
  scale_y_discrete(limits = rev(model_level))+
  scale_x_discrete(limits = pop_level) +
  #geom_rect(aes(xmin = 0.5 , xmax = 18 + 0.5, ymin = 1 - 0.5, ymax = 11 + 0.35),
  # #         fill = "transparent", color ="#C51B7F", size = 1.5)+
  geom_rect(aes(xmin = 0.5 , xmax = 18 + 0.5, ymin = 14 - 0.5, ymax = 18 + 0.55),
            fill = "transparent", color = "darkgreen", size = 1.5)+
  geom_rect(aes(xmin = 0.5 , xmax = 18 + 0.5, ymin = 26 - 0.5, ymax = 30 + 0.55),
            fill = "transparent", color = "darkgreen", size = 1.5) +
  xlab("")+ylab("")+
  theme(text = element_text(size = 20)) 
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("../plot/plot_rank_RMSE.png", h = 11, w = 19)

