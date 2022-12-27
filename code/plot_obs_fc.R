## This program is used to make plot to compare obs with fc models
## questions about this code contract Yi Xu, yi.xu2@dfo-mpo.gc.ca
rm(list=ls())
#setwd("C:/DFO-MPO/Work/Sockeye_paper/code") #set up your own directory that include all the updated env files

library(tidyverse)
library(ggplot2)
library(patchwork)
#library(grid)
data <- read_csv("../data/selected_models_comparison_2022.csv")


p<-ggplot(data, aes(x= model, y = p50/1e6,width=0.5)) +
  geom_linerange(aes(ymin = p10/1e6, ymax = p90/1e6)) +
  geom_hline(aes(yintercept = obs/1e6),col = "red")+
  geom_crossbar(aes(ymin = p25/1e6,ymax = p75/1e6,fill= "white"),size = 0.7) +
  annotate(geom = "text",x = 4.65,y = 1.6,label="Observation",col = "red",hjust = 0,size = 6)+
  coord_cartesian(xlim = c(1,4),clip = 'off')+
  facet_wrap(~pop, ncol = 1, scales = "free") +
  ylab("Numbers of sockeye (million)")+
  theme_bw()+
  theme(text = element_text(size = 20),legend.position = "none",
        plot.margin = unit(c(1,10,1,1), "lines")) 
p

set.seed(100)
data_random <- data.frame(name = "text",y = sample(500))
legend_plot <- ggplot()+
  geom_boxplot(data = data_random, aes(x = name, y = y),
               fill="salmon",fatten = 2,lwd = 1, width = 1,col = "black") +
  geom_text(aes(x = rep(2,5),y = c(1,125.75,250.5,375.25,500),
                label = c("p10","p25","p50","p75","p90")),size = 7)+
  xlab("")+ylab("")+ 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 5/3) +
  coord_cartesian(xlim = c(1,2),clip = 'off')
  
legend_plot

string_want <-c("Best Rank","Best SD","Best R\nBest RMSE",            
"Best Rank\n2nd R\n2nd SD\n2nd RMSE", "Best R\nBest SD\nBest RMSE",          "3rd SD" ,                
"Best R",                  "Best RMSE\nBest Rank" ,        "Best SD"  )  
dat_text <- data.frame(
  label = string_want,#data$justification[c(3,1,2,9,8,7,4,6,5)],
  pop   = data$pop[1:9],
  x     = c(2, 3, 4,2, 3, 4,2, 3, 4),
  y     = c(6,6,6,15.25,15.25,15.25,14,14,14)
)
p_text <- geom_text(data = dat_text,mapping = aes(x = x, y= y, label = label), vjust = 1,size = 6)
p + p_text +inset_element(legend_plot, left = 0.85, bottom = 0.1, right = 1, top = 0.4, align_to = 'full')
ggsave("../plot/plot_obs_fc.png",w = 12, h = 11)

