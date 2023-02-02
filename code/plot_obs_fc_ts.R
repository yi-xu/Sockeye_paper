## This program is used to make time series plot for obs and historical forecast
## questions about this code contract Yi Xu, yi.xu2@dfo-mpo.gc.ca
rm(list=ls())
setwd("C:/DFO-MPO/Work/Sockeye_paper/code") #set up your own directory that include all the updated env files

library(tidyverse)
library(ggplot2)

fc <- read_csv("../data/historical_fc.csv",show_col_types = FALSE)
pname<-unique(fc$Stock)
pseq <- c(1,4,14,15,17,18,8,16,7,6,2,3,5,9,11,12,13,10)
pnew <-pname[pseq]


  df<- fc %>%
    filter(popID <= 18) %>%
    select(retyr,Stock,popID,fc,tot) %>%
    pivot_longer(cols = c("fc","tot"),names_to = "return",values_to = "model")
  
  df$Stock <- factor(df$Stock, levels = pnew)
  
  p1<-ggplot(data = df,
             aes(x=retyr,y=model,colour=return))+
    geom_point(size=3,alpha = 0.7)+
    geom_line(size=1,alpha = 0.7)+
    theme_bw(base_size = 12)+
    theme(legend.position = "bottom",#axis.title.x=element_blank(),
          legend.background  = element_blank(),
          legend.text=element_text(size=16))+
    scale_colour_manual(name="",values=c("red","blue"),
                        labels=c("Forecast","Observation"))+
    scale_x_continuous(breaks=seq(2010,2020,by=5),
                       minor_breaks = 2009:2020,
                       labels=seq(2010,2020,by=5))+
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    facet_wrap(~Stock,scales = "free_y",ncol = 3) +
    ylab("Numbers of Sockeye Salmon")+
    xlab("Year")
p1  
ggsave("../plot/plot_ts.png", h = 9, w = 8)

