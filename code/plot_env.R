# This code is used to plot env conditions
# last update 2022.12.21. Questions contact Yi Xu by email: yi.xu2@dfo-mpo.gc.ca
# 
# rm(list=ls())
library(tidyverse)
library(ggplot2)

env_data <- read_csv(here::here("data","Environmental_Data_Plot.csv"),show_col_types = FALSE) %>%
            mutate(year = yr + 2) %>%
            select(-GOA.SST.Win, -GOA.SST.Sum, -yr) %>%
            pivot_longer(!year) %>%
            group_by(name) %>%
            mutate(mean_env = mean(value),
                   diff = value - mean_env >0)%>%
            mutate(name=factor(name,levels=c("eisst","GOA.SST.Ann",
                                             "pisst","Sockeye",
                                             "frdmn","Chum",
                                             "peak","Pink",
                                             "pdo","Salmon_Total")))
ylabel <- c("eisst"="Entrance Island SST","GOA.SST.Ann"="Gulf of Alaska SST",
            "pisst"="Pine Island SST","Sockeye"="North Pacific Sockeye",
            "frdmn"="Fraser River Discharge (mean)","Chum"="North Pacific Chum",
            "peak"="Fraser River Discharge (peak)","Pink"="North Pacific Pink",
            "pdo"="Pacific Decadal Osciallation","Salmon_Total"="North Pacific Total Salmon"
            )

ggplot(env_data, aes(x = year, y = mean_env, col = diff)) +
  geom_segment(aes(xend = year, yend = value),size = 1.25) +
  scale_color_manual(values = c("blue","red"), guide = "none") +
  facet_wrap(~name, scales = "free_y",ncol = 2,labeller = as_labeller(ylabel)) +
  ylab("")+
  theme_bw()+
  theme(text = element_text(size = 20))    

ggsave(here::here("plot","plot_env.png"), height = 10, width = 8)
