# This is used to plot env conditions

library(tidyverse)

df<-read.csv(here::here("data","FC_Environmental_Data_2022_Std_Offset_Alt.csv"))

df$eisst<-rowMeans(df[,c("apesst","maesst","jnesst")])
df$pisst<-rowMeans(df[,c("appsst","mapsst","jnpsst","jlpsst")])
df$frdmn<-rowMeans(df[,c("aflow","mflow","jflow")])
colname_df<-colnames(df)

y<-colname_df[c(1,32,33,34,5,13,16:19,24:26)]


write.csv(df[,y],here::here("data","Environmental_Data_Plot.csv"),row.names = FALSE)
