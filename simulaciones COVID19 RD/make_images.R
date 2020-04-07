###################################################
###################################################
# Simulaciones de la pandemia del  
# del COVID-19 en República Dominicana
#
# Autor: Gustavo Caffaro (NYU)
###################################################
###################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(gridExtra)
library(ggpubr)

rm(list=ls())
source('~/Documents/PascalAnalytics/Coronavirus/COVID-19_MonteCarlo/model_fun.R')
############## Parámetros de la simulación ##############


############## Supuestos de la simulación ##############
n <- 1e3 # numero de personas
nrep <- 25


# supuestos actuales
# variando num_ca
df1 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 1)
p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.5)
#ggsave("images/sim_ca1_H9.png",plot = p1)

df2 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 3)
p2 <- model_sim_covid_plot(df2, legend = F, y.axis = T)
#ggsave("images/sim_ca3_H9.png",plot = p2)

df3 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 5)
p3 <- model_sim_covid_plot(df3, x.axis = T )
#ggsave("images/sim_ca5_H9.png",plot = p3)

pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", 
                labels=c("CA=1\n H=9","CA=3\n H=9","CA=5\n H=9"), 
                label.x = 0.95,
                font.label = list(size = 10))
ggsave("images/sim_ca1-3-5_H9.png",plot = pp)



df1 <- model_sim_covid(nrep = nrep, n=n, H=6,num_ca = 3)
p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.5)
#ggsave("images/sim_ca3_H5.png",plot = pp)

df2 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 3)
p2 <- model_sim_covid_plot(df2, legend = F)

df3 <- model_sim_covid(nrep = nrep, n=n, H=12,num_ca = 3)
p3 <- model_sim_covid_plot(df3)
#ggsave("images/sim_ca3_H12.png",plot = pp)

pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", 
                labels=c("CA=3\n H=6","CA=3\n H=9","CA=3\n H=12"), 
                label.x = 0.95,
                font.label = list(size = 10))
ggsave("images/sim_ca3_H6-9-12.png",plot = pp)



##### si mas higiene
df1 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 1, tau = .25)
p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.25)
#ggsave("images/sim_ca1_H9.png",plot = p1)

df2 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 3, tau = .25)
p2 <- model_sim_covid_plot(df2, legend = F)
#ggsave("images/sim_ca3_H9.png",plot = p2)

df3 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 5, tau = .25)
p3 <- model_sim_covid_plot(df3, x.axis = T )
#ggsave("images/sim_ca5_H9.png",plot = p3)

pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", 
                labels=c("CA=1\n H=9","CA=3\n H=9","CA=5\n H=9"), 
                label.x = 0.95,
                font.label = list(size = 10))
ggsave("images/sim_ca1-3-5_H9_tau25.png",plot = pp)


df1 <- model_sim_covid(nrep = nrep, n=n, H=6,num_ca = 3, tau = .25)
p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.25)
#ggsave("images/sim_ca3_H5.png",plot = pp)

df2 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 3, tau = .25)
p2 <- model_sim_covid_plot(df2, legend = F)

df3 <- model_sim_covid(nrep = nrep, n=n, H=12,num_ca = 3, tau = .25)
p3 <- model_sim_covid_plot(df3)
#ggsave("images/sim_ca3_H12.png",plot = pp)

pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", 
                labels=c("CA=3\n H=6","CA=3\n H=9","CA=3\n H=12"), 
                label.x = 0.95,
                font.label = list(size = 10))
ggsave("images/sim_ca3_H6-9-12_tau25.png",plot = pp)



# 
# ##### si mucha mas higiene, se desinfectan las calles
# df1 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 1, tau = .1)
# p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.1)
# 
# df2 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 3, tau = .1)
# p2 <- model_sim_covid_plot(df2, legend = F)
# 
# df3 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 5, tau = .1)
# p3 <- model_sim_covid_plot(df3, x.axis = T )
# 
# pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", labels=c("C1","C2","C3"), label.x = 1)
# ggsave("images/sim_ca1-3-5_H9_tau10.png",plot = pp)
# 
# 
# df1 <- model_sim_covid(nrep = nrep, n=n, H=6,num_ca = 3, tau = .1)
# p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.1)
# #ggsave("images/sim_ca3_H5.png",plot = pp)
# 
# df2 <- model_sim_covid(nrep = nrep, n=n, H=9,num_ca = 3, tau = .1)
# p2 <- model_sim_covid_plot(df2, legend = F)
# 
# df3 <- model_sim_covid(nrep = nrep, n=n, H=12,num_ca = 3, tau = .1)
# p3 <- model_sim_covid_plot(df3)
# #ggsave("images/sim_ca3_H12.png",plot = pp)
# 
# pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", labels=c("C4","C5","C6"), label.x = 1)
# ggsave("images/sim_ca3_H6-9-12_tau10.png",plot = pp)
# 
