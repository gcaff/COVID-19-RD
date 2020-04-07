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
df1 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 1)
p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.5)
#ggsave("images/sim_ca1_H9.png",plot = p1)

df2 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 3)
p2 <- model_sim_covid_plot(df2, legend = F, y.axis = T)
#ggsave("images/sim_ca3_H9.png",plot = p2)

df3 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 5)
p3 <- model_sim_covid_plot(df3, x.axis = T )
#ggsave("images/sim_ca5_H9.png",plot = p3)

pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", 
                labels=c("CA=1\n H=11","CA=3\n H=11","CA=5\n H=11"), 
                label.x = 0.95,
                font.label = list(size = 10))
ggsave("images/sim_ca1-3-5_H9.png",plot = pp)

df_R0 <- data.frame(t=df1$t,
                    df1 = df1$R0,
                    df2 = df2$R0,
                    df3 = df3$R0,
                    tau=0.5
)

df1 <- model_sim_covid(nrep = nrep, n=n, H=7,num_ca = 3)
p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.5)
#ggsave("images/sim_ca3_H5.png",plot = pp)

df2 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 3)
p2 <- model_sim_covid_plot(df2, legend = F)

df3 <- model_sim_covid(nrep = nrep, n=n, H=15,num_ca = 3)
p3 <- model_sim_covid_plot(df3)
#ggsave("images/sim_ca3_H12.png",plot = pp)

pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", 
                labels=c("CA=3\n H=7","CA=3\n H=11","CA=3\n H=15"), 
                label.x = 0.95,
                font.label = list(size = 10))
ggsave("images/sim_ca3_H6-9-12.png",plot = pp)

df_R0_1 <- data.frame(t=df1$t,
                    df4 = df1$R0,
                    df5 = df2$R0,
                    df6 = df3$R0,
                    tau=0.5
)

df_R0 <- cbind(df_R0,df_R0_1[,-1])

#p5 <- plot_R0(df_R0,c("CA=3;H=7","CA=3;H=11","CA=3;H=15"),tau=0.5)

############### si mas higiene ###############


df1 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 1, tau=0.25)
p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.25)
#ggsave("images/sim_ca1_H9.png",plot = p1)

df2 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 3, tau=0.25)
p2 <- model_sim_covid_plot(df2, legend = F)
#ggsave("images/sim_ca3_H9.png",plot = p2)

df3 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 5, tau=0.25)
p3 <- model_sim_covid_plot(df3, x.axis = T )
#ggsave("images/sim_ca5_H9.png",plot = p3)

pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", 
                labels=c("CA=1\n H=11","CA=3\n H=11","CA=5\n H=11"), 
                label.x = 0.95,
                font.label = list(size = 10))
ggsave("images/sim_ca1-3-5_H9_tau30.png",plot = pp)


df_R0_1 <- data.frame(t=df1$t,
                      df1 = df1$R0,
                      df2 = df2$R0,
                      df3 = df3$R0,
                      tau=0.25
)

df1 <- model_sim_covid(nrep = nrep, n=n, H=7,num_ca = 3, tau=0.25)
p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.25)
#ggsave("images/sim_ca3_H5.png",plot = pp)

df2 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 3, tau=0.25)
p2 <- model_sim_covid_plot(df2, legend = F)

df3 <- model_sim_covid(nrep = nrep, n=n, H=15,num_ca = 3, tau=0.25)
p3 <- model_sim_covid_plot(df3)
#ggsave("images/sim_ca3_H12.png",plot = pp)

pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", 
                labels=c("CA=3\n H=7","CA=3\n H=11","CA=3\n H=15"), 
                label.x = 0.95,
                font.label = list(size = 10))
ggsave("images/sim_ca3_H6-9-12_tau30.png",plot = pp)

df_R0_2 <- data.frame(t=df1$t,
                      df4 = df1$R0,
                      df5 = df2$R0,
                      df6 = df3$R0,
                      tau=0.25
)

df_R0_1 <- cbind(df_R0_1,df_R0_2[,-1])

df_R0 <- rbind(df_R0,df_R0_1)

leyenda_labs <- c("asd",
                  "\tau=0.25;CA=3;H=7","CA=3;H=11","CA=3;H=15")

pp1 <- df_R0 %>%
  gather(key=key,value=R0,-t,-tau) %>%
  ggplot(aes(x=t,y=R0,color=key,shape=tau)) +
  geom_line(alpha=0.6) +
  theme_minimal() +
  ylim(c(0,5)) +
  ggtitle("Evolución de R0") +
  xlab("Núm. dias desde 1er caso") + ylab("R0") +
  scale_color_discrete(name="",
                       #breaks=c("df1", "df2", "df3", "df4"),
                       labels=leyenda_labs)

ggsave("images/R0.png",plot = pp2)


# 
# ##### si mucha mas higiene, se desinfectan las calles
# df1 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 1, tau = .1)
# p1 <- model_sim_covid_plot(df1, legend = F, title = T, tau=0.1)
# 
# df2 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 3, tau = .1)
# p2 <- model_sim_covid_plot(df2, legend = F)
# 
# df3 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 5, tau = .1)
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
# df2 <- model_sim_covid(nrep = nrep, n=n, H=11,num_ca = 3, tau = .1)
# p2 <- model_sim_covid_plot(df2, legend = F)
# 
# df3 <- model_sim_covid(nrep = nrep, n=n, H=12,num_ca = 3, tau = .1)
# p3 <- model_sim_covid_plot(df3)
# #ggsave("images/sim_ca3_H12.png",plot = pp)
# 
# pp <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = TRUE, legend="right", labels=c("C4","C5","C6"), label.x = 1)
# ggsave("images/sim_ca3_H6-9-12_tau10.png",plot = pp)
# 
