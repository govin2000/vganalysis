#install.packages("xgboost")
install.packages("poweRlaw")

library(tidyr)
library(caret)
library(doParallel)
library(xgboost)
library(dplyr)
library(igraph)
library(poweRlaw)
library(ggpubr)
library(MASS)

#setup parallel backend to use many processors
#cores=detectCores()
#cl <- makeCluster(cores[1]-1) #not to overload your computer
#registerDoParallel(cl)


setwd("/Users/gopoudel/Documents/Research/Projects/HCPDataMining/Script")

subjects<-read.csv('Selected_Subject.csv')


finaldf<-readRDS('../Results/Rest1_Yeo_17.degrees.nvg.rds')

rois<-c(1,6,11,18,30,40,47) #14- precuneus, 263- Prefrontal cortex, Right Thalamus

subjid<-subjects$x

est_data_1<-estimatehist(finaldf, subjid, rois[1])
degree_distr_1<-est_data_1[1]
alpha_1<-cival(unlist(est_data_1[2]))
pval_1<-cival(unlist(est_data_1[3]))
xmin_1<-cival(unlist(est_data_1[4]))

plt1<-ggplot(data.frame(degree_distr_1), aes(x = log(x), y = log(y), group=subject)) + 
  geom_point()+
  xlab("log(Degree)")+
  ylab("log(Density)")+
  theme_pubclean(base_size = 14, base_family = "", flip = FALSE)


est_data_2<-estimatehist(finaldf, subjid, rois[2])
degree_distr2<-est_data_2[1]
alpha_2<-cival(unlist(est_data_2[2]))
pval_2<-cival(unlist(est_data_2[3]))
xmin_2<-cival(unlist(est_data_2[4]))


plt2<-ggplot(data.frame(degree_distr2), aes(x = log(x), y = log(y), group=subject)) + 
  geom_point()+
  xlab("log(Degree)")+
  ylab("log(Density)")+
  theme_pubclean(base_size = 14, base_family = "", flip = FALSE)


est_data_3<-estimatehist(finaldf, subjid, rois[3])
degree_distr3<-est_data_3[1]
alpha_3<-cival(unlist(est_data_3[2]))
pval_3<-cival(unlist(est_data_3[3]))
xmin_3<-cival(unlist(est_data_3[4]))


plt3<-ggplot(data.frame(degree_distr3), aes(x = log(x), y = log(y), group=subject)) + 
  geom_point()+
  xlab("log(Degree)")+
  ylab("log(Density)")+
  theme_pubclean(base_size = 14, base_family = "", flip = FALSE)


est_data_4<-estimatehist(finaldf, subjid, rois[4])
degree_distr4<-est_data_4[1]
alpha_4<-cival(unlist(est_data_4[2]))
pval_4<-cival(unlist(est_data_4[3]))
xmin_4<-cival(unlist(est_data_4[4]))


plt4<-ggplot(data.frame(degree_distr4), aes(x = log(x), y = log(y), group=subject)) + 
  geom_point()+
  xlab("log(Degree)")+
  ylab("log(Density)")+
  theme_pubclean(base_size = 14, base_family = "", flip = FALSE)



est_data_5<-estimatehist(finaldf, subjid, rois[5])
degree_distr5<-est_data_5[1]
alpha_5<-cival(unlist(est_data_5[2]))
pval_5<-cival(unlist(est_data_5[3]))
xmin_5<-cival(unlist(est_data_5[4]))


plt5<-ggplot(data.frame(degree_distr5), aes(x = log(x), y = log(y), group=subject)) + 
  geom_point()+
  xlab("log(Degree)")+
  ylab("log(Density)")+
  theme_pubclean(base_size = 14, base_family = "", flip = FALSE)



est_data_6<-estimatehist(finaldf, subjid, rois[6])
degree_distr6<-est_data_6[1]
alpha_6<-cival(unlist(est_data_6[2]))
pval_6<-cival(unlist(est_data_6[3]))
xmin_6<-cival(unlist(est_data_6[4]))


plt6<-ggplot(data.frame(degree_distr6), aes(x = log(x), y = log(y), group=subject)) + 
  geom_point()+
  xlab("log(Degree)")+
  ylab("log(Density)")+
  theme_pubclean(base_size = 14, base_family = "", flip = FALSE)



est_data_7<-estimatehist(finaldf, subjid, rois[7])
degree_distr7<-est_data_7[1]
alpha_7<-cival(unlist(est_data_7[2]))
pval_7<-cival(unlist(est_data_7[3]))
xmin_7<-cival(unlist(est_data_7[4]))


plt7<-ggplot(data.frame(degree_distr7), aes(x = log(x), y = log(y), group=subject)) + 
  geom_point()+
  xlab("log(Degree)")+
  ylab("log(Density)")+
  theme_pubclean(base_size = 14, base_family = "", flip = FALSE)




plt_final<-ggarrange(plt1, plt2, plt3, plt4, plt5, plt6, plt7, ncol = 3, nrow = 3)
ggsave('../Results/Yeo_17_degree_characteristics_Figure2_v2_log.jpeg', plt_final)


alpha_all<-rbind(alpha_1, alpha_2, alpha_3, alpha_4, alpha_5, alpha_6, alpha_7)









estimatehist <- function (finaldf, subjid, roi) {
  
  degree_distr<-c()
  astat<-c()
  pstat<-c()
  xminstat<-c()
  
  for (subj in subjid) {
    
    #subj<-subjid[i]
    #subj<-subjid[14]
    dt<-c()
    dat<-finaldf[which(finaldf$subject==subj), roi+1]
    #m_pl <- conpl$new(dat)
    m<-hist(dat)
    #plot.data <- plot(m$mids, draw = F)
    plot.data<-c()
    plot.data$x<-m$mids
    plot.data$y<-m$density
    plot.data$subject<-subj
    plot.data<-data.frame(plot.data)
    degree_distr<-rbind(degree_distr,plot.data)
    ft<-fit_power_law(dat)
    
    astat<-rbind(astat,ft$alpha)
    pstat<-rbind(pstat,ft$KS.p)
    xminstat<-rbind(xminstat,ft$xmin)
   
  }
  
  return(list(degree_distr,astat,pstat,xminstat))
}





cival <- function (x, ci = 0.95)
{
  Margin_Error <- qt(ci + (1 - ci)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
  df_out <- data.frame( sample_size=length(x), Mean=mean(x), sd=sd(x),
                        Margin_Error=Margin_Error,
                        'CI lower limit'=(mean(x) - Margin_Error),
                        'CI Upper limit'=(mean(x) + Margin_Error)) 
  return(df_out)
}
