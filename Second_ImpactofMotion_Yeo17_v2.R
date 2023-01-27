#install.packages("xgboost")
#install.packages("mclust")
install.packages('matrixStats')

cor_val<-c()


library(tidyr)
library(caret)
library(doParallel)
library(xgboost)
library(dplyr)
library(mclust)
library(caret)
library(tidyverse)
library(matrixStats)
library(ggpubr)
library(reshape2)
setwd("/Users/gopoudel/Documents/Research/Projects/HCPDataMining/Script")

features<-readRDS('FinalDF_Yeo17.rds')
fdt<-read.csv('FD_Data_All.csv')
nc<-114
#col_pal<-c('#DE3163', '#40E0D0', '#FFBF00', '#6495ED', '#CCCCFF')

col_pal<-c('#40E0D0', '#6495ED', '#FFBF00','#DE3163', '#CCCCFF')
cl_val<-c()
cl_val$col_names<-c("nvg_K", "nvg_d", "nvg_S", "nvg_C" ,  "nvg_Q")
cl_val$col_labels<-c("Degrees", "Average path length", "Communities (N)", "Clustering Cofficient", "Modularity")



yv<-seq(0.1, 0.4, by=0.005)*100
yv<-yv[order(-yv)]
cr_all<-c() 
cl_val<-data.frame(cl_val)
df_all<-c()
for (i in 1:nrow(cl_val)) {
  
  cl<-cl_val[i,1]
  
  features_wide<-features %>% 
    subset(session=='Rest1') %>%  
    subset(select=c("subject", "ROI", cl)) %>%
    pivot_wider(names_from=ROI, values_from = cl) 
  
  fd_perc<-fdt$perc_rest1
  
  crv<-cor(features_wide[,2:nc+1], fd_perc)
  cr_all<-cbind(cr_all,crv)
  
  cor_val<-c()
  
  for (mc in seq(0.6, 0.90, by=0.005)){
    in2<-which(fd_perc>mc)
    selected_features<-features_wide[in2,]
    cor_val<-rbind(cor_val, cor(fd_perc[in2],selected_features[,2:nc+1]))
    print('comes here')
    #break
    
  }
  sd<-rowSds(as.matrix(cor_val))
  ci<-qnorm(0.975)*sd/sqrt(nc)
  
  df<-c()
  df$mean<-rowMeans(cor_val)
  df$sd<-sd
  df$ci<-ci
  df$x<-yv
  df$type<-cl_val[i,2]
  df<-data.frame(df)
  df_all<-rbind(df, df_all)
  
 
}

cr_all<-data.frame(cr_all)
colnames(cr_all)<-cl_val$col_labels
long_cr_all<-melt(cr_all)

colSds(as.matrix(cr_all))
colMeans(cr_all)

plt1<-ggplot(data=df_all, aes(x=x, y=mean, ymin=mean-ci, ymax=mean+ci, fill=type))+
  geom_line()+
  geom_ribbon()+
  xlab('Proportion of motion corrupted frames(%)')+
  ylab('Correlation (r)')+
  ggtitle("(B)")+
  ylim(-0.5, 0.5)+
  scale_fill_manual(values = col_pal)+
  theme(text = element_text(size = 14), axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "lightgray"))


col_pal2<-c('#DE3163', '#40E0D0', '#FFBF00', '#6495ED', '#CCCCFF')
plt1_1<-ggplot(data=long_cr_all, aes(x=variable, y=value, fill=variable))+
  geom_violin(show.legend = FALSE)+
  geom_boxplot(width = 0.07, position = position_dodge(width = 0.9), show.legend = FALSE)+
  xlab('Features')+
  ylab('Correlation (r)')+
  ylim(-1, 1)+
  ggtitle("(A)")+
  scale_fill_manual(values = col_pal2)+
  theme(text = element_text(size = 14), axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "lightgray"))




fplt<-ggarrange(plt1_1, plt1, nrow = 2, ncol = 1 )
show(fplt)
ggsave('Figure2_ImpactofMotion_rest1_nvg_Yeo17.pdf',fplt)


cl_val<-c()
cl_val$col_names<-c("nvg_K", "nvg_d", "nvg_S", "nvg_C" ,  "nvg_Q")
cl_val$col_labels<-c("Degrees", "Average path length", "Communities (N)", "Clustering Cofficient", "Modularity")



yv<-seq(0.1, 0.4, by=0.005)*100
yv<-yv[order(-yv)]
cr_all<-c() 
cl_val<-data.frame(cl_val)
df_all<-c()
for (i in 1:nrow(cl_val)) {
  
  cl<-cl_val[i,1]
  
  features_wide<-features %>% 
    subset(session=='Rest2') %>%  
    subset(select=c("subject", "ROI", cl)) %>%
    pivot_wider(names_from=ROI, values_from = cl) 
  
  fd_perc<-fdt$perc_rest2
  
  crv<-cor(features_wide[,2:nc+1], fd_perc)
  cr_all<-cbind(cr_all,crv)
  
  cor_val<-c()
  
  for (mc in seq(0.6, 0.90, by=0.005)){
    in2<-which(fd_perc>mc)
    selected_features<-features_wide[in2,]
    cor_val<-rbind(cor_val, cor(fd_perc[in2],selected_features[,2:nc+1]))
    print('comes here')
    #break
    
  }
  sd<-rowSds(as.matrix(cor_val))
  ci<-qnorm(0.975)*sd/sqrt(133)
  
  df<-c()
  df$mean<-rowMeans(cor_val)
  df$sd<-sd
  df$ci<-ci
  df$x<-yv
  df$type<-cl_val[i,2]
  df<-data.frame(df)
  df_all<-rbind(df, df_all)
  
  
}

cr_all<-data.frame(cr_all)
colnames(cr_all)<-cl_val$col_labels
long_cr_all<-melt(cr_all)

plt1<-ggplot(data=df_all, aes(x=x, y=mean, ymin=mean-ci, ymax=mean+ci, fill=type))+
  geom_line()+
  geom_ribbon()+
  xlab('Proportion of motion corrupted frames(%)')+
  ylab('Correlation (r)')+
  ggtitle("(B)")+
  ylim(-0.5, 0.5)+
  scale_fill_manual(values = col_pal)+
  theme(text = element_text(size = 14), axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "lightgray"))


col_pal2<-c('#DE3163', '#40E0D0', '#FFBF00', '#6495ED', '#CCCCFF')
plt1_1<-ggplot(data=long_cr_all, aes(x=variable, y=value, fill=variable))+
  geom_violin(show.legend = FALSE)+
  geom_boxplot(width = 0.07, position = position_dodge(width = 0.9), show.legend = FALSE)+
  xlab('Features')+
  ylab('Correlation (r)')+
  ylim(-1, 1)+
  ggtitle("(A)")+
  scale_fill_manual(values = col_pal2)+
  theme(text = element_text(size = 14), axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "lightgray"))




fplt<-ggarrange(plt1_1, plt1, nrow = 2, ncol = 1 )
show(fplt)
ggsave('Figure2_ImpactofMotion_rest2_nvg_Yeo17.pdf',fplt)

cl_val<-c()
cl_val$col_names<-c("hvg_K", "hvg_d", "hvg_S", "hvg_C" ,  "hvg_Q")
cl_val$col_labels<-c("Degrees", "Average path length", "Communities (N)", "Clustering Cofficient", "Modularity")
cl_val<-data.frame(cl_val)
df_all<-c()
cr_all<-c() 
cl_val<-data.frame(cl_val)
df_all<-c()
long_cr_all<-c()

for (i in 1:nrow(cl_val)) {
  
  cl<-cl_val[i,1]
  
  features_wide<-features %>% 
    subset(session=='Rest2') %>%  
    subset(select=c("subject", "ROI", cl)) %>%
    pivot_wider(names_from=ROI, values_from = cl) 
  
  fd_perc<-fdt$perc_rest2
  
  crv<-cor(features_wide[,2:nc+1], fd_perc)
  cr_all<-cbind(cr_all,crv)
  
  cor_val<-c()
  
  for (mc in seq(0.6, 0.90, by=0.005)){
    in2<-which(fd_perc>mc)
    selected_features<-features_wide[in2,]
    cor_val<-rbind(cor_val, cor(fd_perc[in2],selected_features[,2:nc+1]))
    print('comes here')
    #break
    
  }
  sd<-rowSds(as.matrix(cor_val))
  ci<-qnorm(0.975)*sd/sqrt(133)
  
  df<-c()
  df$mean<-rowMeans(cor_val)
  df$sd<-sd
  df$ci<-ci
  df$x<-yv
  df$type<-cl_val[i,2]
  df<-data.frame(df)
  df_all<-rbind(df, df_all)
  
  
}

cr_all<-data.frame(cr_all)
colnames(cr_all)<-cl_val$col_labels
long_cr_all<-melt(cr_all)


plt2<-ggplot(data=df_all, aes(x=x, y=mean, ymin=mean-ci, ymax=mean+ci, fill=type))+
  geom_line()+
  geom_ribbon(show.legend = FALSE)+
  xlab('')+
  ylab('Correlation (r)')+
  ylim(-0.5, 0.5)+
  scale_fill_manual(values = col_pal)+
  theme(text = element_text(size = 18), axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "lightgray"))



plt2_1<-ggplot(data=long_cr_all, aes(x=variable, y=value, fill=variable))+
  geom_violin(show.legend = FALSE)+
  geom_boxplot(width = 0.07, position = position_dodge(width = 0.9), show.legend = FALSE)+
  xlab('Features')+
  ylab('Correlation (r)')+
  ylim(-1, 1)+
  ggtitle("(B) HVG")+
  scale_fill_manual(values = col_pal2)+
  theme(text = element_text(size = 18), axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "lightgray"))


fplt<-ggarrange(plt1_1, plt1, plt2_1, plt2, nrow = 2, ncol = 2)
show(fplt)
ggsave('Figure2_ImpactofMotion_rest2_Yeo17.pdf',fplt)
