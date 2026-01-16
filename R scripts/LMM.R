#Linear mixed effect model
library(lme4)
library(car)
library(dplyr)
library(stringr)

mag.taxa.phy.wide<-read.csv("./data/mag.taxa.phy.wide.csv",row.names = 1)
#MP
lmm.input<-data.frame(scale(mag.taxa.phy.wide[1:60,-c(1:12)]))
#NP
lmm.input<-data.frame(scale(mag.taxa.phy.wide[61:120,-c(1:12)]))

divs1<-sapply(1:ncol(lmm.input),function(j){
  message("Now j=",j," in ",ncol(lmm.input),". ",date())
  if (length(unique(lmm.input[,j]))<3){
    result<-rep(NA,18)
  } else {
    div<-data.frame(divtest=lmm.input[,j],mag.taxa.phy.wide[61:120,1:12])
    div$type <- gsub("MP", "1", div$type.y)
    div$type <- gsub("NP", "0",div$type.y)
    fm1<-lmer(divtest~temp+(1|site),data=div) 
    
    presult<-car::Anova(fm1,type=2)
    coefs<-coef(summary(fm1))[ , "Estimate"]  ##four coefs
    names(coefs)<-paste0(names(coefs),".mean")
    
    SEvalues<-coef(summary(fm1))[ , "Std. Error"] ##standard errors
    names(SEvalues)<-paste0(names(SEvalues),".se")
    
    tvalues<-coef(summary(fm1))[ , "t value"] ##t values
    names(tvalues)<-paste0(names(tvalues),".t")
    
    chisqP<-c(presult[,1],presult[,3])
    names(chisqP)<-c(paste0(row.names(presult),".chisq"),paste0(row.names(presult),".P"))
    
    result<-c(coefs,tvalues,SEvalues,chisqP)}
  result
})
colnames(divs1)<-colnames(lmm.input)


#plot：
divs.mp.phy<-divs1
divs1.reshape=reshape2::melt(divs.mp.phy) 
#extract mean and se
divs1.reshape <- divs1.reshape %>%
  mutate(extracted = str_extract(divs1.reshape$Var1, "(?<=\\.).+"))
divs1.reshape <- divs1.reshape[divs1.reshape$extracted %in% c("mean", "se","P"), ]
divs1.reshape$Var1 <- gsub("\\.se|\\.mean|\\.P", "", divs1.reshape$Var1)
divs1.reshape$Var1 <- gsub("type1", "type", divs1.reshape$Var1)
# Transform the data frame
df_wide <-divs1.reshape %>%
  pivot_wider(names_from = extracted, values_from = value)
df_wide  <- df_wide [df_wide $Var1 != "(Intercept)", ]
df_wide$Var2<-factor(df_wide$Var2,levels =rev(c(unique(df_wide$Var2))))


ggplot(data.frame(df_wide),aes(x = Var2,
                               y = mean,fill =ifelse(mean<0,"#EA7580FF" ,"#088BBEFF"),color=ifelse(mean>0,"#EA7580FF" ,"#088BBEFF")))+
  geom_hline(yintercept = 0, color = 'darkgray', size = 0.6,lty="dashed")+    # 在0处添加垂直线条
  theme_bw()+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),position = position_dodge(0.7), width = 0.5,alpha=0.7) +#调整误差线长度
  geom_point(size=5,position = position_dodge(0.7),alpha=1)+
  scale_colour_manual(values=c("#088BBEFF","#EA7580FF" ))+
  scale_fill_manual(values=c("#088BBEFF","#EA7580FF" ))+
  theme(axis.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        #axis.text.x = element_blank(),    
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        axis.text = element_text(size = 10))+
  ylab("effect size")+xlab("")+#coord_flip()+
  geom_text(aes(label = ifelse(P < 0.001, "***",ifelse(P < 0.01, "**",ifelse(P < 0.05, "*",ifelse(P < 0.1, ".", ""))))), 
            position = position_dodge(0.7), vjust = -0.01, color="black",size = 5)
