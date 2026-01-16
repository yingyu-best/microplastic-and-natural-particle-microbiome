#sanky plot
library(ggalluvial)
sanky=read.csv("./data/sanky.exchange.csv")
sanky$type<-factor(sanky$type,levels = rev(unique(sanky$type)))
sanky$season<-factor(sanky$season,levels = rev(unique(sanky$season)))

ggplot(sanky,
       aes(y = number, axis1= season, axis2 = type)) +
  geom_alluvium(aes(fill = indicator), width = 1/12, alpha = 0.9)+
  geom_stratum(width = 1/12, color = "white",aes(fill =indicator)) +
  geom_label(stat = "stratum", infer.label = TRUE) +
  # label.strata was deprecated so I changed it to infer.label
  scale_x_discrete(limits = c("Taxa","type"), expand = c(.05, .05)) +
  scale_fill_manual(values = c("#F8766D","#F7E3E2","#00BA38","#DEEDD3"))+
  ggtitle("number of exchanged rMAGs")+coord_flip()+
  theme_bw()

#bubble plot
library(ggplot2)
bubble.exchange=read.csv("./data/unique.exchange.bubble.csv",header = T)
bubble.exchange <- bubble.exchange[order(bubble.exchange$phylum), ]
bubble.exchange$MAG=factor(bubble.exchange$MAG,levels=unique(bubble.exchange$MAG))

ggplot(bubble.exchange[,1:6],aes(x=season,y=MAG,size=exchange*0.6,color=phylum))+
  theme_bw()+
  geom_point(aes(shape=indicator),alpha=0.95,stroke = 1.5)+
  scale_shape_manual(values = c(1,19))+
  theme(axis.text.x = element_text(angle = 270,hjust = 0))+
  facet_wrap(~type,scales = 'free_y')+
  scale_color_manual(values = c('#1F77B4FF',"#66C5CCFF","#9EB9F3FF","#F89C74FF","#87C55FFF" ,"#F6CF71FF","#9467BDFF" ,"#FE88B1FF","#C9DB74FF","#D3B484FF","#FF9896FF","#FFBB78FF","#C49C94FF","#98DF8AFF","#DCB0F2FF","#84c3b7"), limits = c("c__Gammaproteobacteria","c__Betaproteobacteria","c__Alphaproteobacteria","p__Actinobacteriota","p__Firmicutes","p__Bacteroidota","p__Planctomycetota","p__Deinococcota","p__Acidobacteriota","p__Cyanobacteria","p__Verrucomicrobiota","p__Acidobacteriota","p__Campylobacterota","p__Chloroflexota","p__Nitrospirota","p__Myxococcota"))+
  theme(axis.title = element_text(face="bold"), 
        axis.text.x = element_text(color="black"), 
        axis.text.y = element_text(color ="black",size=5.5))
