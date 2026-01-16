####indicator venn####
indicator.venn=readxl::read_xlsx("./data/indicator.venn.xlsx")
indicator.venn=data.frame(indicator.venn)
rownames(indicator.venn)=indicator.venn$MAG
venn.winter.list=rownames(unique(indicator.venn[which(indicator.venn$winter=="MP"),]))
venn.spring.list=rownames(unique(indicator.venn[which(indicator.venn$spring=="MP"),]))
venn.autumn.list=rownames(unique(indicator.venn[which(indicator.venn$autumn=="MP"),]))
venn.summer.list=rownames(unique(indicator.venn[which(indicator.venn$summer=="MP"),]))

venn.list.mp <- list(winter = venn.winter.list, spring = venn.spring.list, autumn= venn.autumn.list,summer=venn.summer.list)

ggVennDiagram(venn.list.mp, 
              label = "count", # You can choose "count", "percentage", or "none"
              label_alpha = 0, # Makes the label background transparent
              color = c("#1e3a8a","#4a90d4","#71b7ed","#a0d5f0"), # Color of the circle outlines
              set_color =c("#1e3a8a","#4a90d4","#71b7ed","#a0d5f0")) + 
  scale_fill_gradient(low = "#e9e9e9", high = "#454545",guide = guide_colourbar(direction = "horizontal" ,barwidth = 10, barheight = 0.9)) + # Defines the color gradient
  theme(legend.position = "bottom")

ggsave("indicator.mp.venn.new.pdf",width=4,height=4)
