library("ggplot2")
library("plyr")
library("reshape2")
library("RColorBrewer")
library("grid")
library("gridExtra")
library("scales")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
Figure2_a <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure2_a2.txt")
datm <- melt(Figure2_a)
TumorType_order=c("Breast",  "Lung",	"Other",	"Intraparenchymal_Brain_Mets",	"Primary_Brain_Tumor",	"No_CNS_Cancer")
datm$Type_of_Cases <- factor(datm$Type_of_Cases,levels=TumorType_order)
Classsification_order = c("LM-Positive","LM-Negative")
datm$Classification <- factor(datm$Classification,levels=Classsification_order)
p = ggplot(datm,aes(Type_of_Cases,value,fill=variable)) + 
  geom_bar(position = "fill",stat="identity",color="black") + 
  scale_y_continuous(labels = percent_format()) +
  xlab("Type of Category") +
  ylab("Percentage of Cases") +
  facet_wrap(~Classification,scales="free_x",ncol=2,nrow=1)
p +
  scale_fill_manual(values=c("#009E73","white"),guide = guide_legend(title="Type of Cases: ",title.theme = element_text(angle=0,size=16,face="bold"),label.theme = element_text(angle=0,size=16,face="bold"))) +
  theme_mine()
ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure2_a.pdf",width=18, height=10)
theme_mine <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.text.x = element_text(size=18),
      strip.text.y = element_text(size=18),
      strip.background = element_rect(colour="black", fill="lightgray"),
      axis.text.x = element_text(size=14,angle=45,face="bold"),
      axis.text.y = element_text(size=14,hjust=1,face="bold"),
      axis.ticks.x =  element_line(colour = "black"), 
      axis.ticks.y =  element_line(colour = "black"), 
      axis.title.x =  element_text(size=16,angle=0,face="bold"),
      axis.title.y = element_text(size=16,angle=90,face="bold"),
      legend.position = "top", 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_line(colour = "lightgray"), 
      panel.margin = unit(1.0, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
      axis.line = element_line(colour = "black")
    )
}