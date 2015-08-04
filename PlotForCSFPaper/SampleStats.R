library("ggplot2")
library("plyr")
library("reshape2")
library("RColorBrewer")
library("grid")
library("gridExtra")
library("scales")
theme_mine <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.text.x = element_text(size=18),
      strip.text.y = element_text(size=18),
      strip.background = element_rect(colour="black", fill="lightgray"),
      axis.text.x = element_text(size=14,angle=45,hjust=1,vjust=1,face="bold"),
      axis.text.y = element_text(size=14,hjust=1,face="bold"),
      axis.ticks.x =  element_line(colour = "black"), 
      axis.ticks.y =  element_line(colour = "black"), 
      axis.title.x =  element_text(size=16,angle=0,face="bold"),
      axis.title.y = element_text(size=16,angle=90,face="bold"),
      legend.position = "top", 
      panel.grid.major.y = element_line(colour = "lightgray"), 
      panel.grid.minor = element_blank(),
      panel.margin = unit(1.0, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
      axis.line = element_line(colour = "black")
    )
}
SampleStats <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/SampleStats.txt")
TumorType_order=c("Primary_Brain_Tumor","Solid_Tumor/Brain_Metastasis","Solid_Tumor/No_Brain_Metastasis")
Classification_order = c("LM-Positive","LM-Negative")
SampleStats$Sample_Category <- factor(SampleStats$Sample_Category2,levels=TumorType_order)
SampleStats$Sample_Type <- factor(SampleStats$Sample_Type,levels=Classification_order)

ggplot(SampleStats,aes(factor(Sample_Category2),y=Median_Coverage)) + 
  geom_boxplot(outlier.colour = "green", outlier.size = 3) + 
  xlab("Type of Category") +
  ylab("Median Coverage") +
  facet_wrap(~Sample_Type,scales="free_x",ncol=2,nrow=1) +
  scale_y_continuous(breaks=seq(0,1200,100),limits=c(0, 1200)) + theme_mine()
ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/SampleStats_Coverage.pdf",width=18, height=10)

ggplot(SampleStats,aes(factor(Sample_Category2),y=Total_DNA_Input)) + 
  geom_boxplot(outlier.colour = "green", outlier.size = 3) + 
  xlab("Type of Category") +
  ylab("Total DNA Input (ng)") +
  facet_wrap(~Sample_Type,scales="free_x",ncol=2,nrow=1) +
  scale_y_continuous(breaks=seq(0,60,5),limits=c(0, 60)) + theme_mine()
ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/SampleStats_DNA.pdf",width=18, height=10)

ggplot(SampleStats,aes(factor(Sample_Category2),y=Library_Yield)) + 
  geom_boxplot(outlier.colour = "green", outlier.size = 3) + 
  xlab("Type of Category") +
  ylab("Library Yield (ng/ml)") +
  facet_wrap(~Sample_Type,scales="free_x",ncol=2,nrow=1) +
  scale_y_continuous(breaks=seq(0,750,50),limits=c(0, 750)) + theme_mine()
ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/SampleStats_Library.pdf",width=18, height=10)
