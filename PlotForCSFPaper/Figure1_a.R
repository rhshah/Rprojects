library("ggplot2")
library("plyr")
library("reshape2")
library("RColorBrewer")
library("grid")
library("gridExtra")
library("scales")
cbPalette <- c("#E69F00", "#0072B2")
grayscale <- c("#636363","#bdbdbd")
Figure1_a <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure1_a.txt")
datm <- melt(Figure1_a)
p = ggplot(datm,aes(factor(variable),value,fill=variable)) + geom_bar(stat="identity") + facet_wrap(~MutationType,scales="free_x",ncol=4,nrow=1)
p+ylab("Variant Allele Frequency (VAF)")+xlab("")+
  scale_fill_manual(values=cbPalette,guide = guide_legend(title="CSF-components: ",title.theme = element_text(angle=0,size=18,face="bold"),label.theme = element_text(angle=0,size=18,face="bold"))) + 
  theme_mine()
ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure1_a.pdf",width=12, height=6)
theme_mine <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.text.x = element_text(size=18),
      strip.text.y = element_text(size=18),
      strip.background = element_rect(colour="black", fill="lightgray"),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=14,hjust=1,face="bold"),
      axis.ticks.x =  element_blank(),
      axis.ticks.y =  element_line(colour = "black"), 
      axis.title.x= element_blank(),
      axis.title.y= element_text(size=16,angle=90,face="bold"),
      legend.position = "bottom", 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_line(colour = "lightgray"), 
      panel.margin = unit(1.0, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
      axis.line = element_line(colour = "black")
    )
}