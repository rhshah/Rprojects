library("ggplot2")
library("plyr")
library("reshape2")
library("RColorBrewer")
library("grid")
library("gridExtra")
library("scales")
Figure3_heatmap <- read.delim("/Users/shahr2/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/HeatmapFiles/Pt27_35104659.txt")
datm=melt(Figure3_heatmap)
datm$Gene.AminoAcid = factor(datm$Gene.AminoAcid,levels=Figure3_heatmap$Gene.AminoAcid)
datm$variable = factor(datm$variable,levels=c("FFPE","cfDNA"))
base_size <- 9
p <- ggplot(datm,aes(x=factor(Gene.AminoAcid), y=variable))
p + geom_tile(aes(fill=value)) + geom_tile(aes(fill=value),colour = "black",show_guide=FALSE) + 
  scale_fill_gradient2(breaks=c(0,0.2,0.4,0.6,0.8,1),limits=c(0,1),
                       low=muted("red"),mid="white",high="steelblue",
                      guide = guide_colorbar(title="Variant Allele Frequency (VAF): ", 
                     title.theme = element_text(angle=0,size=16,face="bold"),
                     title.position = "top",
                     label.theme = element_text(angle=0,size=12,face="bold"))) +
  xlab("") + ylab("") +
  coord_equal() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(limit=(rev(levels(datm$variable))),expand = c(0, 0)) +
  theme_mine() 
 #theme(legend.position = "none",axis.ticks = element_blank(), axis.text.x = element_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"))
ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure3_Pt27.pdf",width=25, height=10)

theme_mine <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size=10,hjust=0.3,angle=330,face="bold"),
      axis.text.y = element_text(size=14,,hjust = 1,face="bold"),
      axis.ticks.x =  element_blank(),
      axis.ticks.y =  element_blank(),
      axis.title.x =  element_blank(),
      axis.title.y = element_blank(),
      legend.position = "top",
      legend.direction="horizontal"
    )
}