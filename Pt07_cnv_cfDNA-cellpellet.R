library('MASS');
library("ggplot2")
library("plyr")
library("reshape2")
library("RColorBrewer")
library("grid")
library("gridExtra")
library("scales")
#cellpellet
Pt07_cellpellet <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Pt07_cellpellet.txt")
chr <- do.call('c',lapply(as.character(Pt07_cellpellet[,'region']),function(x){
  f <- unlist(strsplit(x,'\\:'));
  return(f[1]);
}));
chr <- factor(chr,levels=c(seq(1,22,1)))
chr.counts <- table(chr);
chr.midpt <- sapply(chr.counts,function(x){return(round(x/2,0));});
numberpos <- chr.midpt + c(0,cumsum(chr.counts)[-length(chr.counts)]);
linepos <- cumsum(chr.counts)[-length(chr.counts)];
linepos<-c("549","969","1427","1688","2073","2360","2688","2819","3116","3268","3607","3979","4186","4304","4462", "4729", "5143", "5244", "5668", "5831", "5896")
theme_mine <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size=10,hjust=2,angle=0,face="bold"),
      axis.text.y = element_text(size=14,,hjust = 1,face="bold"),
      axis.ticks.x =  element_blank(),
      axis.ticks.y =  element_blank(),
      axis.title.x =  element_blank(),
      axis.title.y = element_blank(),
      legend.position = "top",
      legend.direction="horizontal"
    )
}
ggplot(Pt07_cellpellet,aes(x=seq(1,nrow(Pt07_cellpellet)),y=lr)) + theme_mine() +
  geom_jitter(aes(color = factor(sig))) + 
  scale_y_continuous(limits=c(-4,4)) + 
  scale_x_discrete(breaks=linepos,labels=seq(1,21,1)) + 
  scale_color_manual(values=c("#0072B2","#D55E00"),
                     labels=c("Insignificant","Significant"),
                     guide=guide_legend(title = "", 
                                        label.theme = element_text(angle=0,size=12,face="bold"))) 
ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure_CNV_Pt19-cellpellet.pdf",width=20, height=10)
#cfDNA
Pt07_cfDNA <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Pt07_cfDNA.txt")
chr <- do.call('c',lapply(as.character(Pt07_cfDNA[,'region']),function(x){
  f <- unlist(strsplit(x,'\\:'));
  return(f[1]);
}));
chr <- factor(chr,levels=c(seq(1,22,1)))
chr.counts <- table(chr);
chr.midpt <- sapply(chr.counts,function(x){return(round(x/2,0));});
numberpos <- chr.midpt + c(0,cumsum(chr.counts)[-length(chr.counts)]);
linepos <- cumsum(chr.counts)[-length(chr.counts)];
linepos<-c("549","969","1427","1688","2073","2360","2688","2819","3116","3268","3607","3979","4186","4304","4462", "4729", "5143", "5244", "5668", "5831", "5896")
theme_mine <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size=10,hjust=2,angle=0,face="bold"),
      axis.text.y = element_text(size=14,,hjust = 1,face="bold"),
      axis.ticks.x =  element_blank(),
      axis.ticks.y =  element_blank(),
      axis.title.x =  element_blank(),
      axis.title.y = element_blank(),
      legend.position = "top",
      legend.direction="horizontal"
    )
}
ggplot(Pt07_cfDNA,aes(x=seq(1,nrow(Pt07_cfDNA)),y=lr)) + theme_mine() +
  geom_jitter(aes(color = factor(sig))) + 
  scale_y_continuous(limits=c(-4,4)) + 
  scale_x_discrete(breaks=linepos,labels=seq(1,21,1)) + 
  scale_color_manual(values=c("#0072B2","#D55E00"),
                     labels=c("Insignificant","Significant"),
                     guide=guide_legend(title = "", 
                                        label.theme = element_text(angle=0,size=12,face="bold"))) 

ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure_CNV_Pt07-cfDNA.pdf",width=20, height=10)
