library('MASS');
library("ggplot2")
library("plyr")
library("reshape2")
library("RColorBrewer")
library("grid")
library("gridExtra")
library("scales")
#cfDNA
Pt12_cfDNA <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Pt12_cfDNA.txt")
chr <- do.call('c',lapply(as.character(Pt12_cfDNA[,'region']),function(x){
  f <- unlist(strsplit(x,'\\:'));
  return(f[1]);
}));
chr <- factor(chr,levels=c(seq(1,22,1)))
chr.counts <- table(chr);
chr.midpt <- sapply(chr.counts,function(x){return(round(x/2,0));});
numberpos <- chr.midpt + c(0,cumsum(chr.counts)[-length(chr.counts)]);
linepos <- cumsum(chr.counts)[-length(chr.counts)];
linepos<-c("480","813","1201","1410","1733","1958","2219","2290","2546","2646","2931","3253","3424","3505","3630","3872","4255","4322","4729","4866","4917")
theme_mine <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size=14,hjust=2,angle=0,face="bold"),
      axis.text.y = element_text(size=14,,hjust = 1,face="bold"),
      axis.ticks.x =  element_blank(),
      axis.ticks.y =  element_blank(),
      axis.title.x =  element_blank(),
      axis.title.y = element_text(size=16,angle=90,face="bold"),
      legend.position = "top",
      legend.direction="horizontal",
      panel.grid.major.x = element_line(colour = "grey", linetype = "dashed"),
      panel.grid.major.y = element_line(colour = "grey80")
    )
}
ggplot(Pt12_cfDNA,aes(x=seq(1,nrow(Pt12_cfDNA)),y=lr)) + theme_mine() +
  geom_jitter(aes(color = factor(sig))) + 
  ylab("LOG2 Tumor/Normal Ratio") +
  scale_y_continuous(breaks=seq(-4,4,1),limits=c(-4,4)) + 
  scale_x_discrete(breaks=linepos,labels=seq(1,21,1)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("#0072B2","#D55E00"),
                     labels=c("Insignificant","Significant"),
                     guide=guide_legend(title = "", 
                                        label.theme = element_text(angle=0,size=18,face="bold"))) 

ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure_CNV_Pt12-cfDNA.pdf",width=20, height=5)
#FFPE
Pt12_FFPE <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Pt12_FFPE.txt")
chr <- do.call('c',lapply(as.character(Pt12_FFPE[,'region']),function(x){
  f <- unlist(strsplit(x,'\\:'));
  return(f[1]);
}));
chr <- factor(chr,levels=c(seq(1,22,1)))
chr.counts <- table(chr);
chr.midpt <- sapply(chr.counts,function(x){return(round(x/2,0));});
numberpos <- chr.midpt + c(0,cumsum(chr.counts)[-length(chr.counts)]);
linepos <- cumsum(chr.counts)[-length(chr.counts)];
linepos<-c("480","813","1201","1410","1733","1958","2219","2290","2546","2646","2931","3253","3424","3505","3630","3872","4255","4322","4729","4866","4917")
theme_mine <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size=14,hjust=2,angle=0,face="bold"),
      axis.text.y = element_text(size=14,hjust = 1,face="bold"),
      axis.ticks.x =  element_blank(),
      axis.ticks.y =  element_blank(),
      axis.title.x =  element_blank(),
      axis.title.y = element_text(size=16,angle=90,face="bold"),
      legend.position = "top",
      legend.direction="horizontal",
      panel.grid.major.x = element_line(colour = "grey", linetype = "dashed"),
      panel.grid.major.y = element_line(colour = "grey80")
    )
}
ggplot(Pt12_FFPE,aes(x=seq(1,nrow(Pt12_FFPE)),y=lr)) + theme_mine() +
  geom_jitter(aes(color = factor(sig))) + 
  ylab("LOG2 Tumor/Normal Ratio") +
  scale_y_continuous(breaks=seq(-4,4,1),limits=c(-4,4)) + 
  scale_x_discrete(breaks=linepos,labels=seq(1,21,1)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("#0072B2","#D55E00"),
                     labels=c("Insignificant","Significant"),
                     guide=guide_legend(title = "", 
                                        label.theme = element_text(angle=0,size=18,face="bold"))) 

ggsave("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/Figure_CNV_Pt12-FFPE.pdf",width=20, height=5)
