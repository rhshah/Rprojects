library('MASS');
library("ggplot2")
library("plyr")
library("reshape2")
library("RColorBrewer")
library("grid")
library("gridExtra")
library("scales")
chrom <- c(seq(1,22,1),'X','Y');
order.genomic <- function(dat){
  chr.pos <- do.call('rbind',lapply(dat[,'region'], function(x){
    f <- unlist(strsplit(x,'\\:'));
    g <- unlist(strsplit(f[2],'\\-'));
    return(c('chr0'=f[1],'pos0'=g[1]));
  }));
  dat <- cbind(dat,chr.pos);
  ret <- do.call('rbind',lapply(chrom,function(chr.i){
    dat. <- dat[which(dat[,'chr0'] == chr.i),];
    dat. <- dat.[order(as.numeric(as.character(dat.[,'pos0'])),decreasing=F),];
    return(dat.);
  }));
  return(ret[,-c(ncol(ret)-1,ncol(ret))]);
} 
chr <- do.call('c',lapply(as.character(Pt07_cellpellet[,'region']),function(x){
  f <- unlist(strsplit(x,'\\:'));
  return(f[1]);
}));
chr <- factor(chr,levels=c(seq(1,22,1)))
chr.counts <- table(chr);
chr.midpt <- sapply(chr.counts,function(x){return(round(x/2,0));});
numberpos <- chr.midpt + c(0,cumsum(chr.counts)[-length(chr.counts)]);
linepos <- cumsum(chr.counts)[-length(chr.counts)];
Pt07_cellpellet <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Pt07_cellpellet.txt")

Project.5474D.pellet <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Project-5474-D_copynumber_segclusp.probes.txt")
Project.5500G.cfdna <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Project-5500-G_copynumber_segclusp.probes.txt")
Project.5500M.cfdna <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Project-5500-M_copynumber_segclusp.probes_withoutClinicalSamples.txt")
Project.5500M.cfdna.FFPE <- read.delim("~/Documents/MSKCC/CMO/CSF_Analysis/Analysis_Plots/CopyNumber/Project-5500-M_copynumber_segclusp.probes_withClinicalSamples.txt")
linepos<-c("549","969","1427","1688","2073","2360","2688","2819","3116","3268","3607","3979","4186","4304","4462", "4729", "5143", "5244", "5668", "5831", "5896")
#Pt7_cellPellet_Idx <- which(Project.5474D.pellet$sample == "IM-lmd-007-CSF" ) 
#Pt7_cellPellet <- rbind(Project.5474D.pellet[Pt7_cellPellet_Idx,]);
#row.names(Pt7_cellPellet) = seq(1,nrow(Pt7_cellPellet),1)
#Pt7_cellPellet_datm<-melt(Pt7_cellPellet,measure.vars = c("lr"))
ggplot(Pt07_cellpellet,aes(x=seq(1,nrow(Pt07_cellpellet)),y=lr)) + theme_mine() +
  geom_jitter(color="#0072B2",aes(fill=sig)) + 
  scale_y_continuous(limits=c(-4,4)) + 
  scale_x_discrete(breaks=linepos,labels=seq(1,21,1)) + 
  scale_color_manual(values=c("#0072B2","#D55E00")) 

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