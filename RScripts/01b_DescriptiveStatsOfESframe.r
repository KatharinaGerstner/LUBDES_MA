############################################################################
### Purpose of this skript module 01a is to:
###
### 01.b.1 Protocol structure and summary of variables in the ES.frame
### 01.b.2 Plot Histograms of all variables in the ES.frame 
### 01.b.3 Protocol structure and summary of variables in the ES.frame.noLU
### 01.b.4 Plot Histograms of all variables in the ES.frame.noLU 
###
### General comments:
###
### Authors: KG ...
############################################################################

setwd(path2temp %+% "/")

### 01.b.1 Protocol structure and summary of variables in the ES.frame
sink('str_summary_ESframe.txt')
str(ES.frame)
summary(ES.frame)
sink()

### 01.b.2 Plot Histograms of all variables in the ES.frame 
#pdf("PlotHist_ESframe.pdf", width = 15)
for(i in 1:ncol(ES.frame)){
  p <- ggplot(data=ES.frame) + 
    geom_histogram(aes(x=ES.frame[,i]), size=0.4) + 
    xlab(names(ES.frame)[i]) +
    ggtitle(paste(names(ES.frame)[i])) + 
    theme(axis.title = element_text(size = rel(2)), axis.text = element_text(size = rel(2)),plot.title=element_text(size = rel(2)) , axis.text.x=element_text(angle=45,vjust = 1, hjust=1),legend.text=element_text(size = rel(2)),legend.title=element_text(size = rel(2)))
  #,axis.ticks.length=unit(.4,"cm")
  print(p)
  ggsave(p, file = paste(path2temp,"/PlotHist_ESframe_",gsub(".","",colnames(ES.frame[i]),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
  
}  
#dev.off()

### 01.b.3 Protocol structure and summary of variables in the ES.frame.noLU
sink('str_summary_ES.frame.noLU.txt')
str(ES.frame.noLU)
summary(ES.frame.noLU)
sink()

### 01.b.4 Plot Histograms of all variables in the ES.frame.noLU 
#pdf("PlotHist_ESframenoLU.pdf", width = 15)
for(i in 1:ncol(ES.frame.noLU)){
  p <- ggplot(data=ES.frame.noLU) + 
    geom_histogram(aes(x=ES.frame.noLU[,i]), size=0.4) + 
    xlab(names(ES.frame.noLU)[i]) +
    ggtitle(paste(names(ES.frame.noLU)[i])) + 
    theme(axis.title = element_text(size = rel(2)), axis.text = element_text(size = rel(2)),plot.title=element_text(size = rel(2)), axis.text.x=element_text(angle=45,vjust = 1, hjust=1),legend.text=element_text(size = rel(2)),legend.title=element_text(size = rel(2)))
  #,axis.ticks.length=unit(.4,"cm")
  print(p)
  ggsave(p, file = paste(path2temp,"/PlotHist_ESframenoLU_",gsub(".","",colnames(ES.frame.noLU[i]),fixed=T),".png",sep=""), width = 20, height = 8, type = "cairo-png")
}  
#dev.off()

setwd(path2wd)
