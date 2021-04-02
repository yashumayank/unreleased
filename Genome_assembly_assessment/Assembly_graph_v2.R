rm(list=ls())
## Load library
library(lattice)
library(RColorBrewer)
library(calibrate)

# AWK Script to fix the condensed output
#awk 'NR==1{print $0;next}($2==0){next}{if($2==bg){x++;$3=x}else{bg=$2;x=1;$3=x};$1=""; sub(/^[ \t]+/, "");print}' GMBLNrPeC02_21.stats.condensed |tr ' ' '\t' > GMBLNrPeC02_21.stats.condensed.posfixed
windowSize<-100
windowStep<-50

fileN<-commandArgs(TRUE)[1]
data <- read.table(fileN, h=TRUE, sep="\t", comment="", stringsAsFactors=FALSE)
names(data) <- c("scaff", "pos", "ref", "cov", "var", "short_cov", "short_ins", "long_cov", "long_ins")


NoFiles <- 1
scaffolds<-unique(data$scaff)
records <- (NROW(data)/windowStep) +(100*NROW(scaffolds))  ## there are remaining few bases in the end of each scaffold

out <- data.frame(scaff=character(records), pos=numeric(records), ref=character(records), coverage=numeric(records), divergence=numeric(records), short_goodPairs=numeric(records), short_averageInsert=numeric(records), long_goodPairs=numeric(records), long_averageInsert=numeric(records), stringsAsFactors=FALSE)
#out <- matrix(data="", nrow=records, ncol=8,dimnames=list(NULL,c("scaff", "pos", "cov", "var", "short_cov", "short_ins", "long_cov", "long_ins")))

out <- as.matrix(out)
sIdx<-1

#Calculate all Medians and Standard Deviations
statsAgg <- data.frame(AssemblyName=rep("",NoFiles), covMedian=rep(NA,NoFiles),covSD=rep(NA,NoFiles),varMean=rep(NA,NoFiles),varSD=rep(NA,NoFiles),shortCovMedian=rep(NA,NoFiles),shortCovSD=rep(NA,NoFiles),shortInsMedian=rep(NA,NoFiles),shortInsSD=rep(NA,NoFiles),longCovMedian=rep(NA,NoFiles),longCovSD=rep(NA,NoFiles),longInsMedian=rep(NA,NoFiles),longInsSD=rep(NA,NoFiles),badRegions=rep(NA,NoFiles),badFraction=rep(NA,NoFiles), stringsAsFactors=FALSE)

assNo<-1
statsAgg$covMedian[assNo]<- median(data$cov[data$cov>0])
statsAgg$covSD[assNo]<- sd(data$cov[data$cov>0])
statsAgg$varMean[assNo]<- mean(data$var)
statsAgg$varSD[assNo]<- sd(data$var)
statsAgg$shortCovMedian[assNo]<-median(data$short_cov[data$short_cov>0])
statsAgg$shortCovSD[assNo]<-sd(data$short_cov[data$short_cov>0])
statsAgg$shortInsMedian[assNo]<-median(data$short_ins[data$short_ins>0])
statsAgg$shortInsSD[assNo]<-sd(data$short_ins[data$short_ins>0])
statsAgg$longCovMedian[assNo]<-median(data$long_cov[data$long_cov>0])
statsAgg$longCovSD[assNo]<-sd(data$long_cov[data$long_cov>0])
statsAgg$longInsMedian[assNo]<-median(data$long_ins[data$long_ins>0])
statsAgg$longInsSD[assNo]<-sd(data$long_ins[data$long_ins>0])

#Scaffold stop vector
vScaff<-data.frame(scaff=character(length(scaffolds)), stopPosition=numeric(length(scaffolds)), stringsAsFactors=FALSE)
vScfI<-1
vCtg<-data.frame(scaff=character(length(scaffolds)), stopPosition=numeric(length(scaffolds)), stringsAsFactors=FALSE)
vCtgI<-1

for (scaff in scaffolds){
  data2<-data[data$scaff==scaff, ]
  maxPos<-max(data2$pos)
  st<-1
  sp<-min(windowSize, maxPos)
  if(!identical(data2$pos, 1:nrow(data2))) stop("Position vector has gaps")
  
  # process each frame
  while(maxPos>st){
    data1<-data2[st:sp, ]
    
    #Set all medians
    #Set coverage median and exclude Ns
    tcovMedian<-median(data1$cov)       
    if(length(data1$cov[data1$ref=="N"])>0)
    {
      vCtg[vCtgI, ]<-c(scaff,sIdx)
      vCtgI<-vCtgI+1
    }
    
    #Set insert medians and check for zero insert lengths 
    if(length(data1$short_ins[data1$short_ins>0])>0)
      tshortInsMedian<-median(data1$short_ins[data1$short_ins>0])
    else
      tshortInsMedian<-0
    if(length(data1$long_ins[data1$long_ins>0])>0)
      tlongInsMedian<-median(data1$long_ins[data1$long_ins>0])
    else
      tlongInsMedian<-0
    
    tvarMedian<-median(data1$var)
    tshortCovMedian<-median(data1$short_cov)
    tlongCovMedian<-median(data1$long_cov)

    out[sIdx, ] <- c(scaff, st, "N",  tcovMedian, tvarMedian, tshortCovMedian, tshortInsMedian, tlongCovMedian, tlongInsMedian)
    sIdx<-sIdx+1
    
    st<-st+windowStep
    sp<-min(sp+windowStep, maxPos)
  }
  vScaff[vScfI, ]<-c(scaff,sIdx-1)
  vScfI<-vScfI+1
  rm(data2)
}
out <- as.data.frame(out)

out2<-out[out$scaff!="",]
vCtg<-vCtg[vCtg$stopPosition!="0",]
vScaff<-vScaff[vScaff$stopPosition!="0",]     
gap<-200
outXaxis<-as.character(out2$pos[seq(1, length(out2$pos), gap)])
#seq produces a sequence form 1. remove first value
outXaxis<-outXaxis[-1]

cpalet <- c(brewer.pal(n=9, name="Set1"), brewer.pal(n=9, name="Pastel1"))
pdf(paste(fileN,".pdf",sep=""), width = 268, height = 11 )
i<-2

par(mfrow=c(NCOL(out)-4,1),xaxs="i", mar=c(5,4,3,1)+0.1) 
for (atr in c("coverage", "short_goodPairs", "short_averageInsert", "long_goodPairs", "long_averageInsert"))
{
  rm(outYaxis)
  plot(as.numeric(as.character(out2[,atr])),type="l", col=cpalet[i], axes=FALSE, ann=FALSE)
  mn<-median(as.numeric(as.character(out2[,atr])),na.rm=TRUE)
  sdv<-sd(as.numeric(as.character(out2[,atr])),na.rm=TRUE)
  outYaxis <- c(mn-(2*sdv),mn-sdv,mn+sdv,mn+(2*sdv))
  abline(h = outYaxis, col = cpalet[1],lty=3)
  if(atr %in% c("coverage", "short_goodPairs", "long_goodPairs"))
  {
    #outYaxis <- c(outYaxis,0.1*mn,mn,1.5*mn)
    abline(h = c(0.5*mn,mn,1.5*mn), col = "black",lty=3)
  }
  else
  {
    #outYaxis <- c(outYaxis,0.5*mn,mn,1.5*mn)
    abline(h = c(0.8*mn,mn,1.20*mn), col = "black",lty=3)
  }
  outYaxis <- c(outYaxis,mn)
  title(ylab= atr, col.lab=cpalet[i])
  outYaxis<-round(outYaxis,1)
  axis(2, las=1, at=outYaxis , lab=as.character(outYaxis), cex=.65, col=cpalet[i])
  i<-i+1
}

#axis(1, las=2, at=200*1:NROW(out2), lab=(as.integer(seq(1, NROW(data), length.out=NROW(out2)))),col.lab ="darkblue")
#par(fig=c(0,1,0,1),new=TRUE,xaxs="i")
axis(1, las=2, at=gap*1:length(outXaxis) , lab=outXaxis, col.lab ="darkblue")
par(fig=c(0,1,0,1),new=TRUE,xaxs="i", xpd=TRUE)
abline(v = vScaff[,"stopPosition"], col = "black",lty=1)
#segment(vScaff[,"stopPosition"],0,vScaff[,"stopPosition"],200, col = "black",lty=1)
abline(v = vCtg[,"stopPosition"], col = "black",lty=3)
#title(xlab= "Scaffold_position*10kbp", col.lab=rgb(0,0.5,0), mgp=c(5.2,1,0))
par(fig=c(0,1,0,1),new=TRUE,xaxs="i")
#textxy("Scaffold_position*10kbp", at=c(.5,.5), outer=TRUE)
ln<-1
oscfPos<-0
for (scfName in vScaff$scaff)
{
  mtext(scfName, at=as.character((as.numeric(vScaff$stopPosition[vScaff$scaff==scfName])+oscfPos)/2-20),cex=.8
, side=3, line=ln)
ln<-ln-1  
  if(ln==-3)
  {
    ln<-1
  }
  oscfPos<-as.numeric(vScaff$stopPosition[vScaff$scaff==scfName])
}
dev.off()

write.table(out[out$scaff!="",], paste(fileN,".condensed",sep=""), sep="\t", quote=FALSE)
