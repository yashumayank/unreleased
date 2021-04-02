rm(list=ls())
## Load library
library(lattice)
library(bitops)
library(RColorBrewer)
## Read fileNames of the inpuit and output stat files. first file will be used as reference for the standard deviations
datafiles<-read.table("statFileNames.txt", h=TRUE, sep="\t", comment="", stringsAsFactors=FALSE)
NoFiles<-NROW(datafiles)
dataList <- list()
statsAgg <- data.frame(AssemblyName=rep("",NoFiles), covMedian=rep(NA,NoFiles),covSD=rep(NA,NoFiles),varMean=rep(NA,NoFiles),varSD=rep(NA,NoFiles),shortCovMedian=rep(NA,NoFiles),shortCovSD=rep(NA,NoFiles),shortInsMedian=rep(NA,NoFiles),shortInsSD=rep(NA,NoFiles),longCovMedian=rep(NA,NoFiles),longCovSD=rep(NA,NoFiles),longInsMedian=rep(NA,NoFiles),longInsSD=rep(NA,NoFiles),badRegions=rep(NA,NoFiles),badFraction=rep(NA,NoFiles), stringsAsFactors=FALSE)

#Function to check a single frame
### profiling
#Rprof(tmp <- tempfile())

## Read data, linkers and domain sizes
for (assNo in 1:NoFiles){
  
  dataList[[assNo]] <- read.table(datafiles$InputStatFile[assNo], h=TRUE, sep="\t", comment="", stringsAsFactors=FALSE)
  names(dataList[[assNo]]) <- c("scaff", "pos", "ref", "cov", "var", "short_cov", "short_ins", "long_cov", "long_ins")
  data<-dataList[[assNo]]
  
  #Calculate all Medians and Standard Deviations
  statsAgg$AssemblyName[assNo]<- datafiles$InputStatFile[assNo]
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
  
  scaffolds<-unique(data$scaff)
  regionST<-0
  tFlag<-0
  rgnCov<-0
  rgnVar<-0
  rgnShortCov<-0
  rgnShortIns<-0
  rgnLongCov<-0
  rgnLongIns<-0  
  
  #Create results table
  sIdx<-1
  N<-4000
  s2Idx<-1
  stats <- data.frame(scaff=rep("",N), regionStart=rep(NA,N), regionStop=rep(NA,N), flag=rep(NA,N), coverage=rep(NA,N), variance=rep(NA,N), shortCoverage=rep(NA,N), shortInsert=rep(NA,N), longCoverage=rep(NA,N), longInsert=rep(NA,N), stringsAsFactors=FALSE)
  stats2 <- data.frame(scaff=rep("",N*6), regionStart=rep(NA,N*6), regionStop=rep(NA,N*6), flag=rep(NA,N*6), median=rep(NA,N*6), stringsAsFactors=FALSE)
  
  #process all scoffolds seperately
  #scfName<-scaffolds[1]
  for (scaff in scaffolds){
    data2<-data[data$scaff==scaff, ]
    maxPos<-max(data2$pos)
    st<-1
    sp<-min(100, maxPos)
    if(!identical(data2$pos, 1:nrow(data2))) stop("Position vector has gaps")
    
    # process each frame
    while(maxPos>st){
      #checkFrame <- function(st,sp){
      #data1<-data2[data2$pos>=st & data2$pos<=sp, ]
      data1<-data2[st:sp, ]
      flag<-0
      
      #Set all medians
      #Set coverage median and exclude Ns
      if(length(data1$cov[data1$ref!="N"])>0)
      {
        tcovMedian<-median(data1$cov[data1$ref!="N"])       
      }else{
        tcovMedian<-statsAgg$covMedian[assNo]
      }
      
      #Set insert medians and check for zero insert lengths 
      if(length(data1$short_ins[data1$short_ins>0])>0)
        tshortInsMedian<-median(data1$short_ins[data1$short_ins>0])
      else
        tshortInsMedian<-statsAgg$shortInsMedian[assNo]
      if(length(data1$long_ins[data1$long_ins>0])>0)
        tlongInsMedian<-median(data1$long_ins[data1$long_ins>0])
      else
        tlongInsMedian<-statsAgg$longInsMedian[assNo]
      
      tvarMedian<-median(data1$var)
      tshortCovMedian<-median(data1$short_cov)
      tlongCovMedian<-median(data1$long_cov)
      #set cut-off filters
      lowcutCov<-statsAgg$covMedian[assNo]*0.9;
      highcutCov<-statsAgg$covMedian[assNo]*2;
      lowcutShortCov<-statsAgg$shortCovMedian[assNo]*0.9;
      highcutShortCov<-statsAgg$shortCovMedian[assNo]*2;
      lowcutLongCov<-statsAgg$longCovMedian[assNo]*0.9;
      highcutLongCov<-statsAgg$longCovMedian[assNo]*2;
      
      #Set short coverage filter according to the theoretical coverage
      filterShortCov<-statsAgg$shortCovMedian[assNo]
      if((st+50)<(statsAgg$shortInsMedian[assNo])){
        filterShortCov<-(st+50)*statsAgg$shortCovMedian[assNo]/statsAgg$shortInsMedian[assNo]
      }else{
        if((st+50)>(maxPos-statsAgg$shortInsMedian[assNo])){
          filterShortCov<-(maxPos-(st+50))*statsAgg$shortCovMedian[assNo]/statsAgg$shortInsMedian[assNo]
        }
      }
      #Set long coverage and total coverage filter according to the theoretical coverage
      if((st+50)<(statsAgg$longInsMedian[assNo])){
        filterLongCov<-(st+50)*statsAgg$longCovMedian[assNo]/statsAgg$longInsMedian[assNo]
        filterCov<-statsAgg$covMedian[assNo]*(filterShortCov+filterLongCov)/(statsAgg$longCovMedian[assNo]+statsAgg$shortCovMedian[assNo])
      }else{
        if((st+50)>(maxPos-statsAgg$longInsMedian[assNo])){
          filterLongCov<-(maxPos-(st+50))*statsAgg$longCovMedian[assNo]/statsAgg$longInsMedian[assNo]
          filterCov<-statsAgg$covMedian[assNo]*(filterShortCov+filterLongCov)/(statsAgg$longCovMedian[assNo]+statsAgg$shortCovMedian[assNo])
        }else{
          filterLongCov<-statsAgg$longCovMedian[assNo]
          filterCov<-statsAgg$covMedian[assNo]
        }
      }
      # Apply all filters to check bad region     
      if(tcovMedian > (filterCov+highcutCov) || tcovMedian < (filterCov-lowcutCov)){
        flag<-flag+1
        if(rgnCov == 0){
          rgnCov<-st
        }
      }else{
        if(rgnCov != 0){
          stats2[s2Idx, ] <- c(scaff, rgnCov, st-1, 1, median(data2$cov[rgnCov:(st-1)]))
          s2Idx<-s2Idx+1
          rgnCov<- 0
        }
      }
      if(tvarMedian > (statsAgg$varMean[assNo]+statsAgg$varSD[1]) || tvarMedian < (statsAgg$varMean[assNo]-statsAgg$varSD[1])){
        flag<-flag+2
        if(rgnVar == 0){
          rgnVar<-st
        }
      }else{
        if(rgnVar != 0){
          stats2[s2Idx, ] <- c(scaff, rgnVar, st-1, 2, median(data2$var[rgnVar:(st-1)]))
          s2Idx<-s2Idx+1
          rgnVar<- 0
        }
      }
      if(tshortCovMedian > (filterShortCov+highcutShortCov) || tshortCovMedian < (filterShortCov-lowcutShortCov)){
        flag<-flag+4
        if(rgnShortCov == 0){
          rgnShortCov<-st
        }
      }else{
        if(rgnShortCov != 0){
          stats2[s2Idx, ] <- c(scaff, rgnShortCov, st-1, 4, median(data2$short_cov[rgnShortCov:(st-1)]))
          s2Idx<-s2Idx+1
          rgnShortCov<- 0
        }
      }
      if(tshortInsMedian > (0.5*statsAgg$shortInsMedian[1])+statsAgg$shortInsMedian[assNo] || tshortInsMedian < statsAgg$shortInsMedian[assNo]-(0.5*statsAgg$shortInsMedian[1])){
        flag<-flag+8
        if(rgnShortIns == 0){
          rgnShortIns<-st
        }
      }else{
        if(rgnShortIns != 0){
          stats2[s2Idx, ] <- c(scaff, rgnShortIns, st-1, 8, median(data2$short_ins[rgnShortIns:(st-1)]))
          s2Idx<-s2Idx+1
          rgnShortIns<- 0
        }
      }
      if(tlongCovMedian > (filterLongCov+highcutLongCov) || tlongCovMedian < (filterLongCov-lowcutLongCov)){
        flag<-flag+16
        if(rgnLongCov == 0){
          rgnLongCov<-st
        }
      }else{
        if(rgnLongCov != 0){
          stats2[s2Idx, ] <- c(scaff, rgnLongCov, st-1, 16, median(data2$long_cov[rgnLongCov:(st-1)]))
          s2Idx<-s2Idx+1
          rgnLongCov<- 0
        }
      }
      if(tlongInsMedian > statsAgg$longInsMedian[assNo]+(0.5*statsAgg$longInsMedian[1]) || tlongInsMedian < statsAgg$longInsMedian[assNo]-(0.5*statsAgg$longInsMedian[1])){
        flag<-flag+32
        if(rgnLongIns == 0){
          rgnLongIns<-st
        }
      }else{
        if(rgnLongIns != 0){
          stats2[s2Idx, ] <- c(scaff, rgnLongIns, st-1, 32, median(data2$long_ins[rgnLongIns:(st-1)]))
          s2Idx<-s2Idx+1
          rgnLongIns<- 0
        }
      }
      
      ## Process aggregate region
      if(regionST == 0){
        if(flag>0){
          regionST<- st
          tFlag<-bitOr(tFlag,flag)
        }
      }else{  
        if(regionST != 0){
          if(flag==0){
            stats[sIdx, ] <- c(scaff, regionST, st-1, tFlag, median(data2$cov[regionST:(st-1)]), median(data2$var[regionST:(st-1)]), median(data2$short_cov[regionST:(st-1)]), median(data2$short_ins[regionST:(st-1)]), median(data2$long_cov[regionST:(st-1)]), median(data2$long_ins[regionST:(st-1)]))
            sIdx<-sIdx+1
            regionST<- 0
            tFlag<-0
          }else{
            tFlag<-bitOr(tFlag,flag)
          }
        }
      }
      rm(data1)
      #}
      #  checkFrame(stt, stp)
      st<-st+50
      sp<-min(sp+50, maxPos)
    }
    # process the last regions if open
    if(regionST != 0){
      stats[sIdx, ] <- c(scaff, regionST, maxPos, tFlag, median(data2$cov[regionST:maxPos]), median(data2$var[regionST:maxPos]), median(data2$short_cov[regionST:maxPos]), median(data2$short_ins[regionST:maxPos]), median(data2$long_cov[regionST:maxPos]), median(data2$long_ins[regionST:maxPos]))
      sIdx<-sIdx+1
      regionST<- 0
      tFlag<-0
    }
    if(rgnCov != 0){
      stats2[s2Idx, ] <- c(scaff, rgnCov, maxPos, 1, median(data2$cov[rgnCov:maxPos]))
      s2Idx<-s2Idx+1
      rgnCov<- 0
    }
    if(rgnVar != 0){
      stats2[s2Idx, ] <- c(scaff, rgnVar, maxPos, 2, median(data2$var[rgnVar:maxPos]))
      s2Idx<-s2Idx+1
      rgnVar<- 0
    }
    if(rgnShortCov != 0){
      stats2[s2Idx, ] <- c(scaff, rgnShortCov, maxPos, 4, median(data2$short_cov[rgnShortCov:maxPos]))
      s2Idx<-s2Idx+1
      rgnShortCov<- 0
    }
    if(rgnShortIns != 0){
      stats2[s2Idx, ] <- c(scaff, rgnShortIns, maxPos, 8, median(data2$short_ins[rgnShortIns:maxPos]))
      s2Idx<-s2Idx+1
      rgnShortIns<- 0
    }
    if(rgnLongCov != 0){
      stats2[s2Idx, ] <- c(scaff, rgnLongCov, maxPos, 16, median(data2$long_cov[rgnLongCov:maxPos]))
      s2Idx<-s2Idx+1
      rgnLongCov<- 0
    }
    if(rgnLongIns != 0){
      stats2[s2Idx, ] <- c(scaff, rgnLongIns, maxPos, 32, median(data2$long_ins[rgnLongIns:maxPos]))
      s2Idx<-s2Idx+1
      rgnLongIns<- 0
    }
    rm(data2)
  }
  write.table(stats, datafiles$OutputSummaryFile[assNo], sep="\t", quote=FALSE)
  write.table(stats2, paste(datafiles$OutputSummaryFile[assNo],"2",sep=""), sep="\t", quote=FALSE)
  statsAgg$badRegions[assNo]<-NROW(stats[stats$scaff!="",])
  statsAgg$badFraction[assNo]<-(sum(as.numeric(stats$regionStop[stats$scaff!=""]))-sum(as.numeric(stats$regionStart[stats$scaff!=""])))/NROW(data)
  rm(data)
  rm(stats)
}

write.table(statsAgg, "validationSummary.stats.csv", sep="\t", row.names=FALSE, quote=FALSE)
### profiling & reporting
# Rprof()
# sumRprof <- summaryRprof(tmp)
# unlink(tmp)
# sumRprof
#cpalet<-colors()[c(7,12,24,26,31,32,41,46,48,56,62,68,74,76,81,83, 89,92,99,100)]
cpalet <- c(brewer.pal(n=9, name="Set1"), brewer.pal(n=9, name="Pastel1"))
cairo_pdf("Densities_equalNs.pdf", onefile=TRUE)
for (atr in c("cov", "short_cov", "short_ins", "long_cov", "long_ins"))
{
  limitsX<-quantile((dataList[[1]][,atr][dataList[[1]][atr]!=0]),probs=c(.005,.995),names=FALSE)
  ## Calculating densities
  densities <- list()
  ymax <- 0
  for (i in 1:NoFiles){
    densities[[i]] <- density(dataList[[i]][,atr][dataList[[i]][,atr]!=0], from=0, to=limitsX[2])
    ymax <- max(ymax, densities[[i]]$y)
  }
  ## Plotting
  plot(densities[[i]], xlim = c(limitsX[1], limitsX[2]), ylim = c(0, ymax), main=atr, type="n")
  if(atr=="long_cov"|| atr=="long_ins" || atr=="short_ins"){
    legend("topleft",legend=statsAgg$AssemblyName, col=cpalet[1:NoFiles], lwd=2, lty = 1)
  }else{
    legend("topright",legend=statsAgg$AssemblyName, col=cpalet[1:NoFiles], lwd=2, lty = 1)
  }
  for (i in 1:NoFiles){
    ##lines(density(dataList[[i]][,atr][dataList[[i]][,atr]!=0]),  col=cpalet[i])
    lines(densities[[i]],  col=cpalet[i])
  }
}
dev.off()
