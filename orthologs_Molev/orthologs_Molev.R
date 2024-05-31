#!/usr/bin/Rscript
library(ggplot2)
library(data.table)
suppressMessages(library(dplyr))
library(stringr)
library(parallel)
library(optparse)
#detach(package:Rmpfr, unload=TRUE)

# example:-  Rscript orthologs_Molev.R -p all_vs_all_homology_results.tab -p protein_sizes.tab

#to do list
# check limits of bit score constant "K" ; write about detecting neofunctiontionalization among fast evolving genes; such as muti-domain proteins, signma factors etc.
# Detect non-conserved proteins using the same approach that is used to filter normalized score; Prevent aligned length filtering
# ----------set default protein sizes file

option_list <- list(
  make_option(c("-g", "--number_genomes"), type="integer", default=NULL,
	help="Number of genomes", metavar="Integer"),
  make_option(c("-t", "--taxid_length"), type="integer", default=5,
	help="Number of characters used as taxon ID suffix in the protein IDs [default: 5]", metavar="Integer"),
  make_option(c("-p", "--protein_size_file"), type="character", default=NULL,
	help="File containing length of all proteins in amino acids [required]", metavar="Integer"),
  make_option(c("-f", "--homology_file"), type="character", default=NULL,
	help="homology search file with one line per protein pair in blast tabular format (outfmt=6) [required]", metavar="file_name"),
  make_option(c("-o", "--output_file"), type="character", default="orthologs.clus",
	help="Name of the output file with clustered families of orthologous proteins [default: orthologs.clus]", metavar="file_name"),
  make_option(c("-r", "--orthologs_only"), type="logical", action="store_true", default=FALSE,
	help="Predict orthologous relationship between proteins, and ignore in-paralogous
                 	and co-orthologous relationships.", metavar="TRUE | FALSE"),
  make_option(c("-s", "--homology_score"), type="character", default="bits",
	help="Choose the homology score that should be used during clustering of protein families.
            	[default: bits]
                	bits  	- bit scores
                	bitsLog   - log10 of bit scores
                	evalLog   - negative log10 of evalue scores
                	evalLogBH - Similar to evalLog. Uses bit score to search best inter-species hits."),
  make_option(c("-F", "--aln_length_filter"), type="character", default="est,0.2,0.4",
	help="To reduce the noise, the holology hits with low aligned length can be filtered using
            	one of the following approaches:
    
      	lmp   Filter homology results for each protein using a linear model prediction of the
            	aligned length, as a function the protein length. Individual models are generated
            	for each pair of species.
            	usage (-c lmp,FRAC)  eg. (-c lmp,0.6)
                	FRAC  	- The fraction of the predicted aligned length to be used  
                            	aligned length before the filter is applied. Suggested between 0.8-0.4
                	A lower value minimizes false negatives and a higher value minimizes false positives

      	est   Filter homology results for each protein based on the esimated aligned length of that protein.[default]
            	usage (-c est,MIN,MAX)  eg. (-c est,0.2,0.4)
                	MIN   	- Minimum allowed aligned length variance below the average aligned length.
                            	Suggested between 0.1-0.25 of protein length [default: 0.2]
                	MAX   	- Maximum allowed aligned length variance below the average aligned length.
                            	Suggested between 0.25-0.5 of protein length [default: 0.4]
                	The later values minimize false negatives and the former values minimize false positives

      	log   Filter homology results such that the minimum required aligned length for each protein
            	reduces logarithmically with increasing protein length.
            	usage (-c log,RATIO100,RATIO1600)  eg. (-c log,0.67,0.44)
                	RATIO100  - Minimum required aligned/total length ratio for proteins with 100 amino acids.
                            	Suggested between 0.72-0.6 of protein length
                	RATIO1600 - Minimum required aligned/total length ratio for proteins with 1600 amino acids.
                            	Suggested between 0.52-0.30 of protein length
              	Aligned/total length ratio for proteins with 100 amino acids is used for proteins shorter than that.
             	 
      	lin   Filter homology results such that the minimum required aligned length for each protein
            	increase/decrease linearly with increasing protein length.  
            	usage: (-c lin,RATIO100,RATIO1600)  eg.: (-c lin,0.65,0.45)
                	RATIO100  - Minimum required aligned/total length ratio for proteins with 100 amino acids.
                            	Suggested between 0.70-0.6 of protein length
                	RATIO1600 - Minimum required aigned/total length ratio for proteins with 1600 amino acids.
                            	Suggested between 0.50-0.30 of protein length
              	Aligned/total length ratio for proteins with 2500 amino acids is used for proteins longer than that.
              	A constant or no aligned length filter can be specified by setting RATIO100 and RATIO1600
              	to the same constant or zero, respectively. (eg. -c lin,0.5,0.5 or -c lin,0,0)
             	 
      	none  Do not filter the homology results using aligned length", metavar="par1,par2,par3"),
  make_option(c("-f", "--filter_by_smaller"), type="logical", action="store_true", default=FALSE,
	help="When 'TRUE', The '--aln_length_filter' will applied to the smaller, instead of the
             	larger, protein among each pair of homologs. [dafault FALSE]", metavar="TRUE | FALSE"),
  make_option(c("-C", "--normalize_by"), type="character", default="lmp",
          	help="The MCL algorithm is sensitive to high standard deviation in the scores. Thus, the scores
      	must be normalized using one of the following approaches:
     	 
      	lmp   Scores are normalized using a linear model prediction of the score, as a function of
            	the protein length. Individual models are generated for each pair of species.

      	orm   Scores are normalized using the average score from all or specified number of top hits for
            	each pair of species. The number of top hits to be used can be adjusted using the MAXPSP
            	parameter. 'MAXPSP = 1' can be used to perform orthoMCL-like score normalization.
            	usage (-c orM,MAXPSP)  eg. (-c orM,1) or (-c orM,0.4)
                	MAXPSP	- The maximum proportion of the species-pairs that are allowed to have fewer
                            	homologs that being used to calculate average scores for normalization.
                                                          	" , metavar="par1,par2"),
  make_option(c("-I", "--mclInflation"), type="character", action="store", default="1.6,1.7,1.8",
	help="mcl Inflation parameter to specify the granularity of protein families from 1.2 (larger generic families)
              	to 5.0 (smaller families). Separate output for each comma seperated value [default: 1.6,1.7,1.8]", metavar="1.2-5"),
  make_option(c("-O", "--coOrthologs"), type="character", action="store", default="together",
	help="Normalize the co-ortholog seperately or together with the orthologs. Co-orthologs that are
              	weaker hits usually represent neofunctionalization, and are usually clustered seperately. [default: together]
                	together   - co-orthologs are normalised to the same scale as orthologs.
                	orthoMCLv2 - scores of co-orthologs are normalized and scaled seperately as
                             	co-orthologs tends to have lower scores than orthologs.", metavar="together|orthoMCLv2"),
  make_option(c("-T", "--threads"), type="integer", default=NULL,
	help="Number of threads. Only some parts use multithreading [default: 92% of total cores]", metavar="number_of_threads")#,
)

# option removed
# est_REDUCE	- Higher aligned length cutoffs are relaxed further in proprotion to their difference
#         	from this value. Suggested between 0.65-0.45 of protein length [default 0.45]

#options to be added :-  -evalue_cutoff, --ff_large_file etc.
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$homology_file) | is.null(opt$protein_size_file) | is.null(opt$taxid_length)){
  print_help(opt_parser)
  stop("Parameters --taxid_length, --protein_size_file and --homology_file are required to run this program.", call.=FALSE)
}

#best way to decide inflation index is by checking the SD values in the mclInput
#I-mono    I-#fam    mean    sd    max    min    norm    score
#1.6    1.8    4.2948    0.995    38.011    0.03    subdiv    bits
#1.6    1.8    1    0.1985    1.9526    0.3527    div    bitsL

setDTthreads(percent=92)
if (!is.null(opt$threads)){
  setDTthreads(threads = opt$threads)
}

ram <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
swap <- as.numeric(system("awk '/SwapTotal/ {print $2}' /proc/meminfo", intern=TRUE))

if(length(ram)){
  if(length(swap)){
	ram = ram + swap
  }
  file2RAMratio <- format(round(((file.size(opt$homology_file)/1024)/ram) * 243, 2), nsmall = 2)
  message ("Expected RAM usage: ",file2RAMratio,"%")
  if(file2RAMratio > 95) {
	message ("The RAM barely sufficient for the run.")
	message ("If the program crashes, try closing other applications")
  } else {
	message ("Please ensure that RAM is not occupied by other programs during the run.")
  }
}

ptm <- proc.time()
#ct_bong <- as.integer(1.0*opt$number_genomes)
if(opt$homology_score=="evalLogBH"){
  colSelect <- c(1,2,4,11,12)
  colNames <- c("pid","sid","al","eval","bits")
} else if(opt$homology_score %in% c("evalLog")){
  colSelect <- c(1,2,4,11)
  colNames <- c("pid","sid","al","eval")
}else{
  colSelect <- c(1,2,4,12)
  colNames <- c("pid","sid","al","bits")
}

message ("Reading the data files ...")
data <- fread(cmd = paste("mergeHSPs_v2.3 -p 0.65 -i ", opt$homology_file, sep = " "), select = colSelect, header = FALSE, sep = "\t",stringsAsFactors = FALSE)
#data <- fread(cmd = paste(" awk '{if($1!=oldQ || $2!=oldS){print; oldQ=$1; oldS=$2}}' ", opt$homology_file, sep = " "), select = colSelect, header = FALSE, sep = "\t",stringsAsFactors = FALSE) #use first hits only
#data <- fread(opt$homology_file, select = colSelect, header = FALSE, sep = "\t",stringsAsFactors = FALSE)
#names(data) <- c("pid","sid","identity","al","mis","gap","startq","endq","starts","ends","eval","bits")
dataLen <- fread(opt$protein_size_file, header = FALSE, sep = "\t",stringsAsFactors = FALSE)
names(data) <- colNames
names(dataLen) <- c("pid","plen")

optArgC <- unlist(strsplit(opt$normalize_by, ","))
cType = as.character(optArgC[1])
argsC <- as.numeric(optArgC[-1])

optArgF <- unlist(strsplit(opt$aln_length_filter, ","))
fType = as.character(optArgF[1])
argsF <- as.numeric(optArgF[-1])
if (identical(fType,"log")){
  message ("Homology hits will be filtered using logrithmically varying aligned/total protein length ratio. Parameters: ", opt$aln_length_filter)
  sT <- (argsF[1]-argsF[2])/4
  data3 <- dataLen
  data3$cutoffR <- (sT*log2(1600/data3$plen))+argsF[2]
  msT <- (sT*log2(1600/100))+argsF[2]
  data3$cutoffR[data3$cutoffR > msT] <- msT
  setDT(data3)
  data3$cutoff <- data3$cutoffR * data3$plen
  data3[,c("cutoffR","plen"):=NULL]
} else if (identical(fType,"lin")){
  message ("Homology hits will be filtered using linearly varying aligned/total protein length ratio. Parameters: ", opt$aln_length_filter)
  data3 <- dataLen
  data3$cutoffR <- ((argsF[1]-argsF[2])/(100-1600))*(data3$plen-1600)+argsF[2]
  msT <- ((argsF[1]-argsF[2])/(100-1600))*(2500-1600)+argsF[2]
  data3$cutoffR[data3$cutoffR < msT] <- msT
  setDT(data3)
  data3$cutoff <- data3$cutoffR * data3$plen
  data3[,c("cutoffR","plen"):=NULL]
}

setDT(data)
if(opt$homology_score=="evalLogBH"){
  data$eval[data$eval==0] <- 1e-200
  data[, score := bits]
  data[,c("bits"):=NULL]
} else if(opt$homology_score=="evalLog"){
  data$eval[data$eval==0] <- 1e-200
  data[, score := -log10(eval)]
  data[, eval := NULL]
} else if(opt$homology_score=="bitsLog"){
  data[, score := log10(bits)]
  data[,c("bits"):=NULL]
}else{
  data[, score := bits]
  data[,c("bits"):=NULL]
}

message ("Preparing Common tables ...")
SimilarSequences <- data %>% mutate(taxQ = substr(pid, 1, opt$taxid_length), taxS = substr(sid, 1, opt$taxid_length))
rm(data)
gc(full=T)
#data5[, c("taxQ", "taxS") := list(substr(pid, 1, opt$taxid_length), taxS := substr(sid, 1, opt$taxid_length))]
data6 <- group_by(SimilarSequences, pid, taxS) %>% summarise(score = max(score), taxQ=first(taxQ))
#data6 <- setDT(SimilarSequences)[, .(bits=max(..BH_score), taxQ=first(taxQ)), by = "pid,taxS"]
bestQueryTaxonScore <- ungroup(data6[data6$taxS!=data6$taxQ,])
setDT(bestQueryTaxonScore)[,c("taxQ"):=NULL]
rm(data6)
gc(full=T)

message ("Preparing Ortholog tables ...")
#this inner_join was the crux of memory usage
data7 <- inner_join(SimilarSequences, bestQueryTaxonScore, by = c("pid"="pid","taxS"="taxS","score"="score"))
bestHit <- data7[data7$taxQ!=data7$taxS,]
rm(data7)
if(opt$homology_score=="evalLogBH"){
  bestHit[, score := eval]
  bestHit[,c("eval"):=NULL]
}
gc(full=T)
bestHitP <- left_join(bestHit, dataLen, by = "pid")
RBH <- inner_join(bestHitP,bestHitP, by = c("pid"="sid","sid"="pid"))
rm(bestHit, bestHitP)
setDT(RBH)[,c("taxQ.y","taxS.y"):=NULL]
gc(full=T)

if(identical(fType,"est")){
  message ("Estimated the aligned length and its variation for each proteins. Parameters: ", opt$aln_length_filter)
  #  data4 <- left_join(RBH, dataLen, by = "pid")
  #  data5 <- left_join(data4, dataLen, by = c("sid"="pid"))
  data3 <- group_by(RBH, pid) %>% summarise(plen = first(plen.x),alMean = mean(al.x), alSD = sd(al.x))
  data3$alSD[is.na(data3$alSD)] <- 0
  setDT(data3)[,alSDR := (alSD/plen) + argsF[1]]
  data3[alSDR > argsF[2], alSDR := argsF[2]]
  # REDUCE option has been removed
  # data3$cutoffR <- (data3$alMean/data3$plen) - data3$alSDR
  # rowNumbers = which(data3$cutoffR > argsF[3])
  # data3$cutoffR[rowNumbers] <- ((2 * data3$cutoffR[rowNumbers]) + argsF[3])/3
  # data3[,cutoff := cutoffR * plen]
  data3[,cutoff := alMean - (alSDR * plen)]  #cant switch to medians
  data3[,c("cutoffR","plen","alMean","alSD","alSDR"):=NULL]
}

message ("Applying the chosen aligned length filter Parameters: ", opt$aln_length_filter)
if(fType %in% c("est","log","lin")){
  # keep hits that have the larger or smaller protein in the pid
  if(opt$filter_by_smaller){
	data4 <- RBH[RBH$plen.x < RBH$plen.y | (RBH$plen.x == RBH$plen.y & RBH$pid < RBH$sid),]
  } else {
	data4 <- RBH[RBH$plen.x > RBH$plen.y | (RBH$plen.x == RBH$plen.y & RBH$pid < RBH$sid),]
  }
  rm(RBH)
  setDT(data4)[plen.x == plen.y, al.x := (al.x + al.y)/2]
  data4[,c("al.y"):=NULL]
  data5 <- left_join(data4, data3, by = "pid")
  RBHf <- data5 %>% filter(al.x>=cutoff)
  rm(data4, data5)
  setDT(RBHf)[,c("cutoff","al.x"):=NULL]
  RBHf$smaller_tax_id <- pmin(RBHf$taxQ.x,RBHf$taxS.x)
  RBHf$bigger_tax_id <- pmax(RBHf$taxQ.x,RBHf$taxS.x)                                                                                            	 
 
} else if(identical(fType,"lmp")){
  message ("Homology hits will be filtered using aligned lengths modelled as function of protein lengths. Parameters: ", opt$aln_length_filter)
  RBH$smaller_tax_id <- pmin(RBH$taxQ.x,RBH$taxS.x)
  RBH$bigger_tax_id <- pmax(RBH$taxQ.x,RBH$taxS.x)                                                                                            	 
  pairwiseLMAl <- group_by(RBH[RBH$plen.x<2000,],smaller_tax_id, bigger_tax_id) %>% do(lmp = lm(al.x ~ plen.x, data = .)) %>% mutate(intercept = lmp$coeff[1], slope = lmp$coeff[2])
  setDT(pairwiseLMAl)[,c("lmp"):=NULL]
  pairwiseLMAl$intercept <- pairwiseLMAl$intercept * argsF[1]
  pairwiseLMAl$slope <- pairwiseLMAl$slope * argsF[1]
  if(opt$filter_by_smaller){
	data3 <- RBH[RBH$plen.x < RBH$plen.y | (RBH$plen.x == RBH$plen.y & RBH$pid < RBH$sid),]
  } else {
	data3 <- RBH[RBH$plen.x > RBH$plen.y | (RBH$plen.x == RBH$plen.y & RBH$pid < RBH$sid),]
  }
  rm(RBH)
  setDT(data3)[plen.x == plen.y, al.x := (al.x + al.y)/2]
  data3[,c("al.y"):=NULL]
  data4 <- inner_join(data3, pairwiseLMAl, by = c("smaller_tax_id", "bigger_tax_id"))
  rm(data3)
  setDT(data4)[,cutoff := intercept + (slope * plen.x)]
  #In cases where predicted aligned is too high;reset cutoff to (argsF[1] * plen)
  data4[cutoff > (argsF[1]*plen.x), cutoff := (argsF[1]*plen.x)]
  #Detect proteins with non-conserved regions and use est. cutoffs; est. alR < 0.5 for both homologs

  RBHf <- data4 %>% filter(al.x>=cutoff)
  rm(data4)
  #  setDT(RBHf)[,c("cutoff","al.x","taxQ.x","taxS.x","slope","intercept"):=NULL]
  #  lmAlRBH <- lm(formula = al.x ~ plen, data = data1[data1$plen<=2000,])
  #  instead of a single aligned length model the aligned length for each species pair separately
  #  data3 <- dataLen
  #  data3$cutoff <- lmAlRBH$coefficients[1] + (lmAlRBH$coefficients[2]-argsF[1]) * data3$plen
  #  setDT(data3)[,c("plen"):=NULL]
} else {
  RBHf <- RBH[RBH$pid < RBH$sid,]
  RBHf$smaller_tax_id <- pmin(RBHf$taxQ.x,RBHf$taxS.x)
  RBHf$bigger_tax_id <- pmax(RBHf$taxQ.x,RBHf$taxS.x)                                                                                            	 
  rm(RBH)
}
#Finished applying the aligned length filter

RBHf$unNormScore <- (RBHf$score.x+RBHf$score.y)/2
RBHf$plen <- (RBHf$plen.x+RBHf$plen.y)/2
orthologTemp <- RBHf[,c("pid","sid","taxQ.x","taxS.x","smaller_tax_id", "bigger_tax_id","unNormScore","plen")]
names(orthologTemp) <- c("sequence_id_a","sequence_id_b","taxon_id_a","taxon_id_b","smaller_tax_id","bigger_tax_id","unNormScore","plen")
rm(RBHf)
gc(full=T)

#normalizing the scores
if(identical(cType,"orm")){
  if(argsC[1] < 0 | argsC[1] > 1){
	stop("When selecting 'orm' in --normalize_by option, the MAXPSP parameter must be set between 0 and 1", call.=FALSE)
  }
  totalInterSpHits <- group_by(orthologTemp, smaller_tax_id, bigger_tax_id) %>% tally()
  nOrth2Norm <- as.integer(quantile(totalInterSpHits$n,probs=argsC[1]))
  quantile(totalInterSpHits$n,probs=c(0.2,0.25,0.3,0.35,0.4,0.45,0.5))
  ##data10 <- group_by(orthologTemp, smaller_tax_id, bigger_tax_id) %>% top_n(nOrth2Norm, wt=unNormScore)
  data10 <- group_by(orthologTemp, smaller_tax_id, bigger_tax_id) %>% slice_max(unNormScore, n = nOrth2Norm)
  orthologAvgScore <- group_by(data10, smaller_tax_id, bigger_tax_id) %>% summarise(avg_score = mean(unNormScore))
  rm(data10)
  orthologTable <- inner_join(orthologTemp, orthologAvgScore, by = c("smaller_tax_id"="smaller_tax_id","bigger_tax_id"="bigger_tax_id"))
  rm(orthologTemp)
}else if(identical(cType,"lmp")){
  pairwiseLM <- group_by(orthologTemp[orthologTemp$plen<2000,],smaller_tax_id, bigger_tax_id) %>% do(lmp = lm(unNormScore ~ plen, data = .)) %>% mutate(intercept = lmp$coeff[1], slope = lmp$coeff[2])
  setDT(pairwiseLM)[,c("lmp"):=NULL]
  orthologTable <- left_join(orthologTemp, pairwiseLM, by = c("smaller_tax_id", "bigger_tax_id"))
  rm(orthologTemp)
  #Reset the normalization score in cases where it is predicted to be too low
  setDT(orthologTable)[plen < 100 , plen := 100]
  setDT(orthologTable)[,avg_score := intercept + (slope*plen)]
  setDT(orthologTable)[avg_score < 30 , avg_score := 30]
}
orthologTable$normalized_score <- format(round(orthologTable$unNormScore/orthologTable$avg_score, 5), nsmall = 5)
setDT(orthologTable)[,c("intercept","slope","unNormScore","plen","avg_score"):=NULL]
# ---- add a post-normalization score based filter here
#1057522 of 1057407
gc(full=T)

if(!opt$orthologs_only){
  message ("Preparing InParalog tables ...")
  bestInterTaxonScore <- group_by(bestQueryTaxonScore, pid) %>% summarise(score = max(score))
  rm(bestQueryTaxonScore)
  data11 <- SimilarSequences[SimilarSequences$taxQ == SimilarSequences$taxS,c("pid","sid","taxQ","score","al")]
  BetterHit2 <-  anti_join(data11, bestInterTaxonScore, by = c("pid"="pid")) #inP without outP
  data12 <- inner_join(data11,bestInterTaxonScore, by = c("pid"="pid"))
  rm(data11)
  BetterHit1 <- data12[data12$score.x>=data12$score.y, c("pid","sid","taxQ","score.x","al")]
  names(BetterHit1) <- names(BetterHit2)
  BetterHit <- rbind(BetterHit1,BetterHit2)
  rm(data12, BetterHit1, BetterHit2, bestInterTaxonScore)
  gc(full=T)
  BetterHitP <- left_join(BetterHit, dataLen, by = "pid")
  RBHin <- inner_join(BetterHitP,BetterHitP, by = c("pid"="sid","sid"="pid"))
  setDT(RBHin)[,c("taxQ.y"):=NULL]
  message ("Applying the chosen aligned length filter Parameters: ", opt$aln_length_filter)
  if(fType %in% c("log","lin")){
	# keep hits that have the larger or smaller protein in the pid
	if(opt$filter_by_smaller){
  	data4 <- RBHin[RBHin$plen.x < RBHin$plen.y | (RBHin$plen.x == RBHin$plen.y & RBHin$pid < RBHin$sid),]
	} else {
  	data4 <- RBHin[RBHin$plen.x > RBHin$plen.y | (RBHin$plen.x == RBHin$plen.y & RBHin$pid < RBHin$sid),]
	}
	rm(RBHin)
	setDT(data4)[plen.x == plen.y, al.x := (al.x + al.y)/2]
	data4[,c("al.y"):=NULL]
	data5 <- left_join(data4, data3, by = "pid")
	RBHfin <- data5 %>% filter(al.x>=cutoff)
	rm(data4, data5)
	setDT(RBHfin)[,c("cutoff","al.x"):=NULL]

  } else if(fType %in% c("est","lmp")){
	if(identical(fType,"lmp")){
  	argsFin = argsF[1] + 0.15
  	if(argsFin>0.9){argsFin<-0.9}
	} else {
  	argsFin = 0.9 - argsF[1]
	}
	message ("Inparalogous hits will be filtered using aligned lengths modelled as function of protein lengths. Parameters: ", argsFin)
	SPwiseLMAl <- group_by(RBHin[RBHin$plen.x<2000,],taxQ.x) %>% do(lmp = lm(al.x ~ plen.x, data = .)) %>% mutate(intercept = lmp$coeff[1], slope = lmp$coeff[2])
	setDT(SPwiseLMAl)[,c("lmp"):=NULL]
	SPwiseLMAl$intercept <- SPwiseLMAl$intercept * argsFin
	SPwiseLMAl$slope <- SPwiseLMAl$slope * argsFin
	if(opt$filter_by_smaller){
  	data3 <- RBHin[RBHin$plen.x < RBHin$plen.y | (RBHin$plen.x == RBHin$plen.y & RBHin$pid < RBHin$sid),]
	} else {
  	data3 <- RBHin[RBHin$plen.x > RBHin$plen.y | (RBHin$plen.x == RBHin$plen.y & RBHin$pid < RBHin$sid),]
	}
	rm(RBHin)
	setDT(data3)[plen.x == plen.y, al.x := (al.x + al.y)/2]
	data3[,c("al.y"):=NULL]
	data4 <- inner_join(data3, SPwiseLMAl, by = c("taxQ.x"))
	rm(data3)
	setDT(data4)[,cutoff := intercept + (slope * plen.x)]
	#In cases where predicted aligned is too high;reset cutoff to (argsF[1] * plen)
	data4[cutoff > (argsFin*plen.x), cutoff := (argsFin*plen.x)]
	RBHfin <- data4 %>% filter(al.x>=cutoff)
	rm(data4)
  } else {
	RBHfin <- RBHin[RBHin$pid < RBHin$sid,]
	rm(RBHin)
  }
  #Finished applying aligned length filter
 
  RBHfin$unNormScore <- (RBHfin$score.x+RBHfin$score.y)/2
  RBHfin$plen <- (RBHfin$plen.x+RBHfin$plen.y)/2
  inParalogTemp <- RBHfin[,c("pid","sid","taxQ.x","unNormScore","plen")]
  names(inParalogTemp) <- c("sequence_id_a","sequence_id_b","taxon_id","unNormScore","plen")
  rm(BetterHit, BetterHitP, RBHfin, SPwiseLMAl)
  # --------------- Calculate nOrth2Norm as median of total inter species hits
  #normalizing the scores
  if(identical(cType,"orm")){
	if(argsC[1] < 0 | argsC[1] > 1){
  	stop("When selecting 'orm' in --normalize_by option, the MAXPSP parameter must be set between 0 and 1", call.=FALSE)
	}
	totalIntraSpHits1 <- group_by(inParalogTemp, taxon_id) %>% tally()
	nPara2Norm1 <- as.integer(quantile(totalIntraSpHits1$n,probs=argsC[1]))
	# data14b <- group_by(inParalogTemp, taxon_id) %>% top_n(nPara2Norm1, wt = unNormScore)
	data14b <- group_by(inParalogTemp, taxon_id) %>% slice_max(unNormScore, n = nPara2Norm1)
	inParalogTaxonAvg <- group_by(data14b, taxon_id) %>% summarise(avg_score = mean(unNormScore))
	rm(data14b)
	OrthologUniqueId <- unique(c(orthologTable$sequence_id_a, orthologTable$sequence_id_b))
	data16 <- inParalogTemp[inParalogTemp$sequence_id_a %in% OrthologUniqueId | inParalogTemp$sequence_id_b %in% OrthologUniqueId,]
	totalIntraSpHits2 <- group_by(data16, taxon_id) %>% tally()
	nPara2Norm2 <- as.integer(quantile(totalIntraSpHits2$n,probs=argsC[1]))
	# data16b <- group_by(data16, taxon_id) %>% top_n(nPara2Norm2, wt=unNormScore)
	data16b <- group_by(data16, taxon_id) %>% slice_max(unNormScore, n = nPara2Norm2)
	inPlgOrthTaxonAvg <- group_by(data16b, taxon_id) %>% summarise(avg_score = mean(unNormScore))
	rm(data16)
	rm(OrthologUniqueId)
	data17 <- left_join(inParalogTaxonAvg, inPlgOrthTaxonAvg, by=c("taxon_id"="taxon_id"))
	rm(data16b, inPlgOrthTaxonAvg, inParalogTaxonAvg)
	inParalogAvgScore <- data17 %>% mutate_at(vars(avg_score.y), ~ if_else(is.na(.), avg_score.x, .))
	rm(data17)
	inParalogTable <- inner_join(inParalogTemp,inParalogAvgScore, by = c("taxon_id"="taxon_id"))
	rm(inParalogAvgScore, inParalogTemp)
  }else if(identical(cType,"lmp")){
	message ("Inparalogs are being normalized using a combined linear model for all the species.", call.=FALSE)
	lmp = lm(unNormScore ~ plen, data = inParalogTemp[inParalogTemp$plen<2000,])
	inParalogTable <- inParalogTemp
	rm(inParalogTemp)
	setDT(inParalogTable)[plen < 100 , plen := 100]
	setDT(inParalogTable)[,avg_score.y :=  lmp$coeff[1] + (lmp$coeff[2]*plen)]
	setDT(inParalogTable)[avg_score.y < 30 , avg_score.y := 30]
	rm(lmp)
  }
   inParalogTable$normalized_score <- format(round(inParalogTable$unNormScore/inParalogTable$avg_score.y, 5), nsmall = 5)
   setDT(inParalogTable)[,c("unNormScore","plen","avg_score.y"):=NULL]
  #38412 of 38398
  gc(full=T)
 
  message ("Preparing CoOrtholog tables ...")
  data18 <- inParalogTable[,c("sequence_id_a","sequence_id_b")]
  names(data18) <- c("sequence_id_b","sequence_id_a")
  inParalog2Way <- bind_rows(inParalogTable[,c("sequence_id_a","sequence_id_b")], data18)
  data19 <- orthologTable[,c("sequence_id_a","sequence_id_b")]
  names(data19) <- c("sequence_id_b","sequence_id_a")
  ortholog2Way <- bind_rows(orthologTable[,c("sequence_id_a","sequence_id_b")], data19)
  rm(data18, data19)
  data20 <- inner_join(inParalog2Way, ortholog2Way, by=c("sequence_id_b"="sequence_id_a"))[,c("sequence_id_a","sequence_id_b.y")]
  inPlgOrthoInPlg <- inner_join(data20, inParalog2Way, by=c("sequence_id_b.y"="sequence_id_a"))[,c("sequence_id_a","sequence_id_b")]
  rm(data20)
  inParalogOrtholog <- inner_join(inParalog2Way, ortholog2Way, by=c("sequence_id_b"="sequence_id_a"))[,c("sequence_id_a","sequence_id_b.y")]
  names(inParalogOrtholog) <- c("sequence_id_a","sequence_id_b")
  data21 <- bind_rows(inPlgOrthoInPlg,inParalogOrtholog)
  rm(ortholog2Way, inParalog2Way, inParalogOrtholog, inPlgOrthoInPlg)
  data21$smaller_seq_id <- pmin(data21$sequence_id_a,data21$sequence_id_b)
  data21$bigger_seq_id <- pmax(data21$sequence_id_a,data21$sequence_id_b)
  coOrthologCandidate <- unique(data21[,c("smaller_seq_id", "bigger_seq_id")])
  names(coOrthologCandidate) <- c("sequence_id_a", "sequence_id_b")
  rm(data21)
  coOrthNotOrtholog <- anti_join(coOrthologCandidate, orthologTable, by=c("sequence_id_a"="sequence_id_a","sequence_id_b"="sequence_id_b"))[,c("sequence_id_a","sequence_id_b")]
  #these two inner_joins are the crux of memory usage
  data22 <- inner_join(SimilarSequences, coOrthNotOrtholog, by=c("pid"="sequence_id_a", "sid"="sequence_id_b"))
  data23 <- inner_join(SimilarSequences, data22, by=c("pid"="sid", "sid"="pid"))
  data23P <- left_join(data23, dataLen, by = "pid")
  RBHco <- left_join(data23P, dataLen, by = c("sid"="pid"))
  rm(data22, data23P, data23, coOrthNotOrtholog, coOrthologCandidate)
  setDT(RBHco)[,c("taxQ.y","taxS.y"):=NULL]
  gc(full=T)
 
  message ("Applying the chosen aligned length filter Parameters: ", opt$aln_length_filter)
  if(fType %in% c("log","lin")){
	# keep hits that have the larger or smaller protein in the pid
	if(opt$filter_by_smaller){
  	data4 <- RBHco[RBHco$plen.x < RBHco$plen.y | (RBHco$plen.x == RBHco$plen.y & RBHco$pid < RBHco$sid),]
	} else {
  	data4 <- RBHco[RBHco$plen.x > RBHco$plen.y | (RBHco$plen.x == RBHco$plen.y & RBHco$pid < RBHco$sid),]
	}
	rm(RBHco)
	setDT(data4)[plen.x == plen.y, al.x := (al.x + al.y)/2]
	data4[,c("al.y"):=NULL]
	data5 <- left_join(data4, data3, by = "pid")
	coOrthologTemp <- data5 %>% filter(al.x>=cutoff)
	rm(data4, data5)
	setDT(coOrthologTemp)[,c("cutoff","al.x"):=NULL]
	coOrthologTemp$smaller_tax_id <- pmin(coOrthologTemp$taxQ.x,coOrthologTemp$taxS.x)
	coOrthologTemp$bigger_tax_id <- pmax(coOrthologTemp$taxQ.x,coOrthologTemp$taxS.x)
    
  } else if(fType %in% c("est","lmp")){
	message ("Homology hits will be filtered using aligned lengths modelled as function of protein lengths. Parameters: ", opt$aln_length_filter)
	if(identical(fType,"est")){
  	argsFco = 1 - argsF[2]
	} else {
  	argsFco = argsF[1]
	}
	RBHco$smaller_tax_id <- pmin(RBHco$taxQ.x,RBHco$taxS.x)
	RBHco$bigger_tax_id <- pmax(RBHco$taxQ.x,RBHco$taxS.x)                                                                                            	 
	if(opt$filter_by_smaller){
  	data3 <- RBHco[RBHco$plen.x < RBHco$plen.y | (RBHco$plen.x == RBHco$plen.y & RBHco$pid < RBHco$sid),]
	} else {
  	data3 <- RBHco[RBHco$plen.x > RBHco$plen.y | (RBHco$plen.x == RBHco$plen.y & RBHco$pid < RBHco$sid),]
	}
	rm(RBHco)
	setDT(data3)[plen.x == plen.y, al.x := (al.x + al.y)/2]
	data3[,c("al.y"):=NULL]
	data4 <- inner_join(data3, pairwiseLMAl, by = c("smaller_tax_id", "bigger_tax_id"))
	rm(data3)
	setDT(data4)[,cutoff := intercept + (slope * plen.x)]
	#In cases where predicted aligned is too high;reset cutoff to (argsF[1] * plen)
	data4[cutoff > (argsFco*plen.x), cutoff := (argsFco*plen.x)]
	#Detect proteins with non-conserved regions and use est. cutoffs; est. alR < 0.5 for both homologs
	coOrthologTemp <- data4 %>% filter(al.x>=cutoff)
	rm(data4)
  } else {
	RBHfco <- RBHco[RBHco$pid < RBHco$sid,]
	RBHfco$smaller_tax_id <- pmin(RBHfco$taxQ.x,RBHfco$taxS.x)
	RBHfco$bigger_tax_id <- pmax(RBHfco$taxQ.x,RBHfco$taxS.x)                                                                                            	 
	rm(RBHco)
  }
  #Finished applying aligned length filter
 
  RBHfco$unNormScore <- (RBHfco$score.x + RBHfco$score.y)/2
  RBHfco$plen <- (RBHfco$plen.x + RBHfco$plen.y)/2
  coOrthologTemp <- RBHfco[,c("pid","sid","taxQ.x","taxS.x","smaller_tax_id", "bigger_tax_id","unNormScore","plen")]
  names(coOrthologTemp) <- c("sequence_id_a","sequence_id_b","taxon_id_a","taxon_id_b","smaller_tax_id","bigger_tax_id","unNormScore","plen")
  gc(full=T)
 
  #normalizing the scores
  if(identical(cType,"orm")){
	if(argsC[1] < 0 | argsC[1] > 1){
  	stop("When selecting 'orm' in --normalize_by option, the MAXPSP parameter must be set between 0 and 1", call.=FALSE)
	}
	if(opt$coOrthologs=="orthoMCLv2"){
  	totalInterSpHitsCo <- group_by(coOrthologTemp, smaller_tax_id, bigger_tax_id) %>% tally()
  	nCoOrtho2Norm <- as.integer(quantile(totalInterSpHitsCo$n,probs=argsC[1]))
  	data22b <- group_by(coOrthologTemp, smaller_tax_id, bigger_tax_id) %>% slice_max(unNormScore, n = nCoOrtho2Norm)
  	coOrthologAvgScore <- group_by(data22b, smaller_tax_id, bigger_tax_id) %>% summarise(avg_score = mean(unNormScore))
  	coOrthologTable <- inner_join(coOrthologTemp,coOrthologAvgScore, by = c("smaller_tax_id"="smaller_tax_id","bigger_tax_id"="bigger_tax_id"))
	} else {
  	coOrthologTable <- inner_join(coOrthologTemp,orthologAvgScore, by = c("smaller_tax_id"="smaller_tax_id","bigger_tax_id"="bigger_tax_id"))
	}
  } else if(identical(cType,"lmp")){
	if(opt$coOrthologs=="orthoMCLv2"){
  	message("orthoMCLv2 like normalization of coOrtholog scores is not possible with lmp score normalization")  
	}
	coOrthologTable <- left_join(coOrthologTemp, pairwiseLM, by = c("smaller_tax_id", "bigger_tax_id"))
	#Reset the normalization score in cases where it is predicted to be too low
	setDT(coOrthologTable)[plen < 100 , plen := 100]
	setDT(coOrthologTable)[,avg_score := intercept + (slope*plen)]
	setDT(coOrthologTable)[avg_score < 30 , avg_score := 30]
	setDT(coOrthologTable)[,c("intercept","slope"):=NULL]
  }
  rm(coOrthologTemp)
 
  coOrthologTable$normalized_score <- format(round(coOrthologTable$unNormScore/(coOrthologTable$avg_score), 5), nsmall = 5)
  setDT(coOrthologTable)[,c("unNormScore","plen","avg_score"):=NULL]
  #74526 of 74479
 
  message("Creating the mcl input file ...")
  orthologTableFile <- arrange(orthologTable, taxon_id_a,taxon_id_b,sequence_id_a,sequence_id_b)[,c("sequence_id_a","sequence_id_b","normalized_score")]
  rm(orthologTable)
  fwrite(orthologTableFile, file = 'orthologs.tab', row.names = F, col.names = F, quote = F, sep = '\t')
  inParalogTableFile <- arrange(inParalogTable, taxon_id,sequence_id_a,sequence_id_b)[,c("sequence_id_a","sequence_id_b","normalized_score")]
  rm(inParalogTable)
  fwrite(inParalogTableFile, file = 'inparalogs.tab', row.names = F, col.names = F, quote = F, sep = '\t')
  coOrthologTableFile <- arrange(coOrthologTable, taxon_id_a,taxon_id_b,sequence_id_a,sequence_id_b)[,c("sequence_id_a","sequence_id_b","normalized_score")]
  rm(coOrthologTable)
  fwrite(coOrthologTableFile, file = 'coorthologs.tab', row.names = F, col.names = F, quote = F, sep = '\t')
 
  mclInput <- bind_rows(inParalogTableFile, orthologTableFile, coOrthologTableFile)
  rm(inParalogTableFile, orthologTableFile, coOrthologTableFile)
} else {
  mclInput <- arrange(orthologTable, taxon_id_a,taxon_id_b,sequence_id_a,sequence_id_b)[,c("sequence_id_a","sequence_id_b","normalized_score")]
  rm(orthologTable)
}


#quantile(mclInput$normalized_score,probs=c(0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95))
#if scores>5 rescale values greater than 2/3 between 2/3-5
fwrite(mclInput, file = 'mclInput.tab', row.names = F, col.names = F, quote = F, sep = '\t')


s=gc(full=T)
#-I 5.0 gives fine-grained clusterings, and -I 1.2 gives very coarse grained
message("Running mcl clustering algorithm")
argsI <- as.numeric(unlist(strsplit(opt$mclInflation, ",")))
for(i in argsI){
  system(paste("mcl mclInput.tab -te ",getDTthreads()," --abc -I ", i, " -o ", opt$output_file, "_", opt$homology_score,"_", i, sep = ""))
}
message("Homology file size: ", round(file.size(opt$homology_file)/1048576,2), " MB")
message("Total memory(RAM + swap): ", round(ram/1024,1), " MB")
message("Peak memory usage: ",s[1,6]+s[2,6]," MB")
message("Total run time: ",(proc.time() - ptm)[3]," seconds")
