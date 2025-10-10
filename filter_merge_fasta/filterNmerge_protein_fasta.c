/*
 ====================================================================================
 Name        : ortho_preBlast.c
 Author      : Mayank Mahajan
 Version     : 0.1
 Created on  : 18 Dec 2020
 Copyright   : Provided by Molecular Evolution lab, ICM, Uppsala University
               under GNU's GPL 3
 Description : Process multiple species/organism specific protein fasta files and
               create a single file with species/organism IDs in the fasta headers.
 ====================================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>

void helpText(void){
    printf("Takes multiple the protein fasta files (one for each genome or \n"
  		  "pan-genome) as input and creates a merged protein fasta file with\n"
    	  "the following protein header format: '>speciesID|proteinID' \n\n"
  		  "usage: preHomologySearch -i <directory> -f <list_file> -l <int> -x <int>\n\n"
          "example: preHomologySearch -i /faaFiles -f genomes.list -l 10 -x 20\n\n"
  		  "-i <directory> Directory with all the proteins fasta files \n"
          "               (default: '.' or current directory)\n "
  		  "-f <list_file> A list of protein fasta files with 4 tab separated \n "
    	  "               columns for each file:								\n"
    	  "               Species_ID\tFile_Name\tPosition_of_seq_ID\t(metadata)"
  		  "               The fasta headers in genbank/refseq like format can have \n"
    	  "               multiple unique protein identifiers, eg. \n"
    	  "               >gi|89106888|ref|AP_000668.1| has two at positions 2 and\n "
      	  "	              4, which must be specified in Position_of_seq_ID column.\n "
      	  "	              If 5 or higher position is specified for above example, \n "
          "	              all the strings in-between the '|' will be concatenated \n "
          "	              and used as the sequence ID. The species ID should be \n "
    	  "	 			  alphanumeric, and preferably short (max 8 characters).\n"
  		  "               (default: A list_file named 'genome_list.tab' is created\n"
  		  "               with auto-generated species IDs, and the position of the\n "
  		  "               sequence IDs for each fasta. If a fasta file has '|' in \n"
  		  "				  its fasta headers, then each string in-between the '|'\n"
  		  "               (from left to right), and then the concatenated string\n"
  		  "               is tested as a unique sequence identifier) \n"
    	  "-l <integer>   Minimum required sequence length (default: 10)\n "
    	  "-x <integer>   Maximum percentage of stop codons in the proteins \n "
    	  "				  (default: 20)\n "
  		  "-h             Show help.\n\n "
    	  "It is suggested to first run this function in the fasta folder without the\n"
          "'-f' argument. Then the auto-generated list_file, 'genome_list.tab', can \n"
    	  "be used to customise the species IDs and position of sequence IDs, and to \n"
    	  "rerun the function if needed.\n\n");
}


//global variables
char comStr[256], mpPath[512], mpsPath[512], rpPath[512];
FILE *mp, *rp, *mps;
int minLen=10, maxStp=20;

const char *getFileExt(const char *filename) {
    const char *dot = strrchr(filename, '.');
    if(!dot || dot == filename) return "";
    return dot + 1;
}

//read a fasta file, check duplicate IDs, check sequence length, check stop codon% and print sequences to merged.fasta
int copyFasta(char *seqPath, char *spID, int pidPos){
//	printf("copyFasta start\n");
	FILE *fseq;
	char buf[1024], outbuf[64], *pid, pidLong[64], currSeq[100000];
	char delim[]=">| \t\r\n\v\f";
	int seqArrC, seqArrLen=1000, newseq=0, retVal=0;

	char **seqArrId = malloc(seqArrLen * sizeof(char *));
	char **seqArr = malloc(seqArrLen * sizeof(char *));
//	printf("Initialised variables\n");

	if(NULL == (fseq = fopen(seqPath,"r"))){
		fprintf(stderr, "Error : %s file could not be opened inside readFasta()! Error: %d (%s)\n", seqPath, errno, strerror(errno));
		return 1;
	}
//	printf("outside while scanf2 \n");

	seqArrC=-1;

	while(fscanf(fseq,"%s%*[^\n]\n",buf) == 1){
		if (buf[0]=='>'){
			if(seqArrC>=0){
				seqArr[seqArrC] = (char *)malloc(strlen(currSeq)+1);
				strcpy(seqArr[seqArrC], currSeq);
			}
			seqArrC++;
			if(seqArrC>=seqArrLen){
				seqArrLen*=10;
				seqArrId = realloc(seqArrId, seqArrLen * sizeof(char *));
				seqArr = realloc(seqArr, seqArrLen * sizeof(char *));
			}
			pid = strtok(buf, delim);
			strcpy(pidLong,pid);
//			printf("first call pid:%s\n",pid);
			for(int j=1;j<pidPos;j++){
				pid = strtok(NULL, delim);
				if(pid != NULL)
					if(strlen(pid) > 2){
						strcat(pidLong,"_");
						strcat(pidLong,pid);
					}
//				printf("inside for pid:%s\n",pid);
			}
//			printf("between forNwhile pid:%s pidPos:%d\n",pid, pidPos);
			if(pid == NULL){
				pid = pidLong;
			}
			for(int i=0;i<seqArrC;i++){
				if(strcmp(seqArrId[i],pid)==0)
				{
					fprintf(stderr,"Duplicate PID: %s @ pos. %d\n",pid, pidPos);
					sprintf(buf,"; Duplicate PID: %s @ pos. %d",pid, pidPos);
					strcat(comStr,buf);
					retVal=1;
					goto skip;
				}
			}
			seqArrId[seqArrC] = (char *)malloc(strlen(pid)+1);
			strcpy(seqArrId[seqArrC], pid);
			newseq=1;
		}else{
			if(newseq==1){
				strcpy(currSeq, buf);
				newseq=0;
			}else{
				strcat(currSeq, buf);
			}
		}
	}
	//strcat the last currSeq into last seq
	seqArr[seqArrC] = (char *)malloc(strlen(currSeq)+1);
	strcpy(seqArr[seqArrC], currSeq);
	seqArrC++;

	//write all seqs into merged protein fasta files START
	if(NULL == (mp = fopen(mpPath,"a"))){
		fprintf(stderr, "Unable to open %s!! Error: %d (%s)\n", mpPath, errno, strerror(errno));
		retVal=1;
		goto skip;
	}
	if(NULL == (mps = fopen(mpsPath,"a"))){
		fprintf(stderr, "Unable to open %s!! Error: %d (%s)\n", mpPath, errno, strerror(errno));
		retVal=1;
		goto skip;
	}
	if(NULL == (rp = fopen(rpPath,"a"))){
		fprintf(stderr, "Unable to open %s! Error: %d (%s)\n", rpPath, errno, strerror(errno));
		retVal=1;
		goto skip;
	}
//	printf("before writing\n");
	int stpCount, seqLen, rpCount = 0;
	char *seqT;
//	Print sequences to merged_protein.fasta and removed protein.fasta  START
	for(int i=0;i<seqArrC;i++){
		seqT = seqArr[i];
		seqLen = strlen(seqArr[i]);
//		printf("before checking length and stop codon\n");
		for (stpCount=0; seqT[stpCount]; seqT[stpCount]=='*' ? stpCount++ : *seqT++);
		//Check sequence length and percentage of stop codons.
		if((seqLen>=minLen)&&((stpCount*100)/seqLen<=maxStp)){
			fprintf(mps,"%s|%s\t%d\n",spID,seqArrId[i],seqLen);
			fprintf(mp,">%s|%s\n",spID,seqArrId[i]);
			seqT = seqArr[i];
			while (strlen(seqT)>60){
				snprintf(outbuf,60, "%s",seqT);
				outbuf[60] = 0;
				seqT+=60;
				fprintf(mp,"%s\n",outbuf);
			}
			fprintf(mp,"%s\n",seqT);
		} else {
			rpCount++;
		  	fprintf(rp,">%s|%s\n",spID,seqArrId[i]);
		  	seqT = seqArr[i];
		  	while (strlen(seqT)>60){
		  		snprintf(outbuf,60, "%s",seqT);
		  		outbuf[60] = 0;
				seqT+=60;
				fprintf(rp,"%s\n",outbuf);
		  	}
		  	fprintf(rp,"%s\n",seqT);
		}
	}
	//	Print sequences to merged_protein.fasta and removed protein.fasta  END
	fprintf(stderr,"%d proteins were removed.\n",rpCount);
	sprintf(buf,"; proteins removed: %d",rpCount);
	strcat(comStr,buf);

	//free memory
skip:for(int i=0;i<seqArrC;i++){
		free(seqArrId[i]);
		free(seqArr[i]);
	}
	free(seqArrId);
	free(seqArr);
	fclose(mp);
	fclose(mps);
	fclose(rp);
	fclose(fseq);

	return retVal;
//	return 1;
}

int main(int argc, char **argv)
{
	int opt;
	char *dirName , *listFile, path[512];
	while((opt = getopt(argc, argv, "i:x:l:hf:")) != -1)
	{
	  switch(opt)
	  {
		  case 'i':
			  dirName=malloc(strlen(optarg));
			  strcpy(dirName,optarg);
			  break;
		  case 'l':
			  minLen = atoi(optarg);
			  break;
		  case 'x':
			  maxStp = atoi(optarg);
			  break;
		  case 'f':
			  listFile=malloc(strlen(optarg));
			  strcpy(listFile,optarg);
			  break;
		  case ':':
			  printf("option -%c needs a value\n", optopt);
			  helpText();
			  break;
		  case 'h':
			  helpText();
			  break;
		  case '?':
			  printf("option -%c not found\n", optopt);
			  helpText();
			  break;
	  }
	}

	// fasta directory path
	if(dirName==NULL){
		if (getcwd(path, sizeof(path)) == NULL){
		      fprintf(stderr,"getcwd() error!\n");
		      return 1;
		}
	} else {
		 if(realpath(dirName, path) == NULL){
		      fprintf(stderr,"absolute path error in %s !\n", dirName);
		      return 1;
		 }
	}
    strcat(path, "/" );
	fprintf(stderr,"Location of fasta files: %s\n", path);
	//define variables
	DIR* FD;
	struct dirent* fFile;
    FILE *fmap, *f;
    char spID[8], buf[1024], fPath[512], fmapPath[512], *pid;
    char delim[]=">| \t\r\n\v\f";
//	strcpy(mpPath,path);
	strcpy(mpPath,"all_proteins.fasta");
//	strcpy(rpPath,path);
	strcpy(rpPath,"removed_proteins.fasta");
	strcpy(mpsPath,"all_proteins.sizes");

	if(NULL == (mp = fopen(mpPath,"w"))){
		fprintf(stderr, "Error: Unable to write %s!! Error: %d (%s)\n", mpPath, errno, strerror(errno));
		return 1;
	}
	if(NULL == (mps = fopen(mpsPath,"w"))){
		fprintf(stderr, "Error: Unable to write %s!! Error: %d (%s)\n", mpsPath, errno, strerror(errno));
		return 1;
	}
	if(NULL == (rp = fopen(rpPath,"w"))){
		fprintf(stderr, "Error: Unable to write %s! Error: %d (%s)\n", rpPath, errno, strerror(errno));
		return 1;
	}
	fclose(mp);
	fclose(mps);
	fclose(rp);


//	if no listFile; guess protein IDs and auto-generate species IDs
	if(listFile==NULL)
	{
        int pidPos, oC = 0; // speciesID generator
    	if (NULL == (FD = opendir (path)))
    	{
    		fprintf(stderr, "Unable to open the directory %s! Error: %d (%s)\n", path, errno, strerror(errno));
    		return 1;
    	} //else {
//    		fprintf(stderr, "Processing the fasta/faa files in %s ", path);
//    	}

    	//	strcpy(fmapPath,path);
    		strcpy(fmapPath,"genome_list.tab");
    		fmap = fopen(fmapPath,"w");// create listFile
    		if (fmap == NULL){
    			fprintf(stderr, "Unable to write listFile! Error: %d (%s)\n", errno, strerror(errno));
    			return 1;
    		}

    	while ((fFile = readdir(FD)) != NULL){
     		// Read only .fasta and .faa files
    		if ((strcmp(getFileExt(fFile->d_name), "fasta")!=0) && (strcmp(getFileExt(fFile->d_name), "faa"))!=0)
    			continue;
    		if ((strcmp(fFile->d_name, "all_proteins.fasta")==0) || (strcmp(fFile->d_name, "removed_proteins.fasta")==0))
    			continue;
    		int idCount=0;
    		oC++;
    		pidPos = 0;
			//open the fasta file
    		strcpy(fPath,path);
    		strcat(fPath, fFile->d_name);
    		//	  decide the position of ProteinID in the header  --- START
loopPID:	if(NULL == (f = fopen(fPath,"r"))){
				fprintf(stderr, "Unable to open %s! Error: %d (%s)\n",path, errno, strerror(errno));
				continue;
			} else {
				fprintf(stderr, "Processing file %d: %s \n",oC, fFile->d_name);
			}
			fscanf(f,"%s%*[^\n]\n",buf);
			strcpy(comStr, "fasta header: ");
			strcat(comStr, buf);
			pidPos++;
			pid = strtok(buf, delim);
	//		printf("first call pid:%s\n",pid);
			for(int j=1;j<pidPos;j++){
				pid = strtok(NULL, delim);
//				printf("inside for pid:%s\n",pid);
			}
//			printf("between forNwhile pid:%s pidPos:%d\n",pid, pidPos);
			while(pid != NULL){
				if(strlen(pid) > 2){
					break;
				}
				pid = strtok(NULL, delim);
				pidPos++;
//				printf("inside while pid:%s\n",pid);
			}
			fclose(f);
			snprintf(spID,8,"n%04d",oC);
			if(pid == NULL){
				if(idCount>1){
//					printf("inside try merged pidPos:%d\n",pidPos);
					if(copyFasta(fPath, spID, pidPos)==0){
						fprintf(fmap,"n%04d\t%s\t%d\t%s\n",oC,fFile->d_name, pidPos, comStr);
						continue;
					}
				}
			} else {
//				printf("final pid for copyFasta:%s pidPos:%d \t %d \t %s \n",pid,pidPos, oC, fPath);
				if(copyFasta(fPath, spID, pidPos)==0){
					//write fmap
					fprintf(fmap,"n%04d\t%s\t%d\t%s\n",oC,fFile->d_name,pidPos, comStr);
					continue;
				}else{
					idCount++;
					goto loopPID;
				}
			}
			fprintf(stderr, "Error: Failed to guess the unique Protein IDs in %s! \n",fFile->d_name);
			//write fmap
			fprintf(fmap,"n%04d\t%s\tFailed\t%s\n",oC,fFile->d_name,comStr);
            }
		fclose(fmap);
//  use speciesIDs and ProteinIDs provided in the listFile;
	} else {
		if(realpath(listFile, fmapPath) == NULL){
		      fprintf(stderr,"absolute path error in %s\n", listFile);
		      return 1;
		}
		fprintf(stderr,"Location of listFile: %s\n", fmapPath);
		fmap = fopen(fmapPath,"r");// create fasta and organismID map file
		if (fmap == NULL){
		  fprintf(stderr, "Unable to open listFile %s.\n", listFile);
		  return 1;
		}
		int pidPos;
	    char fName[128], *buftok;
	    int spCount = 0;
	    strcpy(comStr, "reset");
//	    fprintf(stderr, "Outside while! \n");
		while(fgets(buf, 1024, fmap) != NULL){
			spCount++;
//		    fprintf(stderr, "Reading fmap row %d \n",spCount);
			buftok = strtok(buf,"\t");
			if(buftok==NULL){
				fprintf(stderr, "Error reading species ID in line %d of listFile %s.\n", spCount, listFile);
				return 1;
			}
			strcpy(spID,buftok);
			buftok = strtok(NULL,"\t");
			if(buftok==NULL){
				fprintf(stderr, "Error reading filename in line %d of listFile %s.\n", spCount, listFile);
				return 1;
			}
			strcpy(fName,buftok);
			buftok = strtok(NULL,"\t");
			if(buftok==NULL){
				fprintf(stderr, "Error reading position of seq ID in line %d of listFile %s.\n", spCount, listFile);
				return 1;
			}
			pidPos = atoi(buftok);
			if(pidPos<1){
				fprintf(stderr, "Position of seq ID cannot be less than 1 in line %d of listFile %s.\n", spCount, listFile);
				return 1;
			}
/*   	while(fscanf(fmap,"%s\t%s\t%s%*[^\n]\n", spID, fName, buf) == 1){
    	    fprintf(stderr, "Outside while  %s! \n",fName);
	    	fscanf(fmap,"");
	    	pidP = atoi(buf); */
				strcpy(fPath, path);
				strcat(fPath, fName);
				fprintf(stderr, "Processing file %s, sp. ID: %s, pos. seqID: %d \n",fName, spID, pidPos);
				if(copyFasta(fPath, spID, pidPos)!=0)
					fprintf(stderr, "Failed to process file %s! \n",fName);
//				else
//					fprintf(stderr, "Successfully processed file %s! \n",fName);
				//spCount++;
		}
		fclose(fmap);
	}
    return 0;
}
