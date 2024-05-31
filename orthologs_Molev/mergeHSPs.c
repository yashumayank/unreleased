/*
 ====================================================================================
 Name        : mergeHSPs.c
 Author      : Mayank Mahajan
 Version     : 0.2
 Created on  : 18 Dec 2020
 Copyright   : Provided by Molecular Evolution lab, ICM, Uppsala University
               under GNU's GPL 3
 Description : To merge all the blast hits or the high scoring pairs (HSPs) between two proteins
 ====================================================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>

//HSP array [100]
#define ID_SIZE 64
#define BUF_SIZE 512
#define MAX_HSP 2048

struct seqPair
{
    char s[ID_SIZE];
    char q[ID_SIZE];
    char eval[8];
	float id;
	int starts;
	int ends;
	int count;
} pair;

float maxOvl = 0.8;
unsigned int al_bs = 0;
FILE  *fOut;
int alArrC=0, alArrStart[MAX_HSP], alArrEnd[MAX_HSP];

struct alnSegs
{
	int startq;
	int endq;
	float bits;
	struct alnSegs *next;
} temp;

int getAlignedLength( void){
	int alRet = 0, insStart, insEnd, i, j;
// 	sort the arrays; Move elements that are greater than key, to one position ahead of their current position
    for (i = 1; i < alArrC; i++)
    {
    	insStart = alArrStart[i]; insEnd = alArrEnd[i];
    	j = i - 1;
    	while (j >= 0 && alArrStart[j] > insStart) {
    		alArrStart[j + 1] = alArrStart[j]; alArrEnd[j + 1] = alArrEnd[j];
            j = j - 1;
        }
        alArrStart[j + 1] = insStart;
        alArrEnd[j + 1] = insEnd;
    }

//		add length of the first ordered HSP
    alRet += alArrEnd[0] - alArrStart[0] + 1;
//		iterate over rest of alArrStart using ordered indices
	j=1;
    for (i = 1; i < alArrC; i++)
    {
		if(alArrStart[i] <= alArrEnd[i-j])
		{
			if(alArrEnd[i] <= alArrEnd[i-j])
				{j++; continue;}
			else{
//					add length of non overlapping part; reset prev.end
			    alRet += alArrEnd[i] - alArrEnd[i-j];
			    j=1;
			}
		}else{
//			add full al length; reset prev.start, prev.end
		    alRet += alArrEnd[i] - alArrStart[i] + 1;
		    j=1;
		}
    }

//	free(alArrStart); free(alArrEnd);
//	alArrStart = malloc(sizeof(int*) * MAX_HSP);
//	alArrEnd = malloc(sizeof(int*) * MAX_HSP);

	return alRet;
}

void helpText(void){
    printf("This mergeHSP tool takes blast results as input and merges all high scoring\n"
			"pairs (HSPs) between a pair of sequences, and outputs a single row of metrics\n"
			", such as score, identity, aligned length for each pair of sequences.\n\n"
			"usage: mergeHSPs -i <HSP_FILE>(REQUIRED) -o <OUT_FILE> -p <MAX_OVL> -b   \n\n"
			"example: mergeHSPs -i all_vs_all_blast.tab -p 60 \n\n"
			"-i <HSP_FILE>  Blast result file with homology search results in tabular\n"
			"               format (using -outfmt 6 in blast) REQUIRED FIELD\n\n"
			"-o <OUT_FILE>	Output file with merged aligned length and homology scores.\n"
			" 			    (default: STDOUT)\n\n"
			"-p <MAX_OVL>   Lower ranked HSPs with overlapped/total aligned length ratio\n"
			"               > MAX_OVL are ignored while recalculating Bitscore (overlaps\n"
			"				are measured only from higher ranked HSPs). The ratio can be\n"
    		"				set anywhere between 0 to 1.0 (default: 0.8)   \n\n"
			"-b    			Lower ranked HSPs with overlapped/total aligned length ratio\n"
			"               > MAX_OVL are ignored as described in option '-p' (default:\n"
			"               Sum of all the aligned bases in the query sequence)      \n\n"
			"-h             Show this help.\n\n");
}

int main(int argc, char **argv){
	time_t start, end;
	time(&start);

	int opt;
	char *HSP_file, *mergedHSP_file;
	mergedHSP_file = NULL;
	while((opt = getopt(argc, argv, "i:o:p:bh")) != -1)
	{
	  switch(opt)
	  {
		  case 'i':
			  HSP_file=malloc(strlen(optarg));
			  strcpy(HSP_file,optarg);
			  break;
		  case 'o':
			  mergedHSP_file=malloc(strlen(optarg));
			  strcpy(mergedHSP_file,optarg);
			  break;
		  case 'p':
			  maxOvl = atof(optarg);
			  break;
		  case ':':
			  printf("option -%c needs a value\n", optopt);
			  helpText();
			  break;
		  case 'b':
			  al_bs = 1;
			  break;
		  case 'h':
			  helpText();
			  return 0;
		  case '?':
			  printf("option -%c not found\n", optopt);
			  helpText();
			  return 0;
	  }
	}

	char iFilePath[512], *buftok, buf[BUF_SIZE], sT[ID_SIZE], qT[ID_SIZE];
//	alArrStart = malloc(sizeof(int*) * MAX_HSP);
//	alArrEnd = malloc(sizeof(int*) * MAX_HSP);
	int rCount=0;
	char fOut_buf[65536];
	FILE *f;
	int alq=0, alqFull = 0, endq, starts, ends, ovl, len;
	float idq=0, bitsq=0;
//	struct HSPs pair;

	if(realpath(HSP_file, iFilePath) == NULL){
	      fprintf(stderr,"Path error in HSP_file %s\n", HSP_file);
	      helpText();
	      return 1;
	}
	//fprintf(stderr,"Location of homology file: %s\n", filePath);
	if(NULL == (f = fopen(iFilePath,"r"))){
		fprintf(stderr, "Error: Unable to read %s!! Error: %d (%s)\n", iFilePath, errno, strerror(errno));
		return 1;
	}

	if(mergedHSP_file==NULL){
		fOut = stdout;
	}else{
//		if(realpath(mergedHSP_file, oFilePath) == NULL){
//		      fprintf(stderr,"absolute path error in %s\n", mergedHSP_file);
//		      return 1;
//		}
//		fprintf(stderr,"Location of homology file: %s\n", filePath);
		if(NULL == (fOut = fopen(mergedHSP_file,"w"))){
			fprintf(stderr, "Error: Unable to write %s!! Error: %d (%s)\n", mergedHSP_file, errno, strerror(errno));
			return 1;
		}
	}
	setvbuf(fOut, fOut_buf, _IOFBF, 65536);

	struct alnSegs *head, *iter, *prevIter, *temp2;
	head = malloc(sizeof(struct alnSegs));
	iter=head;
	int state;
	pair.count = 0;
	while(fgets(buf, BUF_SIZE, f)!=NULL){
//	while(c++<10 && fread(&pair2, sizeof(pair2), 1, f)){
//		fscanf(f,"%*[^\n]\n");
		rCount++;
//		fprintf(stderr, "Reading row %d \n",rCount);
		buftok = strtok(buf,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading query ID in row %d .\n", rCount);
			return 1;
		}
		strcpy(qT,buftok);
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading subject ID in row %d .\n", rCount);
			return 1;
		}
		strcpy(sT,buftok);

//		if(strcmp(pair.s,sT)!=0 ){
//			printConservedLen();
//		}
// 		count the number of hits per protein and mark protein with very large number of hits

		if(strcmp(pair.s,sT)!=0 || strcmp(pair.q,qT)!=0){
			if(pair.count >= 1){
				alq = head->endq - head->startq + 1;
//				idq = head->id * (float)alq;
				bitsq = head->bits;
				endq = head->endq;
				iter = head->next;
				head->next = NULL;
//				printf("Printing pair %s::%s\t#HSPs:%d\tbest-eval:%s\n",pair.s, pair.q, pair.count, pair.eval);
				while(iter != NULL) {
//					printf("Adding .. \n");
					alq += iter->endq - iter->startq + 1;
//					idq += iter->id * (float)(iter->endq - iter->startq + 1);
					bitsq += iter->bits;
					endq = iter->endq;
					temp2 = iter;
					iter = iter->next;
					free(temp2);
					//check for overlaps here and throw error
				}
//				printf("Printing pair %s::%s\t#HSPs:%d\tbest-eval:%s\n",pair.s, pair.q, pair.count, pair.eval);
//				if(pair.count >= 2){
				if(al_bs==0){
					if(alArrC>1){
						alqFull = getAlignedLength();
					} else {
						alqFull = alq;
					}
					alArrC=0;
					fprintf(fOut,"%s\t%s\t%.2f\t%d\t%d\t0\t%d\t%d\t%d\t%d\t%s\t%.2f\n", pair.q, pair.s, pair.id, alqFull, alq, head->startq, endq, pair.starts, pair.ends, pair.eval, bitsq);
				} else {
					fprintf(fOut,"%s\t%s\t%.2f\t%d\t0\t0\t%d\t%d\t%d\t%d\t%s\t%.2f\n", pair.q, pair.s, pair.id, alq, head->startq, endq, pair.starts, pair.ends, pair.eval, bitsq);
				}

			}
			//free all linkedlist
			strcpy(pair.q,qT);
			strcpy(pair.s,sT);
			pair.count = 0;
			//create new linkedlist and set the size = 0

		}
		pair.count++;

		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading sequence identity in row %d .\n", rCount);
			return 1;
		}
		if(pair.count==1){
			pair.id = atof(buftok);
		}
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading aligned length in row %d .\n", rCount);
			return 1;
		}
//		pair.al[pair.idx] = atoi(buftok);
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading mismatched residues/bases in row %d .\n", rCount);
			return 1;
		}
//		pair.mis[pair.idx] = atoi(buftok);
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading alignment gaps in row %d .\n", rCount);
			return 1;
		}

//		pair.gap[pair.idx] = atoi(buftok);
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading query alignment start in row %d .\n", rCount);
			return 1;
		}
		temp.startq = atoi(buftok);
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading query alignment end in row %d .\n", rCount);
			return 1;
		}
		temp.endq = atoi(buftok);
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading subject alignment start in row %d .\n", rCount);
			return 1;
		}
		starts = atoi(buftok);
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading subject alignment end in row %d .\n", rCount);
			return 1;
		}
		ends = atoi(buftok);
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading e-value in row %d .\n", rCount);
			return 1;
		}
		if(pair.count==1){
			strcpy(pair.eval,buftok);
		}
		buftok = strtok(NULL,"\t");
		if(buftok==NULL){
			fprintf(stderr, "Error reading bit score in row %d .\n", rCount);
			return 1;
		}
		temp.bits = atof(buftok);
//		fprintf(stderr, "Read the row %d \n",rCount);
//		printf("%s %s %f %d %d %d %d %d %d %d %s %f\n",pair.s, pair.q, pair.id, pair.al, pair.mis, pair.gap, pair.startq, pair.endq, pair.starts, pair.ends, pair.eval, pair.bits);

	//For other HSPs run a single pass of following for loop in the fgets while loop
//		fprintf(fOut,"%s\t%s\t%.2f\t0\t0\t0\t%d\t%d\t%d\t%d\t%s\t%.2f\n", pair.q, pair.s, pair.id, temp.startq, temp.endq, starts, ends, pair.eval, temp.bits);
		if(al_bs==0){
			alArrStart[alArrC]=temp.startq;
			alArrEnd[alArrC]=temp.endq;
			alArrC++;
		}

		if(pair.count==1){
//			head->id = temp.id;
			head->startq = temp.startq;
			head->endq = temp.endq;
			head->bits = temp.bits;
			pair.ends = ends;
			pair.starts = starts;
//			if(head->next == NULL){printf("Processing new pair %s::%s\t#HSPs:%d\tbest-eval:%s\n",pair.s, pair.q, pair.count, pair.eval);}

		} else {
			if(ends>pair.ends)
				{pair.ends = ends;}
			if(starts<pair.starts)
				{pair.starts = starts;}

			iter = head;
			prevIter = NULL;
			state = 0;
			ovl = 0;
			len = temp.endq - temp.startq;
			while(iter != NULL){
				if(temp.startq < iter->startq){
					if(temp.endq < iter->startq){
						if ((prevIter != NULL) && (prevIter->endq == (temp.startq - 1))){
							prevIter->endq = temp.endq;
							prevIter->bits += temp.bits;
						} else {
							temp2 = malloc(sizeof(struct alnSegs));
	//						temp2->id = temp.id;
							temp2->startq = temp.startq;
							temp2->endq = temp.endq;
							temp2->bits = temp.bits;
							temp2->next = iter;
							if (prevIter != NULL) {
								prevIter->next = temp2;
							} else {
								head = temp2;
							}
							temp2 = NULL;
						}
						state = 1;
						break;
					} else if(temp.endq < iter->endq){
						//  -- check overlap for this case and set ovl
						ovl += temp.endq - iter->startq;
						//nextSt->xx[nS] = HSP until prevSt->starts[j]-1
						//nextSt->xx[nS] = prevSt->xx[j];
//						temp2 = malloc(sizeof(struct alnSegs));
//						temp2->id = temp.id;
						if(ovl/len <= maxOvl){
							iter->bits += (temp.bits * (float)(iter->startq - temp.startq))/(float)(temp.endq - temp.startq + 1);
							iter->startq = temp.startq;
	//						temp2->endq = iter->startq - 1;
							if ((prevIter != NULL) && (prevIter->endq == (iter->startq - 1))){
								prevIter->endq = iter->endq;
								prevIter->bits += iter->bits;
//								prevIter->next = NULL;    //test this condition somewhere
								prevIter->next = iter->next;
								free(iter);
							}
						}
//	                    temp2->next = iter;
//						if (prevIter != NULL) {
//							prevIter->next = temp2;
//		                } else {
//		                	head = temp2;
//		                }
//						temp2 = NULL;
						state = 1;
						break;
					} else {
						state = 2;
						break;
					}
				} else if(temp.startq < iter->endq){
					if(temp.endq <= iter->endq){
						state = 2;
						break;
					} else {
						//  -- check overlap for this case and set ovl
						ovl +=  iter->endq - temp.startq;
						if(ovl/len <= maxOvl){
							//truncate HSP until iter->endq;
							temp.bits = (temp.bits * (float)(temp.endq - iter->endq))/(float)(temp.endq - temp.startq + 1);
							temp.startq = iter->endq + 1;
						} else {
							state = 2;
							break;
						}
					}
				}
				//not implemented yet: discard if overlap > maxOvl % continue(state =2; break;);
				prevIter = iter;
				iter = iter->next;
			}

			if (state == 0){
				if ((prevIter != NULL) && (prevIter->endq == (temp.startq - 1))){
					prevIter->endq = temp.endq;
					prevIter->bits += temp.bits;
					prevIter->next = NULL;
				} else {
					iter = malloc(sizeof(struct alnSegs));
	//				iter->id = temp.id;
					iter->startq = temp.startq;
					iter->endq = temp.endq;
					iter->bits = temp.bits;
					iter->next = NULL;
					if (prevIter != NULL) {
						prevIter->next = iter;
					} else {
						fprintf(stderr, "Error processing row %d .\n", rCount);
					}
				}
			}
//			if (state == 2){
//				continue;
//			}
		}
	}
	fclose(f);


		alq = head->endq - head->startq + 1;
//		idq = head->id * (float)alq;
		bitsq = head->bits;
		endq = head->endq;
		iter = head->next;
		while(iter != NULL) {
			alq += iter->endq - iter->startq + 1;
//			idq += iter->id * (float)(iter->endq - iter->startq + 1);
			bitsq += iter->bits;
			endq = iter->endq;
			temp2 = iter;
			iter = iter->next;
			free(temp2);
			//check for overlaps here and throw error
		}
//		if(pair.count >= 2){
			fprintf(fOut,"%s\t%s\t%.2f\t%d\t0\t0\t%d\t%d\t%d\t%d\t%s\t%.2f\n", pair.q, pair.s, pair.id, alq, head->startq, endq, pair.starts, pair.ends, pair.eval, bitsq);
//		}
	free(head);

	time(&end);
	float runtime = difftime(end, start);
	fprintf(stderr,"Runtime: %d min %d sec\n", (int)runtime/60, (int)runtime%60);
	return 0;
}


/*
awk 'BEGIN{ PROCINFO["sorted_in"] = "@ind_num_asc"}{if($1==$2){next};\
if($1!=qid||$2!=sid){\
if(qid!=""){n = asorti(startq,sqX,"@val_num_asc");\
identityA=identity[sqX[1]]*al[sqX[1]];alA=al[sqX[1]];misA=mis[sqX[1]];gapA=gap[sqX[1]];startqA=startq[sqX[1]];\
endqA=endq[sqX[1]];startsA=starts[sqX[1]];endsA=ends[sqX[1]];evalA=eval[sqX[1]];bitsA=bits[sqX[1]];\
for (i = 2; i <= n; i++){\

if(endsA<ends[sqX[i]]){endsA=ends[sqX[i]]};if(startsA>starts[sqX[i]]){startsA=starts[sqX[i]]};\
if(evalA>eval[sqX[i]]){evalA=eval[sqX[i]]};\
if(endq[sqX[i]]>endqA){\
ovl=endqA-startq[sqX[i]]+1;\
if(ovl<0){ovl=0};\
if(bitsA/alA<bits[sqX[i]]/al[sqX[i]]){tmp=bitsA;bitsA=bits[sqX[i]]+((1-ovl/alA)*tmp)}\

else{bitsA+=(1-ovl/al[sqX[i]])*bits[sqX[i]]};\

identityA+=identity[sqX[i]]*(al[sqX[i]]-ovl);\

alA+=al[sqX[i]]-ovl;endqA=endq[sqX[i]];}else{

ovl=endq[sqX[i]]-startq[sqX[i]]+1;\

if(bitsA/alA<bits[sqX[i]]/al[sqX[i]]){tmp=bitsA;bitsA=bits[sqX[i]]+((1-ovl/alA)*tmp);\
tmp=identityA;identityA=identity[sqX[i]]*al[sqX[i]]+tmp*(1-ovl/alA);}}}\
print qid"\t"sid"\t"identityA/alA"\t"alA"\t"misA"\t"gapA"\t"startqA"\t"endqA"\t"startsA"\t"endsA"\t"evalA"\t"bitsA;}\
qid=$1;sid=$2;idx=0;\
delete identity; delete al; delete mis; delete gap; delete startq; delete endq; delete starts; delete ends; delete eval; delete bits}\
idx++;\
identity[idx]=$3;al[idx]=$8-$7+1;mis[idx]=$5;gap[idx]=$6;startq[idx]=$7;endq[idx]=$8;starts[idx]=$9;ends[idx]=$10;eval[idx]=$11;bits[idx]=$12;}\


END{\
n = asorti(startq,sqX,"@val_num_asc");\
identityA=identity[sqX[1]]*al[sqX[1]];alA=al[sqX[1]];misA=mis[sqX[1]];gapA=gap[sqX[1]];startqA=startq[sqX[1]];\
endqA=endq[sqX[1]];startsA=starts[sqX[1]];endsA=ends[sqX[1]];evalA=eval[sqX[1]];bitsA=bits[sqX[1]];\
for (i = 2; i <= n; i++) {\
if(endsA<ends[sqX[i]]){endsA=ends[sqX[i]]};if(startsA>starts[sqX[i]]){startsA=starts[sqX[i]]};\
if(evalA>eval[sqX[i]]){evalA=eval[sqX[i]]};\
if(endq[sqX[i]]>endqA){\
ovl=endqA-startq[sqX[i]]+1;\
if(ovl<0){ovl=0};\
if(bitsA/alA<bits[sqX[i]]/ovl){tmp=bitsA;bitsA=bits[sqX[i]]+((1-ovl/alA)*tmp)}\
else{bitsA+=(1-ovl/al[sqX[i]])*bits[sqX[i]]};\
identityA+=identity[sqX[i]]*(al[sqX[i]]-ovl);\
alA+=al[sqX[i]]-ovl;endqA=endq[sqX[i]];}else{
ovl=al[sqX[i]];\
if(bitsA/alA<bits[sqX[i]]/ovl){tmp=bitsA;bitsA=bits[sqX[i]]+((1-ovl/alA)*tmp);\
tmp=identityA;identityA=identity[sqX[i]]*al[sqX[i]]+tmp*(1-ovl/alA);}}}\
print qid"\t"sid"\t"identityA/alA"\t"alA"\t"misA"\t"gapA"\t"startqA"\t"endqA"\t"startsA"\t"endsA"\t"evalA"\t"bitsA;}' \
allvallblast.tab > allvallblast.merged.tab

*/

//while(c++<10 && fscanf(f,"%s %s %f %d %d %d %d %d %d %d %s %f", pair.s, pair.q, &pair.id, &pair.al, &pair.mis, &pair.gap, &pair.startq, &pair.endq, &pair.starts, &pair.ends, pair.eval, &pair.bits) == 1){
//	printf("%s %s %d %s %f\n",pair.s, pair.q, pair.ends, pair.eval, pair.bits);
//}
