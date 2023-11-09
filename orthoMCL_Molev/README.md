# OrthoMCL_Molev
This is a customized tool to cluster orthologous proteins using homology search results from tools like Blastp or MMseq. I built it during my years at Molev, ICM, Uppsala Universitet. Orthology prediction is the foundation of comparative genomics and protein function prediction. This project was aimed to port orthoMCL from Perl+SQL to C+R, and address some of the drawbacks of orthoMCL and tribeMCL. Ortholog clustering of proteins with repeated domains and promiscuous domains is challenging. This tool uses a new step-by-step approach to merge homology scores from multiple conserved domains in the same pair of proteins. Then it normalizes the homology scores via a linear model, which is trained to predict the score for a pair of species/taxa given the protein length. This new approach is able to preserve the ortholog clusters of the multi-domain or non-globular proteins, while filtering false positives. Hence, it serves as a really good helper tool to fetch ortholog clusters of multi-domain proteins.

### Prerequisites

C compiler, and R with following libraries: ggplot2, data.table, dplyr, stringr, parallel, optparse

markov chain clustering tool MCL. Check download and install instructions on the MCL github page: https://github.com/micans/mcl

Compile the C code:-

```
gcc preHomologySearch.c -o preHomologySearch
gcc mergeHSP.c -o mergeHSP
```

### Running orthoMCL_Molev

Merge proteins from all the protein fasta files in 'faaFiles' folder and create a single fasta with taxa names in the headers (Inspired by orthoMCL and tribeMCL). Also creates the protein_sizes.tab 

```
preHomologySearch -i /faaFiles -f genomes.list -l 10 -x 20
```

All-vs-all homology search: Search proteins in the merged protein file against themselves using Blastp or MMseq. Set the output format to blast-like tabular format.

Run orhoMCL.R on the homology scores from above. Use the following command to run with default settings

```
Rscript orthoMCL.R -p all_vs_all_homology_results.tab -p protein_sizes.tab
```

### Performance and scalability
Clusters homology results from roughly 100 bacterial species in less than 2 hours on a single 2.4 GHz processor and consuming 52GB of RAM . 
