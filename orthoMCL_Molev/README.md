# OrthoMCL_Molev
This is a customized tool to cluster orthologous proteins using homology search results from tools like blastp or MMseq. I built it during my years at Molev, ICM, Uppsala, and used it as a helper tool to verify protein orthologs. Orthology prediction is the foundation of comparative genomics and protein function prediction. It addresses some of the drawbacks of orthoMCL and tribeMCL. Ortholog clustering of proteins with repeated domains and promiscuous domains is challenging. It uses a new step-by-step approach to merge homology scores from proteins with multiple conserved domains. Then it Normalizes the homology scores via a linear model that is trained to predict the score for a pair of species/taxa given the protein length. This new approach filters out false positives, while preserving the orthology of the multi-domain or non-globular proteins.

### Performance and scalability
Clusters homology results from roughly 100 bacterial species consumes 52GB of RAM and takes 1hr45min on single processor (2.4 GHz). 

### Prerequisites

Compile the C code 

```
gcc preHomologySearch.c -o preHomologySearch
gcc mergeHSP.c -o mergeHSP
```

Instal mcl for makov chain clustering from https://github.com/micans/mcl

### Running orthoMCL_Molev

Merge all protein fasta files in the <faaFiles> folder and create a single fasta with taxa names in the headers (Inspired by orthoMCL and tribeMCL). Also create the protein_sizes.tab 
```

preHomologySearch -i /faaFiles -f genomes.list -l 10 -x 20
```

Run all-vs-all homology search with your preferred tool such as Blastp or MMseq. Set the output format to blast-like tabular format.

Use the homology search results from above to run ORX with default settings

```
Rscript orthoMCL.R -p all_vs_all_homology_results.tab -p protein_sizes.tab
```
