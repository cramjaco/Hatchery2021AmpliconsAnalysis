## Blast for vibrios

## Examples
library('rBLAST')
setwd(here::here("Blast"))
?blast
setwd("Blast")
download.file("https://ftp.ncbi.nlm.nih.gov/blast/db/16S_ribosomal_RNA.tar.gz",
   "16S_ribosomal_RNA.tar.gz", mode='wb')
untar("16S_ribosomal_RNA.tar.gz", exdir="16SMicrobialDB")

seq <- readRNAStringSet(system.file("examples/RNA_example.fasta",
                        package="rBLAST"))
bl <- blast(db="./16SMicrobialDB/16S_ribosomal_RNA")

cl <- predict(bl, seq[1:5,], BLAST_args = "-perc_identity 99")
cl
