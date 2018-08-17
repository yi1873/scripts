awk '{if(NR%4 == 1){print ">" substr($0, 2)}}{if(NR%4 == 2){print}}' KBS000000.R1.fastq > KBS000000.R1.fasta
