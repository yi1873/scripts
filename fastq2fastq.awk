
awk '{if(NR%4 == 1){print ">" substr($0, 2)}}{if(NR%4 == 2){print}}' KBS000000.R1.fastq > KBS000000.R1.fasta


other:

awk求累加
awk '{a=a+$2}END{print a}'
awk -F '\t' '{sum[$2]+=$4}END{for (key in sum ) print key"\t"sum[key] }'  txt > genome.length

求最大值
les H12_S3.gene.txt|awk -F ' ' '{print $5}'|awk -F ':' 'BEGIN{max=0}{if($NF+0 > max+0) max=$NF}END{print "Max=",max}'

求最小值
les H12_S3.gene.txt|awk -F ' ' '{print $5}'|awk -F ':' 'BEGIN{min=7452}{if($NF+0 < min+0) min=$NF}END{print "Min=",min}'
