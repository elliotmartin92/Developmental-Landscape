#!/bin/bash
#SBATCH --job-name=align_STAR1_CM         # Job name
#SBATCH --mail-type=END,FAIL               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=etmartin@albany.edu    # Where to send mail
#SBATCH --ntasks=1                 	   # Run a single task
#SBATCH --mem=50gb                         # Job memory request
#SBATCH --cpus-per-task=8                  # Number of CPU cores per task
#SBATCH --time=48:00:00                    # Time limit hrs:min:sec
#SBATCH --output=ALIGNSTAR1.log     		   # Standard output and error log | %j is a variable that holds the job ID, so your output log will match your job ID
date;hostname;pwd

#source ~/.bashrc               #send to noninteractive node


for prefix in $(ls *.fastq.gz | sed -r 's/[.]fastq.gz//' | uniq)
do
mkdir "$prefix"
STAR --genomeDir /network/rit/misc/scratch/HumanGenome --runThreadN 8 --outSAMtype BAM SortedByCoordinate --quantMode GeneCounts --readFilesIn \
    "${prefix}.fastq.gz" --outFileNamePrefix "$prefix/${prefix}" 
    sleep 1
done
