#loading library for count_seq_pattern function
library(baseq)
#reading txt file and turning it into a vector
Sequence <- readLines('testATCG.txt')
#transforming vector into string
StringSeq <- paste(Sequence, collapse = '')
#using count_seq_pattern to count the unique Dinucleotide sequences in the string
AA <- count_seq_pattern(StringSeq, "AA")
AT <- count_seq_pattern(StringSeq, "AT")
AC <- count_seq_pattern(StringSeq, "AC")
AG <- count_seq_pattern(StringSeq, "AG")
TA <- count_seq_pattern(StringSeq, "TA")
TT <- count_seq_pattern(StringSeq, "TT")
TC <- count_seq_pattern(StringSeq, "TC")
TG <- count_seq_pattern(StringSeq, "TG")
CA <- count_seq_pattern(StringSeq, "CA")
CT <- count_seq_pattern(StringSeq, "CT")
CC <- count_seq_pattern(StringSeq, "CC")
CG <- count_seq_pattern(StringSeq, "CG")
GA <- count_seq_pattern(StringSeq, "GA")
GT <- count_seq_pattern(StringSeq, "GT")
GC <- count_seq_pattern(StringSeq, "GC")
GG <- count_seq_pattern(StringSeq, "GG")
#combining all values for the dinucleotide count into one
SeqCount <- c(AA, AT, AC, AG, TA, TT, TC, TG, CA, CT, CC, CG, GA, GT, GC, GG)
#naming the values with the appropriate dinucleotide
names(SeqCount) <- c('AA', 'AT', 'AC', 'AG', 'TA', 'TT', 'TC', 'TG', 'CA', 'CT', 'CC', 'CG', 'GA', 'GT', 'GC', 'GG')
#transforming into a dataframe
SeqTable <- as.data.frame(SeqCount)
#printing results
SeqTable
