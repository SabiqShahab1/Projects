
#loading appropriate library
library(seqinr)

#reading fasta file
sequence <- read.fasta('gene.fsa')

#unlisting the sequence
sequence <- unlist(sequence)

#collapsing the sequence into a single string
sequence <- paste(sequence, collapse = "")

#Making sure all bases are capitilized
sequence <- toupper(sequence)

#Making sure the sequence is a data frame
sequence <- as.data.frame(sequence)

#Creating a vector of all dinucleotides
dinucleotides <- c("AA", "AT", "AC", "AG", "TA", "TT", "TC", "TG", "CA", "CT", "CC", "CG", "GA", "GT", "GC", "GG" )

#Creating a vector of zeros the length of the total number of dinucleotides
Count <- rep(0, length(dinucleotides))

#combining the two vectors to set the initial value of each dinucleotide pair to zero
DiCount <- setNames(Count, dinucleotides)

#creating a for loop that reads the total count of each dinucleotide pair and assigns it to the appropriate pair
for (dinucleotide in dinucleotides) {
  DiCount[dinucleotide] <- str_count(sequence, dinucleotide)
}

#printing results
print(DiCount)

#Creating data frame that contains the names of the pairs and the count of each
Didf <- data.frame(dinucleotide = names(DiCount), count = DiCount)

#splitting the pairs into individual bases
DiSplit <- strsplit(Didf$dinucleotide, "")

#separating the base
Split <- Reduce(rbind, DiSplit)

#assign the bases to the first and second bases for the dinucleotides pairs
Didf$first <- Split[,1]
Didf$second <- Split[,2]

#printing results
print(Didf)

#adding the sequence to the dataframe 
Didf[nrow(Didf) + 1,] <- c(sequence, '','','')

#creating a file that contains the results
write.table(Didf, 'ShahabFrequencies.tsv')

#reading the file to ensure it was created correctly
Gene <- read.table('ShahabFrequencies.tsv')
