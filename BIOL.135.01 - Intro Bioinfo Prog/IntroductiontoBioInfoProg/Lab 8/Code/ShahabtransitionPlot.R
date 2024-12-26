#Loading in relevant Libraries
library(ggplot2)
library(stringr)
library(seqinr)

#Reading fsa file
sequence <- read.fasta('S288C_YNCL0020C_RDN37-2_genomic.fsa')

#unlisting the sequence
sequence <- unlist(sequence)

#Collapsing into a single string
sequence <- paste(sequence, collapse = "")

#making sure sequence is all capital case
sequence <- toupper(sequence)

#Creating a vector storing the names of the dinucleotides
dinucleotides <- c("AA", "AT", "AC", "AG", "TA", "TT", "TC", "TG", "CA", "CT", "CC", "CG", "GA", "GT", "GC", "GG" )

#Creating a vector of zeros the length of the previous vector
Count <- rep(0, length(dinucleotides))

#Combining the two vectors to set the intial value of the dinucleotides to 0
DiCount <- setNames(Count, dinucleotides)

#Creating a for loop that goes through each dinucleotide and assigns the appropriate count from the sequence
for (dinucleotide in dinucleotides) {
  DiCount[dinucleotide] <- str_count(sequence, dinucleotide)
}

#printing results
print(DiCount)

#Creating a dataframe with the name of the dinucleotides and the count from the sequence
Didf <- data.frame(dinucleotide = names(DiCount), count = DiCount)

#Splitting the string, seperating the nucleotides into the individual bases
DiSplit <- strsplit(Didf$dinucleotide, "")

#Seperating the single bases
Split <- Reduce(rbind, DiSplit)

#Setting the bases that were split as the first base and second base in the dinucleotide
Didf$first <- Split[,1]
Didf$second <- Split[,2]

#printing results
print(Didf)

#Creating a heatmap plot of the dinucleotide counts
ggplot(Didf, aes(x = second, y = first, fill = count)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = count), color = "white", size = 4) +
  labs(title = "Dinucleotide Counts Heatmap",
       x = "Second Nucleotide",
       y = "First Nucleotide",
       fill = "Count")
