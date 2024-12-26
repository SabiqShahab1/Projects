library(stringr)
library(igraph)

Kmerize <- function(seq, n) {
  #create empty lit
  kmers <- list()
  #calculate total number of kmers
  TotalKmers <- nchar(seq) - n + 1
  #create an iteration loop that goes from 1 to total number of kmers
  for (i in 1:TotalKmers) {
  #split the string into a kmer by going from the starting point i to the final point calculated by adding the kmer length to the initial position - 1
    kmer <- substring(seq, i, i + n - 1)
  #storing the Kmers into the empty list at the specific location indicated
    kmers[i] <- kmer
  }
  #return list of kmers
return(unlist(kmers))
}

kmerGraph <- function(kmers) {
 #create empty vector
 edges <- c()
 #create loop that goes through each kmer in the list and stores its beginning and end
 for (kmer in kmers) {
 #take the beginning of the kmer until length - 1
  beg <- substring(kmer, 1, nchar(kmer)-1)
 #take the end of the kmer until the second place of the kmer
  end <- substring(kmer, 2, nchar(kmer))
 #create a list that adds the beginning and endings of all the kmers in order
 edges <- c(edges, beg, end)
 }
 #create graph object from the list of edges created and then return it
 graph <- make_graph(edges, directed = TRUE)
 return(graph)
}  

getAssembly <- function(graph) {
  #checks if the graph object is Eulerian
   if (has_eulerian_path(graph)) {
  #finds the Eulerian path
    path <- eulerian_path(graph)
  #extract bases (node names) from path
    parts <- names(unlist(path$vpath))
  #start the reconstructed sequence with the first node from the path
    sequence <- parts[1]
  #go through the remaining nodes from the path starting from the second one
    for (i in 2:length(parts)) {
  #Attach the part of each subsequent node to the sequence and return the reconstructed sequence
      sequence <- paste0(sequence, substring(parts[i], nchar(parts[i])))
    }
    return(sequence)
  #if no Eulerian path, stop function
  } else {stop("no Eulerian path")
  }
}  


sequences <- list(
  "ATGCGTGATGCGT",
  "ATATATATATAT",
  "ACGTACGTACGT",
  "GGGTTTCCCAGTA"
)
klengths <- c(2, 3, 4, 5)
for (seq in sequences) {
  for (k in klengths) {
    kmers <- Kmerize(seq, k)
    graph <- kmerGraph(kmers)
    plot(graph)
    assembly <- getAssembly(graph)
    print(assembly)
  }
}

#The kmer length must be greater than the length of the longest repeat in the sequence

#Function that takes a sequence, the size of the read, and how many reads
readWriter <- function(seq, readSize, numReads) {
#Create a character vector for the reads  
  reads <- vector("character", numReads)
#Find length of the sequence
  length <- nchar(seq)
#Loop for the number of reads indicated
  for (i in 1:numReads) {
#Randomly pick a starting position for the read
    start <- sample(1:(length - readSize + 1), 1)
#Extracts a substring that is the read size set from the starting position picked
    reads[i] <- substring(seq, start, start + readSize - 1)
  }
#Return list of generated reads
  return(reads)
}

#Function that takes reads and introduces error based on rate (0-1)
readWronger <- function(reads, errorRate) {
#Create vector of possible bases
  bases <- c("A", "T", "C", "G")
#Go through each read
  for (i in 1:length(reads)) {
#Split the reads into individual bases
    read <- strsplit(reads[i], "")[[1]]
#Calculate number of errors based on error rate set
    numErrors <- rbinom(1, length(read), errorRate)
#Randomly pick locations to create error
    errorPositions <- sample(1:length(read), numErrors)
#Go through each location chosen
    for (pos in errorPositions) {
#Find orignial base
      originalBase <- read[pos]
#Pick random base from the vector earlier that is not the original
      newBase <- sample(bases[bases != originalBase], 1)
#Replace oringinal with random chosen
      read[pos] <- newBase
    }
#Reassemble modified read and put it back in the reads list and then returnt the final list with the errors
    reads[i] <- paste(read, collapse = "")
  }
  return(reads)
}

#Fixed for taking either a single sequence or a string of sequences
Kmerize <- function(seqs, n) {
#Create an empty list to store kmers
  kmerslist <- list()
#Go through each sequence
  for (seq in seqs) {
#Calculate number of kmers for the sequence
    TotalKmers <- nchar(seq) - n + 1
#Create a vector for kmers of the sequence
    kmers <- vector("character", TotalKmers)
#Create an iteration loop that goes from 1 to total number of kmers
    for (i in 1:TotalKmers) {
#Split the string into a kmer by going from the starting point i to the final point calculated by adding the kmer length to the initial position - 1
      kmers[i] <- substring(seq, i, i + n - 1)
    }
#Add the kmers to the list and return it, the kmers for each sequence should be returned
    kmerslist <- c(kmerslist, list(kmers))
  }
  return(kmerslist)
}


sequences <- list(
  "ATGCGTGATGCGT",
  "ATATATATATAT",
  "ACGTACGTACGT",
  "GGGTTTCCCAGTA"
)
error_rates <- c(0, 0.01, 0.05, 0.1)
klengths <- c(3, 4)
for (seq in sequences) {
  for (error_rate in error_rates) {
    reads <- readWriter(seq, readSize = 5, numReads = 10)
    error_reads <- readWronger(reads, error_rate)
    for (k in klengths) {
      kmers <- Kmerize(error_reads, k)
      graph <- kmerGraph(kmers)
      plot(graph)
    }
  }
}

#A higher error rate increases the number of nodes in the graph, which are additional fake kmers
#The larger the kmer length, the less impact the errors have but it then requires longer reads to cover
#Graphs with a lot of errors have more disconnected components, making assembly harder
