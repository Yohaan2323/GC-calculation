
  getwd()
setwd("C:\\Users\\Yohaan\\Downloads")
#The readLines() function in R can be used to read some or all text lines from a connection object.
fastadata<- readLines("random_fasta.fa")
#Choose only header lines
headers<- grep(">",fastadata) #returns their positions, thats why u see 1,4,and 7

#+1 means look at the line after a header
seq_start<- headers +1

#headers[-1]removes the first element, giving the next two headersc(4, 7).
#Why remove the first? Because we want to look at the next header for each sequence to know where it ends.
#why (headers[-1] - 1)? Subtract 1 because the sequence ends just before the next header.
#For example:seq1 starts at line 2, ends at 4 - 1 = 3
#seq2 starts at line 5, ends at 7 - 1 = 6

seq_ends <- c((headers[-1] - 1), length(fastadata))

# Prepare empty vectors
names <- c()
sequences <- c()

#For i from 1 to the number of headers:
#Extract the sequence lines: fastadata[seq_starts[i]:seq_ends[i]].
#Combine them into one string (using concatenation).
#Store the result in your sequences vector.
#Store the header name (remove the > symbol) in the names vector.

for (i in 1:length(headers)) {
  # 1. Extract the sequence lines for this sequence
  seq_parts<- fastadata[seq_start[i]:seq_ends[i]]
#Combine them into one string
  full_sequence<- paste0(seq_parts, collapse = "")
  #Store the full sequence
  sequences <- c(sequences, full_sequence)
  
  #Store the header name (remove ">")
  names <- c(names, substring(fastadata[headers[i]], 2))
}

df <- data.frame(name = names, sequence = sequences, stringsAsFactors = FALSE)

#toupper() converts all of the characters in a string in R to uppercase. Sometimes fasta files contain both upper and lowercase, so we do this for consistency
df[c('sequence')] <- sapply(df[c('sequence')], function(x) toupper(x))


library(stringr)

Gcount<-sum( str_count(df$sequence, "G"))
Acount<-sum( str_count(df$sequence, "A"))
Tcount<- sum( str_count(df$sequence, "T"))
Ccount<- sum( str_count(df$sequence, "C"))
GCcount<- Gcount+Ccount
total_count<- Gcount+Tcount+Acount+Ccount

GC<- sum( str_count(df$sequence, "GC"))

total_pairs<- total_count - 2 #because there are 2 sequences
#total_pairs <- sum(nchar(df$sequence) - 1) better formula i found, more applicable across more amounts of sequences, you dont have to count how many sequences you have
GC_percent<- (GC/total_pairs)*100

CG<- sum(str_count(df$sequence, "CG"))
CG_percent<- (CG/total_pairs)*100
