#Class 35 Blank

# Corpus in Multiple Files
?list.files # Return vector of file names within 
# specific directory
list.files("./inaugural_addresses") 
# 56 US presidential inaugural addresses (1789-2009)

# Create corpus using DirSource() and Corpus()
inaugural_corpus<-Corpus(DirSource("inaugural_addresses"))
inaugural_corpus 

# Use [[ ]] to extract elements by index or file name
inaugural_corpus[[56]] # Returns document summary
inaugural_corpus[["2009-Obama.txt"]]

# Can use string functions to find file names that
# match specific criteria
# Example: return all of Abraham Lincoln's inaugural
# addresses without hard-coding the index/name
all_files<-list.files("inaugural_addresses")
lincoln_files<- grep("Lincoln", all_files)
lincoln_files
lincoln_docs<-inaugural_corpus[lincoln_files]

# Use loops or apply() function to extract only
# the text of Lincoln's addresses
?vapply
lincoln_addresses<-vapply(lincoln_docs, "[", "content")
lincoln_addresses

rm(list=ls())
### Term Frequency ###

## Load Packages ##
library(NLP)  
library(tm)  

## Term Frequency Representation ##
inaugural_corpus<-Corpus(DirSource("inaugural_addresses"))

# Document Term Matrix (DTM) represents a document as 
# "bag of words" based on presence (0/1), frequency, or 
# weight within document
?DocumentTermMatrix
dtm<-DocumentTermMatrix(inaugural_corpus)
class(dtm) # Result is DTM object
dtm # DTM summary
inspect(dtm) # DTM summary plus snippet of DTM
# Rows = documents, Columns = terms
# Entries = number of occurrences of term in document
# Non-sparse entries = non-zero cells in matrix 
# Sparse entries = 0 cells in matrix
# Sparsity = number of 0 cells / total cells
# Maximal term length = characters in longest term
# Weighting = numeric representation of term 

# DTM has structure similar to regular matrix
ncol(dtm) # Number of terms
nrow(dtm) # Number of documents
colnames(dtm) # Vector with all terms
rownames(dtm) # Vector with all document names

# Transform DTM into regular matrix with as.matrix()
# Example: return number of occurrences of "country"
# in George Washington's first inaugural address
as.matrix(dtm["1789-Washington.txt","country"]) 

# Corpus Term Frequency (CTF): total occurrences 
# of specific term over entire corpus
# Use colSums() to compute CTF
freq<- colSums(as.matrix(dtm)) 
freq # Named, numeric vector
freq["country"] # Total occurrences of "country"
# over all inaugural addresses

# Sort terms in order of frequency
freq_sorted<-sort(freq, decreasing=TRUE)
freq_sorted[1:10] # Top 10 terms

# Use names() to extract just the terms (not the counts)
names(freq_sorted[1:10])

?findFreqTerms # Find terms that occur at least n times
# (or have a weight of at least n)
findFreqTerms(dtm,500) # Returns character vector

## Term Weighting ##
# NOTE: Adding the weighting option to the control list
# might produce a warning, but the code should work!

# Default term weight is tf (term frequency)
# Use control list to set different weighting
# (e.g., binary weighting)
ctrl<-list(weighting=weightBin)
dtm2<-DocumentTermMatrix(inaugural_corpus, control = ctrl)
inspect(dtm2) # Same number of terms, but
# all weights are 0 or 1 
freq<-colSums(as.matrix(dtm2))
freq["country"] # Number of addresses using
# word "country"

# Normalized TF-IDF weighting
ctrl<-list(weighting=weightTfIdf)
dtm3<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm3) 
freq<-colSums(as.matrix(dtm3))
freq["country"] # Relative importance of word
# country, balancing frequency (TF) and 
# novelty (IDF), normalized by document length

# Non-normalized TF-IDF weighting
ctrl<-list(weighting=function(x) 
  weightTfIdf(x, normalize = FALSE))
dtm4<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm4) 
freq<-colSums(as.matrix(dtm4))
freq["country"] # Relative importance of word
# country, balancing frequency (TF) and 
# novelty (IDF), non-normalized


rm(list=ls())
### Document Term Matrix ###

## Load Packages ##
library(NLP)  
library(tm)  

## Create Corpus ##
inaugural_corpus<-Corpus(DirSource("inaugural_addresses"))

# Document Term Matrix (DTM) with default settings
dtm<-DocumentTermMatrix(inaugural_corpus)
inspect(dtm)  # 56 documents, 13673 terms

# DTM still contains punctuation
colnames(dtm) # "it." "it;"

# And synonymous terms
colnames(dtm)[364:366] # "nation", "national", "nations"

## Refining the Vocabulary ##
# Add options for refining DTM to control list

# Strip remaining punctuation from terms
ctrl<-list(removePunctuation=TRUE)
dtm2<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm2) # 9170 terms instead of 13,673
# "justice," and "justice" now treated as one term

# Remove numbers from terms
ctrl<-list(removePunctuation=TRUE,
           removeNumbers=TRUE)
dtm3<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm3) # 9110 terms

# Strip stopwords
stopwords("en") # 174 common English words
# Can also create custom stopwords lists
ctrl<-list(removePunctuation=TRUE,
           removeNumbers=TRUE,
           stopwords=TRUE)
dtm4<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm4) # 9012 terms

# Set min/max length for terms
ctrl<-list(removePunctuation=TRUE,
           removeNumbers=TRUE,
           stopwords=TRUE,
           wordLengths=c(4,17))
dtm5<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm5) #8885 terms (terms < 4 letters 
# or > 17 letters discarded)

# Set min/max document frequency for terms
ctrl<-list(removePunctuation=TRUE,
           removeNumber2=TRUE,
           stopwords=TRUE,
           wordLengths=c(4,17),
           bounds=list(global=c(3,Inf)))
dtm6<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm6) #3431 terms (terms that appear
# in fewer than 3 addresses discarded)

# Apply stemming (reduce words to root forms)
install.packages("SnowballC")
library(SnowballC)
ctrl<-list(removePunctuation=TRUE,
           removeNumbers=TRUE,
           stopwords=TRUE,
           wordLengths=c(4,17),
           bounds=list(global=c(3,Inf)),
           stemming=TRUE)
dtm7<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm7) #2441 terms ("govern", "governor",
# "governing", "government", "governs" >> "govern")

# Custom vocabulary (assume we only want top 500 words 
# from the previous DTM)
freq<-colSums(as.matrix(dtm7))
top500<-names(sort(freq, decreasing=TRUE)[1:500])

ctrl<-list(removePunctuation=TRUE,
           removeNumbers=TRUE,
           stemming=TRUE,
           dictionary=top500)
dtm8<-DocumentTermMatrix(inaugural_corpus,
                         control=ctrl)
inspect(dtm8)

# Different control list options and weighting
# have a HUGE effect on your final vocabulary
# and term weights
