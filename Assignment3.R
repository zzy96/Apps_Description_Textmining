rm(list=ls(all=TRUE)) # Clear all 

### Text-Mining ###

##  Activate Packages ##
library(NLP) 
library(tm)
library(SnowballC) 
library(wordcloud)
library(RColorBrewer)  
library(stats)     # Clustering

## Data Loading ##
Apps_data <- read.csv("Apps_Games500.csv") #The original dataset is stored in your project folder. 
Apps_desc <- Apps_data[,4]                 #Descriptions are recorded in the 4th column.  
n_desc <- length(Apps_desc)                #Check the number of app descriptions
n_desc

##--- Data Preparation ---##

# Use the First 300 characters #
test_300 <- substr(Apps_desc[1],0,300) #Test to extract first 300 chars
test_300
nchar(test_300)

Apps_300 <- vector()
for (i in 1:n_desc)
{
  #temp <- unlist(strsplit(as.character(Apps_desc[i]), split = "[.!?]+")) 
  
  Apps_300[[i]] <- substr(Apps_desc[i],0,300)          #Use the first 300 chars
}

Apps_300[1:5]

# Convert List to Corpus #
Apps <- Corpus(VectorSource(Apps_300))
Apps # Return Corpus Information

##--- Parsing ---##

# Convert upper-case letters to lower-case letters #
Apps  <- tm_map(Apps, content_transformer(tolower)) 
as.character(Apps[[1]])
# Remove HTML Tags #
for (j in 1:n_desc) Apps[[j]] <- gsub("u2019", " ", Apps[[j]]) #delete "u2019"
for (j in 1:n_desc) Apps[[j]] <- gsub("u'", " ", Apps[[j]])  #delete u'
for (j in 1:n_desc) Apps[[j]] <- gsub("u\"", " ", Apps[[j]]) #delete u"
as.character(Apps[[1]])
inspect(Apps[1:3])

for (j in 1:n_desc) Apps[[j]] <- gsub("u2605", " ", Apps[[j]]) #delete "u2605"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2606", " ", Apps[[j]]) #delete "u2606"
for (j in 1:n_desc) Apps[[j]] <- gsub("u201c", " ", Apps[[j]]) #delete "u201c"
for (j in 1:n_desc) Apps[[j]] <- gsub("u201d", " ", Apps[[j]]) #delete "u201d"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2011", " ", Apps[[j]]) #delete "u2011"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2013", " ", Apps[[j]]) #delete "u2013"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2014", " ", Apps[[j]]) #delete "u2014"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2022", " ", Apps[[j]]) #delete "u2022"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2122", " ", Apps[[j]]) #delete "u2122"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2026", " ", Apps[[j]]) #delete "u2026
for (j in 1:n_desc) Apps[[j]] <- gsub("u2028", " ", Apps[[j]]) #delete "u2028"
for (j in 1:n_desc) Apps[[j]] <- gsub("u2729", " ", Apps[[j]]) #delete "u2729"
for (j in 1:n_desc) Apps[[j]] <- gsub("u20ac", " ", Apps[[j]]) #delete "u20ac"
for (j in 1:n_desc) Apps[[j]] <- gsub("amp", " ", Apps[[j]]) #amp
for (j in 1:n_desc) Apps[[j]] <- gsub("xae", " ", Apps[[j]]) #xae
for (j in 1:n_desc) Apps[[j]] <- gsub("xa0", " ", Apps[[j]]) #xa0
for (j in 1:n_desc) Apps[[j]] <- gsub("xa3", " ", Apps[[j]]) #xa3
inspect(Apps[1:3])

# Remove less important terms : Device names #
for (j in 1:n_desc) Apps[[j]] <- gsub("apple", " ", Apps[[j]]) #Apple
for (j in 1:n_desc) Apps[[j]] <- gsub("iphone", " ", Apps[[j]]) #iphone
for (j in 1:n_desc) Apps[[j]] <- gsub("touch", " ", Apps[[j]]) #touch
for (j in 1:n_desc) Apps[[j]] <- gsub("ipod", " ", Apps[[j]]) #ipod
for (j in 1:n_desc) Apps[[j]] <- gsub("ipad", " ", Apps[[j]]) #ipad
for (j in 1:n_desc) Apps[[j]] <- gsub("3gs", " ", Apps[[j]]) #iPohne 3GS 
for (j in 1:n_desc) Apps[[j]] <- gsub("3rd", " ", Apps[[j]]) #3rd Gen. iPod
for (j in 1:n_desc) Apps[[j]] <- gsub("2nd", " ", Apps[[j]]) #2nd Gen. iPod
for (j in 1:n_desc) Apps[[j]] <- gsub("4th", " ", Apps[[j]]) #4th Gen. iPod
inspect(Apps[1:3])

# Remove less important terms : App Store-related terms #
for (j in 1:n_desc) Apps[[j]] <- gsub("app", " ", Apps[[j]]) #app
for (j in 1:n_desc) Apps[[j]] <- gsub("store", " ", Apps[[j]]) #store
for (j in 1:n_desc) Apps[[j]] <- gsub("game", " ", Apps[[j]]) #game 
for (j in 1:n_desc) Apps[[j]] <- gsub("play", " ", Apps[[j]]) #play
for (j in 1:n_desc) Apps[[j]] <- gsub("mobile", " ", Apps[[j]]) #mobile
for (j in 1:n_desc) Apps[[j]] <- gsub("free", " ", Apps[[j]]) #Free
for (j in 1:n_desc) Apps[[j]] <- gsub("new", " ", Apps[[j]]) #new
for (j in 1:n_desc) Apps[[j]] <- gsub("world", " ", Apps[[j]]) #world
inspect(Apps[1:3])

# Remove following additional less-informative terms #
for (j in 1:n_desc) Apps[[j]] <- gsub("now", " ", Apps[[j]]) #now
for (j in 1:n_desc) Apps[[j]] <- gsub("best", " ", Apps[[j]]) #best
for (j in 1:n_desc) Apps[[j]] <- gsub("time", " ", Apps[[j]]) #time
for (j in 1:n_desc) Apps[[j]] <- gsub("will", " ", Apps[[j]]) #will

for (j in 1:n_desc) Apps[[j]] <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", Apps[[j]]) #Remove 1-2 letter words

# Convert Important/Meaningful numbers to chars #
for (j in 1:n_desc) Apps[[j]] <- gsub("#1", "numberone", Apps[[j]]) #1
for (j in 1:n_desc) Apps[[j]] <- gsub("99", "nintyninecent", Apps[[j]]) #$0.99
for (j in 1:n_desc) Apps[[j]] <- gsub("%", "percent", Apps[[j]]) #percent
inspect(Apps[1:3])

# Remove stop words: articles, prepositions, conjuctions, pronous,... #
Apps <- tm_map(Apps, removeWords, stopwords("english"))
inspect(Apps[1:3])

# define new stopwords #
newstopwords <-c("and", "for", "the", "to", "in", "when", "then", "he", "she", "than", "can", "get", "one");
Apps <- tm_map(Apps, removeWords, newstopwords)

for (j in 1:n_desc) Apps[[j]] <- gsub("don", " ", Apps[[j]]) #don
for (j in 1:n_desc) Apps[[j]] <- gsub("won", " ", Apps[[j]]) #won't
for (j in 1:n_desc) Apps[[j]] <- gsub("ing", " ", Apps[[j]]) # -ing
for (j in 1:n_desc) Apps[[j]] <- gsub("http", " ", Apps[[j]]) # http
for (j in 1:n_desc) Apps[[j]] <- gsub("'ll", " ", Apps[[j]]) #'ll   
for (j in 1:n_desc) Apps[[j]] <- gsub("www", " ", Apps[[j]]) # www
for (j in 1:n_desc) Apps[[j]] <- gsub("com", " ", Apps[[j]]) # com
inspect(Apps[1:3])

# Remove numbers #
Apps <- tm_map(Apps, removeNumbers) 
inspect(Apps[1:3])

# Remove puncutuations and symbols #
Apps <- tm_map(Apps, removePunctuation) 
inspect(Apps[1:3])

# Remove extra white space #
Apps <- tm_map(Apps, stripWhitespace)
inspect(Apps[1:3])

# Return first 10 description after parsing #
inspect(Apps[1:10])

##--- Stemming ---##

Apps <- tm_map(Apps, PlainTextDocument)  # Remove common word endings ("es", "ed", "s", "ing")
Apps <- tm_map(Apps, stemDocument)

as.character(Apps[[1]])

##--- Create a DTM without restrictions ---##

dtm_Apps <- DocumentTermMatrix(Apps)
dtm_Apps
inspect(dtm_Apps)

##--- Create a DTM with restrictions ---##

dtm_Apps_Ctrl <- DocumentTermMatrix(Apps, control=list(wordLength=c(3,10), bounds=list(global=c(20,500)))) #terms with 3-10 chars in 50-500 app desc
dtm_Apps_Ctrl

# Number of terms selected #
# Sparsity Rate of Terms #
inspect(dtm_Apps_Ctrl[1:10,1:10]) # DTM for the first 10 descriptions with the first 10 terms

##--- Frequency of terms ---##

# Find the terms that occur at least 20 times #
findFreqTerms(dtm_Apps_Ctrl, 20)

# Frequency Table #
Freq_term <-colSums(as.matrix(dtm_Apps_Ctrl))
Order_Freq_term <- order(Freq_term, decreasing = TRUE)
Freq_term[Order_Freq_term]

# Frequency Diagram #
library(grDevices); # for colours  
Apps_DTM_DF = as.data.frame(as.matrix(dtm_Apps_Ctrl))
numwords <- 20; # the most frequent 20 terms  

# sum each column and sort by descending order # 
Terms_Freq <- as.matrix(sort(sapply(Apps_DTM_DF, FUN=sum),decreasing=TRUE)[1:numwords], colnames=count)
x <- sort(Terms_Freq[1:numwords,], decreasing=FALSE) 
barplot(x, horiz=TRUE, cex.names=0.5, space=1, las=1, col=grey.colors(10), main="Frequency of Terms")

## Word Cloud ##
# For Original DTM #
set.seed(2406) #set the same seed each time ensures consistent look across clouds
m <- as.matrix(t(dtm_Apps)) # Convert it to a matrix
v <- sort(rowSums(m),decreasing=TRUE) # Sort the terms in a descending order 
w <- data.frame(word = names(v),freq=v) # Create a data frame indicating the name&frequency of terms

WC_color <- brewer.pal(8,"Set2")
wordcloud(w$word,w$freq, scale=c(3,.1),min.freq=1, max.words=200, random.order=F, rot.per=.3, colors=WC_color)

# For DTM with Controls #
dev.off()
set.seed(2406) #set the same seed each time ensures consistent look across clouds
m <- as.matrix(t(dtm_Apps_Ctrl)) # Convert it to a matrix
v <- sort(rowSums(m),decreasing=TRUE) # Sort the terms in a descending order 
w <- data.frame(word = names(v),freq=v) # Create a data frame indicating the name&frequency of terms

WC_color <- brewer.pal(8,"Set2")
wordcloud(w$word,w$freq, scale=c(3,.1),min.freq=1, max.words=200, random.order=F, rot.per=.3, colors=WC_color)

### Clustering ###

##--- Data Preparation ---##
## Select the number of Terms for Clustering ##
dtm_Apps_Sparse <- removeSparseTerms(dtm_Apps_Ctrl, 0.93) # Remove sparse terms with sparsity larger than 93%

dtm_Apps_Sparse
dimnames(dtm_Apps_Sparse)$Terms # Display the selected terms

dtm_Apps_cluster <- as.matrix(dtm_Apps_Sparse)

##--- K_Mean Clustering ---##

# Model 1 - Seed = 1234 #
set.seed(1234)
Apps_KM1 <- kmeans(t(dtm_Apps_cluster),3) #Set K = 3
Apps_KM1 #Run K-mean
Apps_KM1$size
sort(Apps_KM1$cluster)

# Model 2 - Seed = 2406 #
set.seed(2406)
Apps_KM2 <- kmeans(t(dtm_Apps_cluster),3) #Set K = 3
Apps_KM2 #Run K-mean
Apps_KM2$size
sort(Apps_KM2$cluster)

set.seed(2406)
wss <- 1:5
for( i in 1:5) {wss[i] <- sum(kmeans(t(dtm_Apps_cluster),i)$withinss)}
plot(1:5, wss[1:5], type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")
#type ="b" creates a plot with lines between points #
wss

##--- Hierachical Clustering ---##
dtm_Apps_cluster <- as.matrix(dtm_Apps_Sparse)

#Calculate the Distance between terms#
distance <- dist(t(dtm_Apps_cluster), method="euclidean")

#Present Distance Matrix#
distance

# Model 3 - Complete Max Distance#
Apps_HC <- hclust(distance,method="complete")
plot(Apps_HC)
rect.hclust(Apps_HC,k=3,border="blue")

Apps_HC_Cut <- cutree(Apps_HC, k=3) #cut tree into 3 clusters
Apps_HC_Cut

sort(Apps_HC_Cut)

# Model 4 - Ward's Method#
Apps_HC2 <- hclust(distance,method="ward.D")
plot(Apps_HC2)
rect.hclust(Apps_HC2,k=3,border="blue")

Apps_HC_Cut2 <- cutree(Apps_HC2, k=3) #cut tree into 3 clusters
Apps_HC_Cut2

sort(Apps_HC_Cut2)

### Regression ###

## Data Preparation ##
# Count the number of terms in a description ##
library(stringr)
Num_Terms <- matrix(data = 0, n_desc,1)

for(j in 1:n_desc){
  str1 <- Apps[[j]]
  str2 <- str_match_all(str1,"\\S+")
  Num_Terms[j] <- length(str2[[1]])
}

head(Num_Terms)

## Compute Cluster Scores ##
# Identify the terms for each cluster #

# K-means
cluster1 <- dtm_Apps_cluster[,c("percent","sale","limit","nintyninec")]
cluster2 <- dtm_Apps_cluster[,c("arcad","featur","fun","level","avail","hit","experi","take","ever","like")]
cluster3 <- dtm_Apps_cluster[,c("numberon","download","million","top")]
# complete
cluster1 <- dtm_Apps_cluster[,c("limit","nintyninec")]
cluster2 <- dtm_Apps_cluster[,c("arcad","top","featur","fun","level","avail","hit","experi","take","ever","like","numberon","download","million")]
cluster3 <- dtm_Apps_cluster[,c("percent","sale")]
# ward.D
cluster1 <- dtm_Apps_cluster[,c("percent","sale","limit","nintyninec")]
cluster2 <- dtm_Apps_cluster[,c("arcad","top","featur","fun","level","avail","hit","experi","take","ever","like")]
cluster3 <- dtm_Apps_cluster[,c("numberon","download","million")]


head(cluster1)
head(cluster2)
head(cluster3)

# Sums #
C1_Sum <- rowSums(cluster1)
C2_Sum <- rowSums(cluster2)
C3_Sum <- rowSums(cluster3)
C4_Sum <- Num_Terms - (C1_Sum + C2_Sum + C3_Sum)

# Create a Score table #
Score <- matrix(data = 0, n_desc,4)
Score[,1] <- as.matrix(C1_Sum)
Score[,2] <- as.matrix(C2_Sum)
Score[,3] <- as.matrix(C3_Sum)
Score[,4] <- as.matrix(C4_Sum)

# Name the Columns/Clusters #
colnames(Score) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4")
head(Score)

## Add a Score matrix to the original Data ##
Apps_new <- cbind(Apps_data, Score)
str(Apps_new)

summary(Apps_new[,c('Rank','Price','Screenshot','Size','StarCurrentVersion','RatingCurrentVersion','Cluster1','Cluster2','Cluster3','Cluster4')])

# Variable Transformation #
Sales <- -log(Apps_new$Rank)
Log_Rating_Num <- log(Apps_new$RatingCurrentVersion+1)

Apps_new <- cbind(Apps_new,Sales)
Apps_new <- cbind(Apps_new, Log_Rating_Num)

Apps_Reg <- lm(Sales ~ Price + Screenshot + Size + StarCurrentVersion + Log_Rating_Num + Cluster1 + Cluster2+ Cluster3 + Cluster4,data = Apps_new)
summary(Apps_Reg)



