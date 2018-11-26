#Removing old variables
rm(list = ls())              # Remove everything from your current workspace
ls()                         # Anything there? Nope.
getwd()                      # Check your working directory

#Setting working directory
setwd("~/Desktop/Yasin/Data Engineering/Data Analytics/10th Assignment")
getwd()

#Installing all packages
install.packages("rvest")
install.packages('stringi', configure.args='--disable-cxx11')
install.packages("magrittr")
library(magrittr)
library(xml2)
library(rvest)
library(NLP)
library(tm)

#1. Convert the html to text files and separate the individual news items. The individual press
#release items serve as documents.

#Install 2000-2008 news
url_2008 <- "https://www.jacobs-university.de/press-release-archive-2008"

text_2008 <- read_html(url_2008) %>% 
  html_nodes(xpath = "//font[@class='list']") %>% 
  html_text()
#Install 2009-2015 news
url_2015 <- "https://www.jacobs-university.de/press-release-archive-2015"

text_2015 <- read_html(url_2015) %>% 
  html_nodes(xpath = "//div[@class='news-result clear-block']") %>% 
  html_text()

#Binding two vectors
bind0815 <- c(text_2008, text_2015)
str(bind0815)
length(bind0815)

#library(data.table)
#library(stringr)
#bind0815_df <- data.frame(bind0815)
#bind0815_df[1]
#as.numeric(sapply(strsplit(years, " "), "[[", 1))

#Check Missing Values
length(which(!complete.cases(bind0815df)))

#Text length
library(stringr)
str(bind0815df)

#Creating Corpus- For binding vector
doc.vec <- VectorSource(bind0815)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)
inspect(doc.corpus[15])

#[1] Eine Reise durchs Universum: Jacobs University feiert den 250. Geburtstag des Astronomen Heinrich Wilhelm Olbers Oct 15,
#2008 Am 18. Oktober 2008 lädt die Jacobs University Bremen anlässlich des 250. Jahrestages von Heinrich Wilhelm Olbers’ 
#Geburtstag herzlich ein zu einer „Reise durchs Universum“ mit populärwissenschaftlichen Vorträgen über den berühmten 
#Bremer Astronomen und die Wissenschaftswelt seiner Zeit sowie spannende aktuelle Forschungsthemen aus Astronomie 
#und Astrophysik. Der Eintritt für die deutschsprachige Veranstaltung ist frei. more ... »

#before 2008
doc.vec1 <- VectorSource(text_2008)
doc.corpus1 <- Corpus(doc.vec1)
summary(doc.corpus1)
inspect(doc.corpus1[15])

#before 2015
doc.vec2 <- VectorSource(text_2015)
doc.corpus2 <- Corpus(doc.vec2)
summary(doc.corpus2)
inspect(doc.corpus2[15])

#2. Remove stop words and perform stemming.
library(SnowballC)
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("german"))
#stemming
doc.corpus <- tm_map(doc.corpus, stemDocument)
#These transformations causes white spaces now remove them
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
#See the same text after amandment
inspect(doc.corpus[15])
#reis durch universum jacob univers feiert geburtstag astronomen heinrich wilhelm
#olber oct oktob lädt jacob univers bremen anlässlich jahrestag heinrich wilhelm
#olbers’ geburtstag herzlich „reis durch universum“ populärwissenschaftlichen
#vorträgen berühmten bremer astronomen wissenschaftswelt zeit sowi spannend
#aktuell forschungsthemen astronomi astrophysik eintritt deutschsprachig veranstaltung frei

#before 2008
doc.corpus1 <- tm_map(doc.corpus1, content_transformer(tolower))
doc.corpus1 <- tm_map(doc.corpus1, removePunctuation)
doc.corpus1 <- tm_map(doc.corpus1, removeNumbers)
doc.corpus1 <- tm_map(doc.corpus1, removeWords, stopwords("english"))
doc.corpus1 <- tm_map(doc.corpus1, removeWords, stopwords("german"))
#stemming
doc.corpus1 <- tm_map(doc.corpus1, stemDocument)
#These transformations causes white spaces now remove them
doc.corpus1 <- tm_map(doc.corpus1, stripWhitespace)
inspect(doc.corpus1[15])
#before 2015

doc.corpus2 <- tm_map(doc.corpus2, content_transformer(tolower))
doc.corpus2 <- tm_map(doc.corpus2, removePunctuation)
doc.corpus2 <- tm_map(doc.corpus2, removeNumbers)
doc.corpus2 <- tm_map(doc.corpus2, removeWords, stopwords("english"))
doc.corpus2 <- tm_map(doc.corpus2, removeWords, stopwords("german"))
#stemming
doc.corpus2 <- tm_map(doc.corpus2, stemDocument)
#These transformations causes white spaces now remove them
doc.corpus2 <- tm_map(doc.corpus2, stripWhitespace)
inspect(doc.corpus2[15])
#3. Perform a frequency analysis to compute the term-document (TD) matrix. What are the
#most common terms?

TDM <- TermDocumentMatrix(doc.corpus)
TDM
#TermDocumentMatrix (terms: 9210, documents: 639)>>
#Non-/sparse entries: 26361/5858829
#Sparsity           : 100%
#Maximal term lgth: 41
#Weighting          : term frequency (tf)

tdmmatrix <- as.matrix(TDM)
tdmmatrix
dim(tdmmatrix)
dtmmatrix<- t(tdmmatrix)
dim(dtmmatrix)
#Transpose of above
DTM <- DocumentTermMatrix(doc.corpus)
inspect(DTM[1:10,1:10])

#Most frequest words
findFreqTerms(TDM, 100)

#The words more than hundred times
#> findFreqTerms(TDM, 100)
# [1] "euro"        "jacob"       "univers"     "bremen"
#"bremer"      "professor"   "universität" "intern"      "jahr"       
#[10] "neue"        "–"           "campus"      "iub" 
findAssocs(TDM, "jacob", 0.5)
findAssocs(TDM, "euro", 0.5)

#before 2008
TDM1 <- TermDocumentMatrix(doc.corpus1)
TDM1
tdmmatrix1 <- as.matrix(TDM1)
tdmmatrix1
dim(tdmmatrix1)
dtmmatrix1<- t(tdmmatrix1)
dim(dtmmatrix1)
#Transpose of above
DTM1 <- DocumentTermMatrix(doc.corpus1)
inspect(DTM1[1:10,1:10])

#Most frequest words
findFreqTerms(TDM1, 100)
#before 2015
TDM2 <- TermDocumentMatrix(doc.corpus2)
TDM2
tdmmatrix2 <- as.matrix(TDM2)
tdmmatrix2
dim(tdmmatrix2)
dtmmatrix2<- t(tdmmatrix2)
dim(dtmmatrix2)
#Transpose of above
DTM2 <- DocumentTermMatrix(doc.corpus2)
inspect(DTM2[1:10,1:10])
#Most frequest words
findFreqTerms(TDM2, 100)

#4-Compute inverse-document frequency (IDF) and term importance (TI). What are now the
#most common terms?

# Our function for calculating relative term frequency (TF)
term.frequency <- function(row) {
  row / sum(row)
}

# Our function for calculating inverse document frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}

# Our function for calculating TF-IDF.
tf.idf <- function(tf, idf) {
  tf * idf
}

# First step, normalize all documents via TF.
tokens.df <- apply(TDM, 1, term.frequency)
dim(tokens.df)
View(tokens.df[1:20, 1:100])

# Second step, calculate the IDF vector that we will use - both
# for training data and for test data!
tokens.idf <- apply(TDM, 2, inverse.doc.freq)
str(tokens.idf)

tokens.idf
# Lastly, calculate TF-IDF for our corpus.
tokens.tfidf <-  apply(tokens.df, 2, tf.idf, idf = tokens.idf)
dim(tokens.tfidf)
View(tokens.tfidf[1:25, 1:25])


Terms<- colSums(tokens.tfidf)
library(reshape2)
commterms <- subset(melt(Terms), value >2.9)
commterms

#> commterms
#value
#farbstoff                    3.010017
#komplexierung                3.010017
#kürbismanschett              3.010017
#manschettenförmigen          3.010017
#rhodaminfluoreszenzfarbstoff 3.010017
#stabilisiert                 3.010017
#kooperationsvereinbarung     2.922867
#nordwesten march             2.922867
#schließen                    2.922867
#synergien                    2.922867
#spatenstich                  3.010017
#technologiepark              3.010017

#Before 2008
# First step, normalize all documents via TF.
tokens.df1 <- apply(TDM1, 1, term.frequency)
dim(tokens.df1)
View(tokens.df1[1:20, 1:100])

# Second step, calculate the IDF vector that we will use - both
# for training data and for test data!
tokens.idf1 <- apply(TDM1, 2, inverse.doc.freq)
str(tokens.idf1)

tokens.idf1
# Lastly, calculate TF-IDF for our corpus.
tokens.tfidf1 <-  apply(tokens.df1, 2, tf.idf, idf = tokens.idf1)
dim(tokens.tfidf1)
View(tokens.tfidf1[1:25, 1:25])


Terms1<- colSums(tokens.tfidf1)
library(reshape2)
commterms1 <- subset(melt(Terms1), value >2.6)
commterms1


#Before 2015
# First step, normalize all documents via TF.
tokens.df2 <- apply(TDM2, 1, term.frequency)
dim(tokens.df2)
View(tokens.df2[1:20, 1:100])

# Second step, calculate the IDF vector that we will use - both
# for training data and for test data!
tokens.idf2 <- apply(TDM2, 2, inverse.doc.freq)
str(tokens.idf2)

tokens.idf2
# Lastly, calculate TF-IDF for our corpus.
tokens.tfidf2 <-  apply(tokens.df2, 2, tf.idf, idf = tokens.idf2)
dim(tokens.tfidf2)
View(tokens.tfidf2[1:25, 1:25])


Terms2<- colSums(tokens.tfidf2)
library(reshape2)
commterms2 <- subset(melt(Terms2), value >2.7)
commterms2

#5-Compute pairwise cosine and Euclidean distance between all documents.
#Cosine Similarity
library(SnowballC)
library(lsa)
library(ggplot2)
library(text2vec)

#Cosine Distance
cos.dist <- dist2( dtmmatrix,method = c("cosine"))

#Euclidean Distance
euc.dist <- dist2(dtmmatrix, method = c("euclidean"))

#Before 2008
#Cosine Distance
cos.dist1 <- dist2( dtmmatrix1,method = c("cosine"))
#Euclidean Distance
euc.dist1 <- dist2(dtmmatrix1, method = c("euclidean"))

#Before 2015
#Cosine Distance
cos.dist2 <- dist2( dtmmatrix2,method = c("cosine"))
#Euclidean Distance
euc.dist2 <- dist2(dtmmatrix2, method = c("euclidean"))

#6-Apply a multi-dimensional scaling approach to the distance matrix and render a 2D scatter-
#plot. Compare the two distance metrics.
#Cos MDS
fitcos <- cmdscale(cos.dist,eig=TRUE, k=2) # k is the number of dim
fitcos # view results
x <- fitcos$points[,1]
y <- fitcos$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS-Cosine-Union Set", type="n")
text(x, y, labels = row.names(dtmmatrix), cex=.7)
#Euclidean MDS
fiteuc <- cmdscale(euc.dist,eig=TRUE, k=2) # k is the number of dim
fiteuc # view results
x1 <- fiteuc$points[,1]
y1 <- fiteuc$points[,2]
plot(x1, y1, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS-Euclidean-Union Set", type="n")
text(x1, y1, labels = row.names(dtmmatrix), cex=.7)

#before2008
#Cos MDS
fitcos1 <- cmdscale(cos.dist1,eig=TRUE, k=2) # k is the number of dim
fitcos1 # view results
x <- fitcos1$points[,1]
y <- fitcos1$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS-Cosine-Before 2008", type="n")
text(x, y, labels = row.names(dtmmatrix1), cex=.7)
#Euclidean MDS
fiteuc1 <- cmdscale(euc.dist1,eig=TRUE, k=2) # k is the number of dim
fiteuc # view results
x1 <- fiteuc1$points[,1]
y1 <- fiteuc1$points[,2]
plot(x1, y1, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS-Euclidean-Before 2008", type="n")
text(x1, y1, labels = row.names(dtmmatrix1), cex=.7)

#before2015
#Cos MDS
fitcos2 <- cmdscale(cos.dist2,eig=TRUE, k=2) # k is the number of dim
fitcos2 # view results
x <- fitcos2$points[,1]
y <- fitcos2$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS-Cosine-2008-2015", type="n")
text(x, y, labels = row.names(dtmmatrix2), cex=.7)
#Euclidean MDS
fiteuc2 <- cmdscale(euc.dist2,eig=TRUE, k=2) # k is the number of dim
fiteuc2 # view results
x1 <- fiteuc2$points[,1]
y1 <- fiteuc2$points[,2]
plot(x1, y1, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS-Euclidean-2008-2015", type="n")
text(x1, y1, labels = row.names(dtmmatrix2), cex=.7)


#7-Bonus: Capture the year of release during parsing and color code the scatterplot by time.
#Produce a Word Cloud for each year.

#Sparse Terms
library(RColorBrewer)
library(wordcloud)

sparse_dtm = removeSparseTerms(DTM, 0.90)
sparse_dtm
findFreqTerms(sparse_dtm, 50)
freq = data.frame(sort(colSums(as.matrix(sparse_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

#Before 2008
sparse_dtm1 = removeSparseTerms(DTM1, 0.90)
sparse_dtm1
findFreqTerms(sparse_dtm1, 50)
freq = data.frame(sort(colSums(as.matrix(sparse_dtm1)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

#Before 2015
sparse_dtm2 = removeSparseTerms(DTM2, 0.90)
sparse_dtm2
findFreqTerms(sparse_dtm2, 50)
freq = data.frame(sort(colSums(as.matrix(sparse_dtm2)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))
