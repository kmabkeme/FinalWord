## 2018-06-09
# This file is where the Document-Feature Matrix (created by dfm() in the Quanteda package) gets made
# in order to generate 4-grams, 3-grams, 2-grams, and 1-grams, which are combinations of different numbers of
# adjacent words that can be used to have some predictive power. We organize the n-grams into tables that
# can be massaged into being the backbone of the Final Word word prediction app.
# Part 2 of 3. Start at Week4ScriptForTextProc0609.R before coming here.

## 2018-04-07
## Creating the n-gram data tables
## 2018-05-07, KA - Need to redo the corpora being more careful about the processing of the punctuation and
## stripping out profanity - Also, need to blend the old way of creating the data tables with the new way

library(sourcetools)
library(quanteda)
library(data.table)

setwd("/Users/kristinabkemeier/DataScience/Capstone/2018-06-09")

## Make 4-grams

tweetsCorpus <- readRDS("tweetsCorpus.rds")
# dfm() creates the huge sparse matrix of 4-grams
wordMatTweet4 <- dfm(tweetsCorpus, ngrams=4, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatTweet4, "tweetDFM4.rds")
rm(tweetsCorpus)

## This gives a named vector with the ngrams listed in decreasing order of their count totals
wordVectorTweet4 <- sort(colSums(wordMatTweet4),decreasing=TRUE)
saveRDS(wordVectorTweet4, "tweetVector4.rds")
rm(wordMatTweet4)
rm(wordVectorTweet4)

blogsCorpus <- readRDS("blogsCorpus.rds")
wordMatBlog4 <- dfm(blogsCorpus, ngrams=4, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatBlog4, "blogDFM4.rds")
rm(blogsCorpus)

## This gives a named vector with the ngrams listed in decreasing order of their count totals
wordVectorBlog4 <- sort(colSums(wordMatBlog4),decreasing=TRUE)
saveRDS(wordVectorBlog4, "blogVector4.rds")
rm(wordMatBlog4)
rm(wordVectorBlog4)

newsCorpus <- readRDS("newsCorpus.rds")
wordMatNews4 <- dfm(newsCorpus, ngrams=4, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatNews4, "newsDFM4.rds")
rm(newsCorpus)

## This gives a named vector with the ngrams listed in decreasing order of their count totals
wordVectorNews4 <- sort(colSums(wordMatNews4),decreasing=TRUE)
saveRDS(wordVectorNews4, "newsVector4.rds")
rm(wordMatNews4)

wordVectorTweet4 <- readRDS("tweetVector4.rds")
wordVectorBlog4 <- readRDS("blogVector4.rds")
##wordVectorNews4 <- readRDS("newsVector4.rds") #wordVectorNews was created just above, don't need to read it in

# Filter out the 4-grams that occur only once, which cuts down the number of items dramatically
wordVectorTweet4smaller <- wordVectorTweet4[which(wordVectorTweet4 > 1)]
wordVectorBlog4smaller <- wordVectorBlog4[which(wordVectorBlog4 > 1)]
wordVectorNews4smaller <- wordVectorNews4[which(wordVectorNews4 > 1)]

rm(wordVectorTweet4)
rm(wordVectorBlog4)
rm(wordVectorNews4)

# Combine the totals for identical 4-grams that exist in more than one of the vectors. This takes several minutes.
totVector4 <- c(wordVectorTweet4smaller, wordVectorBlog4smaller, wordVectorNews4smaller)
rm(wordVectorTweet4smaller)
rm(wordVectorBlog4smaller)
rm(wordVectorNews4smaller)

wordVector4 <- tapply(totVector4, names(totVector4), sum)
rm(totVector4)

wordVector4Sorted <- sort(wordVector4, decreasing = TRUE)
rm(wordVector4)

wbv4names <- names(wordVector4Sorted) ## To get the names of the vector entries = n-grams

## The last word in the 4-gram is the predicted word, so we want to break the 4-gram up into a 3-gram and
## a predicted word.
word4 <- sapply(strsplit(wbv4names, "_"), "[[", 4) 

## Actually, I think we need to make the remaining 3-grams and 4-gram by pasting these split strings back
## together, because trying to truncate strings doesn't seem to be too easy
word1 <- sapply(strsplit(wbv4names, "_"), "[[", 1)
word2 <- sapply(strsplit(wbv4names, "_"), "[[", 2)
word3 <- sapply(strsplit(wbv4names, "_"), "[[", 3)
rm(wbv4names)

## Now we create the 4-gram that is the part of the 5-gram leading to the predicted word
wbv4to3names <- paste(word1, word2, word3, sep = "_")

frequency <- wordVector4Sorted
rm(wordVector4Sorted)

## Let's create a table that connects the n-gram to the n-1 grams, and to the count of word4
table4gram <- cbind(wbv4to3names, word1, word2, word3, word4, frequency)

rm(word1)
rm(word2)
rm(word3)
rm(word4)
rm(wbv4to3names)
rm(frequency)
## convert to datatable format - May or may not need to keep ID, let's see if it's useful
## though it could work as a key for joining the n-gram and n-1-gram tables
dt4gram <- as.data.table(table4gram, keep.rownames = "id")
rm(table4gram)

fwrite(dt4gram, file="dt4gram.csv")
##dt4gram <- fread("dt4gram.csv")

## Trying to rank the 4-grams that start with the same 3 words and remove anything that is ranked higher than 5
## because those won't get accessed for any backoff method
dt4gram[, frequency := as.numeric(dt4gram$frequency)]
dt4gram[, grpRank:=rank(-frequency,ties.method="first"),by=c("word1","word2","word3")]
dt4gramTop5 <- dt4gram[grpRank < 6]
fwrite(dt4gramTop5, file="dt4gramTop5.csv")

rm(dt4gram)
rm(dt4gramTop5)

## Next: 3-grams!

tweetsCorpus <- readRDS("tweetsCorpus.rds")
wordMatTweet3 <- dfm(tweetsCorpus, ngrams=3, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatTweet3, "tweetDFM3.rds")
rm(tweetsCorpus)
## This gives a named vector with the ngrams listed in decreasing order of their count totals
wordVectorTweet3 <- sort(colSums(wordMatTweet3),decreasing=TRUE)
saveRDS(wordVectorTweet3, "tweetVector3.rds")
rm(wordMatTweet3)
rm(wordVectorTweet3)

blogsCorpus <- readRDS("blogsCorpus.rds")
wordMatBlog3 <- dfm(blogsCorpus, ngrams=3, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatBlog3, "blogDFM3.rds")
rm(blogsCorpus)
wordVectorBlog3 <- sort(colSums(wordMatBlog3),decreasing=TRUE)
saveRDS(wordVectorBlog3, "blogVector3.rds")
rm(wordMatBlog3)
rm(wordVectorBlog3)

newsCorpus <- readRDS("newsCorpus.rds")
wordMatNews3 <- dfm(newsCorpus, ngrams=3, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatNews3, "newsDFM3.rds")
rm(newsCorpus)
wordVectorNews3 <- sort(colSums(wordMatNews3),decreasing=TRUE)
saveRDS(wordVectorNews3, "newsVector3.rds")
rm(wordMatNews3)

wordVectorTweet3 <- readRDS("tweetVector3.rds")
wordVectorBlog3 <- readRDS("blogVector3.rds")
##wordVectorNews3 <- readRDS("newsVector3.rds")

# The vectors from the 3-grams were small enough in size that my 16Gb RAM could handle it, unlike with the
# 4-grams, which would fail after many minutes. So, no truncation here!
totVector3 <- c(wordVectorTweet3, wordVectorBlog3, wordVectorNews3)
rm(wordVectorTweet3)
rm(wordVectorBlog3)
rm(wordVectorNews3)

## 2018-04-08, KA - the tapply operation on the non-truncated vectors took 45 minutes, but it did complete!
## Went from totVector3 having 50M elements to wordVector3 having almost 45M elements
## 2018-04-15, KA - Only took 30 minutes today, going from 45.6M elements down to 40.6M
## 2018-06-03, KA - Took about 30 minutes today, going from 40.6M elements down to 36.2M
wordVector3 <- tapply(totVector3, names(totVector3), sum)
rm(totVector3)
saveRDS(wordVector3, "wordVector3.rds")

wordVector3 <- readRDS("wordVector3.rds")

wordVector3Sorted <- sort(wordVector3, decreasing = TRUE)
rm(wordVector3)

wbv3names <- names(wordVector3Sorted) ## To get the names of the vector entries = n-grams

## The last word in the 3-gram is the predicted word, so we want to break the 3-gram up into a 2-gram and
## a predicted word.
word4 <- sapply(strsplit(wbv3names, "_"), "[[", 3) 

## Actually, I think we need to make the remaining 1-gram and 2-gram by pasting these split strings back
## together, because trying to truncate strings doesn't seem to be too easy
word2 <- sapply(strsplit(wbv3names, "_"), "[[", 1)
word3 <- sapply(strsplit(wbv3names, "_"), "[[", 2)
rm(wbv3names)

## Now we create the 2-gram that is leading to the predicted word
wbv3to2names <- paste(word2, word3, sep = "_")

frequency <- wordVector3Sorted
rm(wordVector3Sorted)

## Let's create a table that connects the n-gram to the n-1 grams, and to the count of word4
table3gram <- cbind(wbv3to2names, word2, word3, word4, frequency)

rm(word2)
rm(word3)
rm(word4)

rm(wbv3to2names)
rm(frequency)
## convert to datatable format - May or may not need ot keep ID, let's see if it's useful
## though it could work as a key for joining the n-gram and n-1-gram tables
dt3gram <- as.data.table(table3gram, keep.rownames = "id")

fwrite(dt3gram, file="dt3gram.csv")
##dt3gram <- fread("dt3gram.csv")
rm(table3gram)

dt4gram <- fread("dt4gramTop5.csv")

## Now we need to get the total 3gram counts for the 4gram table
tot3gramCountfor4 <- merge(x=dt4gram, y=dt3gram, by.x="wbv4to3names", by.y="id", all.x = TRUE)[]
## get it sorted into the correct descending order to match dt4gram
## Need to sort both on wordVector4Sorted AND wbv4to3names
dt4gram <- dt4gram[order(-frequency, wbv4to3names)]
tot3gramCountfor4 <- tot3gramCountfor4[order(-frequency.x, wbv4to3names)]
## Now we have the total counts to calculate stupid backoff ratios
dt4gram[, tot3gramCt := as.numeric(tot3gramCountfor4$frequency.y)]
dt4gramTop5 <- dt4gram[grpRank < 6]

## We need to save dt4gram again, now that we have the tot3gramCt column
fwrite(dt4gramTop5, file="dt4gramTop5Final.csv")
rm(dt4gram)
rm(dt4gramTop5)
rm(tot3gramCountfor4)


## Trying to rank the 3-grams that start with the same 2 words and remove anything that is ranked higher than 5
## because those won't get accessed for any backoff method
dt3gram[, frequency := as.numeric(dt3gram$frequency)]
## 2018-06-03, KA - This ranking step took about 45 minutes to complete
dt3gram[, grpRank:=rank(-frequency,ties.method="first"),by=c("word2","word3")]
dt3gramTop5 <- dt3gram[grpRank < 6]
fwrite(dt3gramTop5, file="dt3gramTop5.csv")

rm(dt3gram)
rm(dt3gramTop5)

## Next: 2-grams!

tweetsCorpus <- readRDS("tweetsCorpus.rds")
wordMatTweet2 <- dfm(tweetsCorpus, ngrams=2, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatTweet2, "tweetDFM2.rds")
rm(tweetsCorpus)
## This gives a named vector with the ngrams listed in decreasing order of their count totals
wordVectorTweet2 <- sort(colSums(wordMatTweet2),decreasing=TRUE)
saveRDS(wordVectorTweet2, "tweetVector2.rds")
rm(wordMatTweet2)
rm(wordVectorTweet2)

blogsCorpus <- readRDS("blogsCorpus.rds")
wordMatBlog2 <- dfm(blogsCorpus, ngrams=2, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatBlog2, "blogDFM2.rds")
rm(blogsCorpus)
wordVectorBlog2 <- sort(colSums(wordMatBlog2),decreasing=TRUE)
saveRDS(wordVectorBlog2, "blogVector2.rds")
rm(wordMatBlog2)
rm(wordVectorBlog2)

newsCorpus <- readRDS("newsCorpus.rds")
wordMatNews2 <- dfm(newsCorpus, ngrams=2, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatNews2, "newsDFM2.rds")
rm(newsCorpus)
wordVectorNews2 <- sort(colSums(wordMatNews2),decreasing=TRUE)
saveRDS(wordVectorNews2, "newsVector2.rds")
rm(wordMatNews2)

wordVectorTweet2 <- readRDS("tweetVector2.rds")
wordVectorBlog2 <- readRDS("blogVector2.rds")
##wordVectorNews2 <- readRDS("newsVector2.rds")

totVector2 <- c(wordVectorTweet2, wordVectorBlog2, wordVectorNews2)
rm(wordVectorTweet2)
rm(wordVectorBlog2)
rm(wordVectorNews2)

wordVector2 <- tapply(totVector2, names(totVector2), sum)
rm(totVector2)
saveRDS(wordVector2, "wordVector2.rds")

wordVector2 <- readRDS("wordVector2.rds")

wordVector2Sorted <- sort(wordVector2, decreasing = TRUE)
rm(wordVector2)

wbv2names <- names(wordVector2Sorted) ## To get the names of the vector entries = n-grams

## The last word in the 2-gram is the predicted word, so we want to break the 2-gram up into a 1-gram and
## a predicted word.
word4 <- sapply(strsplit(wbv2names, "_"), "[[", 2) 

## Actually, I think we need to make the remaining 1-gram and 2-gram by pasting these split strings back
## together, because trying to truncate strings doesn't seem to be too easy
word3 <- sapply(strsplit(wbv2names, "_"), "[[", 1)
rm(wbv2names)

## Now we create the 1-gram that is leading to the predicted word
wbv2to1names <- word3

frequency <- wordVector2Sorted
rm(wordVector2Sorted)

## Let's create a table that connects the n-gram to the n-1 grams, and to the count of predicted word4
table2gram <- cbind(wbv2to1names, word3, word4, frequency)

rm(word3)
rm(word4)
rm(wbv2to1names)
rm(frequency)
## convert to datatable format - May or may not need ot keep ID, let's see if it's useful
## though it could work as a key for joining the n-gram and n-1-gram tables
dt2gram <- as.data.table(table2gram, keep.rownames = "id")

fwrite(dt2gram, file="dt2gram.csv")
#dt2gram <- fread("dt2gram.csv")
rm(table2gram)

dt3gram <- fread("dt3gramTop5.csv")

## Now we need to get the total 2gram counts for the 3gram table
tot2gramCountfor3 <- merge(x=dt3gram, y=dt2gram, by.x="wbv3to2names", by.y="id", all.x = TRUE)[]
## get it sorted into the correct descending order to match dt3gram
## Need to sort both on wordVector3Sorted AND wbv3to2names
dt3gram <- dt3gram[order(-frequency, wbv3to2names)]
tot2gramCountfor3 <- tot2gramCountfor3[order(-frequency.x, wbv3to2names)]
## Now we have the total counts to calculate stupid backoff ratios
dt3gram[, tot2gramCt := as.numeric(tot2gramCountfor3$frequency.y)]

## We need to save dt3gram again, now that we have the tot2gramCt column
fwrite(dt3gram, file="dt3gramTop5Final.csv")
rm(dt3gram)
rm(tot2gramCountfor3)

dt2gram[, frequency := as.numeric(dt2gram$frequency)]
dt2gram[, grpRank:=rank(-frequency,ties.method="first"),by="word3"]
dt2gramTop5 <- dt2gram[grpRank < 6]
fwrite(dt2gramTop5, file="dt2gramTop5.csv")

rm(dt2gram)
rm(dt2gramTop5)

## Finally, 1-grams.

tweetsCorpus <- readRDS("tweetsCorpus.rds")
wordMatTweet1 <- dfm(tweetsCorpus, ngrams=1, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatTweet1, "tweetDFM1.rds")
rm(tweetsCorpus)
## This gives a named vector with the ngrams listed in decreasing order of their count totals
wordVectorTweet1 <- sort(colSums(wordMatTweet1),decreasing=TRUE)
saveRDS(wordVectorTweet1, "tweetVector1.rds")
rm(wordMatTweet1)

blogsCorpus <- readRDS("blogsCorpus.rds")
wordMatBlog1 <- dfm(blogsCorpus, ngrams=1, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatBlog1, "blogDFM1.rds")
rm(blogsCorpus)
wordVectorBlog1 <- sort(colSums(wordMatBlog1),decreasing=TRUE)
saveRDS(wordVectorBlog1, "blogVector1.rds")
rm(wordMatBlog1)

newsCorpus <- readRDS("newsCorpus.rds")
wordMatNews1 <- dfm(newsCorpus, ngrams=1, remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE,remove_separators = TRUE,remove_twitter=TRUE, remove_hyphens=FALSE)
saveRDS(wordMatNews1, "newsDFM1.rds")
rm(newsCorpus)
wordVectorNews1 <- sort(colSums(wordMatNews1),decreasing=TRUE)
saveRDS(wordVectorNews1, "newsVector1.rds")
rm(wordMatNews1)

totVector1 <- c(wordVectorTweet1, wordVectorBlog1, wordVectorNews1)
rm(wordVectorTweet1)
rm(wordVectorBlog1)
rm(wordVectorNews1)

wordVector1 <- tapply(totVector1, names(totVector1), sum)
rm(totVector1)
saveRDS(wordVector1, "wordVector1.rds")

wordVector1 <- readRDS("wordVector1.rds")

wordVector1Sorted <- sort(wordVector1, decreasing = TRUE)
rm(wordVector1)

frequency <- wordVector1Sorted
rm(wordVector1Sorted)

## dt1gram is the end of the line for predictions, so we don't need any other columns
## only the words and their counts
dt1gram <- as.data.table(frequency, keep.rownames = "id")

fwrite(dt1gram, file="dt1gram.csv")
dt1gram <- fread("dt1gram.csv")

dt2gramTop5 <- fread("dt2gramTop5.csv")

## Now we need to get the total 1gram counts for the 2gram table
tot1gramCountfor2 <- merge(x=dt2gramTop5, y=dt1gram, by.x="wbv2to1names", by.y="id", all.x = TRUE)[]
## get it sorted into the correct descending order to match dt2gram
## Need to sort both on wordVector2Sorted AND wbv2to1names
dt2gramTop5 <- dt2gramTop5[order(-frequency, wbv2to1names)]
tot1gramCountfor2 <- tot1gramCountfor2[order(-frequency.x, wbv2to1names)]
## Now we have the total counts to calculate stupid backoff ratios
dt2gramTop5[, tot1gramCt := as.numeric(tot1gramCountfor2$frequency.y)]

## We need to save dt2gram again, now that we have the tot1gramCt column
fwrite(dt2gramTop5, file="dt2gramTop5Final.csv")

