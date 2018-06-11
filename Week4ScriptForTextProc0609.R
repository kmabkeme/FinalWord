# Kristin Abkemeier
## 2018-06-09
# Start here to generate the lookup tables from the sets of tweets, blogs, and news items provided by SwiftKey.
# Part 1 of 3.

library(sourcetools)
library(quanteda)
library(stringr)
setwd("/Users/kristinabkemeier/DataScience/Capstone/2018-06-09")

## 2018-05-20, KA - Remember that it takes about 10 minutes to go through all of these gsub() calls now,
## because there are so many of them
removeProfanity <- function(str) {

  ## 2018-06-03, KA - Trying to remove ..., but escaping the character so I don't remove EVERYTHING.
  str <- gsub("\\.\\.\\.", "", str, ignore.case = TRUE)
  ##2018-06-09, KA - We want to strip off any punctuation that might be in the middle of what appears to be
  ## a single word: a comma without a space after it, a semicolon 
  str <- gsub("[,;=]+"," ", str) 
  
  ## Get rid of asterisks that might be used as bullets, though I think these get stripped out by
  # Quanteda anyway
  str <- gsub("\\*"," ",str)
  ## 2018-05-01, KA - Need to get rid of unhelpful word combos that exist because people are fallible
  ## typists but which do not represent how people actually intend to communicate
  str <- gsub("\\ba[ a]+\\b","a ",str)

  ## 2018-06-09, KA - Ampersands and percent signs can be useful
  str <- gsub("&"," and ",str)
  str <- gsub("%"," percent ",str)

  ## 2018-06-09, KA - It turns out that if we're not going to predict expletives, and if they just take up a
  ## space that a more useful word could do, then we might as well keep them out
  str <- gsub("[a-z0-9]*fuc[ck][a-z]*","",str)
  str <- gsub("[a-z]*f[*][*]k[a-z]*","",str)
  str <- gsub("[a-z]*f[c]*uk[s]*","",str)
  str <- gsub("[a-z]*phuk[a-z]*","",str)
  str <- gsub("[a-z]*m[0o]f[0o][a-z]*","",str)
  str <- gsub("jerk[-]?[0o]ff[a-z]*","",str)
  str <- gsub("\\ba[s$*][s$*]+[a-z]*","",str)
  str <- gsub("\\bcunt[a-z]*\\b","",str)
  str <- gsub("\\bc[o0]ck[s]*\\b","",str)
  str <- gsub("\\b[ck]um[sz]*\\b","",str)
  str <- gsub("[a-z]*b[i1\\*]tch[a-z]*","",str)
  str <- gsub("\\barse[a-z]*","",str)
  str <- gsub("\\bcr[a@p]\\b","",str)
  str <- gsub("[a-z]*damn[a-z]*","",str)
  str <- gsub("\\bn[i1]gg[a4]*","",str)
  str <- gsub("\\bn[i1]gg[e3]r*","",str)
  str <- gsub("\\b(bull)*[s$]h[i1]+t[a-z0-9]*","",str)
  str <- gsub("\\btwat[a-z]*","",str)
  str <- gsub("\\bt[i1]ts?t?[a-z]*","",str)
  str <- gsub("\\bwan[gk]+[a-z0-9]*","",str)
  str <- gsub("\\bb[o0][o0]+b[a-z]*","",str)
  str <- gsub("\\bc[o0][o0]+n[a-z]*","",str)

  ## This one is for any string that starts with something that is not a number and ends with a number
  str <- gsub("\\b[^0-9]+[0-9]+\\b"," kanumber",str) 
  ## Just a number
  str <- gsub("\\b[0-9]+\\b","kanumber",str) 
  str <- gsub("\\b(\\$|\\£).*\\b", "kamoney", str)
  ## 2018-06-09, KA - kaphone never appeared in my lookup table anyway
  ##str <- gsub("\\b[0-9]{3}\\-[0-9]{3}\\-[0-9]{4}\\b", "kaphone", str)
  str <- gsub("\\b[0-9]+[ap]+\\.*[m]*\\.*\\b"," katime",str) 

  ## Remove entire URLs, not just the ones that start with http(s) (as Quanteda does)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.fr/*[-a-zA-Z0-9]*","kaurl",str) 
  str <- gsub("[-a-zA-Z0-9\\.]*\\.com/*[-a-zA-Z0-9]*","kaurl",str) 
  str <- gsub("[-a-zA-Z0-9\\.]*\\.edu/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.net/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.org/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.biz/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.gov/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.co\\.uk/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.ac\\.uk/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.jp/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.de/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.ca/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.it/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*\\.php/*[-a-zA-Z0-9]*","kaurl",str)
  str <- gsub("[-a-zA-Z0-9\\.]*bit\\.ly[-a-zA-Z0-9]*","kaurl",str)
  
  ## Also, we need to get rid of extra spaces between words, because otherwise we get blank 1-grams
  str <- gsub("\\s+"," ", trimws(str))
}

cleanBeforeMakingList <- function(str) {
  ## Convert some non-ASCII characters that we want to treat with more subtlety before we do the wholesale
  ## conversion to ASCII. I don't want to screw up smart apostrophes and words like "résumé."
  str <- gsub("é", "e", str, ignore.case = TRUE)
  str <- gsub("è", "e", str, ignore.case = TRUE)
  ## 2018-06-09, KA - Handle the diaeresis, in case a New Yorker editor uses this app
  str <- gsub("ä", "a", str, ignore.case = TRUE)
  str <- gsub("ë", "e", str, ignore.case = TRUE)
  str <- gsub("ï", "i", str, ignore.case = TRUE)
  str <- gsub("ö", "o", str, ignore.case = TRUE)
  str <- gsub("ô", "o", str, ignore.case = TRUE)
  str <- gsub("ü", "u", str, ignore.case = TRUE)
  str <- gsub("û", "u", str, ignore.case = TRUE)
  str <- gsub("ŏ", "o", str, ignore.case = TRUE)
  str <- gsub("ç", "c", str, ignore.case = TRUE)
  str <- gsub("ñ", "n", str, ignore.case = TRUE)
  ## This a with circumflex appears to be a substitute for an apostrophe in the news corpus
  str <- gsub("â", "'", str, ignore.case = TRUE)
  
  ## 2018-04-23 - Change curly apostrophe to a straight ASCII apostrophe
  str <- gsub("’", "'", str, ignore.case = TRUE)
  ## 2018-05-27, KA - Clean out any non-ASCII characters - no point in doing any further processing on those.
  ## Except some of these characters may be separating words, and some may be within words.
  str <- iconv(str, "latin1", "ASCII", sub=" ")
  
  ## Get rid of the entire retweet header, with or without the colon
  str <- str_replace(str,"rt @[a-zA-Z0-9_]*:*","")
  ## Get rid of Twitter hashtags and such
  str <- str_replace_all(str,"#\\w*","")
  str <- str_replace_all(str,"@\\w*","")

  
  # Get rid of URLs
  str <- str_replace_all(str, "http://t.co/[a-z,A-Z,0-9]{10}","")
  str <- str_replace_all(str, "http[:]*[[:punct:]]*[a-z,A-Z,0-9]*","")
  
  ## 2018-06-09, KA - This fails where "ain't" is concerned, so skip it
  ## For kickin' and screamin' -> kicking and screaming
  ##str <- str_replace_all(str,"in'","ing")
}

# Preserving the code for doing the raw file cleaning
# just in case I need to go back and clean the initial raw files again.

## Let's test out on a small Twitter sample table first
##r = readBin("/Users/kristinabkemeier/DataScience/Capstone/final/en_US/en_US.twitter_sample_small.txt", raw(), 
##            file.info("/Users/kristinabkemeier/DataScience/Capstone/final/en_US/en_US.twitter_sample_small.txt")$size)
##r[r==as.raw(0)] = as.raw(0x20) 
##writeBin(r, "/Users/kristinabkemeier/DataScience/Capstone/final/en_US/en_US.twitter_sample_small_clean.txt")
##tweets <- read_lines("/Users/kristinabkemeier/DataScience/Capstone/final/en_US/en_US.twitter_sample_small_clean.txt")

# First, convert the tweets from a big text file into a tweets corpus.
## 2018-04-14, KA - I've split the data sets into training and test sets, so we read in the train data only
## to generate our tables
tweets <- read_lines("/Users/kristinabkemeier/DataScience/Capstone/final/en_US/en_US.twitter_clean_train.txt")

## 2018-06-03, KA - Need to clean out the ASCII characters before making it into a list, otherwise I will
## wipe out all but the first row of the list if I try to do it in removeProfanity()
tweetsCleanFirst <- cleanBeforeMakingList(tweets)

## Lowercase all words
tweetsList <- as.list(tolower(tweetsCleanFirst))
tweetsGood <- removeProfanity(tweetsList)
tweetsCorpus <- corpus(tweetsGood)
## Took about 15-20 minutes for corpus_reshape() to work - and as of 4/23, it didn't split at the periods,
## only at the question marks and exclamation points. As of 4/30, still the same.
tweetsCorpus2 <- corpus_reshape(tweetsCorpus, to = "sentences")
saveRDS(tweetsCorpus2, "tweetsCorpus.rds")
rm(tweets)
rm(tweetsCleanFirst)
rm(tweetsGood)
rm(tweetsList)
rm(tweetsCorpus)
rm(tweetsCorpus2)

# Next, convert the blog texts to a blogs corpus
blogs <- read_lines("/Users/kristinabkemeier/DataScience/Capstone/final/en_US/en_US.blogs_clean_train.txt")

blogsCleanFirst <- cleanBeforeMakingList(blogs)
blogsList <- as.list(tolower(blogsCleanFirst))
blogsGood <- removeProfanity(blogsList)

blogsCorpus <- corpus(blogsGood)
blogsCorpus2 <- corpus_reshape(blogsCorpus, to = "sentences")
saveRDS(blogsCorpus2, "blogsCorpus.rds")

rm(blogs)
rm(blogsCleanFirst)
rm(blogsGood)
rm(blogsList)
rm(blogsCorpus)
rm(blogsCorpus2)

# Finally, convert the news items to a news corpus
news <- read_lines("/Users/kristinabkemeier/DataScience/Capstone/final/en_US/en_US.news_clean_train.txt")

newsCleanFirst <- cleanBeforeMakingList(news)
newsList <- as.list(tolower(newsCleanFirst))
newsGood <- removeProfanity(newsList)

newsCorpus <- corpus(newsGood)
newsCorpus2 <- corpus_reshape(newsCorpus, to = "sentences")
saveRDS(newsCorpus2, "newsCorpus.rds")

rm(news)
rm(newsCleanFirst)
rm(newsGood)
rm(newsList)
rm(newsCorpus)
rm(newsCorpus2)
