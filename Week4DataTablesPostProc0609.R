## 2018-06-09
# This file is where the finishing touches get put onto the lookup tables that were generated from the n-grams
# made in the previous file, which were in turn from the corpora made in the first file.
# Part 3 of 3. Start at Week4ScriptForTextProc0609.R before coming here and then go to Week4DataTables0609.R
# before you come here.

## 2018-05-20, KA - This is where the sboWeight column (for "stupid backoff weight") is added
## and the frequency column is removed, so that we can reduce the size of the lookup tables that we will
## need to load into memory for the Shiny app

## 2018-06-09, KA - Changed to refer to 2018-06-09 folder

library(sourcetools)
library(data.table)
setwd("/Users/kristinabkemeier/DataScience/Capstone/2018-06-09")

dt4gram <- fread("dt4gramTop5Final.csv")
## Truncate to only keep those 4-grams that appear at least four times. This reduces the size of the table a lot.
dt4gram <- dt4gram[frequency > 3]
## 2018-06-05, KA - Keep only the top 3 ranked 4-grams, to save space and possibly speed up the benchmark algorithm.
dt4gram <- dt4gram[grpRank < 4]
## Setting column names to NULL in data.table disposes of them.
dt4gram <- dt4gram[,id:=NULL]
dt4gram <- dt4gram[,wbv4to3names:=NULL]
## Calculate the stupid backoff weight
dt4gram[, sboWeight := frequency/tot3gramCt]
dt4gram <- dt4gram[,frequency:=NULL]
dt4gram <- dt4gram[,tot3gramCt:=NULL]
dt4gram <- dt4gram[,grpRank:=NULL]
##fwrite(dt4gram, file="dt4gramTop5FinalPostProc.csv")
## 2018-06-05, KA - Let's get rid of any places where any of the "ka*" variables is predicted in word4
## Also get rid of any places where we need at least two of these generalized lookup words to look things up,
## because at that point we might as well just guess.
dt4gramNoKA <- subset(dt4gram,  word4 != "kanumber" & word4 != "katime" & word4 != "kaphone"
                      & word4 != "kamoney" & word4 != "kaurl" & word4 != "katwitter")
dt4gramNoKA2 <- subset(dt4gramNoKA, subset=!((word1=="kanumber" | word1 =="katime") & (word2=="kanumber" | word2=="katime") & (word3 == "kanumber"|word3 == "katime")))
dt4gramNoKA3 <- subset(dt4gramNoKA2, subset=!((word1=="kanumber" & word2=="kanumber") | (word2 == "kanumber" & word3 == "kanumber")))
dt4gramNoKA4 <- subset(dt4gramNoKA3, subset=!((word1=="katime" & word2=="katime") | (word2 == "katime" & word3 == "katime")))
dt4gramNoKA5 <- subset(dt4gramNoKA4, word1 != "kanumber" & word1 != "katime" & word1 != "kaphone"
                      & word1 != "kamoney" & word1 != "kaurl" & word1 != "katwitter" & word2 != "katwitter")
fwrite(dt4gramNoKA5, file="dt4gramTop3FinalPostProc.csv")
dt4gram <- dt4gramNoKA5

rm(dt4gramNoKA)
rm(dt4gramNoKA2)
rm(dt4gramNoKA3)
rm(dt4gramNoKA4)
rm(dt4gramNoKA5)

saveRDS(dt4gram, "dt4gram.rds")
##dt4gramRDS <- readRDS("dt4gram.rds")

dt3gramTop5 <- fread("dt3gramTop5Final.csv")
## 2018-05-21, KA - Make sure I do this truncation! Or else the final table will be way too big
## Truncate to only keep those that appear at least four times
dt3gram <- dt3gramTop5[frequency > 3]
## 2018-06-05, KA - Keep only the top 3 ranked 3-grams, to save space and possibly speed up the benchmark algorithm
dt3gram <- dt3gram[grpRank < 4]
rm(dt3gramTop5)
dt3gram <- dt3gram[,id:=NULL]
dt3gram <- dt3gram[,wbv3to2names:=NULL]
## Calculate the stupid backoff weight
dt3gram[, sboWeight := frequency/tot2gramCt]
dt3gram <- dt3gram[,frequency:=NULL]
dt3gram <- dt3gram[,tot2gramCt:=NULL]
dt3gram <- dt3gram[,grpRank:=NULL]
## 2018-06-05, KA - Let's get rid of any places where any of the "ka*" variables is predicted in word4
dt3gramNoKA <- subset(dt3gram, word4 != "kanumber" & word4 != "katime" & word4 != "kaphone"
                      & word4 != "kamoney" & word4 != "kaurl" & word4 != "katwitter")
dt3gramNoKA2 <- subset(dt3gramNoKA, subset=!((word2=="kanumber" | word2=="katime") & (word3 == "kanumber"|word3 == "katime")))
dt3gramNoKA3 <- subset(dt3gramNoKA2, word2 != "kanumber" & word2 != "katime" & word2 != "kaphone"
                       & word2 != "kamoney" & word2 != "kaurl" & word3 != "katwitter")
fwrite(dt3gramNoKA3, file="dt3gramTop3FinalPostProc.csv")
dt3gram <- dt3gramNoKA3

rm(dt3gramNoKA)
rm(dt3gramNoKA2)
rm(dt3gramNoKA3)

saveRDS(dt3gram, "dt3gram.rds")


dt2gram <- fread("dt2gramTop5Final.csv")
## Truncate to only keep those that appear at least four times
## Maybe I won't truncate these so that I have more potential answers for 2-grams
dt2gram <- dt2gram[frequency > 3]
## 2018-06-05, KA - Keep only the top 3 ranked 3-grams, to save space and possibly speed up the benchmark algorithm
dt2gram <- dt2gram[grpRank < 4]
dt2gram <- dt2gram[,id:=NULL]
dt2gram <- dt2gram[,wbv2to1names:=NULL]
## Calculate the stupid backoff weight
dt2gram[, sboWeight := frequency/tot1gramCt]
dt2gram <- dt2gram[,frequency:=NULL]
dt2gram <- dt2gram[,tot1gramCt:=NULL]
dt2gram <- dt2gram[,grpRank:=NULL]
## 2018-06-05, KA - Let's get rid of any places where any of the "ka*" variables is predicted in word4
dt2gramNoKA <- subset(dt2gram, word4 != "kanumber" & word4 != "katime" & word4 != "kaphone"
                      & word4 != "kamoney" & word4 != "kaurl" & word4 != "katwitter")
fwrite(dt2gramNoKA, file="dt2gramTop3FinalPostProc.csv")
dt2gram <- dt2gramNoKA

rm(dt2gramNoKA)

saveRDS(dt2gram, "dt2gram.rds")

dt1gram <- fread("dt1gram.csv")
## 2018-06-09, KA - Need to calculate a weighting by summing over the total number of 1-gram instances
total1gram <- sum(dt1gram$frequency)
dt1gram[, weight := frequency/total1gram]
## 2018-06-04, KA - We want to remove the top 5 non-number results when we don't find any words in the
## existing corpus
# Skip over row 5, which predicts "kanumber" - this is not a useful result!
dt1gramOnly5 <- as.data.table(dt1gram[c(1,2,3,4,6),])
dt1gramOnly5[, word4 := id]
dt1gramOnly5[, id:=NULL]
dt1gramOnly5[, frequency:=NULL]
fwrite(dt1gramOnly5, file="dt1gramOnly5.csv")

saveRDS(dt1gramOnly5, "dt1gram.rds")

rm(dt1gram)