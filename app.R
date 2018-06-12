#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Kristin Abkemeier
# 2018-06-11 update
# Coursera Data Science Specialization Capstone project

library(shiny)
library(shinyjs)
library(stringr)
library(data.table)
# markdown library for including the about.md document on the About tab
library(markdown)

# Read in the data.tables that we need in order to look up words 
dt4gram <- readRDS("data/dt4gram.rds")
dt3gram <- readRDS("data/dt3gram.rds")
dt2gram <- readRDS("data/dt2gram.rds")
dt1gramOnly5 <- readRDS("data/dt1gram.rds")
# Don't want scientific notation to get in the way of ranking the returned results correctly
options(scipen=999)

# Define our colors here and assign to both the text and the barplot
col1 <-"#22CC99"
col2 <-"#CC2299"
col3 <-"#9922CC"
echoColor <- "#000000"

ui <- navbarPage("Final Word",
   # Predict is the panel where all of the word lookup action happens
   tabPanel("Predict",
    fluidRow(
      column(4,
             shinyjs::useShinyjs(),
             id = "entry",
           textInput(inputId = "typeInText",
                      label = h4("Type for top 3 next words."),
                      value = " ")
      ),
      column(1,
             actionButton(inputId = "resetInput","Clear  ")
      ) 
    ),
    fluidRow(
      column(9,
            h3(htmlOutput("wordTyped", inline = FALSE, container = div))
      )
    ),
    fluidRow(
      column(9,
            plotOutput("distPlot")
      )
    )
  ),
  # How to Use is the instructions panel
  tabPanel("How to Use",
           fluidRow(
             column(6,
                    includeMarkdown("howto.md")
             )
           ),
           fluidRow(
             column(6,
                    a(href="http://rpubs.com/kmabkeme/361843",
                      "Progress report describing initial data analysis")
             )
           )
  ),
  # About - provide some context for the app
  tabPanel("About",
           fluidRow(
             column(6,
                    includeMarkdown("about.md")
             )
          ),
           fluidRow(
            column(6,
                   a(href="https://www.linkedin.com/in/kristinabkemeier/",
                     "https://www.linkedin.com/in/kristinabkemeier/")
            )
          )
   )
)

# Define server logic required to look-up predicted words in lookup tables, create a bar plot,
# and superimpose words on the bar plot.
server <- function(input, output, session) {
  
  observeEvent(input$resetInput, {
    shinyjs::reset("entry")
  })

  # Use reactiveValues
  state <- reactiveValues()
  
  observe({
    state$x <- input$typeInText
    # state$y contains the output of our "stupid backoff" algorithm below, which is where the word
    # prediction lookups happen.
    state$y <- backoff7(state$x)
  })
  
  output$wordTyped <- renderText({
    paste("<font color=\"", echoColor,"\">", (state$y)[7], "[ </font>",
          "<font color=\"", col3, "\"><b>", (state$y)[1], "</b></font>",
          "<font color=\"", col2, "\"><b>", (state$y)[2], "</b></font>",
          "<font color=\"", col1, "\"><b>", (state$y)[3], "</b></font> ]", sep=" ")
  }) 
  
  output$distPlot <- renderPlot({
    ## Generate a bar plot for each of the top 3 recommendations.
    
    ## Because I chose to display the lookup words with the prediction adjacent to the bars in the
    ## barplot, the text lines don't automatically wrap in the manner that the htmlOutput() text does.
    ## So, I have to hack together a solution. But it was worth it in order to achieve a cool look for
    ## the app that works both on a smartphone and a big screen!
    
    ## 2018-06-10, KA - If the phrase with lookup words and predicted word is too long,
    ## then we will break it up and show it on two lines instead. The reason is that on the 
    ## smartphone screen, the unbroken text runs off the side of the screen.
    
    # All of the lookup words are returned separately now, so I need to paste them together for 
    # getting the entire cleaned up string
    allLookupWords <- trimws(paste((state$y)[8], (state$y)[9], (state$y)[10], sep=" "))
    
    # The reactiveValue state$y contains the outputs from the word prediction lookup method backoff7()
    # below. They are indexed in order in which they were indicated in backoff7(). I don't want to rename
    # these too much in order to reduce the number of levels of cascading changing values in this app.
    predWordLength <- max(c(nchar((state$y)[3]), nchar((state$y)[2]), nchar((state$y)[1])))
    
    # 2018-06-11, KA - Now returning the lookup words separately so I can break them across lines more
    # easily. On my iPhone 7 with the text size set at 2, I can fit 23 characters, which means that 4 words
    # can total 20 characters with 3 spaces if they are to fit onto a single line.
    if ((nchar((state$y)[8]) + nchar((state$y)[9]) + nchar((state$y)[10]) + predWordLength) >= 20)
    {
      # If we can't fit all three lookup words on one line, we put the third one on the second line.
      if (nchar((state$y)[8]) + nchar((state$y)[9]) + nchar((state$y)[10]) >= 21)
      {
        # If we can't fit the first two lookup words on one line, we make them fit. Calling Procrustes!
        if (nchar((state$y)[8]) + nchar((state$y)[9]) >= 22)
        {
          if (nchar((state$y)[8]) >= 12)
          {
            state8Str <- (state$y)[8]
            state8Str9Chars <- substr((state$y)[8], start=1, stop=9)
            str8Use <- paste(state8Str9Chars,"...",sep="")
          }
          else
          {
            str8Use <- (state$y)[8]
          }
          
          if (nchar((state$y)[9]) >= 10)
          {
            state9Str <- (state$y)[9]
            state9Str9Chars <- substr((state$y)[9], start=1, stop=7)
            str9Use <- paste(state9Str9Chars,"...",sep="")
          }
          else
          {
            str9Use <- (state$y)[9]
          }
          
          firstLine <- trimws(paste(str8Use, str9Use,sep=" "))
          ## Truncate the lookup word string and show that it continues beyond 20 characters
          predictedTextLbls <- c(firstLine,
                                 firstLine,
                                 firstLine
          )
          
          if ((nchar((state$y)[10]) + predWordLength) >= 22)
          {
            predictedTextLbls2 <- secondRowHelper()
            
            # The first two lookup words fit one one line, but the third lookup word and predicted words don't
            # fit on a line. So we truncate. Gotta fit everything onto two lines.
            numberOfRows <- 2
            
          }
          else
          {
            # If the first two lookup words fit on a line, and the last lookup word and predicted words fit,
            # then we put them each on separate lines and we're done
            
            
            ## The third lookup word and the predicted words on their own line next to each bar
            predictedTextLbls2 <- c(trimws(paste((state$y)[10], (state$y)[3], sep=" ")),
                                    trimws(paste((state$y)[10], (state$y)[2], sep=" ")),
                                    trimws(paste((state$y)[10], (state$y)[1], sep=" "))
            )
            
            numberOfRows <- 2
          }
          
        }
        else
        {
          # The first two lookup words get their own line next to each bar in the plot
          first2Words <- trimws(paste((state$y)[8], (state$y)[9], sep=" "))
          predictedTextLbls <- c(first2Words, 
                                 first2Words,
                                 first2Words
          )
          
          if (nchar((state$y)[10]) + predWordLength >= 22)
          {
            predictedTextLbls2 <- secondRowHelper()
            
            numberOfRows <- 2
            
          }
          else
          {
            # If the first two lookup words fit on a line, and the last lookup word and predicted words fit,
            # then we put them each on separate lines and we're done

            ## The third lookup word and the predicted words on their own line next to each bar
            predictedTextLbls2 <- c(trimws(paste((state$y)[10], (state$y)[3], sep=" ")),
                                    trimws(paste((state$y)[10], (state$y)[2], sep=" ")),
                                    trimws(paste((state$y)[10], (state$y)[1], sep=" "))
            )
            
            numberOfRows <- 2
          }
        }
      }
      else
      {
        ## The lookup words get their own line next to each bar
        predictedTextLbls <- c(allLookupWords, 
                               allLookupWords,
                               allLookupWords
        )
        
        ## The predicted words on their own line next to each bar
        predictedTextLbls2 <- c((state$y)[3],
                                (state$y)[2],
                                (state$y)[1]
        )
        
        numberOfRows <- 2
      }
    }
    else
    {
      # Paste both the lookup phrase and the predicted words when they do all fit
      predictedTextLbls <- c(trimws(paste(allLookupWords, (state$y)[3], sep=" ")),
                             trimws(paste(allLookupWords, (state$y)[2], sep=" ")),
                             trimws(paste(allLookupWords, (state$y)[1], sep=" "))
      )
      
      numberOfRows <- 1
    }

    wordWeights <- c(as.numeric((state$y)[4]), as.numeric((state$y)[5]), as.numeric((state$y)[6])) 
    lbls <- c((state$y)[1], (state$y)[2], (state$y)[3])
    df <- data.frame(wordWeights, lbls)
    ## Plot the bars in decreasing order of weight
    dfdecreasing <- df[order(df[,1]),]
    pct <- dfdecreasing[,1]
    pctlbls <- paste(pct,"%",sep="") # add percents to labels 
    par(mar=c(0, 0, 0, 3))
    theColorOrder <- c(col1, col2, col3)
    
    # Plot the barplot horizontally
    xx <- barplot(dfdecreasing[,1], col=theColorOrder, 
                  horiz=TRUE, las=1, xlim=c(0,120), axes=FALSE, space=1, ylim=c(0,7))
    
    ## Depending on the length of the lookup phrase above, we print either to one row or two rows
    # The text is in the same color as the corresponding bars (both are in theColorOrder)
    if (numberOfRows == 1)
    {
      text(x = 0, y = xx + 0.9, label = predictedTextLbls, cex = 2, pos=4, col=theColorOrder)
    }
    else
    {
      text(x = 0, y = xx + 1.2, label = predictedTextLbls, cex = 2, pos=4, col=theColorOrder)
      text(x = 0, y = xx + 0.7, label = predictedTextLbls2, cex = 2, pos=4, col=theColorOrder)      
    }
    # These are the percent labels, also in the same color
    text(x = dfdecreasing[,1], y = xx - 0.18, label = pctlbls, pos = 4, cex = 1.5, col = theColorOrder)
    
  })
  
  
  secondRowHelper <- function(){
    
    # The first two lookup words fit one one line, but the third lookup word and predicted words don't
    # fit on a line. So we truncate. Gotta fit everything onto two lines.
    
    lengthPred3 <- nchar((state$y)[3])
    lengthPred2 <- nchar((state$y)[2])
    lengthPred1 <- nchar((state$y)[1])
    
    # Handle each of the predicted words differently...truncation might not be necessary for all of them
    if ((nchar((state$y)[10]) + lengthPred3) >= 22)
    {
      luWord3Trunc3 <- substr((state$y)[10], start=1, stop=10)
      line3 <- trimws(paste(luWord3Trunc3, "... ", (state$y)[3], sep=""))
    }
    else
    {
      line3 <- trimws(paste((state$y)[10], (state$y)[3], sep=" "))
    }
    
    if ((nchar((state$y)[10]) + lengthPred2) >= 22)
    {
      luWord3Trunc2 <- substr((state$y)[10], start=1, stop=10)
      line2 <- trimws(paste(luWord3Trunc2, "... ", (state$y)[2], sep=""))
    }
    else
    {
      line2 <- trimws(paste((state$y)[10], (state$y)[2], sep=" "))
    }
    
    if ((nchar((state$y)[10]) + lengthPred1) >= 22)
    {
      luWord3Trunc1 <- substr((state$y)[10], start=1, stop=10)
      line1 <- trimws(paste(luWord3Trunc1, "... ", (state$y)[1], sep=""))
    }
    else
    {
      line1 <- trimws(paste((state$y)[10], (state$y)[1], sep=" "))
    }
    
    predictedTextLbls2 <- c(line3,
                            line2,
                            line1
    )
    
    predictedTextLbls2
  }
  
  
  
  cleanUpKAStuff <- function(str){
    ## Let's replace the generalized values with "(url)", etc. so the presentation onscreen is better
    str <- gsub("kaurl","(URL)",str)
    str <- gsub("katwitter","(hashtag)",str)
    str <- gsub("kanumber","(number)",str)
    str <- gsub("kamoney","(monetary)",str)
    str <- gsub("katime","(time)",str)
    
    str
  }
  
  
  backoff7 <- function(enteredText){
    trimEnteredText <- trimws(enteredText)
    lengthTrimEntered <- nchar(trimEnteredText)
    
    lookup1 <- ""
    lookup2 <- ""
    lookup3 <- ""
    
    # 2018-06-08, KA - Let's split the string first, before we clean it, so we can give an accurate depiction
    # of what was actually entered, versus the lookup words we used
    # The reason we have the loop here is because I decided I wanted to display the portion of the input text
    # where the lookup words are derived from. If I didn't want to echo the input on my app, I would skip the loop
    # and do everything on the entire phrase instead of word by word.
    # Need to count how many words we have in here = # of underscores + 1
    if (lengthTrimEntered > 0)
    {
      numberOfWords <- str_count(trimEnteredText, " +") + 1
      
      # Define a 
      keepCleanedText <- vector(mode="character", length=numberOfWords)
      
      splitOriginalText <- unlist(tail(strsplit(trimEnteredText, " +")[[1]], numberOfWords))
      
      # Let's figure out how much to keep of the original text for display along with the words we actually
      # use to perform the lookup
      for (i in 1:numberOfWords)
      {
        if (!is.na(splitOriginalText[i]))
        {
          str <- gsub("é", "e", splitOriginalText[i], ignore.case = TRUE)  
          str <- gsub("è", "e", str, ignore.case = TRUE)
          # 2018-06-09, KA - Handle the diaeresis, in case a New Yorker editor uses this app
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
          # This a with circumflex appears to be a substitute for an apostrophe in the news corpus
          str <- gsub("â", "'", str, ignore.case = TRUE)
          str <- gsub("’", "'", str, ignore.case = TRUE)
          
          # 2018-05-27, KA - Clean out any non-ASCII characters - no point in doing any further processing on those.
          # Except some of these characters may be separating words, and some may be within words.
          str <- iconv(str, "latin1", "ASCII", sub=" ")
          
          # Get rid of the entire retweet header, with or without the colon
          str <- str_replace(str,"rt @[a-zA-Z0-9_]*:*","")
          # Get rid of Twitter hashtags and such
          str <- str_replace_all(str,"#\\w*","")
          str <- str_replace_all(str,"@\\w*","")
          
          # Get rid of URLs formatted for Twitter, as they are useless in predictions
          str <- str_replace_all(str, "http://t.co/[a-z,A-Z,0-9]{10}","")
          str <- str_replace_all(str, "http[:]*[[:punct:]]*[a-z,A-Z,0-9]*","")
          
          # Generalize many numbers, monetary values, times, URLs, etc. so they can still be used in
          # predictions. This may be a minor enhancement to accuracy, but I'm giving it a shot.
          str <- gsub("\\b[^0-9]+[0-9]+\\b"," kanumber",str) 
          # Just a number
          str <- gsub("\\b[0-9]+\\b","kanumber",str) 
          str <- gsub("\\b(\\$|\\£).*\\b", "kamoney", str)
          # 2018-06-09, KA - kaphone never appeared in my lookup table anyway
          #str <- gsub("\\b[0-9]{3}\\-[0-9]{3}\\-[0-9]{4}\\b", "kaphone", str)
          str <- gsub("\\b[0-9]+[ap]+\\.*[m]*\\.*\\b"," katime",str) 
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
          
          str <- gsub("&"," and ",str)
          str <- gsub("%"," percent ",str)
          
          # Strip off leading and trailing punctuation, to get to the lookup words.
          str <- gsub("^([[:punct:]])+", "", trimws(str))
          str <- gsub("([[:punct:]])+$", "", trimws(str))
          
          # If we still have a string of at least one alphanumeric character left after this, then we keep it
          if (nchar(str) > 0)
          {
            # 2018-06-09, KA - We want to do a separate check for profanity so that we can print out 
            # asterisks when we echo the language on the interface
            str <- gsub("[a-z0-9]*fuc[ck][a-z0-9]*","",str)
            str <- gsub("[a-z0-9]*f[*][*]k[a-z0-9]*","",str)
            str <- gsub("[a-z0-9]*f[c]*uk[s]*","",str)
            str <- gsub("[a-z0-9]*phuk[a-z0-9]*","",str)
            str <- gsub("[a-z0-9]*m[0o]f[0o][a-z0-9]*","",str)
            str <- gsub("jerk[-]?[0o]ff[a-z0-9]*","",str)
            # What it someone is talking about their donkey? This also had a problem with "assay",
            # so I'm not going to bowdlerize this word and their variants
            #str <- gsub("\\ba[s$*][s$*]+[a-z0-9]*","",str)
            str <- gsub("\\bcunt[a-z0-9]*\\b","",str)
            str <- gsub("\\bc[o0]ck[s]*\\b","",str)
            str <- gsub("\\b[ck]um[sz]*\\b","",str)
            str <- gsub("[a-z]*b[i1\\*]tch[a-z0-9]*","",str)
            ##str <- gsub("\\barse[a-z0-9]*","",str)
            str <- gsub("\\bcr[a@]p[a-z0-9]*","",str)
            str <- gsub("[a-z0-9]*damn[a-z0-9]*","",str)
            str <- gsub("\\bn[i1]gg[a4][a-z0-9]*","",str)
            str <- gsub("\\bn[i1]gg[e3]r[a-z0-9]*","",str)
            str <- gsub("\\b(bull)*[s$]h[i1]+t[a-z0-9]*","",str)
            str <- gsub("\\btwat[a-z0-9]*","",str)
            str <- gsub("\\bt[i1]ts?t?[a-z0-9]*","",str)
            str <- gsub("\\bwan[gk]+[a-z0-9]*","",str)
            str <- gsub("\\bb[o0][o0]+b[a-z0-9]*","",str)
            str <- gsub("\\bc[o0][o0]+n[a-z0-9]*","",str)
            
            if (nchar(str) > 0)
            {
              # Keep the index of the word that made it through - we'll use it as a lookup word
              keepCleanedText[i] <- trimws(str)
            }
            else
            {
              # If it's a naughty word, we won't echo it exactly but instead show "******"
              splitOriginalText[i] <- gsub(".","*", splitOriginalText[i])
            }
            
          }
        }
      }
      
      # We figure out which words of the entered phrase we can use as lookup words lookup1, lookup2, and lookup3.
      # If there were more than 4 indices we can keep, we only want to use the last three.
      nonemptyCleanedTextIndices <- which(keepCleanedText != "")
      lengthNonEmptyClean <- length(nonemptyCleanedTextIndices)
      if (lengthNonEmptyClean >= 3)
      {
        indicesKept <- nonemptyCleanedTextIndices[(lengthNonEmptyClean-2):lengthNonEmptyClean]
        lookup1 <- tolower(keepCleanedText[indicesKept[1]])
        lookup2 <- tolower(keepCleanedText[indicesKept[2]])
        lookup3 <- tolower(keepCleanedText[indicesKept[3]])
        numberOfLookupWords <- 3
      }
      else if (lengthNonEmptyClean == 2)
      {
        indicesKept <- nonemptyCleanedTextIndices[1:2]
        lookup1 <- ""
        lookup2 <- tolower(keepCleanedText[nonemptyCleanedTextIndices[1]])
        lookup3 <- tolower(keepCleanedText[nonemptyCleanedTextIndices[2]])
        numberOfLookupWords <- 2
      }
      else if (lengthNonEmptyClean == 1)
      {
        indicesKept <- nonemptyCleanedTextIndices[1]
        lookup1 <- ""
        lookup2 <- ""
        lookup3 <- tolower(keepCleanedText[nonemptyCleanedTextIndices[1]])
        numberOfLookupWords <- 1
      }
      else
      {
        indicesKept[1] <- -1
      }
      
      ## And we keep the range of pre-cleaned words starting from the first index, so we can display the 
      ## originally input text
      if (indicesKept[1] != -1)
      {
        originalTextIndices <- indicesKept[1]:numberOfWords
        originalTextPhrase <- paste(splitOriginalText[originalTextIndices],collapse=" ")
      }
      else
      {
        originalTextPhrase <- paste(splitOriginalText[1:numberOfWords],collapse=" ")
      }
    }
    else
    {
      originalTextPhrase <- " "
      numberOfLookupWords <- 0
    }
    
    ## Want to initialize a result datatable
    result <- data.table(NULL)
    
    # Do the lookup from the 4-grams table (three words are indices, and one word is the prediction)
    if ( numberOfLookupWords == 3 ) 
    {
      setkey(dt4gram, word1, word2, word3)
      result4 <- dt4gram[.(lookup1, lookup2, lookup3)]
      
      # If our query has a nonempty result, then we start our cumulative results
      if (!is.na((result4$word4)[1]))
      {
        result4[, weight := sboWeight]
        result <- result4[, !c("word1","word2","word3","sboWeight"), with=FALSE]
      }
    }
    ## 2018-06-06 - Only go here if we don't have a complete set of results already
    if ( numberOfLookupWords >= 2 && nrow(result) < 3) 
    {
      ## Let's also look at the next step down to the 3-grams table to see if there are any other options
      setkey(dt3gram, word2, word3)
      result3 <- dt3gram[.(lookup2, lookup3)]
      
      # If our query has a nonempty result, then we add it to our cumulative results (or start cumulative results)
      if (!is.na((result3$word4)[1]))
      {
        ## Need to multiply this resultWeight by 0.4 for stupid backoff model
        result3[, weight := 0.4 * sboWeight]
        if (nrow(result) > 0)
        {
          result <- rbind(result, result3[, !c("word2","word3","sboWeight"), with=FALSE])
        }
        else
        {
          result <- result3[, !c("word2","word3","sboWeight"), with=FALSE]
        }
      }
    }
    
    ## 2018-06-06 - Only go here if we don't have a complete set of results already
    if ( numberOfLookupWords >= 1 && nrow(result) < 5) 
    { 
      ## Next we look in the 2-grams table and see what the best candidates are there, if any
      ## First, we need to take only the last word
      setkey(dt2gram, word3)
      result2 <- dt2gram[.(lookup3)]
      
      # If our query has a nonempty result, then we add it to our cumulative results (or start cumulative results)
      if (!is.na((result2$word4)[1]))
      {
        if (nrow(result) > 0)
        {
          ## Need to multiply this resultWeight by 0.4 for stupid backoff model
          result2[, weight := 0.4 * 0.4 * sboWeight]
          result <- rbind(result, result2[, !c("word3","sboWeight"), with=FALSE])
        }
        else
        {
          result2[, weight := sboWeight]
          result <- result2[, !c("word3","sboWeight"), with=FALSE]
        }
      }
    }
    
    ## 2018-05-24, KA - If we've gotten just a row of NA's from all of our previous searches, then
    ## let's just return the top 5 1-grams
    if (nrow(result) == 0)
    {
      result <- dt1gramOnly5
    }
    ## If we don't have three rows in results, we need to fill in, also putting in backoff weight
    else
    {
      dt1weight <- dt1gramOnly5
      dt1weight[, weight := 0.4 * weight]
      result <- rbind(result, dt1weight) 
    }
    
    result[order(-weight)]
    
    # Find the unique items that have the highest weights, since some phrases here may be duplicates
    # (it was slower to do the exclude at each tier of the search, so I figured let's just add them all in
    # and sort them out at the end)
    result.first <- result[match(unique(result$word4), result$word4),]
    
    ## Normalize the percentages of the weights here
    # We want to see if one word dominates over the other two, out of the three of them
    # This will give a visual indication of the skewness of the distribution
    weight1 <- as.numeric(result.first[1,weight])
    weight2 <- as.numeric(result.first[2,weight])
    weight3 <- as.numeric(result.first[3,weight])
    totalWeight <- weight1 + weight2 + weight3
    
    # Indicate where I used a general "kaurl", "kanumber", etc. indicator to predict words based on common
    # info that fits into a type, e.g., URLs, numbers, that can be useful if generalized, even if a specific
    # URL or number has limited predictive power in this model.
    lookupWords <- trimws(paste(cleanUpKAStuff(lookup1), cleanUpKAStuff(lookup2), cleanUpKAStuff(lookup3), sep=" "))
    
    # Send the predicted words, their weights, original text, and lookup words out to the reactiveValue state$y
    result <- c(result.first[1,word4], result.first[2,word4], result.first[3,word4], 
                round(weight1/totalWeight*100), round(weight2/totalWeight*100), round(weight3/totalWeight*100),
                originalTextPhrase, cleanUpKAStuff(lookup1), cleanUpKAStuff(lookup2), cleanUpKAStuff(lookup3))
                #originalTextPhrase, lookupWords)
    
    result
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

