library(quanteda)
library(readtext)
library(tidyverse)
library(stringr)
library(stringi)

setwd("C:/Users/207014104/Desktop/Box Sync/Personal/DataScience/capstone/Coursera-SwiftKey/final/en_US")

con.t <- file("en_US.twitter.txt", "r") 
con.n <- file("en_US.news.txt", "r")
con.b <- file("en_US.blogs.txt", "r")

#divisor for reduction of lines
redx <- 2

set.seed(17)

t <- readLines(con.t, skipNul = TRUE)
size.t <- length(t) / redx 
twitter <- sample(t, size.t)
writeLines(twitter, con = "twitter-sized.txt")
close(con.t)

n <- readLines(con.n, skipNul = TRUE)
size.n <- length(n) / redx 
news <- sample(n, size.n)
writeLines(news, con = "news-sized.txt")
close(con.n)

b <- readLines(con.b, skipNul = TRUE)
size.b <- length(b) / redx
blogs <- sample(b, size.b)
writeLines(blogs, con = "blogs-sized.txt")
close(con.b)

rm(list=ls())

start <- Sys.time()
print(start)

corp <- corpus(readtext("*sized.txt")) 
texts(corp) <- iconv(texts(corp), from = "UTF-8", to = "ASCII", sub = "")

# load and read the profanity doc from the internet if not downloaded, or just read it 
destfile <- "profanity.csv"
profanity.url <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
if (!file.exists(destfile)) {
        download.file(profanity.url, mode="wb")
}
profanity <- read.csv(destfile, header = FALSE, stringsAsFactors = FALSE)

toks <- tokens(corp, 
               what = "word",
               remove_numbers = TRUE, 
               remove_punct = TRUE, 
               remove_symbols = TRUE, 
               remove_twitter = TRUE, 
               remove_hyphens = FALSE,
               remove_url = TRUE) %>%
        tokens_tolower() %>%
        tokens_remove(profanity$V1) 

one.grams <- dfm(toks, n = 1, concatenator = " ") %>%
        dfm_trim(min_termfreq = 2) %>%
        topfeatures(n = 10) 
one.grams <- as_tibble(names(one.grams)) %>% #split the words into columns
        rename(prediction = value)

two.grams <- dfm(toks, n = 2, concatenator = " ") 
two.grams <- topfeatures(two.grams, n = length(two.grams))
two.grams <- as_tibble(str_split_fixed(names(two.grams), " ", n = 2)) #split the words into columns
two.grams <- two.grams[match(unique(two.grams$V1), two.grams$V1), ] %>% #only keep most frequent first word combo
        rename(prediction = V2)

three.grams <- dfm(toks, n = 3, concatenator = " ") %>%
        dfm_trim(min_termfreq = 2)
three.grams<- topfeatures(three.grams, n = length(three.grams))
three.grams <- as_tibble(str_split_fixed(names(three.grams), " ", n = 3)) %>% #split the words into columns
        unite(col = V1, V1, V2, sep = " ")
three.grams <- three.grams[match(unique(three.grams$V1), three.grams$V1),] %>% #only keep most frequent first word combo
        rename(prediction = V3)

four.grams <- dfm(toks, n = 4, concatenator = " ") %>%
        dfm_trim(min_termfreq = 2) 
four.grams <- topfeatures(four.grams, n = length(four.grams))
four.grams <- as_tibble(str_split_fixed(names(four.grams), " ", n = 4)) %>% #split the words into columns
        unite(col = V1, V1, V2, V3, sep = " ") 
four.grams <- four.grams[match(unique(four.grams$V1), four.grams$V1),] %>%  #only keep most frequent first word combo
        rename(prediction = V4)

five.grams <- dfm(toks, n = 5, concatenator = " ") %>%
        dfm_trim(min_termfreq = 2) 
five.grams <- topfeatures(five.grams, n = length(five.grams))
five.grams <- as_tibble(str_split_fixed(names(five.grams), " ", n = 5)) %>% #split the words into columns
        unite(col = V1, V1, V2, V3, V4, sep = " ") 
five.grams <- five.grams[match(unique(five.grams$V1), five.grams$V1),] %>%  #only keep most frequent first word combo
        rename(prediction = V5)

one.grams <- readRDS(file = "onegrams.rds")
two.grams <- readRDS(file = "twograms.rds")
three.grams <- readRDS(file = "threegrams.rds")
four.grams <- readRDS(file = "fourgrams.rds")
five.grams <- readRDS(file = "fivegrams.rds")

save(one.grams, 
     two.grams, 
     three.grams, 
     four.grams, 
     five.grams, 
     file = "prediction-tables.RData")

load("prediction-tables.RData")

nextword <- function(phrase) {
        
        phrase <- tokens(phrase) 
        count.words <- ntoken(phrase)
        ptable <- data.frame()
        
        if(count.words >= 4) {
                last4 <- tail(phrase$text1, n = 4)
                last4 <- str_c(last4, collapse = " ")
                p5 <- filter(five.grams, last4 == V1) 
                ptable <- bind_rows(ptable, p5)
        }
        
        if(count.words >= 3) {
                last3 <- tail(phrase$text1, n = 3)
                last3 <- str_c(last3, collapse = " ")
                p4 <- filter(four.grams, last3 == V1) 
                ptable <- bind_rows(ptable, p4)
        }
        
        if(count.words >= 2) {
                last2 <- tail(phrase$text1, n = 2) %>%
                        str_c(collapse = " ")
                p3 <- filter(three.grams, last2 == V1)
                ptable <- bind_rows(ptable, p3)
        }

        if(count.words >= 1) {
                last1 <- tail(phrase$text1, n = 1) 
                p2 <- filter(two.grams, last1 == V1) 
                ptable <- bind_rows(ptable, p2)
        }
        
        if(is.na(ptable[1,1] == TRUE)) {
                p1 <- one.grams %>%
                        sample_n(1) 
                ptable <- bind_rows(ptable, p1)
        }
        
        answer <- ptable[1,2]
        
        return(answer)
}

end <- Sys.time()

elapsed <- end - start

print(elapsed)

