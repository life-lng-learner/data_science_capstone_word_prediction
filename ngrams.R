####################################################################################
##Author: PD
##File: ngrams.R
##Description: This files contains functions to download, unzip and read blog, news 
## and twitter data. After data is cleaned it is broken in n-grams (from unigram to 
## pentagram). First few thousand rows of the n-grams are saved in their respective
## files to be read later by the prediction model.
##Date: 04/26/2021
####################################################################################

# Install required libraries if not already installed
if(!require(DT)){
  install.packages("DT")
}

if(!require(textmineR)){
  install.packages("textmineR")
}

if(!require(dplyr)){
  install.packages("dplyr")
}

if(!require(tidytext)){
  install.packages("tidytext")
}

if(!require(stringr)){
  install.packages("stringr")
}

if(!require(tidyr)){
  install.packages("tidyr")
}

#Load the libraries
library(DT)
library(textmineR)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)

# Download and unzip data
if(!file.exists("Coursera-SwiftKey.zip"))
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip")

unzip("Coursera-SwiftKey.zip")

# total blog lines - 800k - take approx. 10% as sample data
conn_blog = file("./final/en_US/en_US.blogs.txt")
blog_data_sample<-readLines(conn_blog,80000)
close(conn_blog)

#total news lines - 77k - take approx. 10% as sample data
conn_news = file("./final/en_US/en_US.news.txt")
news_data_sample<-readLines(conn_news,7700)
close(conn_news)

#total twitter lines - 2.3M - take approx. 10% as sample data
conn_twitter = file("./final/en_US/en_US.twitter.txt")
twitter_data_sample<-readLines(conn_twitter,230000)
close(conn_twitter)

#Convert sample data to data frames and combine all 3 data samples (blog, news and twitter data)
sample_data<-rbind(data_frame(text=blog_data_sample),data_frame(text=news_data_sample),data_frame(text=twitter_data_sample))


## Data cleaning
# Replace punctuation
sample_data_clean<-str_replace_all(sample_data,"(https?://\\S*)|[[:punct:]]+","")

#Change to lower case
sample_data_clean<-tolower(sample_data_clean)

#Remove Numbers
sample_data_clean<-str_replace_all(sample_data_clean,"[[:digit:]]+","")

#Convert clean data sample to data frame
sample_data_clean<-data_frame(text=sample_data_clean)


#Create n-grams upto pentagram
unigram_data<-sample_data_clean %>% unnest_tokens(unigram,text)
bigram_data<-sample_data_clean %>% unnest_tokens(bigram,text,token="ngrams",n=2)
trigram_data<-sample_data_clean %>% unnest_tokens(trigram,text,token="ngrams",n=3)
quadgram_data<-sample_data_clean %>% unnest_tokens(quadgram,text,token="ngrams",n=4)
pentagram_data<-sample_data_clean %>% unnest_tokens(pentagram,text,token="ngrams",n=5)

##Assign frequencies to the n-grams, remove NAs and filter the first x rows

#Unigram - filter the first 15k rows
unigram_freq_filter_data<-arrange(filter(count(unigram_data,unigram),!is.na(unigram)),desc(n))
unigram_freq_filter_data<-head(unigram_freq_filter_data,15000)

#Bigram - filter the first 15k rows
bigram_freq_filter_data<-arrange(filter(count(bigram_data,bigram),!is.na(bigram)),desc(n))
bigram_freq_filter_data<-head(bigram_freq_filter_data,15000)

#Trigram - filter the first 12k rows
trigram_freq_filter_data<-arrange(filter(count(trigram_data,trigram),!is.na(trigram)),desc(n))
trigram_freq_filter_data<-head(trigram_freq_filter_data,12000)

#Quadgram - filter the first 10k rows
quadgram_freq_filter_data<-arrange(filter(count(quadgram_data,quadgram),!is.na(quadgram)),desc(n))
quadgram_freq_filter_data<-head(quadgram_freq_filter_data,10000)

#Pentagram - filter the first 10k rows
pentagram_freq_filter_data<-arrange(filter(count(pentagram_data,pentagram),!is.na(pentagram)),desc(n))
pentagram_freq_filter_data<-head(pentagram_freq_filter_data,10000)

#Rename column for unigram
unigram_words<-rename(unigram_freq_filter_data,w1=unigram,freq=n)

#Word separation and rename columns (w1 = first word, w2 = second word, w3 = third word, w4= fourth word, w5 = fifth word)
bigram_words<-rename(separate(bigram_freq_filter_data,bigram,c("w1","w2"),sep=" "),freq=n)
trigram_words<-rename(separate(trigram_freq_filter_data,trigram,c("w1","w2","w3"),sep=" "),freq=n)
quadgram_words<-rename(separate(quadgram_freq_filter_data,quadgram,c("w1","w2","w3","w4"),sep=" "),freq=n)
pentagram_words<-rename(separate(pentagram_freq_filter_data,pentagram,c("w1","w2","w3","w4","w5"),sep=" "),freq=n)


#Save n-grams in their respective files. This data will be used by prediction model/algorithm
saveRDS(unigram_words,"./app_data/uni_gram_words.rds")
saveRDS(bigram_words,"./app_data/bi_gram_words.rds")
saveRDS(trigram_words,"./app_data/tri_gram_words.rds")
saveRDS(quadgram_words,"./app_data/quad_gram_words.rds")
saveRDS(pentagram_words,"./app_data/penta_gram_words.rds")