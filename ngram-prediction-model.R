
####################################################################################
##Author: PD
##File: ngram-prediction-model.R
##Description: This files contains functions to analyze the data in the form of
## n-grams to predict the next word for the phrase entered by the user. This model/
## algorithm will be the core part of the user facing shiny app. The stupid back-off
## algorithm is used to predict the next word.
##Date: 04/12/2021
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

#Read the n-gram words from the files created by ngrams.R
unigram_words<-readRDS("./app_data/uni_gram_words.rds")
bigram_words<-readRDS("./app_data/bi_gram_words.rds")
trigram_words<-readRDS("./app_data/tri_gram_words.rds")
quadgram_words<-readRDS("./app_data/quad_gram_words.rds")
pentagram_words<-readRDS("./app_data/penta_gram_words.rds")

#Return the frequency of the quadgram if 4 words of the input 
#string match the words of the quadgram
#@param input_words - input phrase
quadgram_freq<-function(input_words){
  
  input_words<-paste(input_words[1],input_words[2],input_words[3],input_words[4])
  
  #Compare each of the 4 input words with the 4 words of the quadgram and return
  #the frequency of the quadgram if the 4 words match
  for(i in 1:nrow(quadgram_words)){
    if(word(input_words,1)==quadgram_words[i,"w1"] && 
       word(input_words,2)==quadgram_words[i,"w2"] && 
       word(input_words,3)==quadgram_words[i,"w3"] &&
       word(input_words,4)==quadgram_words[i,"w4"]){
      return (quadgram_words[i,"freq"])
    }
  }
  
  return(0)
  
}

#Return the frequency of the trigram if 3 words of the input 
#string match the words of the trigram
#@param input_words - input phrase
trigram_freq<-function(input_words){
  
  input_words<-paste(input_words[1],input_words[2],input_words[3])
  
  #Compare each of the 3 input words with the 3 words of the trigram and return
  #the frequency of the trigram if the 3 words match
  for(i in 1:nrow(trigram_words)){
    if(word(input_words,1)==trigram_words[i,"w1"] && 
       word(input_words,2)==trigram_words[i,"w2"] && 
       word(input_words,3)==trigram_words[i,"w3"]){
      return (trigram_words[i,"freq"])
    }
  }
  
  return(0)
}

#Return the frequency of the bigram if 2 words of the input 
#string match the 2 words of the bigram
#@param input_words - input phrase
bigram_freq<-function(input_words){
  
  input_words<-paste(input_words[1],input_words[2])
  
  #Compare each of the 2 input words with the 2 words of the bigram and return
  #the frequency of the bigram if the 2 words match
  for(i in 1:nrow(bigram_words)){
    if(word(input_words,1)==bigram_words[i,"w1"] && 
       word(input_words,2)==bigram_words[i,"w2"]){
      return (bigram_words[i,"freq"])
    }
  }
  return(0)
   
}

#Return the frequency of the unigram if 1 word of the input 
#string match 1 word of the unigram
#@param input_words - input phrase
unigram_freq<-function(input_words){
  
  for(i in 1:nrow(unigram_words)){
    if(word(input_words,1)==unigram_words[i,"w1"]){
      return(unigram_words[i,"freq"])
    }
  }
  
  return(0)
  
}

#Based on the length of the input word, predict the next word from the respective
#n-gram and add it to the list of predicted words with its associated frequency.
#If the list of predicted words has reached 5, then return the list and exit.
#@param input_words - input string
#@param next_word_prob_map - list of predicted words
#@param alpha - back-off factor
#@param word_length - length of the input word
predict_next_word<-function(input_words,next_word_prob_map,alpha,word_length){
  
  #If the word length is 1, predict the next word from the list of bigrams
  if(word_length==1){
    
    #Extract the word from the input string
    input_unigram_words<-tail(strsplit(input_words,split=" ")[[1]],1)
    
    #Retrieve the frequency of the input word from unigram
    uni_freq<-as.numeric(unigram_freq(input_unigram_words))
    
    alpha<-as.numeric(alpha)
    
    #If the input word matches the first word from the bigram list, then the
    #next predicted word is the 2nd word in the bigram list entry with its associated
    #frequency. Frequency is calculated based on the stupid back-off algorithm.
    for(i in 1: nrow(bigram_words)){
      if(input_unigram_words[1]==bigram_words[i,"w1"] &&
         !(as.character(bigram_words[i,"w2"]) %in% next_word_prob_map$word) &&
         nrow(next_word_prob_map)<5 &&
         uni_freq!=0)
      {
        next_word<-as.character(bigram_words[i,"w2"])
        next_word_freq<-alpha*(as.numeric(bigram_words[i,"freq"])/uni_freq)
        
        next_word_prob_map<-add_row(next_word_prob_map,word=next_word,freq=next_word_freq)
      }
      
      #If the list of predicted words has reached 5, then return the list.
      if(nrow(next_word_prob_map)==5){
        return(next_word_prob_map)
      }
      
    }
    
    return (next_word_prob_map)
  }
  
  #If the input word length is 2, predict the next word from the list of trigrams
  if(word_length==2){
    
    alpha<-as.numeric(alpha)
    
    #Extract the last 2 words from the input string
    input_bigram_words<-tail(strsplit(input_words,split=" ")[[1]],2)
    
    #Retrieve the frequency of the last 2 input words from bigram list
    bi_freq<-as.numeric(bigram_freq(input_bigram_words))
    
    #If the 2 input words match the first 2 words from the trigram list, then the next
    #predicted word is the 3rd word from the trigram list with its associated frequency.
    #Frequency is calculated based on the stupid back-off algorithm.
    for(i in 1:nrow(trigram_words)){
      if(input_bigram_words[1]==trigram_words[i,"w1"] &&
         input_bigram_words[2]==trigram_words[i,"w2"] &&
         nrow(next_word_prob_map)<5 &&
         !(as.character(trigram_words[i,"w3"]) %in% next_word_prob_map$word) &&
         bi_freq!=0){
        
        next_word<-as.character(trigram_words[i,"w3"])
        next_word_freq<-alpha*(as.numeric(trigram_words[i,"freq"])/bi_freq)        
        
        next_word_prob_map<-add_row(next_word_prob_map,word=next_word,freq=next_word_freq)            
        
      }
      
      #If the list of predicted words has reached 5, then return the list.
      if(nrow(next_word_prob_map)==5){
        return(next_word_prob_map)
      }
      
    }
    return(next_word_prob_map)
  }
  
  #If the input word length is 3, predict the next word from the list of quadgrams
  if(word_length==3){
    alpha<-as.numeric(alpha)
    
    #Extract the last 3 words from the input string
    input_trigram_words<-tail(strsplit(input_words,split=" ")[[1]],3)
    
    #Retrieve the frequency of the last 3 input words from trigram list
    tri_freq<-as.numeric(trigram_freq(input_trigram_words))
    
    #If the 3 input words match the first 3 words from the quadgram list, then the next
    #predicted word is the 4th word from the quadgram list with its associated frequency.
    #Frequency is calculated based on the stupid back-off algorithm.
    for(i in 1:nrow(quadgram_words)){
      if(input_trigram_words[1]==quadgram_words[i,"w1"] &&
         input_trigram_words[2]==quadgram_words[i,"w2"] &&
         input_trigram_words[3]==quadgram_words[i,"w3"] &&
         !(as.character(quadgram_words[i,"w4"]) %in% next_word_prob_map$word) &&
         nrow(next_word_prob_map)<5 &&
         tri_freq!=0){
        
        next_word<-as.character(quadgram_words[i,"w4"])
        
        next_word_freq<-alpha*(as.numeric(quadgram_words[i,"freq"])/tri_freq)
        
        next_word_prob_map<-add_row(next_word_prob_map,word=next_word,freq=next_word_freq)
        
      }
      
      #If the list of predicted words has reached 5, then return the list.
      if(nrow(next_word_prob_map)==5){
        return(next_word_prob_map)
      }
    } 
    
    return(next_word_prob_map)
    
  }
  
  #If the input word length is 4, predict the next word from the list of pentagrams
  if(word_length==4){
    alpha<-as.numeric(alpha)
    
    #Extract the last 4 words from the input string
    input_quadgram_words<-tail(strsplit(input_words,split=" ")[[1]],4)
    
    #Retrieve the frequency of the last 4 input words from quadgram list
    quad_freq<-as.numeric(quadgram_freq(input_quadgram_words))
    
    #If the 4 input words match the first 4 words from the pentagram list, then the next
    #predicted word is the 5th word from the pentagram list with its associated frequency.
    #Frequency is calculated based on the stupid back-off algorithm.
    for(i in 1:nrow(pentagram_words)){
      if(input_quadgram_words[1]==pentagram_words[i,"w1"] &&
         input_quadgram_words[2]==pentagram_words[i,"w2"] &&
         input_quadgram_words[3]==pentagram_words[i,"w3"] &&
         input_quadgram_words[4]==pentagram_words[i,"w4"] &&
         !(as.character(pentagram_words[i,"w5"]) %in% next_word_prob_map$word) &&
         nrow(next_word_prob_map)<5 &&
         quad_freq!=0){
        
        next_word<-as.character(pentagram_words[i,"w5"])
        
        next_word_freq<-alpha*(as.numeric(pentagram_words[i,"freq"])/quad_freq)
        
        next_word_prob_map<-add_row(next_word_prob_map,word=next_word,freq=next_word_freq)
        
      }
      
      #If the list of predicted words has reached 5, then return the list.
      if(nrow(next_word_prob_map)==5){
        
        return(next_word_prob_map)
      }
    }
    
    return(next_word_prob_map)
    
  }
  
}


#Fill up the next word list with unigrams if there is still room in the list
#@param input_words - input string
#@param next_word_prob_map - list of predicted words
#@param alpha - back-off factor
fill_up_unigrams<-function(input_words,next_word_prob_map,alpha){
  
  index<-0
  
  #Extract the last 2 words from the input string
  input_bigram_words<-tail(strsplit(input_words,split=" ")[[1]],2)
  
  #Retrieve the frequency of the last 2 input words from bigram list
  bi_freq<-as.numeric(bigram_freq(input_bigram_words))
  
  #While the next word list has not reached the limit of 5 and there are still unigrams
  #left to look at in the unigram list continue filling up the next word list
  while(nrow(next_word_prob_map)<5 && 
        index<nrow(unigram_words) &&
        bi_freq!=0){
    next_word<-as.character(unigram_words[index,"w1"])
    next_word_freq<-alpha*(as.numeric(unigram_words[index,"freq"])/bi_freq)
  
    next_word_prob_map<-add_row(next_word_prob_map,word=next_word,freq=next_word_freq)
    
    index<-index+1
  
  }

  return(next_word_prob_map)
  
}


#Fill up the list of next words for the input string up to 5 words in decreasing
#frequency order
#@param input_words - input string
#
predict_next_prob<-function(input_words){
 
  #List of next 5 predicted words
  next_word_prob_map<-data.frame(word=character(),freq=numeric(),stringsAsFactors = FALSE)
  
  input_words_length<-sapply(strsplit(input_words," "),length)

  #set alpha (back off factor) for the stupid backoff algorithm
  alpha<-c(0.4)
  alpha<-as.numeric(alpha)
  
  #Call the predict_next_word function based on the length of the input phrase
  if(input_words_length==1){
    
    next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,1,1)
    return(next_word_prob_map)
      
  }
  else if(input_words_length==2){
    
    next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,1,2)
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,alpha,1)
    }
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-fill_up_unigrams(input_words,next_word_prob_map,(alpha*alpha))
    }
    
    return(next_word_prob_map)
    
  }
  else if(input_words_length==3){
    
    next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,1,3)
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,alpha,2)
    }
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,(alpha*alpha),1)
    }
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-fill_up_unigrams(input_words,next_word_prob_map,(alpha*alpha*alpha))
    }
    
    return(next_word_prob_map)
    
  }
  else if(input_words_length>=4){
    
    next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,1,4)
    
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,alpha,3)
    }
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,(alpha*alpha),2)
    }
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-predict_next_word(input_words,next_word_prob_map,(alpha*alpha*alpha),1)
    }
    
    if(nrow(next_word_prob_map)<5){
      next_word_prob_map<-fill_up_unigrams(input_words,next_word_prob_map,(alpha*alpha*alpha*alpha))
    }
    
    return(next_word_prob_map)
    
  }
}
