#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
####################################################################################
##Author: PD
##File: server.R
#Description: This is the server file for the next word prediction tool shiny app
##Date: 04/26/2021
####################################################################################

library(shiny)
library(DT)

#Load the prediction model as function from this file will be used to predict the 
#next 5 words based on the input phrase
source("ngram-prediction-model.R")


# Define the server logic
# Description:
# 1. Cleanup the input phrase 
# 2. Call the predict function to fill up the list of predicted words
# 3. Return the next predicted word and the list of top 5 predicted words
shinyServer(function(input, output) {

    
    list_of_predicted_words<-reactive({
        input_words<-input$input_phrase
        
        ##Clean input_phrase
        
        #Remove punctuation
        input_words<-str_replace_all(input_words,"(https?://\\S*)|[[:punct:]]+","")
        
        #Change to lower case
        input_words<-tolower(input_words)
        
        #Remove numbers
        input_words<-str_replace_all(input_words,"[[:digit:]]+","")
        
        #Call the predict_next_prob function from ngram-prediction-model.R file
        #to fill up the list of predicted words
        return(predict_next_prob(input_words))
        
        
    })
    
    
    output$next_word<-renderText({
        
        #Retrieve the list of predicted words
        next_word_list<-req(list_of_predicted_words())
        
        #Count the number of predicted words in the list
        as.character(nrow(next_word_list))
        
        #Prompt the user to enter a phrase if not already entered.
        if(input$input_phrase=="")
        {
            message<-"Please enter the phrase"   
            message
        }
        #If predicted words list is empty ask the user to check the phrase or enter more words.
        else if(nrow(next_word_list)==0){
            message<-"Cannot predict the next word. Please check existing phrase or enter more words"
            message
        }
        #Return the next predicted word. Word with the highest frequency
        else{
            paste("<b>Next word is:</b> ",next_word_list[1,1])
        }
        
    })
    
    #Return a table of top 5 predicted words
    output$word_list<-DT::renderDataTable({
        
        next_word_list<-req(list_of_predicted_words())
        if(nrow(next_word_list)!=0){
            
            next_word_list[1]
        }
        
    },options = list(dom='t',ordering=FALSE,autoWidth=TRUE,columnDefs=list(list(width='30%',className='dt-center',targets=0))), rownames = FALSE, colnames=c("Predicted Words"), caption='Table: List of Top 5 Predicted Words based on decreasing probability',extensions='Responsive')
    
    #Send instructions to the user
    output$instructions<-renderText({
        paste("<b>Instructions</b>","<br>","<br>",
              "1. Please enter your phrase in the box at the bottom","<br>","<br>",
              "2. Once you are done entering click outside the box the tool will calculate the next word","<br>","<br>",
              "3. The next word will be displayed besides \"Next word is:\". It may take a few seconds for the next word to be displayed ","<br>","<br>",
              "4. After this a list of top 5 predicted words will also be displayed","<br>","<br>",
              "5. If the tool cannot predict the next word, a message will be displayed \"Cannot predict the next word. Please check existing phrase or enter more words")
    })

})
