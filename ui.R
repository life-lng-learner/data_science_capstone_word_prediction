#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
####################################################################################
##Author: PD
##File: ui.R
#Description: This is the ui file for the next word prediction tool shiny app
##Date: 04/26/2021
####################################################################################

library(shiny)

# Define UI logic for the prediction tool
shinyUI(fluidPage(

    # Application title - tool title
    titlePanel("Next Word Prediction Tool"),

    # Display the instructions in the side panel
    sidebarLayout(
        sidebarPanel(
            htmlOutput("instructions"),width=6
        ),

        # Main panel - input box for the user to enter the phrase, output the next predicted
        # word and display a list of top 5 predicted words
        mainPanel(
            
            #Input box for the user to enter the phrase
            textInput("input_phrase","Enter the phrase",""),
            
            #Output the next predicted word.
            htmlOutput("next_word"),
            
            #Output a list of next predicted words
            dataTableOutput("word_list")
        )
    )
))