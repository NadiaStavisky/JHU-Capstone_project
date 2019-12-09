#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {
    
    next_word <- eventReactive(input$update, {
        withProgress(value = NULL, {
            setProgress(message = "Processing")
            NextWord(input$input, input$numWords)$next_word
        })
                         
        })
    next_word_wfreq <- eventReactive(input$update, {
        NextWord(input$input, input$numWords)$weighted_part_freq
    })
    output$radioTest <- renderUI({
        options <- next_word()
        # The options are dynamically generated on the server
        radioButtons('selected', 'Select your next word:', options, selected = character(0))
    })
    observeEvent(input$selected, {
        updateTextInput(session, inputId = "input", value = paste(input$input, input$selected))
    })
    observeEvent(input$reset, {
        updateTextInput(session, inputId = "input", value = "")
    })
})