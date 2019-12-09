#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("What Is the Next Word?"),
    wellPanel(width = 12,
    verticalLayout(
        sliderInput(inputId = "numWords",
                             label = "Maximum number of the predicted words",
                             value = 3, min = 1, max = 20),
        textAreaInput(inputId = "input",label = "Start typing here:", height = "100%", resize = "both"),
        
        x <- uiOutput('radioTest')),
    splitLayout(actionButton(inputId = "update", label = "Next"),
             actionButton(inputId = "reset", label = "Reset"))
    )
)
)


