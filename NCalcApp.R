#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Oyster Nutrient Removal Calculator"),
    
    selectInput("state", "State:",
                c("Overall average","Connecticut","Delaware","Maine",
                  "Maryland","Massachusetts","New Hampshire",
                  "New Jersey","New York","North Carolina",
                  "Rhode Island","Virginia")
                ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          numericInput("sz0", "Number of oysters: 0 - 1.99 Inches", 0, min=0, max=NA),
          numericInput("sz1", "Number of oysters: 2.0 - 2.49 Inches", 0, min=0, max=NA),
          numericInput("sz2", "Number of oysters: 2.5 - 3.49 Inches", 0, min=0, max=NA),
          numericInput("sz3", "Number of oysters: 3.5 - 4.49 Inches", 0, min=0, max=NA),
          numericInput("sz4", "Number of oysters: 4.5 - 5.49 Inches", 0, min=0, max=NA),
          numericInput("sz5", "Number of oysters: 5.5 - 7.49 Inches", 0, min=0, max=NA),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barPlot <- renderPlot({
        # group input values
        xa    <- c(input$sz0, input$sz1, input$sz2, input$sz3, input$sz4, input$sz5)
        # draw barplot 
        barplot(xa, col = 'darkgray', border = 'white', xlab="Oyster Size Class (Inches)", 
                names.arg=c("<2", "2-2.49", "2.5-3.49", "3.5-4.49", "4.5-5.49", ">5.5"),
                ylab="Number of Oysters")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
