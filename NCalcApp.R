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
    # States with significant regressions, other states should use overall average option
    # selectInput("state", "State:",
    #             c("Overall average","Connecticut","Maine",
    #               "New Jersey","New York","North Carolina",
    #               )
    #             ),
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
           plotOutput("barPlot"),
           plotOutput("nutbplot")
           
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
    output$nutbplot <- renderPlot({
      # get a and b values based on selected location (Default to resampled average value for ifelse statement==F)
      taval=ifelse(input$state=="Connecticut", 0.000661292,
                         ifelse(input$state=="Maine", 2.35E-06, 
                                ifelse(input$state=="New Jersey", 0.000181615,
                                       ifelse(input$state=="New York", 1.06E-05, 
                                              ifelse(input$state=="North Carolina", 5.22E-05, 3.967457e-05)))))
      tbval=ifelse(input$state=="Connecticut", 1.801627333,
                         ifelse(input$state=="Maine", 3.016394768, 
                                ifelse(input$state=="New Jersey", 2.104415861,
                                       ifelse(input$state=="New York", 2.72612343, 
                                              ifelse(input$state=="North Carolina", 2.133216559, 2.393042)))))
      # Nutrient values by location (or default to overall average)
      # Tissue N percent values
      tNv=ifelse(input$state=="Connecticut", 7.51,
                         ifelse(input$state=="Maine", 7.960433, 
                                ifelse(input$state=="New Jersey", 7.960433,
                                       ifelse(input$state=="New York", 9.35, 
                                              ifelse(input$state=="North Carolina", 8.78, 7.960433)))))
      # Tissue P (mean of VA and MD, not currently options for drop down)
      # tPv==ifelse(input$state=="Virginia", 0.87, 
                         # ifelse(input$state=="Maryland", 0.82,  0.845702))           
      
      saval=ifelse(input$state=="Massachusetts", 0.002172198,
                   ifelse(input$state=="Connecticut", 0.0014647,
                          ifelse(input$state=="Maine", 0.000101427, 
                                 ifelse(input$state=="New York", 0.0427973, 
                                               ifelse(input$state=="North Carolina", 0.000462, 0.0002393)))))
      sbval=ifelse(input$state=="Massachusetts", 2.21324,
                   ifelse(input$state=="Connecticut", 2.32495,
                          ifelse(input$state=="Maine", 2.88729, 
                                 ifelse(input$state=="New York", 1.331250589, 
                                        ifelse(input$state=="North Carolina", 2.49833, 2.6874)))))
      # Nutrient values by location (or default to overall average)
      # Shell N
      sNv=ifelse(input$state=="Connecticut", 0.14,
                        ifelse(input$state=="Massachusetts", 0.24, 
                               ifelse(input$state=="Maryland", 0.17,
                                      ifelse(input$state=="Virginia", 0.24, 0.1904083))))
      # Shell P (mean of VA and MD, not currently options for drop down)
      # sPv==ifelse(input$state=="Virginia", 0.044, 
                         # ifelse(input$state=="Maryland", 0.046, 0.0450174))

      # Dry weights of oyster tissue by size class input (g): input numbers x SH:DW relationship based on selected location
      # approximate midpoint of input oyster size class (mm):
      # 24
      # 57
      # 76
      # 102
      # 127
      # 152 * using values from CB BMP report 2023
      #tissue
      tdw0=input$sz0*(taval*(24^tbval))
      tdw1=input$sz1*(taval*(57^tbval))
      tdw2=input$sz2*(taval*(76^tbval))
      tdw3=input$sz3*(taval*(102^tbval))
      tdw4=input$sz4*(taval*(127^tbval))
      tdw5=input$sz5*(taval*(152^tbval))
      #shell
      sdw0=input$sz0*(taval*(24^sbval))
      sdw1=input$sz1*(taval*(57^sbval))
      sdw2=input$sz2*(taval*(76^sbval))
      sdw3=input$sz3*(taval*(102^sbval))
      sdw4=input$sz4*(taval*(127^sbval))
      sdw5=input$sz5*(taval*(152^sbval))
      
      tN=((tNv/100)*tdw0)+((tNv/100)*tdw1)+((tNv/100)*tdw2)+((tNv/100)*tdw3)+((tNv/100)*tdw4)+((tNv/100)*tdw5)
      sN=((sNv/100)*sdw0)+((sNv/100)*sdw1)+((sNv/100)*sdw2)+((sNv/100)*sdw3)+((sNv/100)*sdw4)+((sNv/100)*sdw5)
      
      barplot(c(tN,sN), col = 'darkgray', border = 'white', xlab="N removal)", 
              names.arg=c("Tissue", "Shell"),
              ylab="mg N")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
