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
                c("US East Coast","Connecticut","Delaware","Maine",
                  "Maryland","Massachusetts","New Hampshire",
                  "New Jersey","New York","North Carolina",
                  "Rhode Island","Virginia")
    ),
    
    selectInput("units", "Units for nutrient removal:",c("Pounds (lbs)", "Kilograms (kg")
    ),
    
    selectInput("gear", "Gear used for growing oysters:",c("Floating", "Bottom", "No Gear")
    ),
    
    sliderInput(
      "hsize",
      "Average oyster size at harvest (Inches)",
      2.0,
      4.0,
      3.0,
      step = 0.05,
      round = FALSE,
      ticks = TRUE,
      animate = FALSE,
      width = NULL,
      sep = ",",
      pre = NULL,
      post = NULL,
      timeFormat = NULL,
      timezone = NULL,
      dragRange = TRUE
    ),
    
    numericInput("Num", "Number of oysters at harvest", 0, min=0, max=NA),
    helpText("Please enter the total number of oysters harvested at the selected size"),
    dateInput("Htime", "Date of harvest yyyy-mm-dd", NULL, min=NA, max=NA, startview = "month"),
    
    actionButton("add", "Add another harvest size"),
    
    # sidebarLayout(
    #     sidebarPanel(
    #       numericInput("sz0", "Number of oysters: 0 - 1.99 Inches", 0, min=0, max=NA),
    #       numericInput("sz1", "Number of oysters: 2.0 - 2.49 Inches", 0, min=0, max=NA),
    #       numericInput("sz2", "Number of oysters: 2.5 - 3.49 Inches", 0, min=0, max=NA),
    #       numericInput("sz3", "Number of oysters: 3.5 - 4.49 Inches", 0, min=0, max=NA),
    #       numericInput("sz4", "Number of oysters: 4.5 - 5.49 Inches", 0, min=0, max=NA),
    #       numericInput("sz5", "Number of oysters: 5.5 - 7.49 Inches", 0, min=0, max=NA),
    #     ),

        mainPanel(
           # plotOutput("barPlot"),
           plotOutput("nutbplot"),
           tableOutput("table")
           
        )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$barPlot <- renderPlot({
    #     # group input values
    #     xa    <- c(input$sz0, input$sz1, input$sz2, input$sz3, input$sz4, input$sz5)
    #     # draw barplot 
    #     barplot(xa, col = 'darkgray', border = 'white', xlab="Oyster Size Class (Inches)", 
    #             names.arg=c("<2", "2-2.49", "2.5-3.49", "3.5-4.49", "4.5-5.49", ">5.5"),
    #             ylab="Number of Oysters")
    # })
    output$nutbplot <- renderPlot({
      # get a and b values based on selected location (Default to resampled average value for ifelse statement==F)
      # Tisssue SH:DW regression a and b values
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
      # Tissue N percent values by location (or default to overall average)
      tNv=ifelse(input$state=="Connecticut", 7.51,
                         ifelse(input$state=="Maine", 7.960433, 
                                ifelse(input$state=="New Jersey", 7.960433,
                                       ifelse(input$state=="New York", 9.35, 
                                              ifelse(input$state=="North Carolina", 8.78, 7.960433)))))
      # Tissue P percent (mean of VA and MD, not currently options for drop down)
      tPv=ifelse(input$state=="Virginia", 0.87,
                 ifelse(input$state=="Maryland", 0.82,  0.845702))
      # Shell SH:DW regression a and b values
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
      # Shell N percent
      sNv=ifelse(input$state=="Connecticut", 0.14,
                 ifelse(input$state=="Massachusetts", 0.24, 
                        ifelse(input$state=="Maryland", 0.17,
                               ifelse(input$state=="Virginia", 0.24, 0.1904083))))
      # Shell P percent (mean of VA and MD, not currently options for drop down)
      sPv=ifelse(input$state=="Virginia", 0.044,
                 ifelse(input$state=="Maryland", 0.046,  0.0450174))
      
      # Calculate dry weights of oyster tissue and shell by size class input (g) based on selected location
      # approximate midpoint of input oyster size class (mm):
      # 24
      # 57
      # 76
      # 102
      # 127
      # 152 * using values from CB BMP report 2023
      #tissue
      # tdw0=input$sz0*(taval*(24^tbval))
      # tdw1=input$sz1*(taval*(57^tbval))
      # tdw2=input$sz2*(taval*(76^tbval))
      # tdw3=input$sz3*(taval*(102^tbval))
      # tdw4=input$sz4*(taval*(127^tbval))
      # tdw5=input$sz5*(taval*(152^tbval))
      # #shell
      # sdw0=input$sz0*(saval*(24^sbval))
      # sdw1=input$sz1*(saval*(57^sbval))
      # sdw2=input$sz2*(saval*(76^sbval))
      # sdw3=input$sz3*(saval*(102^sbval))
      # sdw4=input$sz4*(saval*(127^sbval))
      # sdw5=input$sz5*(saval*(152^sbval))
      
      tdw=input$Num*(taval*((input$hsize*25.4)^tbval))
      sdw=input$Num*(saval*((input$hsize*25.4)^sbval))
      
      #Convert dry weight of tissue and shell (g) to nutrients (g)
      # tNi=((tNv/100)*tdw0)+((tNv/100)*tdw1)+((tNv/100)*tdw2)+((tNv/100)*tdw3)+((tNv/100)*tdw4)+((tNv/100)*tdw5)
      # sNi=((sNv/100)*sdw0)+((sNv/100)*sdw1)+((sNv/100)*sdw2)+((sNv/100)*sdw3)+((sNv/100)*sdw4)+((sNv/100)*sdw5)
      # tPi=((tPv/100)*tdw0)+((tPv/100)*tdw1)+((tPv/100)*tdw2)+((tPv/100)*tdw3)+((tPv/100)*tdw4)+((tPv/100)*tdw5)
      # sPi=((sPv/100)*sdw0)+((sPv/100)*sdw1)+((sPv/100)*sdw2)+((sPv/100)*sdw3)+((sPv/100)*sdw4)+((sPv/100)*sdw5)
      # 
      #Convert dry weight of tissue and shell (g) to nutrients (g)
      tNi=((tNv/100)*tdw)+((tNv/100)*tdw)
      sNi=((sNv/100)*sdw)+((sNv/100)*sdw)
      tPi=((tPv/100)*tdw)+((tPv/100)*tdw)
      sPi=((sPv/100)*sdw)+((sPv/100)*sdw)
      
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds",0.00220462,0.001)
      tN=tNi*cnvrt
      sN=sNi*cnvrt
      tP=tPi*cnvrt
      sP=sPi*cnvrt
      
      barplot(c(tN,sN, tP, sP), col = 'lightblue', border = 'white', xlab="N removal)", 
              names.arg=c("Tissue N", "Shell N", "Tissue P", "Shell P"),
              ylab=input$units)
      
      output$table <- renderTable(
      data.frame("Location"=input$state, "Tissue N"=tN,"Shell N"=sN,"Tissue P"=tP,
                 "Shell P"=sP, "Total N"=sN+tN, "Total P"=sP+tP,"Units"=input$units),
      striped = T,
      hover = F,
      bordered = T,
      spacing = c("s", "xs", "m", "l"),
      width = "auto",
      align = NULL,
      rownames = FALSE,
      colnames = TRUE,
      digits = NULL,
      na = "NA",
      quoted = FALSE
      )
    })
    
    observeEvent(input$add, {
      output_name <- paste0("out_", input$add)
      output[[output_name]] <- renderText({
        isolate(input$add)
      })
      insertUI(
        selector = ifelse(input$add == 0L, "#add", paste0("#", "out_", input$add-1)),
        where = "afterEnd",
        ui = verbatimTextOutput(output_name)
      )
    }, ignoreNULL = FALSE)
 }

# Run the application 
shinyApp(ui = ui, server = server)
