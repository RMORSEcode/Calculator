# # adding module for NCalcApp Dec 2024
library(shiny)
library(ggplot2)

graph_UI <- function(id) {
  # ns <- NS(id)
  div(
    # id = id,
    sliderInput(
      NS(id,"hsize2"),
      strong("Average oyster size at harvest (Inches)"),
      2.0,
      6.0,
      3.0,
      step = 0.1,
      round = FALSE,
      ticks = TRUE,
      animate = FALSE,
      width = "100%",
      sep = ",",
      dragRange = TRUE
    ),
    numericInput(
      NS(id,"Num"),
      div(
        strong("Number of oysters at harvest:"),
        "Please enter the total number of oysters harvested at the selected size"),
      0,
      min=0,
      max=NA,
      width="100%"),
    radioButtons(
      NS(id,"units"),
      div(strong("Units:")," Select the units for nutrient removal"),
      choices =c("Pounds (lbs)", "Kilograms (kg)"),
      selected ="Pounds (lbs)",
      inline = T,
      width="100%"),
    dateRangeInput(
      NS(id,"Htime"),
      div(
        strong("Period of harvest (yyyy-mm-dd):"),
        em("(does not affect calculation)")),
      start=NULL,
      end=NULL,
      min=Sys.Date()-(5*365),
      max=Sys.Date(),
      startview = "month",
      width="100%"),
    tableOutput(NS(id, "mytable"))
  )
}

graph_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 table <- reactive({
                   taval=1.42E-05
                   tbval=2.60727827
                   saval=0.00039042
                   sbval=2.579747757
                   tdw=taval*(input$hsize2*25.4)^tbval
                   sdw=saval*(input$hsize2*25.4)^sbval
                   #Convert dry weight of tissue and shell (g) to nutrients (g)
                   tNi=reactiveValues()
                   sNi=reactiveValues()
                   tNi=0.0770*tdw
                   sNi=0.0019*sdw
                   
                   #convert grams N to lbs or kg
                   cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
                   tN=reactiveValues()
                   tN=round(tNi*cnvrt*input$Num,1)
                   sN=reactiveValues()
                   sN=round(sNi*cnvrt*input$Num,1)
                   df=data.frame(matrix(c(sN, tN, tN+sN), nrow=1, ncol=3))
                   # df=data.frame("Shell_N"=sN, "Tissue_N"=tN, "Total"=sN+tN)#, "Units"=input$units)
                   colnames(df)=c("Shell", "Tissue", "Total")#, "Units")
                   df$Units=input$units
                   df
                 })
                 output$mytable <-
                   renderTable({
                     table()
                 })
               }
  )
}

ui <- fluidPage(
  actionButton(
    inputId = "add_module",
    label = "Add a module"
  ),
  actionButton(
    inputId = "remove_module",
    label = "Remove a module"
  ),
  div(
    id = "add_here",
  )
)

server <- function(input, output, session) {
  
  active_modules <- reactiveVal(value = NULL)
  
  observeEvent(input$add_module, {
    # update the list of currently shown modules
    current_id <- paste0("id_", input$add_module)
    active_modules(c(current_id, active_modules()))
    
    graph_server(
      id = current_id
    )
    
    insertUI(
      selector = "#add_here",
      ui = graph_UI(id = current_id)
    )
  })
  
  observeEvent(input$remove_module, {
    
    # only remove a module if there is at least one module shown
    if (length(active_modules()) > 0) {
      current_id <- active_modules()[1]
      removeUI(
        selector = paste0("#", current_id)
      )
      
      # update the list of currently shown modules
      active_modules(active_modules()[-1])
    }
  })
}

shinyApp(ui, server)
