#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)

clicks <- data.frame(lat = numeric(), lng = numeric(), .nonce = numeric())

ui <- fluidPage(
  
  tags$img(src='noaa-emblem-rgb-sm2-2022.png'),
  
  # Application title
  # titlePanel( div(column(width = 6, h2("My Header")), 
  #                column(width = 6, tags$img(src = 'noaa-emblem-rgb-2022.png'))),
  #            windowTitle="Oyster Nutrient Removal Calculator"
  # ),
  
  titlePanel(h1("US East Coast Oyster Nutrient Removal Calculator")),
  
  # States with significant regressions, other states should use overall average option
  # selectInput("state", "State:",
  #             c("Overall average","Connecticut","Maine",
  #               "New Jersey","New York","North Carolina",
  #               )
  #             ),
  # checkboxInput('selvarLoc', "Selcect Location", value = FALSE, width = NULL),
  # checkboxInput('selvarGear', "Selcect Gear", value = FALSE, width = NULL),
  # checkboxInput('selvarPloidy', "Selcect Ploidy", value = FALSE, width = NULL),
  # 
  radioButtons('selvar', 'Selection factor for oyster growth', c('Gear', 'Ploidy'),
               inline = TRUE),
  # downloadButton('downloadReport'),
  
  selectInput("units", "Units for nutrient removal:",c("Pounds (lbs)", "Kilograms (kg")
  ),
  
  # selectInput("state", "Region:",
  #             c("US East Coast","Connecticut","Delaware","Maine",
  #               "Maryland","Massachusetts","New Hampshire",
  #               "New Jersey","New York","North Carolina",
  #               "Rhode Island","Virginia")
  # ),
  
  selectInput("gear", "Gear used for growing oysters:",c("Floating", "Bottom", "No Gear")
  ),
  
  selectInput("ploidy", "Select Diploid or Triploid:",c("Diploid", "Triploid")
  ),
  
  sliderInput(
    "hsize",
    "Average oyster size at harvest (Inches)",
    2.0,
    5.0,
    3.0,
    step = 0.1,
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
  
  helpText("Please enter the total number of oysters harvested at the selected size"),
  numericInput("Num", "Number of oysters at harvest", 0, min=0, max=NA),
  
  dateRangeInput("Htime", "Period of harvest (yyyy-mm-dd)", start=NULL, end=NULL, min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month"),
  
  # textInput("farmloc", "Farm Location - City, State", value = "", width = NULL, placeholder = NULL),
  helpText(h2("Farm Location")),
  leafletOutput("mymap"),
  helpText("Please zoom to state level and add a location marker for your farm"),
  
  tableOutput('loctable'),
  
  # actionButton("add", "Add another harvest size"),
  
  radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
               inline = TRUE),
  # downloadButton('downloadReport'),
  downloadButton("report", "Generate report"),
  
  mainPanel(
    # plotOutput("barPlot"),
    plotOutput("nutbplot"),
    tableOutput("table")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Location <- reactiveValues(clickedMarker=NULL)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
      addTiles() %>%
      setView(lng = -70, lat = 40, zoom = 5) %>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        polygonOptions=FALSE,
        markerOptions = T,
        rectangleOptions =F,
        circleOptions = F,
        circleMarkerOptions = F,
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  })
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    output$loctable <- renderTable(
      data.frame("Lon"=feature$geometry$coordinates[[1]],"Lat"=feature$geometry$coordinates[[2]]),
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
  
  
  # dataInput=reactive({
  #   taval=ifelse(input$gear=="Floating",4.33E-06,
  #                ifelse(input$gear=="Bottom",0.000301, 0.000381))
  #   tbval=ifelse(input$gear=="Floating",2.88,
  #                ifelse(input$gear=="Bottom",1.89, 1.88))
  #   saval=ifelse(input$gear=="Floating",0.00012,
  #                ifelse(input$gear=="Bottom",0.0005, 0.0011))
  #   sbval=ifelse(input$gear=="Floating",2.84,
  #                ifelse(input$gear=="Bottom",2.522, 2.34))
  #   tNv=ifelse(input$state=="Connecticut", 7.51,
  #              ifelse(input$state=="Maine", 7.960433, 
  #                     ifelse(input$state=="New Jersey", 7.960433,
  #                            ifelse(input$state=="New York", 9.35, 
  #                                   ifelse(input$state=="North Carolina", 8.78, 7.960433)))))
  #   sNv=ifelse(input$state=="Connecticut", 0.14,
  #              ifelse(input$state=="Massachusetts", 0.24, 
  #                     ifelse(input$state=="Maryland", 0.17,
  #                            ifelse(input$state=="Virginia", 0.24, 0.1904083))))
  #   tdw=input$Num*(taval*((input$hsize*25.4)^tbval))
  #   sdw=input$Num*(saval*((input$hsize*25.4)^sbval))
  #   
  #   #Convert dry weight of tissue and shell (g) to nutrients (g)
  #   tNi=((tNv/100)*tdw)+((tNv/100)*tdw)
  #   sNi=((sNv/100)*sdw)+((sNv/100)*sdw)
  #   
  #   #convert grams N to lbs or kg
  #   cnvrt=ifelse(input$units=="Pounds",0.00220462,0.001)
  #   tN=tNi*cnvrt
  #   sN=sNi*cnvrt
  # })
  
  # dataInput=reactive({
  #   taval=ifelse(input$gear=="Floating",4.33E-06,
  #                ifelse(input$gear=="Bottom",0.000301, 0.000381))
  #   tbval=ifelse(input$gear=="Floating",2.88,
  #                ifelse(input$gear=="Bottom",1.89, 1.88))
  #   saval=ifelse(input$gear=="Floating",0.00012,
  #                ifelse(input$gear=="Bottom",0.0005, 0.0011))
  #   sbval=ifelse(input$gear=="Floating",2.84,
  #                ifelse(input$gear=="Bottom",2.522, 2.34))
  #   tdw=input$Num*(taval*((input$hsize*25.4)^tbval))
  #   sdw=input$Num*(saval*((input$hsize*25.4)^sbval))
  #   
  #   #Convert dry weight of tissue and shell (g) to nutrients (g)
  #   tNi=(0.0796*tdw)
  #   sNi=(0.0019*sdw)
  #   
  #   #convert grams N to lbs or kg
  #   cnvrt=ifelse(input$units=="Pounds",0.00220462,0.001)
  #   tN=tNi*cnvrt
  #   sN=sNi*cnvrt
  # })
  
  output$nutbplot <- renderPlot({
    # # #Floating
    # # taval=4.33372435089667E-06
    # # tbbal=2.87897996925124
    # # saval=0.000004368781292322
    # # sbval=2.87724414276531
    # # #Bottom
    # # taval=0.000301023631573294
    # # tbbal=1.88875141240511
    # # saval=0.000161318358295551
    # # sbval=2.02143772694841
    # # #No Gear
    # # taval=0.000381111318078651
    # # tbbal=1.89841023951104
    # # saval=0.000380450078626961
    # # sbval=1.89882484544669
    # 
    # ## Gear
    # taval=ifelse(input$gear=="Floating",4.33E-06,
    #              ifelse(input$gear=="Bottom",0.000301, 0.000381))
    # tbval=ifelse(input$gear=="Floating",2.88,
    #              ifelse(input$gear=="Bottom",1.89, 1.88))
    # saval=ifelse(input$gear=="Floating",0.00012,
    #              ifelse(input$gear=="Bottom",0.0005, 0.0011))
    # sbval=ifelse(input$gear=="Floating",2.84,
    #              ifelse(input$gear=="Bottom",2.522, 2.34))
    # 
    # # ## Ploidy
    # # taval=ifelse(input$ploidy=="Diploid",3E-05, 2.7E-05)
    # # tbval=ifelse(input$ploidy=="Diploid",2.43, 2.37)
    # # saval=ifelse(input$ploidy=="Diploid",0.000451, 0.000234)
    # # sbval=ifelse(input$ploidy=="Diploid",2.551, 2.659)
    # 
    # ## get a and b values based on selected location (Default to resampled average value for ifelse statement==F)
    # ## Tisssue SH:DW regression a and b values
    # # taval=ifelse(input$state=="Connecticut", 0.000661292,
    # #                    ifelse(input$state=="Maine", 2.35E-06,
    # #                           ifelse(input$state=="New Jersey", 0.000181615,
    # #                                  ifelse(input$state=="New York", 1.06E-05,
    # #                                         ifelse(input$state=="North Carolina", 5.22E-05, 3.967457e-05)))))
    # # tbval=ifelse(input$state=="Connecticut", 1.801627333,
    # #                    ifelse(input$state=="Maine", 3.016394768,
    # #                           ifelse(input$state=="New Jersey", 2.104415861,
    # #                                  ifelse(input$state=="New York", 2.72612343,
    # #                                         ifelse(input$state=="North Carolina", 2.133216559, 2.393042)))))
    # # Tissue N percent values by location (or default to overall average)
    # tNv=ifelse(input$state=="Connecticut", 7.51,
    #                    ifelse(input$state=="Maine", 7.960433,
    #                           ifelse(input$state=="New Jersey", 7.960433,
    #                                  ifelse(input$state=="New York", 9.35,
    #                                         ifelse(input$state=="North Carolina", 8.78, 7.960433)))))
    # # Tissue P percent (mean of VA and MD, not currently options for drop down)
    # # tPv=ifelse(input$state=="Virginia", 0.87,
    # #            ifelse(input$state=="Maryland", 0.82,  0.845702))
    # # Shell SH:DW regression a and b values
    # # saval=ifelse(input$state=="Massachusetts", 0.002172198,
    # #              ifelse(input$state=="Connecticut", 0.0014647,
    # #                     ifelse(input$state=="Maine", 0.000101427,
    # #                            ifelse(input$state=="New York", 0.0427973,
    # #                                          ifelse(input$state=="North Carolina", 0.000462, 0.0002393)))))
    # # sbval=ifelse(input$state=="Massachusetts", 2.21324,
    # #              ifelse(input$state=="Connecticut", 2.32495,
    # #                     ifelse(input$state=="Maine", 2.88729,
    # #                            ifelse(input$state=="New York", 1.331250589,
    # #                                   ifelse(input$state=="North Carolina", 2.49833, 2.6874)))))
    # # Shell N percent
    # sNv=ifelse(input$state=="Connecticut", 0.14,
    #            ifelse(input$state=="Massachusetts", 0.24,
    #                   ifelse(input$state=="Maryland", 0.17,
    #                          ifelse(input$state=="Virginia", 0.24, 0.1904083))))
    # # Shell P percent (mean of VA and MD, not currently options for drop down)
    # # sPv=ifelse(input$state=="Virginia", 0.044,
    # #            ifelse(input$state=="Maryland", 0.046,  0.0450174))
    # 
    # 
    # tdw=input$Num*(taval*((input$hsize*25.4)^tbval))
    # sdw=input$Num*(saval*((input$hsize*25.4)^sbval))
    # 
    # 
    # #Convert dry weight of tissue and shell (g) to nutrients (g)
    # tNi=((tNv/100)*tdw)+((tNv/100)*tdw)
    # sNi=((sNv/100)*sdw)+((sNv/100)*sdw)
    # # tPi=((tPv/100)*tdw)+((tPv/100)*tdw)
    # # sPi=((sPv/100)*sdw)+((sPv/100)*sdw)
    # 
    # #convert grams N to lbs or kg
    # cnvrt=ifelse(input$units=="Pounds",0.00220462,0.001)
    # tN=tNi*cnvrt
    # sN=sNi*cnvrt
    # # tP=tPi*cnvrt
    # # sP=sPi*cnvrt
    
    if(input$selvar=="Ploidy"){
      taval=ifelse(input$ploidy=="Diploid",3E-05, 2.7E-05)
      tbval=ifelse(input$ploidy=="Diploid",2.43, 2.37)
      saval=ifelse(input$ploidy=="Diploid",0.000451, 0.000234)
      sbval=ifelse(input$ploidy=="Diploid",2.551, 2.659)
    }
    else{
      taval=ifelse(input$gear=="Floating",4.33E-06,
                   ifelse(input$gear=="Bottom",0.000301, 0.000381))
      tbval=ifelse(input$gear=="Floating",2.88,
                   ifelse(input$gear=="Bottom",1.89, 1.88))
      saval=ifelse(input$gear=="Floating",0.00012,
                   ifelse(input$gear=="Bottom",0.0005, 0.0011))
      sbval=ifelse(input$gear=="Floating",2.84,
                   ifelse(input$gear=="Bottom",2.522, 2.34))
    }
  tdw=input$Num*(taval*((input$hsize*25.4)^tbval))
  sdw=input$Num*(saval*((input$hsize*25.4)^sbval))
  
  #Convert dry weight of tissue and shell (g) to nutrients (g)
  tNi=(0.0796*tdw)
  sNi=(0.0019*sdw)
  
  #convert grams N to lbs or kg
  cnvrt=ifelse(input$units=="Pounds",0.00220462,0.001)
  tN=tNi*cnvrt
  sN=sNi*cnvrt
  
  barplot(c(tN, sN), col = 'lightblue', border = 'white', xlab="N removal)", 
          names.arg=c("Tissue N", "Shell N"),
          ylab=input$units)
  
  output$table <- renderTable(
    data.frame("Tissue N"=tN,"Shell N"=sN, "Total N"=sN+tN, "Units"=input$units),
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

output$report <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "report.pdf",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params <- list(Location=input$state, Tissue.N=output$table$tN,Shell.N=sN, Total.N=dataInput$sN+dataInput$tN, 
                   Units=input$units, gear=input$gear, ploidy=input$ploidy, hsize=input$hsize) 
    # "Num"=input$Num, "htime"=input$Htime, "loc2"=input$farmloc)
    # "Location"=input$state, "Tissue.N"=output$tN,"Shell.N"=output$sN,
    # "Total.N"=sN+tN,"Units"=input$units,
    # "gear"=input$gear, "ploidy"=input$ploidy, "hsize"=input$hsize, 
    # "Num"=input$Num, "htime"=input$Htime, "loc2"=input$farmloc)
    
    # n = c("units"=input$units, "gear"=input$gear, "ploidy"=input$ploidy, "hsize"=input$hsize, "Num"=input$Num, "htime"=input$Htime, "loc"=input$farmloc))
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)


# output$downloadReport <- downloadHandler(
#   filename = function() {
#     paste('my-report', sep = '.', switch(
#       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
#     ))
#   },
#   
#   content = function(file) {
#     src <- normalizePath('report.Rmd')
#     
#     # temporarily switch to the temp dir, in case you do not have write
#     # permission to the current working directory
#     owd <- setwd(tempdir())
#     on.exit(setwd(owd))
#     file.copy(src, 'report.Rmd', overwrite = TRUE)
#     
#     library(rmarkdown)
#     out <- render('report.Rmd', switch(
#       input$format,
#       PDF = pdf_document(), HTML = html_document(), Word = word_document()
#     ))
#     file.rename(out, file)
#   }
# )
}

# Run the application 
shinyApp(ui = ui, server = server)
