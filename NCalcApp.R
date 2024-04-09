# https://connect.fisheries.noaa.gov/Oyster-Calculator/
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyscreenshot)
library(ggplot2)
library(formatR)
library(tinytex)
library(gh)

ui <- fluidPage(
  ### Title ###
  # theme = bslib::bs_theme(bootswatch = "lux"),
  # shinythemes::themeSelector(),
  theme = bslib::bs_theme(bootswatch = "superhero"),
  # tags$img(src='swooshgn2.png'),
  # titlePanel(h1("Aquaculture Nutrient Removal Calculator")),
  # titlePanel(h6(em("Oyster nutrient removal data coverage ranges from ME to NC"))),
  helpText(strong("Calculator Version:")),
  textOutput("githubversion"),
  helpText(br()),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("About", 
               # tags$img(src='swooshgn2.png'),
               tags$img(src='gn_swoosh_shellfish3.png'),
               titlePanel(h1("Aquaculture Nutrient Removal Calculator")),
               titlePanel(h6(em("Oyster nutrient removal data coverage ranges from ME to NC"))),
               helpText(br()),
               tags$p(
                 h4("Background"),
                 p("Shellfish incorporate nutrients into their tissues and shell as they grow. At harvest, these nutrients are permanently removed from the coastal environment, providing a benefit to water quality in the form of excess nutrient reduction. The Aquaculture Nutrient Removal Calculator (ANRC) is a tool designed for resource managers to inform shellfish aquaculture permitting. Resource managers have expressed interest in easy-to-use tools that produce location and operation-appropriate values for beneficial services, and they need the values to be produced in a format that aligns with their permitting process."
                 ),
                 p("The nutrient removal calculations are based on relationships of oyster dry weight-to-length and the average nitrogen (N) concentrations in shell and tissue material. First, we estimate the weight of the oysters based on the typical size of oysters harvested on a farm. The weight estimates are based on non-linear quantile regressions of oyster shell height and dry-weight for both tissue and shell material. Next, the N portion of total oyster weight is calculated using the average N concentration value for both shell and tissue. Adding the tissue and shell nitrogen yields the total weight of  N per oyster. This result is scaled to the total number of oysters harvested, as input by the user."
                 ),
                 br(),
                 h4("Calculator Inputs"),
                 p("The Oyster Nutrient Removal Calculator can be used for new permit applications based on estimated production value or to provide information on existing farms from actual harvest numbers. The grower provides information on:"
                 ),
                 p(strong("- Number of oysters harvested or to be harvested")
                 ),
                 p(strong("- Size of oysters at harvest")
                 ),
                 p("- Culture method (floating gear vs. off-bottom gear vs. on-bottom)*"
                 ),
                 p("- Ploidy (diploid, triploid, or a combination)*"
                 ),
                 p(em("*We are actively seeking feedback from the aquaculture community on the inclusion of these factors, given the small effect that they had in our data analysis.")
                 ),
                 p("Farm location and period of harvest (1 day to 5 years) will be included as inputs for use in generating the report, but will not affect the calculation."
                 ),
                 br(),
                 h4("Summary"),
                 p("We have synthesized available literature for eastern oyster farms across the Northeast region (North Carolina to Maine), and applied methodology used by the Chesapeake Bay Program to calculate nutrient removal at harvest. Variability in oyster tissue and shell nutrient concentration was low, and an assessment of farm location, ploidy, and cultivation practice (with vs. without gear) suggested that a single average value could reasonably be applied across all farms."
                 ),
               )
      ),
      tabPanel("Calculator", 
               # tags$img(src='swooshgn2.png'),
               tags$img(src='gn_swoosh_shellfish3.png'),
               titlePanel(h1("Aquaculture Nutrient Removal Calculator")),
               titlePanel(h6(em("Oyster nutrient removal data coverage ranges from ME to NC"))),
               helpText(br()),
               ### Farm Practices ###
               helpText(h3("Farm Practices")),
               helpText("Please enter the name of the oyster farm"),
               textInput("farmname", strong("Project Name:"), value = "", width = NULL, placeholder = NULL),
               
               # helpText("Please enter the name of the water body where the oysters were harvested from"),
               # textInput("projloc", strong("Harvest Location:"), value = "", width = NULL, placeholder = NULL),
               # 
               helpText("Select the gear type primarily used for growing oysters, or select 'On-Bottom' for no gear"),
               selectInput("gear", strong("Culture Method:"),c("Floating", "Off-bottom", "On-Bottom")),
               
               helpText("Please select the ploidy of the oysters that were harvested"),
               selectInput("ploidy", strong("Oyster Ploidy:"),c("Diploid", "Triploid", "Combination")),
               
               # textInput("farmloc", "Farm Location - City, State", value = "", width = NULL, placeholder = NULL),
               helpText(h3("Farm Location")),
               
               helpText("Please enter the name of the water body where the oysters were harvested from"),
               textInput("projloc", strong("Harvest Location:"), value = "", width = NULL, placeholder = NULL),
               
               helpText(h6("Approximate Coordinates: "),"Please scroll or pinch to zoom to the harvest location, then click once on the marker pin and select the site to record the coordinates"),
               leafletOutput("mymap", width="70%", height=400),
               
               tableOutput('loctable'),
               helpText(br()),
               
               ### Harvest Details ###
               helpText(h3("Harvest Details")),
               helpText("Please drag the slider to select the average size of the oysters that were harvested"),
               sliderInput(
                 "hsize",
                 strong("Average oyster size at harvest (Inches):"),
                 2.0,
                 5.0,
                 3.0,
                 step = 0.1,
                 round = FALSE,
                 ticks = TRUE,
                 animate = FALSE,
                 width = NULL,
                 sep = ",",
                 dragRange = TRUE
               ),
               
               helpText("Please enter the total number of oysters harvested at the selected size"),
               numericInput("Num", strong("Number of oysters at harvest:"), 0, min=0, max=NA),
               
               dateRangeInput("Htime", strong("Period of harvest (yyyy-mm-dd):"), start=NULL, end=NULL, min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month"),
               
               helpText("Units for nutrient removal:"),
               # selectInput("units", strong("Units:"),c("Pounds (lbs)", "Kilograms (kg)")),
               radioButtons(
                 "units",
                 strong("Units:"),
                 choices =c("Pounds (lbs)", "Kilograms (kg)"),
                 selected ="Pounds (lbs)",
                 inline = T),
               helpText(br()),
               
               plotOutput("nutbplot", width="50%"), 
               tableOutput("mytable"), 
               actionButton("go", "Screenshot"),
               downloadButton(
                 outputId = "downloader",
                 label = "Download PDF"
               )
      ),
      
      tabPanel("Harvest Estimator",
               # tags$img(src='swooshgn2.png'),
               tags$img(src='gn_swoosh_shellfish3.png'),
               titlePanel(h1("Aquaculture Nutrient Removal Calculator")),
               titlePanel(h6(em("Oyster nutrient removal data coverage ranges from ME to NC"))),
               helpText(br()),
               helpText(h3("Harvest Estimator For Nitrogen (N) Load Removal")),
               helpText("This will estimate the number of oysters required for harvest in order to offset the specified N load"),
               numericInput("Nload", strong("Nitrogen load into waterbody (lbs N)"), 0, min=0, max=NA),
               sliderInput(
                 "hsize2",
                 strong("Average oyster size at harvest (Inches)"),
                 2.0,
                 5.0,
                 3.0,
                 step = 0.1,
                 round = FALSE,
                 ticks = TRUE,
                 animate = FALSE,
                 width = NULL,
                 sep = ",",
                 dragRange = TRUE
               ),
               tableOutput("mytable2"), 
               
      ),
      
      tabPanel("Data Sources", 
               # tags$img(src='swooshgn2.png'),
               tags$img(src='gn_swoosh_shellfish3.png'),
               titlePanel(h1("Aquaculture Nutrient Removal Calculator")),
               titlePanel(h6(em("Oyster nutrient removal data coverage ranges from ME to NC"))),
               helpText(br()),
               tags$p(
                 h4("Data Contributors"),
                 p("ME: Damian Brady and Tom Kiffney - University of Maine"
                 ),
                 p("NH: Ray Grizzle and Krystin Ward - University of New Hampshire"
                 ),
                 p("MA: Josh Reitsma - Cape Cod Cooperative Extension"
                 ),
                 p("RI: Suzy Ayvazian - EPA Narragansett"
                 ),
                 p("CT: Skylar Bayer, Matt Poach, Shannon Meseck, and Julie Rose - NOAA Milford"
                 ),
                 p("NY: Jeff Levinton and Daria Sebastiano - Stony Brook University"
                 ),
                 p("NJ: Daphne Munroe and Janine Barr - Rutgers University"
                 ),
                 p("MD: Matt Poach - NOAA Milford; Julie Reichert-Nguyen - NOAA Chesapeake Bay Office; Suzanne Bricker - NOAA Oxford, and Matt Parker - Maryland Sea Grant"
                 ),
                 p("VA: Matt Poach - NOAA Milford; Julie Reichert-Nguyen - NOAA Chesapeake Bay Office; Suzanne Bricker - NOAA Oxford, and Matt Parker - Maryland Sea Grant"
                 ),
                 p("NC: Beth Darrow and Jessica Kinsella - University of North Carolina Wilmington"
                 )
               ),
               tags$img(src='Fig1.png'),
      ),
      tabPanel("References", 
               # tags$img(src='swooshgn2.png'),
               tags$img(src='gn_swoosh_shellfish3.png'),
               titlePanel(h1("Aquaculture Nutrient Removal Calculator")),
               titlePanel(h6(em("Oyster nutrient removal data coverage ranges from ME to NC"))),
               helpText(br()),
               tags$p(
                 h4("References"),
                 p("Cornwell, J., Rose, J., Kellogg, L., Luckenbach, M., Bricker, S., Paynter, K., Moore, C., Parker, M., Sanford, L., Wolinski, B., Lacatell, A., Fegley, L., and Hudson, K. (2016). Panel Recommendations on the Oyster BMP Nutrient and Suspended Sediment Reduction Effectiveness Determination Decision Framework and Nitrogen and Phosphorus Assimilation in Oyster Tissue Reduction Effectiveness for Oyster Aquaculture Practices. (Report to the Chesapeake Bay Program.  Available online at http://www.chesapeakebay.net/documents/Oyster_BMP_1st_Report_Final_Approved_2016-12-19.pdf).")
               ),
               p("Cornwell, J., S. Bricker, A. Lacatell, M. Luckenbach, F. Marenghi, C. Moore, M. Parker, K. Paynter, J. Rose, L. Sanford, W. Wolinski, O.N. Caretti, J. Reichert-Nguyen, & H.W. Slacum. 2023. Nitrogen and phosphorus reduction associated with harvest of hatchery-produced oysters and reef restoration: Assimilation and enhanced denitrification: Panel recommendations. Report submitted to the Chesapeake Bay Program Partnership Water Quality Goal Implementation Team January 27, 2023. (Report to the Chesapeake Bay Program.  Available online at https://d18lev1ok5leia.cloudfront.net/chesapeakebay/documents/Animal-Mortality-Mngmnt-Expert-Panel-Report-WQGIT-Approved.pdf)."
               ),
               p("Grizzle, R.E., Ward, K.M., Peter, C.R., Cantwell, M., Katz, D., and Sullivan, J. (2017). Growth, morphometrics and nutrient content of farmed eastern oysters, Crassostrea virginica (Gmelin), in New Hampshire, USA. Aquaculture Research 48, 1525-1537."
               ),
               p("Higgins, C.B., Stephenson, K., and Brown, B.L. (2011). Nutrient bioassimilation capacity of aquacultured oysters: quantification of an ecosystem service. Journal of Environmental Quality 40, 271-277."
               ),
               p("Lindahl, O., Hart, R., Hernroth, B., Kollberg, S., Loo, L.-O., Olrog, L., Rehnstam-Holm, A.-S., Svensson, J., Svensson, S., and Syversen, U. (2005). Improving marine water quality by mussel farming - a profitable solution for Swedish society. Ambio 34, 129-136."
               ),
               p("Poach, M., Morse, R., Meseck, S. Alvarado, A.,; Reichert-Nguyen, J., McFarland, K. Elliott, H., Kellogg, M. L., Luckenbach, M., and Rose, J. (2024, in review). Nutrient reduction by eastern oysters exhibits low variability associated with season, ploidy, and farm location. Marine Pollution Bulletin."
               ),
               p("Reitsma, J., Murphy, D.C., Archer, A.F., and York, R.H. (2017). Nitrogen extraction potential of wild and cultured bivalves harvested from nearshore waters of Cape Cod, USA. Marine Pollution Bulletin 116, 175-181."
               ),
               p("Rose, J.M., Bricker, S.B., Tedesco, M.A., and Wikfors, G.H. (2014). A Role for Shellfish Aquaculture in Coastal Nitrogen Management. Environmental Science & Technology 48, 2519-2525."
               ),
               p("Sebastiano, D., Levinton, J.S., Doall, M., and Kamath, S. (2015). Using a Shellfish Harvest Strategy to Extract High Nitrogen Inputs in Urban and Suburban Coastal Bays: Practical and Economic Implications. Journal of Shellfish Research 34, 573-583, 511."
               )
      ),
      tabPanel("Disclaimer",
               # tags$img(src='swooshgn2.png'),
               tags$img(src='gn_swoosh_shellfish3.png'),
               titlePanel(h1("Aquaculture Nutrient Removal Calculator")),
               titlePanel(h6(em("Oyster nutrient removal data coverage ranges from ME to NC"))),
               helpText(br()),
               h4("Disclaimer"),
               p("This is a scientific product and is not an official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government."
               )
      ),
    )
  )
)





server <- function(input, output) {
  
 # Add github version to top of page
  output$githubversion <- renderText({
    releases <- gh("GET /repos/{owner}/{repo}/releases", 
                   owner = "RMORSEcode",
                   repo = "Calculator")
    releases[[1]][["name"]]
    })
    
  output$mymap <- renderLeaflet({
    leaflet(height="50%") %>%
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
      digits = 4,
      na = "NA",
      quoted = FALSE
    )
  })
  
  
  
  table <- reactive({
    taval=1.42E-05
    tbval=2.60727827
    saval=0.00039042
    sbval=2.579747757
    tdw=taval*(input$hsize*25.4)^tbval
    sdw=saval*(input$hsize*25.4)^sbval
    
    #Convert dry weight of tissue and shell (g) to nutrients (g)
    tNi=reactiveValues()
    sNi=reactiveValues()
    tNi=0.0770*tdw
    sNi=0.0019*sdw
    
    #convert grams N to lbs or kg
    cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
    tN=reactiveValues()
    tN=round((tNi*cnvrt*input$Num),1)
    sN=reactiveValues()
    sN=round((sNi*cnvrt*input$Num),1)
    df=data.frame("Shell_N"=sN, "Tissue_N"=tN, "Total"=sN+tN, "Units"=input$units)
    colnames(df)=c("Shell N", "Tissue N", "Total", "Units")
    df
  })
  
  # estimate number of oysters required for N load
  esttable <- reactive({
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
    
    #convert grams N to lbs
    cnvrt=0.00220462
    # tN=reactiveValues()
    tN=tNi*cnvrt
    # sN=reactiveValues()
    sN=sNi*cnvrt
    ReNum=round(input$Nload/(sN+tN),-3)
    df3=data.frame("Total_N_load"=input$Nload, "Num"=ReNum)
    # formatC(df3, big.mark=",", scientific=FALSE)
    colnames(df3)=c("Total N Load", "Number of Oysters to Harvest")
    df3
  })
  
  plot <- reactive({
    taval=1.42E-05
    tbval=2.60727827
    saval=0.00039042
    sbval=2.579747757
    tdw=taval*((input$hsize*25.4)^tbval)
    sdw=saval*((input$hsize*25.4)^sbval)
    
    #Convert dry weight of tissue and shell (g) to nutrients (g)
    # tNi=reactiveValues()
    # sNi=reactiveValues()
    tNi=(0.0770*tdw)*input$Num
    sNi=(0.0019*sdw)*input$Num
    
    #convert grams N to lbs or kg
    cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
    # tN=reactiveValues()
    tN=round((tNi*cnvrt),1)
    # sN=reactiveValues()
    sN=round((sNi*cnvrt),1)
    # barplot(c(tN, sN), col = 'lightgray', border = 'white', xlab="N removal", 
    #         names.arg=c("Tissue N", "Shell N"), ylab=input$units)
    # df=data.frame("Tissue N"=tN,"Shell N"=sN, "Total N"=sN+tN, "Units"=input$units)
    df2=data.frame(matrix(tN, nrow=1, ncol=1))
    colnames(df2)="N"
    df2$var="Tissue"
    df2=rbind(df2, list(N=sN,var="Shell" ))
    df2=rbind(df2, list(N=sN+tN,var="Total" ))
    df2$units=input$units
    
    P=ggplot(df2, aes(x=var, y=N))+
      geom_bar(stat="identity" , fill="steelblue", width = 0.65)+
      # coord_cartesian(ylim=c(0, NA), xlim=NULL, clip = "on")+
      # ylim(0,max(df2$N))+
      # scale_y_continuous(limits = c(0, NA))+
      # aes(ymin=0)+
      theme_minimal()+
      ylab(input$units)+
      xlab("Nitrogen Removed")+
      theme(axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16))
    P
  })
  
  
  # Output Components
  output$nutbplot <- 
    renderPlot({
      plot()
    })
  output$mytable <-
    renderTable({
      table()
    })
  output$mytable2 <-
    renderTable({
      esttable()
    })
  
  # observeEvent(input$add, {
  #   output_name <- paste0("out_", input$add)
  #   output[[output_name]] <- renderText({
  #     isolate(input$add)
  #   })
  #   insertUI(
  #     selector = ifelse(input$add == 0L, "#add", paste0("#", "out_", input$add-1)),
  #     where = "afterEnd",
  #     ui = verbatimTextOutput(output_name)
  #   )
  # }, ignoreNULL = FALSE)
  
  observeEvent(input$go, {
    screenshot()
  })
  
  output$downloader <- 
    downloadHandler(
      "results_from_shiny.pdf",
      content = 
        function(file)
        {
          rmarkdown::render(
            input = "report.Rmd",
            output_file = "built_report.pdf",
            params = list(table = table(),
                          plot = plot(),
                          Location=input$projloc, 
                          Units=input$units, 
                          gear=input$gear, 
                          ploidy=input$ploidy, 
                          hsize=input$hsize,
                          Farm=input$farmname,
                          Number=input$Num,
                          Dates=input$Htime,
                          Lat=input$mymap_draw_new_feature$geometry$coordinates[[2]],
                          Lon=input$mymap_draw_new_feature$geometry$coordinates[[1]])
          ) 
          readBin(con = "built_report.pdf", 
                  what = "raw",
                  n = file.info("built_report.pdf")[, "size"]) %>%
            writeBin(con = file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
