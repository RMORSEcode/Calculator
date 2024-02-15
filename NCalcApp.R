# https://connect.fisheries.noaa.gov/Oyster-Calculator/
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyscreenshot)
library(ggplot2)
library(formatR)
library(tinytex)

ui <- fluidPage(
  
  tags$img(src='noaa-emblem-rgb-sm2-2022.png'),
  
  titlePanel(h1("US East Coast Oyster Nutrient Removal Calculator")),
  titlePanel(h4("Data coverage ranges from ME to NC")),
  
  textInput("farmname", "Project Name", value = "", width = NULL, placeholder = NULL),
  helpText("Please enter the name of the oyster farm"),
  
  textInput("projloc", "Harvest Location", value = "", width = NULL, placeholder = NULL),
  helpText("Please enter the name of the water body where the oysters were harvested from"),
  
  selectInput("units", "Units for nutrient removal:",c("Pounds (lbs)", "Kilograms (kg")
  ),
  
  selectInput("gear", "Gear type primarily used for growing oysters:",c("Floating", "Off-bottom", "On-Bottom")
  ),
  
  selectInput("ploidy", "Select oyster ploidy:",c("Diploid", "Triploid", "Combination")
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
  helpText("Please zoom to state level and add a location marker in the water where the oysters were harvested from"),
  
  tableOutput('loctable'),
  
  
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Calculator", 
               plotOutput("nutbplot", width="50%"), 
               tableOutput("mytable"), 
               actionButton("go", "Screenshot"),
               # downloadButton("report", "Generate report"),
               downloadButton(
                 outputId = "downloader",
                 label = "Download PDF"
               )
      ),
      tabPanel("About", 
               tags$p(
                 h2(strong("Background")),
                 p("Shellfish incorporate nutrients into their tissues and shell as they grow. At harvest, these nutrients are permanently removed from the coastal environment, providing a benefit to water quality in the form of excess nutrient reduction. The US East Coast Oyster Nutrient Removal Calculator is a tool designed for resource managers to inform shellfish aquaculture permitting. Resource managers have expressed interest in easy-to-use tools that produce location and operation-appropriate values for beneficial services, and they need the values to be produced in a format that aligns with their permitting process."
                   ),
                 p("The nutrient removal calculations are based on relationships of oyster dry weight-to-length and the average nitrogen (N) concentrations in shell and tissue material. First, we estimate the weight of the oysters based on the typical size of oysters harvested on a farm. The weight estimates are based on non-linear quantile regressions of oyster shell height and dry-weight for both tissue and shell material. Next, the N portion of total oyster weight is calculated using the average N concentration value for both shell and tissue. Adding the tissue and shell nitrogen yields the total weight of  N per oyster. This result is scaled to the total number of oysters harvested, as input by the user."
                   ),
                 br(),
                 h2(strong("Calculator Inputs")),
                 p("The Oyster Nutrient Removal Calculator can be used for new permit applications based on estimated production value or to provide information on existing farms from actual harvest numbers. The grower provides information on:"
                   ),
                 p(strong("- Number of oysters harvested or to be harvested")
                 ),
                 p(strong("- size of oysters at harvest")
                 ),
                 p("- type of gear (floating gear vs. off-bottom gear vs. on-bottom)*"
                 ),
                 p("- ploidy (diploid or triploid, or a combination)*"
                 ),
                 p(em("*We are actively seeking feedback from the aquaculture community on the inclusion of these factors, given the small effect that they had in our data analysis.")
                 ),
                 p("Farm location and period of harvest (1 day to 5 years) will be included as inputs for use in generating the report, but will not affect the calculation."
                   ),
                 br(),
                 h2(strong("Summary")),
                 p("We have synthesized available literature for eastern oyster farms across the Northeast region (North Carolina to Maine), and applied methodology used by the Chesapeake Bay Program to calculate nutrient removal at harvest. Variability in oyster tissue and shell nutrient concentration was low, and an assessment of farm location, ploidy, and cultivation practice (with vs. without gear) suggested that a single average value could reasonably be applied across all farms."
                   ),
               )
      ),
      
      tabPanel("Data Sources", 
               tags$p(
                 h2(strong("Data Contributors")),
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
                 p("MD: Matt Poach - NOAA Milford; Julie Reichert-Nguyen - NOAA Chesapeake Bay Office; Suzanne Bricker and Matt Parker - NOAA Oxford"
                 ),
                 p("VA: Matt Poach - NOAA Milford; Julie Reichert-Nguyen - NOAA Chesapeake Bay Office; Suzanne Bricker and Matt Parker - NOAA Oxford"
                 ),
                 p("NC: Beth Darrow and Jessica Kinsella - University of North Carolina Wilmington"
                 )
               )
      ),
      tabPanel("References", 
               tags$p(
                 h2(strong("References")),
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
               h2(strong("Disclaimer")),
                 p("This is a scientific product and is not an official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government."
                   )
               ),
    )
  )
)
  

  


server <- function(input, output) {
  

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
  
  #****************************************
  #* Reactive Values
  
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
    
    # if(input$selvar=="Ploidy"){
    #   taval=ifelse(input$ploidy=="Diploid",3E-05, 2.7E-05)
    #   tbval=ifelse(input$ploidy=="Diploid",2.43, 2.37)
    #   saval=ifelse(input$ploidy=="Diploid",0.000451, 0.000234)
    #   sbval=ifelse(input$ploidy=="Diploid",2.551, 2.659)
    # }
    # else{
    #   taval=ifelse(input$gear=="Floating",4.33E-06,
    #                ifelse(input$gear=="Bottom",0.000301, 0.000381))
    #   tbval=ifelse(input$gear=="Floating",2.88,
    #                ifelse(input$gear=="Bottom",1.89, 1.88))
    #   saval=ifelse(input$gear=="Floating",0.00012,
    #                ifelse(input$gear=="Bottom",0.0005, 0.0011))
    #   sbval=ifelse(input$gear=="Floating",2.84,
    #                ifelse(input$gear=="Bottom",2.522, 2.34))
    # }
    
    # # Single regression values
    # taval=1.42E-05
    # tbval=2.60727827
    # saval=0.00039042
    # sbval=2.579747757
    # tdw=input$Num*(taval*((input$hsize*25.4)^tbval))
    # sdw=input$Num*(saval*((input$hsize*25.4)^sbval))
    # 
    # #Convert dry weight of tissue and shell (g) to nutrients (g)
    # tNi=reactiveValues()
    # sNi=reactiveValues()
    # tNi=(0.0796*tdw)
    # sNi=(0.0019*sdw)
    # 
    # #convert grams N to lbs or kg
    # cnvrt=ifelse(input$units=="Pounds",0.00220462,0.001)
    # tN=reactiveValues()
    # tN=tNi*cnvrt
    # sN=reactiveValues()
    # sN=sNi*cnvrt
    
    table <- reactive({
      taval=1.42E-05
      tbval=2.60727827
      saval=0.00039042
      sbval=2.579747757
      tdw=input$Num*(taval*((input$hsize*25.4)^tbval))
      sdw=input$Num*(saval*((input$hsize*25.4)^sbval))
      
      #Convert dry weight of tissue and shell (g) to nutrients (g)
      tNi=reactiveValues()
      sNi=reactiveValues()
      tNi=(0.0796*tdw)
      sNi=(0.0019*sdw)
      
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds",0.00220462,0.001)
      tN=reactiveValues()
      tN=round((tNi*cnvrt),1)
      sN=reactiveValues()
      sN=round((sNi*cnvrt),1)
      data.frame("Shell N"=sN, "Tissue N"=tN, "Total N"=sN+tN, "Units"=input$units)
      })
    
    plot <- reactive({
      taval=1.42E-05
      tbval=2.60727827
      saval=0.00039042
      sbval=2.579747757
      tdw=input$Num*(taval*((input$hsize*25.4)^tbval))
      sdw=input$Num*(saval*((input$hsize*25.4)^sbval))
      
      #Convert dry weight of tissue and shell (g) to nutrients (g)
      tNi=reactiveValues()
      sNi=reactiveValues()
      tNi=(0.0796*tdw)
      sNi=(0.0019*sdw)
      
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds",0.00220462,0.001)
      tN=reactiveValues()
      tN=round((tNi*cnvrt),1)
      sN=reactiveValues()
      sN=round((sNi*cnvrt),1)
      # barplot(c(tN, sN), col = 'lightgray', border = 'white', xlab="N removal", 
      #         names.arg=c("Tissue N", "Shell N"), ylab=input$units)
      # df=data.frame("Tissue N"=tN,"Shell N"=sN, "Total N"=sN+tN, "Units"=input$units)
      df=data.frame(matrix(tN, nrow=1, ncol=1))
      colnames(df)="N"
      df$var="Tissue"
      df=rbind(df, list(N=sN,var="Shell" ))
      df=rbind(df, list(N=sN+tN,var="Total" ))
      df$units=input$units
      
      P=ggplot(df, aes(x=var, y=N))+
        geom_bar(stat="identity" , fill="steelblue")+
        theme_minimal()+
        ylab(input$units)+
        xlab("Nitrogen Removed")
      P
      })
    
    #****************************************
    #*
    #* Output Components
    output$nutbplot <- 
      renderPlot({
        plot()
      })
    output$mytable <-
      renderTable({
        table()
      })

    
    # output$nutbplot <- renderPlot({
    #   
    # barplot(c(tN, sN), col = 'lightgray', border = 'white', xlab="N removal", 
    #         names.arg=c("Tissue N", "Shell N"), ylab=input$units)
    # })
    # output$mytable <- 
    #   renderTable({
    #     table()
    #   })
    # output$mytable <- renderTable({
    #   table("Tissue N"=tN,"Shell N"=sN, "Total N"=sN+tN, "Units"=input$units,
    #   striped = T,
    #   hover = F,
    #   bordered = T,
    #   spacing = c("s", "xs", "m", "l"),
    #   width = "auto",
    #   align = NULL,
    #   rownames = FALSE,
    #   colnames = TRUE,
    #   digits = NULL,
    #   na = "NA",
    #   quoted = FALSE
    #   )
    #   })
  
    #   output$mytable <- renderTable(
    #     data.frame("Tissue N"=tN,"Shell N"=sN, "Total N"=sN+tN, "Units"=input$units),
    #     striped = T,
    #     hover = F,
    #     bordered = T,
    #     spacing = c("s", "xs", "m", "l"),
    #     width = "auto",
    #     align = NULL,
    #     rownames = FALSE,
    #     colnames = TRUE,
    #     digits = NULL,
    #     na = "NA",
    #     quoted = FALSE
    #     )
    # })
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

  
  
  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.pdf",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     # testing pdf
  #     params <- list(Location=input$projloc, Units=input$units, gear=input$gear, ploidy=input$ploidy, hsize=input$hsize,
  #                    Lat=input$mymap_draw_new_feature$geometry$coordinates[[2]], Lon=input$mymap_draw_new_feature$geometry$coordinates[[1]]) 
  #     # params <- list(Location=input$state, Tissue.N=output$table$tN,Shell.N=sN, Total.N=dataInput$sN+dataInput$tN, 
  #     #                Units=input$units, gear=input$gear, ploidy=input$ploidy, hsize=input$hsize) 
  #     # table=nutbplot(), plot=nutbplot(), 
  #     # "Lon"=feature$geometry$coordinates[[1]],"Lat"=feature$geometry$coordinates[[2]]
  #     # "Num"=input$Num, "htime"=input$Htime, "loc2"=input$farmloc)
  #     # "Location"=input$state, "Tissue.N"=output$tN,"Shell.N"=output$sN,
  #     # "Total.N"=sN+tN,"Units"=input$units,
  #     # "gear"=input$gear, "ploidy"=input$ploidy, "hsize"=input$hsize, 
  #     # "Num"=input$Num, "htime"=input$Htime, "loc2"=input$farmloc)
  #     
  #     # n = c("units"=input$units, "gear"=input$gear, "ploidy"=input$ploidy, "hsize"=input$hsize, "Num"=input$Num, "htime"=input$Htime, "loc"=input$farmloc))
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
  
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
