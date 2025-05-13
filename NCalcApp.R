### Open version:
# https://connect.fisheries.noaa.gov/ANRC/ open version
### Test version (NOAA internal):
# https://test-connect.fisheries.noaa.gov/Calculator/

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
# library(shinyscreenshot)
library(ggplot2)
library(formatR)
library(tinytex)
library(gh)
library(png)
library(gridExtra)
library(grid)
library(bslib)

ui <- fluidPage(style = 'margin-left: 10%; margin-right: 10%;',
                ### Title ###
                # theme = bslib::bs_theme(bootswatch = "lux"),
                # shinythemes::themeSelector(),
                theme = bslib::bs_theme(bootswatch = "cerulean"),
                helpText(strong("Calculator Version:", style = "font-size:18px;")),
                textOutput("githubversion"),
                helpText(br()),
                # setBackgroundImage(src='background1.png'),
                
                mainPanel(
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Calculator", 
                             # tags$img(src='swooshgn2.png'),
                             # tags$img(src='gn_swoosh_shellfish3.png'),
                             tags$img(src='white_swoosh_cage_500pxH.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and multiple oysters on and in cage"),
                             titlePanel(h1("Aquaculture Nutrient Removal Calculator"), windowTitle = "Aquaculture Nutrient Removal Calculator"),
                             helpText(br()),
                             
                             ### add text box with black border ### #5761C0  style = "border-style: solid; border-color: #C6E6F0#5EB6D9; background-color: #5EB6D9;",
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("This calculator predicts the amount of nitrogen and phosphorous farmed eastern oysters remove from the water when harvested, a key environmental benefit that oysters provide. This tool applies to oyster farms located within the geographic range of North Carolina to Maine, USA.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px;color: white"),
                                  p("To use the tool, please fill in information about your farm in sections 1-3 below.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white"),
                                  p("To download a report, click on ",strong("Generate PDF Report")," at the bottom", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white")),
                             helpText(br()),
                             
                             ### 1 FARM PRACTICES ###
                             helpText(h3("1) Farm Practices")),
                             ## Name
                             textAreaInput("farmname", div(strong("Project Name:"), " Please enter the name of the oyster farm"),value = "", width="100%", rows=2, placeholder = NULL),
                             helpText(br()),
                             ## Culture Method
                             selectInput("gear", div(strong("Culture Method:")," Select the gear type primarily used for growing oysters, or select 'On-Bottom' for no gear", em("(will not affect calculation)")),c("Floating", "Off-bottom", "On-Bottom", "Multiple methods used"), width="100%"),
                             helpText(br()),
                             ## Ploidy
                             selectInput("ploidy", div(strong("Oyster Ploidy:")," Please select the ploidy of the oysters that were harvested", em("(will not affect calculation)")),c("Diploid", "Triploid", "Combination"), width="100%"),
                             helpText(br()),
                             
                             # #####______________________________________
                             # ### 2 LOCATION ###
                             # helpText(h3("2) Farm Location")),
                             # textAreaInput("projloc", div(strong("Harvest Location:"), " Please enter the name of the water body where the oysters were harvested from", em("(will not affect calculation)")), value = "", width ="100%", rows=2, placeholder = NULL),
                             # helpText(br()),
                             # helpText(h6("Approximate Coordinates: "),"Please scroll or pinch to zoom to the harvest location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
                             # leafletOutput("mymap", width="100%", height=400),
                             # ## Location table
                             # tableOutput('loctable'),
                             # helpText(br()),
                             # 
                             # ### 3 HARVEST DETAILS ###
                             # helpText(h3("3) Harvest Details")),
                             # input_switch("switch", div(strong("Click Here If Oyster Seed Is Imported And Planted For Growout"), value=F, width = "100%")), 
                             # conditionalPanel(
                             #   condition = "input.switch == true",
                             #   input_switch("switch2", div(strong("Click Here If Oyster Nursery And Growout Locations Are Different"), value=F, width = "100%")), 
                             #   conditionalPanel(
                             #     condition = "input.switch2 == true",
                             #     helpText(h6("Nursery Location: "),"Please scroll or pinch to zoom to the nursery area, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
                             #     leafletOutput("spatmap", width="100%", height=400),
                             #     tableOutput('spatloctable'),
                             #   ),
                             #   sliderInput(
                             #     "sizeIn",
                             #     div(strong("Average seed oyster size at planting (Inches):"), " Please drag the slider to select the average size of the oysters at the time of planting"),
                             #     0,
                             #     2.0,
                             #     0.2,
                             #     step = 0.1,
                             #     round = FALSE,
                             #     ticks = TRUE,
                             #     animate = FALSE,
                             #     width = "100%",
                             #     sep = ",",
                             #     dragRange = TRUE
                             #   ),
                             #   # sliderInput(
                             #   #   "sizeOut",
                             #   #   div(strong("Select size of oysters at harves (inches):"), " Please drag the slider to select the average size of the oysters at the time of harvest"),
                             #   #   2.0,
                             #   #   6.0,
                             #   #   3.0,
                             #   #   step = 0.1,
                             #   #   round = FALSE,
                             #   #   ticks = TRUE,
                             #   #   animate = FALSE,
                             #   #   width = "100%",
                             #   #   sep = ",",
                             #   #   dragRange = TRUE
                             #   # ),
                             # ),
                             # 
                             # ## Size
                             # sliderInput(
                             #   "hsize",
                             #   div(strong("Average oyster size at harvest (Inches):"), " Please drag the slider to select the average size of the oysters that were harvested"),
                             #   2.0,
                             #   6.0,
                             #   3.0,
                             #   step = 0.1,
                             #   round = FALSE,
                             #   ticks = TRUE,
                             #   animate = FALSE,
                             #   width = "100%",
                             #   sep = ",",
                             #   dragRange = TRUE
                             # ),
                             # # ),
                             # helpText(br()),
                             # ## Number
                             # numericInput("Num", div(strong("Number of oysters at harvest:")," Please enter the total number of oysters harvested at the selected size"), 0, min=0, max=NA, width="100%"),
                             # helpText(br()),
                             # ## Dates
                             # # dateRangeInput("Htime", div(strong("Period of harvest (yyyy-mm-dd):"), em("(does not affect calculation)")), start=NULL, end=NULL, min=Sys.Date()-(5*365), max=Sys.Date()+(2*365), startview = "month", width="100%"),
                             # dateRangeInput("Htime", div(strong("Period of harvest (yyyy-mm-dd):"), em("(does not affect calculation)")), start=NULL, end=NULL, min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month", width="100%"),
                             # #####_________________________________________
                             
                             ### 2 REMOVAL DETAILS ###
                             helpText(h3("2) Oyster Removal Details")),
                             helpText(h4("Please Select From The Following Choices:")),
                             input_switch("seedonly", div(strong("I Only Grow Seed Oysters"), value=F, width = "100%")), 
                             conditionalPanel(
                               condition = "input.seedonly == true",
                               helpText(h6("Nursery Location: "),"Please scroll or pinch to zoom to the nursery area, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
                               leafletOutput("spatonlymap", width="100%", height=400),
                               tableOutput('spatonlyloctable'),
                               sliderInput(
                                 "spatSizeOut",
                                 div(strong("Average seed oyster size (mm):"), " Please drag the slider to select the average size of the oyster seed removed for sale"),
                                 1,
                                 30,
                                 5,
                                 step = 1,
                                 round = FALSE,
                                 ticks = TRUE,
                                 animate = FALSE,
                                 width = "100%",
                                 sep = ",",
                                 dragRange = TRUE
                               ),
                               numericInput("spatNum", div(strong("Number of spat removed:")," Please enter the total number of spat removed from the selected size"), 0, min=0, max=NA, width="100%"),
                               helpText(br()),
                             ),
                             input_switch("plantseed", div(strong("I Buy Seed Oysters And Plant On Site For Harvest"), value=F, width = "100%")), 
                             conditionalPanel(
                               condition = "input.plantseed == true",
                               input_switch("nurseryloc", div(strong("My Oyster Nursery And Growout Locations Are Different"), value=F, width = "100%")), 
                               conditionalPanel(
                                 condition = "input.nurseryloc == true",
                                 helpText(h6("Nursery Location: "),"Please scroll or pinch to zoom to the nursery area, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
                                 leafletOutput("spatmap", width="100%", height=400),
                                 tableOutput('spatloctable'),
                               ),
                               textAreaInput("projloc", div(strong("Waterbody Name:"), " Please enter the name of the water body where the oysters were harvested from", em("(will not affect calculation)")), value = "", width ="100%", rows=2, placeholder = NULL),
                               helpText(br()),
                               helpText(h6("Harvest Location: "),"Please scroll or pinch to zoom to the harvest location, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
                               leafletOutput("mymap", width="100%", height=400),
                               ## Location table
                               tableOutput('loctable'),
                               helpText(br()),
                               sliderInput(
                                 "sizeIn",
                                 div(strong("Average seed oyster size at planting (mm):"), " Please drag the slider to select the average size of the oysters at the time of planting"),
                                 1,
                                 30,
                                 5,
                                 step = 1,
                                 round = FALSE,
                                 ticks = TRUE,
                                 animate = FALSE,
                                 width = "100%",
                                 sep = ",",
                                 dragRange = TRUE
                               ),
                               br(),
                               sliderInput(
                                 "sizeOut",
                                 div(strong("Select size of oysters at harves (inches):"), " Please drag the slider to select the average size of the oysters at the time of harvest"),
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
                               ## Number
                               numericInput("HNum", div(strong("Number of oysters at harvest:")," Please enter the total number of oysters harvested at the selected size"), 0, min=0, max=NA, width="100%"),
                               helpText(br()),
                               dateRangeInput("Htime", div(strong("Period of harvest (yyyy-mm-dd):"), em("(does not affect calculation)")), start=NULL, end=NULL, min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month", width="100%"),
                               br()
                             ),
                             
                             helpText(br()),
                             ## Units
                             radioButtons(
                               "units",
                               div(strong("Units:")," Select the units for nutrient removal"),
                               choices =c("Pounds (lbs)", "Kilograms (kg)"),
                               selected ="Pounds (lbs)",
                               inline = T,
                               width="100%"),
                             helpText(br()),
                             downloadButton(
                               outputId = "downloader",
                               label = "Generate PDF Report"
                             ),
                             br(),
                             br(),
                             
                             fluidRow(
                               splitLayout(style = "border: 1px solid silver:", cellWidths = c(300,300), 
                                           plotOutput("nutbplot", width="80%"), 
                                           plotOutput("nutbplot2", width="80%")
                               )
                             ),
                             # plotOutput("nutbplot", width="50%"), 
                             # helpText(br()),
                             # plotOutput("nutbplot2", width="50%"), 
                             # helpText(br()),
                             tableOutput("mytable"),
                             # renderTable("mytable", rownames = TRUE), 
                             
                             # actionButton("go", "Screenshot"),
                             br(),
                             plotOutput("fertplot", width="75%"),
                             br(),
                             # radioButtons("extension", "Save As:",
                             #              choices = c("png", "svg"), inline = TRUE),
                             downloadButton(
                               outputId = "download",
                               label = "Download Customized Infographic"
                             ),
                             br(),
                             br(),
                             h6(tags$a(target="_blank", href="https://doi.org/10.5281/zenodo.11966672",
                                       "Access publicly available data used to create this tool >")),
                             h6(tags$a(target="_blank", href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0310062",
                                       "Read about methods used to create this tool >")),
                             br(),
                             h4("Disclaimer"),
                             p("This is a scientific product and is not an official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government."
                             ),
                    ),
                    
                    tabPanel("Reverse Calculator",
                             tags$img(src='white_swoosh_orange_bin_500pxH.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and oysters in orange basket."),
                             titlePanel(h1("Aquaculture Nutrient Removal Calculator"), windowTitle = "Aquaculture Nutrient Removal Calculator"),
                             helpText(br()),
                             ### add text box with black border ### "border-style: solid; border-color: gray; background-color: #838B8B;"
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("The Reverse Calculator predicts the number of eastern oysters needed to harvest in order to offset a specified nitrogen load at a farm located within the geographic range of North Carolina to Maine, USA.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p("To use the tool, please enter a nitrogen load and the average size of oyters at harvest.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;")),
                             # p("To download a report, click on ",strong("Generate PDF Report")," at the bottom", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:15px;")),
                             helpText(br()),
                             helpText(br()),
                             helpText(h3("Harvest Estimator For Nitrogen (N) Load Removal")),
                             numericInput("Nload", strong("Nitrogen load into waterbody (lbs N)"), 0, min=0, max=NA, width="100%"),
                             helpText(br()),
                             sliderInput(
                               "hsize2",
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
                             helpText(br()),
                             tableOutput("mytable2"),
                             helpText(br()),
                             h4("Disclaimer"),
                             p("This is a scientific product and is not an official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government."
                             ),
                    ),
                    
                    tabPanel("About", 
                             tags$img(src='white_swoosh_hand_left2_500pxH.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, and five oysters being held in palm, with additional oysters in the background."),
                             titlePanel(h1("Aquaculture Nutrient Removal Calculator"), windowTitle = "Aquaculture Nutrient Removal Calculator"),
                             helpText(br()),
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("About the Calculator:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:20px; color: white;"),
                                  p("The Aquaculture Nutrient Removal Calculator can be used for new permit applications based on estimated production value, or to provide information on existing farms from actual harvest numbers. The grower provides information on:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- Number of oysters harvested or to be harvested"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- Size of oysters at harvest"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p("- Culture method (floating gear vs. off-bottom gear vs. on-bottom)*", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p("- Ploidy (diploid, triploid, or a combination)*", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p("- Farm location and period of harvest (1 day to 5 years) will be included as inputs for use in generating the report, but will not affect the calculation.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(em("*We are actively seeking feedback from the aquaculture community on the inclusion of these factors, given the small effect that they had in our data analysis."), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;")),
                             helpText(br()),
                             tags$p(
                               h4("Background"),
                               helpText(strong("Excess nutrients in coastal waters"), style = "font-size:18px;"),
                               p("Nitrogen (N) and phosphorous (P) are essential nutrients, but excess levels of these nutrients in coastal waters can lead to algal blooms, low oxygen concentrations, and other detrimental effects. Shellfish incorporate nutrients into their tissues and shell as they grow. At harvest, these nutrients are permanently removed from the coastal environment, providing a benefit to water quality in the form of excess nutrient reduction."
                               ),
                               tags$img(src='1500x1000-Oyster-Farms-Nutrients-Infographic-NEFSC.png', width = "100%", alt="This illustration shows a landscape in the background with agricultural fields, houses with lawns, and a river washing nutrients from those sources into an underwater scene in the foreground where small dots representing algae flow from the left into a cage stacked with oysters at the center, a farmer in a small boat harvests the oysters, and clear water on the right with a variety of fish represents a healthier habitat with better water quality."),
                               helpText(br()),
                               helpText(strong("The Aquaculture Nutrient Removal Calculator"), style = "font-size:18px;"),
                               p("The calculator is a tool designed for shellfish growers and resource managers to inform shellfish aquaculture permitting. Resource managers have expressed interest in easy-to-use tools that produce location and operation-appropriate values for the environmental benefits, or ecosystem services, shellfish farms provide. The calculator provides estimated values for nutrient removal in a format that aligns with the shellfish aquaculture permitting process."
                               ),
                               p("The nutrient removal calculations are based on relationships of oyster dry weight-to-length and the average nitrogen and phoshphorous concentrations in oyster shell and tissue. First, we estimate the weight of the oysters based on the typical size of oysters harvested on a farm. The weight estimates are based on non-linear quantile regressions of oyster shell height and dry-weight for both tissue and shell. Next, the nutrient portion of total oyster weight is calculated using the average nitrogen and phosphorous concentration value for both shell and tissue. Adding the tissue and shell nutrients yields the total weight of nitrogen and phosphorous per oyster. This result is scaled to the total number of oysters harvested, as input by the user."
                               ),
                               p("We have synthesized available literature for eastern oyster farms across the Northeast region, from North Carolina to Maine, and applied methodology used by the Chesapeake Bay Program to calculate nutrient removal at harvest ",
                                 tags$a(style="font-weight:bold", target="_blank", href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0310062",
                                        "(Rose et al. 2024)."),
                                 " Variability in oyster tissue and shell nutrient concentration was low, and an assessment of farm location, ploidy, and cultivation practice (with vs. without gear) suggested that a single average value could reasonably be applied across all farms."
                               ),
                               
                             ),
                             h6(tags$a(target="_blank", href="https://doi.org/10.5281/zenodo.11966672",
                                       "Access publicly available data used to create this tool >")
                             ),
                             br(),
                             h4("Location of Eastern oyster", em("(Crassostrea virginica)"), "samples from aquaculture farm sites used to develop the calculator"
                             ),
                             br(),
                             leafletOutput("contmap", width="100%", height=400),
                             br(),
                             tags$p(
                               h4("Data Contributors:"),
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
                             br(),
                             tags$p(
                               h4("Project Team"),
                               tags$a(target="_blank", href="https://www.fisheries.noaa.gov/contact/julie-m-rose-phd", "Julie Rose,"),
                               tags$a(target="_blank", href="https://coastalscience.noaa.gov/staff/christopher-schillaci/", "Chris Schillaci,"),
                               tags$a(target="_blank", href="https://www.fisheries.noaa.gov/contact/zachary-gordon", "Zach Gordon,"),
                               tags$a(target="_blank", href="https://www.fisheries.noaa.gov/contact/ryan-morse-phd","Ryan Morse"),
                             ),
                             div( style = "border-style: solid; border-radius: 10px; border-color: #0085CA; background-color: #0085CA;",
                                  p("Send questions or comments to:",style="text-align:center; padding-left:10px; padding-right:10px; font-size:16px; color: white"),
                                  p("ES.Tools@noaa.gov",style="text-align:center; padding-left:10px; padding-right:10px; font-size:16px; color: white"),
                             ),
                             tags$p(
                               h4("References:"),
                               p("Barr, J. M., Munroe, D., Rose, J. M., Calvo, L., Cheng, K. M., Bayer, S., & D. Kreeger. (2023). Seasonal Feeding Behavior of Aquaculture Eastern Oysters (Crassostrea virginica) in the Mid-Atlantic. Estuaries and Coasts. doi 10.1007/s12237-023-01293-9"
                               ),
                               p("Bayer, S. R., Cubillo, A. M., Rose, J. M., Ferreira, J. G., Dixon, M., Alvarado, A., Barr, J., Bernatchez, G., Meseck, S., Poach, M., Pousse, E., Wikfors, G. H., & S. Bricker. (2024). Refining the Farm Aquaculture Resource Management Model for Shellfish Nitrogen Removal at the Local Scale. Estuaries and Coasts. doi 10.1007/s12237-024-01354-7"
                               ),
                               p("Clements, J. C., & L. A. Comeau. (2019). Nitrogen removal potential of shellfish aquaculture harvests in eastern Canada: A comparison of culture methods. Aquaculture Reports, 13, 100183. https://doi.org/https://doi.org/10.1016/j.aqrep.2019.100183"
                               ),
                               p("Cornwell, J., Rose, J., Kellogg, L., Luckenbach, M., Bricker, S., Paynter, K., Moore, C., Parker, M., Sanford, L., Wolinski, B., Lacatell, A., Fegley, L., & K. Hudson. (2016). Panel Recommendations on the Oyster BMP Nutrient and Suspended Sediment Reduction Effectiveness Determination Decision Framework and Nitrogen and Phosphorus Assimilation in Oyster Tissue Reduction Effectiveness for Oyster Aquaculture Practices. (Report to the Chesapeake Bay Program. Available online at http://www.chesapeakebay.net/documents/Oyster_BMP_1st_Report_Final_Approved_2016-12-19.pdf)."
                               ),
                               p("Cornwell, J., S. Bricker, A. Lacatell, M. Luckenbach, F. Marenghi, C. Moore, M. Parker, K. Paynter, J. Rose, L. Sanford, W. Wolinski, O. N. Caretti, J. Reichert-Nguyen, & H. W. Slacum. (2023). Nitrogen and phosphorus reduction associated with harvest of hatchery-produced oysters and reef restoration: Assimilation and enhanced denitrification: Panel recommendations. Report submitted to the Chesapeake Bay Program Partnership Water Quality Goal Implementation Team January 27, 2023. (Report to the Chesapeake Bay Program. Available online at https://d18lev1ok5leia.cloudfront.net/chesapeakebay/documents/Animal-Mortality-Mngmnt-Expert-Panel-Report-WQGIT-Approved.pdf)."
                               ),
                               p("Grizzle, R. E., Ward, K. M., Peter, C. R., Cantwell, M., Katz, D., & J. Sullivan. (2017). Growth, morphometrics and nutrient content of farmed eastern oysters, Crassostrea virginica (Gmelin), in New Hampshire, USA. Aquaculture Research 48, 1525-1537."
                               ),
                               p("Higgins, C. B., Stephenson, K., & B. L. Brown. (2011). Nutrient bioassimilation capacity of aquacultured oysters: quantification of an ecosystem service. Journal of Environmental Quality 40, 271-277."
                               ),
                               p("Lindahl, O., Hart, R., Hernroth, B., Kollberg, S., Loo, L.-O., Olrog, L., Rehnstam-Holm, A.-S., Svensson, J., Svensson, S., & U. Syversen. (2005). Improving marine water quality by mussel farming - a profitable solution for Swedish society. Ambio 34, 129-136."
                               ),
                               p("Morse, R., Rose, J., Schillaci, C., Ayvazian, S., Barr, J., Bayer, S., Brady, D., Bricker, S., Darrow, E., Doall, M., Grizzle, R., Kiffney, T., Kinsella, J., Levinton, J., Meseck, S., Munroe, D., Parker, M., Poach, M., Reichert-Nguyen, J., … Ward, K. (2024). Morphometrics and nutrient concentration of farmed eastern oysters (Crassostrea virginica) from the US Northeast Region [Data set]. In PLOS ONE. Zenodo.",
                                 tags$a(target="_blank", href="https://doi.org/10.5281/zenodo.11966672", "https://doi.org/10.5281/zenodo.11966672"),
                               ),
                               p("Poach, M., Morse, R., Meseck, S. L., Alvarado, A., Reichert-Nguyen, J., McFarland, K., Elliott, H., Kellogg, M. L., Luckenbach, M. W., & J. M. Rose. (2024). Nutrient reduction by eastern oysters exhibits low variability associated with reproduction, ploidy, and farm location. Marine Pollution Bulletin 202, 116286. doi 10.1016/j.marpolbul.2024.116286"
                               ),
                               p("Reitsma, J., Murphy, D. C., Archer, A. F., & R. H. York. (2017). Nitrogen extraction potential of wild and cultured bivalves harvested from nearshore waters of Cape Cod, USA. Marine Pollution Bulletin 116, 175-181."
                               ),
                               p("Rose, J. M., Bricker, S. B., Tedesco, M. A., & G. H. Wikfors. (2014). A Role for Shellfish Aquaculture in Coastal Nitrogen Management. Environmental Science & Technology 48, 2519-2525."
                               ),
                               p("Rose, J. M., Morse, R. E., & C. Schillaci. (2024). Development and application of an online tool to quantify nitrogen removal associated with harvest of cultivated eastern oysters. PLoS ONE 19(9): e0310062.",
                                 tags$a(target="_blank", href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0310062", "https://doi.org/10.1371/journal.pone.0310062")
                               ),
                               p("Sebastiano, D., Levinton, J. S., Doall, M., & S. Kamath. (2015). Using a Shellfish Harvest Strategy to Extract High Nitrogen Inputs in Urban and Suburban Coastal Bays: Practical and Economic Implications. Journal of Shellfish Research 34, 573-583, 511."
                               ),
                               h4("Disclaimer:"),
                               p("This is a scientific product and is not an official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government."
                               ),
                             ),
                    )
                  )
                )
)





server <- function(input, output, session) {
  session$onSessionEnded(function() { stopApp() })
  stations=readxl::read_xlsx("Location_data.xlsx",sheet='final2', range='A1:F34')
  # aquaculture=sf::st_read("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Habitat/Marine Cadastre/Aquaculture.shp")
  # NESaquaculture=sf::st_crop(aquaculture, xmin=-77, ymin=35, xmax=-66.98481, ymax=60.83483) #xmin: -160.7436 ymin: 19.52108 xmax: -66.98481 ymax: 60.83483
  
  # Add github version to top of page
  output$githubversion <- renderText({
    releases <- gh("GET /repos/{owner}/{repo}/releases", 
                   owner = "RMORSEcode",
                   repo = "Calculator")
    releases[[1]][["name"]]
  })
  
  output$mymap <- renderLeaflet({
    leaflet(height="50%") %>%
      # addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
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
  
  output$spatonlymap <- renderLeaflet({
    leaflet(height="50%") %>%
      # addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
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
  
  output$spatmap <- renderLeaflet({
    leaflet(height="50%") %>%
      # addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
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
  
  # output$aquaMap <- renderLeaflet({
  #   NESaquaculture %>% 
  #     leaflet(height="50%") %>%
  #     addTiles() %>% 
  #     setView(lng = -70, lat = 40, zoom = 5) %>%
  #     addPolygons()
  #   addDrawToolbar(
  #     targetGroup='Selected',
  #     polylineOptions=FALSE,
  #     polygonOptions=FALSE,
  #     markerOptions = T,
  #     rectangleOptions =F,
  #     circleOptions = F,
  #     circleMarkerOptions = F,
  #     editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  # })
  
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
  
  observeEvent(input$spatmap_draw_new_feature,{
    feature2 <- input$spatmap_draw_new_feature
    
    output$spatloctable <- renderTable(
      data.frame("Lon"=feature2$geometry$coordinates[[1]],"Lat"=feature2$geometry$coordinates[[2]]),
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
  
  observeEvent(input$spatonlymap_draw_new_feature,{
    feature3 <- input$spatonlymap_draw_new_feature
    
    output$spatonlyloctable <- renderTable(
      data.frame("Lon"=feature3$geometry$coordinates[[1]],"Lat"=feature3$geometry$coordinates[[2]]),
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
  # add  data contributor map
  output$contmap <- renderLeaflet({
    leaflet(height="100%") %>%
      addTiles() %>%
      setView(lng = -70, lat = 40, zoom = 5) %>%
      addMarkers(stations$Longitude, stations$Latitude, popup = stations$Waterbody_Name, label =stations$Waterbody_Name )
  })
  
  # table <- reactive({
  #   taval=1.42E-05
  #   tbval=2.60727827
  #   saval=0.00039042
  #   sbval=2.579747757
  #   tdw=taval*(input$hsize*25.4)^tbval
  #   sdw=saval*(input$hsize*25.4)^sbval
  #   
  #   #Convert dry weight of tissue and shell (g) to nutrients (g)
  #   tNi=reactiveValues()
  #   sNi=reactiveValues()
  #   tNi=0.0770*tdw
  #   sNi=0.0019*sdw
  #   tPi=0.008345*tdw
  #   sPi=0.000438*sdw
  #   
  #   #convert grams N to lbs or kg
  #   cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
  #   # tN=reactiveValues()
  #   tN=round((tNi*cnvrt*input$Num),1)
  #   # sN=reactiveValues()
  #   sN=round((sNi*cnvrt*input$Num),1)
  #   # tP=reactiveValues()
  #   tP=round((tPi*cnvrt*input$Num),1)
  #   # sP=reactiveValues()
  #   sP=round((sPi*cnvrt*input$Num),1)
  #   # df=data.frame("Shell_N"=sN, "Tissue_N"=tN, "Total_N"=sN+tN, "Shell_P"=sP, "Tissue_P"=tP, "Total_P"=sP+tP, "Units"=input$units)
  #   # colnames(df)=c("Shell N", "Tissue N", "Total N", "Shell P", "Tissue P", "Total P", "Units")
  #   
  #   df=data.frame(matrix(c(sN, tN, tN+sN), nrow=1, ncol=3))
  #   colnames(df)=c("Shell", "Tissue", "Total")
  #   df=rbind(df, list(Shell=sP, Tissue=tP, Total=sP+tP))
  #   df$Units=input$units
  #   row.names(df)=c("Nitrogen", "Phosphorous")
  #   
  #   df
  # })
  table <- reactive({
    taval=1.42E-05
    tbval=2.60727827
    saval=0.00039042
    sbval=2.579747757
    if(input$seedonly==T){
      tdw=taval*(input$spatSizeOut)^tbval
      sdw=saval*(input$spatSizeOut)^sbval
      tNi=0.0770*tdw
      sNi=0.0019*sdw
      tPi=0.008345*tdw
      sPi=0.000438*sdw
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN=round((tNi*cnvrt*input$spatNum),1)
      sN=round((sNi*cnvrt*input$spatNum),1)
      tP=round((tPi*cnvrt*input$spatNum),1)
      sP=round((sPi*cnvrt*input$spatNum),1)
      df=data.frame(matrix(c(sN, tN, tN+sN), nrow=1, ncol=3))
      colnames(df)=c("Shell", "Tissue", "Total")
      df=rbind(df, list(Shell=sP, Tissue=tP, Total=sP+tP))
      df$Units=input$units
      row.names(df)=c("Nitrogen", "Phosphorous")
    }
    
    else{
      tdw1=taval*(input$sizeIn)^tbval
      sdw1=saval*(input$sizeIn)^sbval
      tdw2=taval*(input$sizeOut*25.4)^tbval
      sdw2=saval*(input$sizeOut*25.4)^sbval
      tNi1=0.0770*tdw1
      sNi1=0.0019*sdw1
      tPi1=0.008345*tdw1
      sPi1=0.000438*sdw1
      tNi2=0.0770*tdw2
      sNi2=0.0019*sdw2
      tPi2=0.008345*tdw2
      sPi2=0.000438*sdw2
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN1=round((tNi1*cnvrt*input$HNum),1)
      sN1=round((sNi1*cnvrt*input$HNum),1)
      tP1=round((tPi1*cnvrt*input$HNum),1)
      sP1=round((sPi1*cnvrt*input$HNum),1)
      tN2=round((tNi2*cnvrt*input$HNum),1)
      sN2=round((sNi2*cnvrt*input$HNum),1)
      tP2=round((tPi2*cnvrt*input$HNum),1)
      sP2=round((sPi2*cnvrt*input$HNum),1)
      sN=sN2-sN1
      tN=tN2-tN1
      sP=sP2-sP1
      tP=tP2-tP1
      df=data.frame(matrix(c(sN, tN, tN+sN), nrow=1, ncol=3))
      colnames(df)=c("Shell", "Tissue", "Total")
      df=rbind(df, list(Shell=sP, Tissue=tP, Total=sP+tP))
      df$Units=input$units
      row.names(df)=c("Nitrogen", "Phosphorous")
    }
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
    ReNum=prettyNum(round(input$Nload/(sN+tN),-4),big.mark=",")
    df3=data.frame("Total_N_load"=input$Nload, "Num"=ReNum)
    colnames(df3)=c("Total N Load (lbs)", "Number of Oysters to Harvest")
    df3
  })
  
  # Nplot <- reactive({
  #   taval=1.42E-05
  #   tbval=2.60727827
  #   saval=0.00039042
  #   sbval=2.579747757
  #   tdw=taval*((input$hsize*25.4)^tbval)
  #   sdw=saval*((input$hsize*25.4)^sbval)
  #   
  #   #Convert dry weight of tissue and shell (g) to nutrients (g)
  #   # tNi=reactiveValues()
  #   # sNi=reactiveValues()
  #   tNi=(0.0770*tdw)*input$Num
  #   sNi=(0.0019*sdw)*input$Num
  #   tPi=(0.008345*tdw)*input$Num
  #   sPi=(0.000438*sdw)*input$Num
  #   
  #   #convert grams N to lbs or kg
  #   cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
  #   # tN=reactiveValues()
  #   tN=round((tNi*cnvrt),1)
  #   # sN=reactiveValues()
  #   sN=round((sNi*cnvrt),1)
  #   tP=round((tPi*cnvrt),1)
  #   sP=round((sPi*cnvrt),1)
  #   
  #   ## Nitrogen only
  #   # df2=data.frame(matrix(c(tN, tP), nrow=1, ncol=2))
  #   # colnames(df2)=c("N", "P")
  #   # df2$var="Tissue"
  #   # df2=rbind(df2, list(N=sN, P=sP, var="Shell" ))
  #   # df2=rbind(df2, list(N=sN+tN, P=sP+tP, var="Total" ))
  #   # df2$units=input$units
  #   # P=ggplot(df2, aes(x=var, y=N))+
  #   #   geom_bar(stat="identity" , fill="steelblue", width = 0.65)+
  #   #   # coord_cartesian(ylim=c(0, NA), xlim=NULL, clip = "on")+
  #   #   # ylim(0,max(df2$N))+
  #   #   # scale_y_continuous(limits = c(0, NA))+
  #   #   # aes(ymin=0)+
  #   #   theme_minimal()+
  #   #   ylab(input$units)+
  #   #   xlab("Nitrogen Removed")+
  #   #   theme(axis.title.x = element_text(size = 16),
  #   #         axis.text.x = element_text(size = 14),
  #   #         axis.text.y = element_text(size = 14),
  #   #         axis.title.y = element_text(size = 16))
  #   # P
  #   
  #   ##update to add P data
  #   # df2=data.frame(matrix(c(tN, tP), nrow=1, ncol=2))
  #   # colnames(df2)=c("Nitrogen", "Phosphorous")
  #   # df2$var="Tissue"
  #   # df2=rbind(df2, list(Nitrogen=sN, Phosphorous=sP, var="Shell" ))
  #   # df2=rbind(df2, list(Nitrogen=sN+tN, Phosphorous=sP+tP, var="Total" ))
  #   # df2$units=input$units
  #   # df3=df2 %>% tidyr::pivot_longer(cols=c("Nitrogen", "Phosphorous"), names_to="Nutrients")
  #   ### both N and P
  #   # P=ggplot(df3, aes(x=var, y=value, fill=Nutrients))+
  #   #   geom_bar(stat="identity" , position='dodge', width = 0.9)+
  #   #   # coord_cartesian(ylim=c(0, NA), xlim=NULL, clip = "on")+
  #   #   # ylim(0,max(df2$N))+
  #   #   # scale_y_continuous(limits = c(0, NA))+
  #   #   # aes(ymin=0)+
  #   #   # facet_wrap(~ Nutrients) +
  #   #   theme_minimal()+
  #   #   ylab(input$units)+
  #   #   xlab("Nutrients Removed")+
  #   #   theme(axis.title.x = element_text(size = 16),
  #   #         axis.text.x = element_text(size = 14),
  #   #         axis.text.y = element_text(size = 14),
  #   #         axis.title.y = element_text(size = 16),
  #   #         legend.text=element_text(size=12),
  #   #         legend.title=element_text(size=12))
  #   # P
  #   ### N and P individually
  #   df2=data.frame(matrix(tN, nrow=1, ncol=1))
  #   colnames(df2)="Nitrogen"
  #   df2$var="Tissue"
  #   df2=rbind(df2, list(Nitrogen=sN, var="Shell" ))
  #   df2=rbind(df2, list(Nitrogen=sN+tN, var="Total" ))
  #   df2$units=input$units
  #   P=ggplot(df2, aes(x=var, y=Nitrogen))+
  #     geom_bar(stat="identity" , fill="steelblue", width = 0.65)+
  #     # coord_cartesian(ylim=c(0, NA), xlim=NULL, clip = "on")+
  #     # ylim(0,max(df2$N))+
  #     # scale_y_continuous(limits = c(0, NA))+
  #     # aes(ymin=0)+
  #     # facet_wrap(~ Nutrients) +
  #     theme_minimal()+
  #     ylab(input$units)+
  #     xlab("Nitrogen Removed")+
  #     theme(axis.title.x = element_text(size = 16),
  #           axis.text.x = element_text(size = 14),
  #           axis.text.y = element_text(size = 14),
  #           axis.title.y = element_text(size = 16))
  #   
  #   P
  # })
  # 
  # Pplot <- reactive({
  #   taval=1.42E-05
  #   tbval=2.60727827
  #   saval=0.00039042
  #   sbval=2.579747757
  #   tdw=taval*((input$hsize*25.4)^tbval)
  #   sdw=saval*((input$hsize*25.4)^sbval)
  #   
  #   tPi=(0.008345*tdw)*input$Num
  #   sPi=(0.000438*sdw)*input$Num
  #   
  #   cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
  #   tP=round((tPi*cnvrt),1)
  #   sP=round((sPi*cnvrt),1)
  #   
  #   df3=data.frame(matrix(tP, nrow=1, ncol=1))
  #   colnames(df3)="Phosphorous"
  #   df3$var="Tissue"
  #   df3=rbind(df3, list(Phosphorous=sP, var="Shell" ))
  #   df3=rbind(df3, list(Phosphorous=sP+tP, var="Total" ))
  #   df3$units=input$units
  #   
  #   P2=ggplot(df3, aes(x=var, y=Phosphorous))+
  #     geom_bar(stat="identity" , fill="firebrick", width = 0.65)+
  #     # coord_cartesian(ylim=c(0, NA), xlim=NULL, clip = "on")+
  #     # ylim(0,max(df2$N))+
  #     # scale_y_continuous(limits = c(0, NA))+
  #     # aes(ymin=0)+
  #     theme_minimal()+
  #     ylab(input$units)+
  #     xlab("Phosphorous Removed")+
  #     theme(axis.title.x = element_text(size = 16),
  #           axis.text.x = element_text(size = 14),
  #           axis.text.y = element_text(size = 14),
  #           axis.title.y = element_text(size = 16))
  #   P2
  # })
  Nplot <- reactive({
    taval=1.42E-05
    tbval=2.60727827
    saval=0.00039042
    sbval=2.579747757
    if(input$seedonly==T){
      tdw=taval*(input$spatSizeOut)^tbval
      sdw=saval*(input$spatSizeOut)^sbval
      tNi=0.0770*tdw
      sNi=0.0019*sdw
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN=round((tNi*cnvrt*input$spatNum),1)
      sN=round((sNi*cnvrt*input$spatNum),1)
    }
    else{
      tdw1=taval*(input$sizeIn)^tbval
      sdw1=saval*(input$sizeIn)^sbval
      tdw2=taval*(input$sizeOut*25.4)^tbval
      sdw2=saval*(input$sizeOut*25.4)^sbval
      tNi1=0.0770*tdw1
      sNi1=0.0019*sdw1
      tNi2=0.0770*tdw2
      sNi2=0.0019*sdw2
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN1=round((tNi1*cnvrt*input$HNum),1)
      sN1=round((sNi1*cnvrt*input$HNum),1)
      tN2=round((tNi2*cnvrt*input$HNum),1)
      sN2=round((sNi2*cnvrt*input$HNum),1)
      sN=sN2-sN1
      tN=tN2-tN1
    }
    df2=data.frame(matrix(tN, nrow=1, ncol=1))
    colnames(df2)="Nitrogen"
    df2$var="Tissue"
    df2=rbind(df2, list(Nitrogen=sN, var="Shell" ))
    df2=rbind(df2, list(Nitrogen=sN+tN, var="Total" ))
    df2$units=input$units
    P=ggplot(df2, aes(x=var, y=Nitrogen))+
      geom_bar(stat="identity" , fill="steelblue", width = 0.65)+
      theme_minimal()+
      ylab(input$units)+
      xlab("Nitrogen Removed")+
      theme(axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16))
    P
  })
  
  Pplot <- reactive({
    taval=1.42E-05
    tbval=2.60727827
    saval=0.00039042
    sbval=2.579747757
    if(input$seedonly==T){
      tdw=taval*(input$spatSizeOut)^tbval
      sdw=saval*(input$spatSizeOut)^sbval
      tPi=0.008345*tdw
      sPi=0.000438*sdw
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tP=round((tPi*cnvrt*input$spatNum),1)
      sP=round((sPi*cnvrt*input$spatNum),1)
    }
    else {
      tdw1=taval*(input$sizeIn)^tbval
      sdw1=saval*(input$sizeIn)^sbval
      tdw2=taval*(input$sizeOut*25.4)^tbval
      sdw2=saval*(input$sizeOut*25.4)^sbval
      tPi1=0.008345*tdw1
      sPi1=0.000438*sdw1
      tPi2=0.008345*tdw2
      sPi2=0.000438*sdw2
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tP1=round((tPi1*cnvrt*input$HNum),1)
      sP1=round((sPi1*cnvrt*input$HNum),1)
      tP2=round((tPi2*cnvrt*input$HNum),1)
      sP2=round((sPi2*cnvrt*input$HNum),1)
      sP=sP2-sP1
      tP=tP2-tP1
    }
    df3=data.frame(matrix(tP, nrow=1, ncol=1))
    colnames(df3)="Phosphorous"
    df3$var="Tissue"
    df3=rbind(df3, list(Phosphorous=sP, var="Shell" ))
    df3=rbind(df3, list(Phosphorous=sP+tP, var="Total" ))
    df3$units=input$units
    P2=ggplot(df3, aes(x=var, y=Phosphorous))+
      geom_bar(stat="identity" , fill="firebrick", width = 0.65)+
      theme_minimal()+
      ylab(input$units)+
      xlab("Phosphorous Removed")+
      theme(axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16))
    P2
  })
  # fertilplot <- reactive({
  fertilplot <- function(){
    # taval=1.42E-05
    # tbval=2.60727827
    # saval=0.00039042
    # sbval=2.579747757
    # tdw=taval*((input$hsize*25.4)^tbval)
    # sdw=saval*((input$hsize*25.4)^sbval)
    # tNi=(0.0796*tdw)*input$Num
    # sNi=(0.0019*sdw)*input$Num
    # cnvrt=0.00220462
    # tN=round((tNi*cnvrt),1)
    # sN=round((sNi*cnvrt),1)
    taval=1.42E-05
    tbval=2.60727827
    saval=0.00039042
    sbval=2.579747757
    
    if(input$seedonly==T){
      tdw=taval*(input$spatSizeOut)^tbval
      sdw=saval*(input$spatSizeOut)^sbval
      
      tNi=0.0770*tdw
      sNi=0.0019*sdw
      
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN=round((tNi*cnvrt*input$spatNum),1)
      sN=round((sNi*cnvrt*input$spatNum),1)
    }
    else{
      tdw1=taval*(input$sizeIn)^tbval
      sdw1=saval*(input$sizeIn)^sbval
      tdw2=taval*(input$sizeOut*25.4)^tbval
      sdw2=saval*(input$sizeOut*25.4)^sbval
      
      tNi1=0.0770*tdw1
      sNi1=0.0019*sdw1
      tNi2=0.0770*tdw2
      sNi2=0.0019*sdw2
      
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN1=round((tNi1*cnvrt*input$HNum),1)
      sN1=round((sNi1*cnvrt*input$HNum),1)
      tN2=round((tNi2*cnvrt*input$HNum),1)
      sN2=round((sNi2*cnvrt*input$HNum),1)
      
      sN=sN2-sN1
      tN=tN2-tN1
    }
    nBags=round(((sN+tN)/5),0)
    sqftlawns=round((sN+tN),0)*1000
    # pic="C:/Users/ryan.morse/Documents/GitHub/Calculator/WWW/fertilizerNew.png"
    # # filename <- rep(normalizePath('fertilizer.png'),nBags)
    # # filename <- rep(file.path(here::here('WWW/fertilizer.png')),nBags)
    # filename <- rep(pic,nBags)
    # pngs = lapply(filename, readPNG)
    # asGrobs = lapply(pngs, rasterGrob)
    # F <- grid.arrange(grobs=asGrobs)
    # F
    ## add single graphic to Calculator
    #read file
    img1<-readPNG("fertilizerInfographic.png")
    # img1<tags$img(src='fertilizer_infographic2.png')
    # img1<-readPNG("C:/Users/ryan.morse/Documents/GitHub/Calculator/WWW/fertilizer_infographic2.png")
    #get size
    h<-dim(img1)[1]
    w<-dim(img1)[2]
    par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
    plot.new()
    plot.window(0:1, 0:1)
    #fill plot with image
    usr<-par("usr")    
    F=rasterImage(img1, usr[1], usr[3], usr[2], usr[4])
    #add text
    text(.35,.90, "Nitrogen removal", cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(.35,.80, "equal to:", cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(.35,.70, nBags, cex=3, col='red', pos=4)
    text(.35,.60, "50-lb bags", cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(.35,.50, "of fertilizer*, or", cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0.35,0.40, sqftlawns, cex=3, col='red', pos=4)
    text(.35,.30, "square feet of",cex=2, col=rgb(.2,.2,.2,.7), pos=4) 
    text(.35,.20,"lawns fertilized**",cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(.35,.15, "* Equivalency based on fertilizer", cex=1.2, col=rgb(.2,.2,.2,.7), pos=4)
    text(.35,.10, "with 10% nitrogen content", cex=1.2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0.35,0.05,"** Using 1-lb of N per 1000 sq. ft.", cex=1.2, col=rgb(.2,.2,.2,.7), pos=4)
    # text(0.0,0.0,"https://connect.fisheries.noaa.gov/ANRC/", cex=1.2, col='blue', pos=4)
    F
    # dev.off()
    # })
  }
  
  ## QR code
  # ANRC=qrcode::qr_code('https://connect.fisheries.noaa.gov/ANRC/')
  # png("linkQR.png") 
  # plot(ANRC) 
  # dev.off()
  
  infoplot <- function(){
    taval=1.42E-05
    tbval=2.60727827
    saval=0.00039042
    sbval=2.579747757
    if(input$seedonly==T){
      tdw=taval*(input$spatSizeOut)^tbval
      sdw=saval*(input$spatSizeOut)^sbval
      
      tNi=0.0770*tdw
      sNi=0.0019*sdw
      
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN=round((tNi*cnvrt*input$spatNum),1)
      sN=round((sNi*cnvrt*input$spatNum),1)
    }
    else{
      tdw1=taval*(input$sizeIn)^tbval
      sdw1=saval*(input$sizeIn)^sbval
      tdw2=taval*(input$sizeOut*25.4)^tbval
      sdw2=saval*(input$sizeOut*25.4)^sbval
      
      tNi1=0.0770*tdw1
      sNi1=0.0019*sdw1
      tNi2=0.0770*tdw2
      sNi2=0.0019*sdw2
      
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN1=round((tNi1*cnvrt*input$HNum),1)
      sN1=round((sNi1*cnvrt*input$HNum),1)
      tN2=round((tNi2*cnvrt*input$HNum),1)
      sN2=round((sNi2*cnvrt*input$HNum),1)
      
      sN=sN2-sN1
      tN=tN2-tN1
    }
    # tdw=taval*((input$hsize*25.4)^tbval)
    # sdw=saval*((input$hsize*25.4)^sbval)
    # tNi=(0.0796*tdw)*input$Num
    # sNi=(0.0019*sdw)*input$Num
    # cnvrt=0.00220462
    # tN=round((tNi*cnvrt),1)
    # sN=round((sNi*cnvrt),1)
    nBags=round(((sN+tN)/5),0)
    sqftlawns=round((sN+tN),0)*1000
    # img2<-readPNG("FarmInfoG2.png")
    # img2<-readPNG("Oyster-Farms-Nutrients-no-text-small.png") # magnified and phyto dots
    # img2<-readPNG("Oyster-Farms-Nutrients-phyto-icons-small.png") # phyto dots only
    img2<-readPNG("Oyster-Farms-Nutrients-no-phyto-small.png") # no phyto at all
    #get size
    h<-dim(img2)[1]
    w<-dim(img2)[2]
    # par(mar=c(1,0,1,0), xpd=T, mgp=c(0,0,0), oma=c(1,0,1,0), ann=F)
    # par(mar=c(1,0,1,0), xpd=T, mgp=c(0,0,0), oma=c(1,0,1,0), ann=F, xaxs="i", yaxs="i")
    par(mar=c(2,0,2,0), xpd=T, mgp=c(0,0,0), oma=c(2,0,2,0), ann=F, xaxs="i", yaxs="i", bg=NA)
    plot.new()
    plot.window(0:1, 0:1)
    #fill plot with image
    usr<-par("usr")    
    Z=rasterImage(img2, usr[1], usr[3], usr[2], usr[4])
    #add text
    # text(-0.05,0.2, "Nitrogen removal equals:", cex=0.9, col='yellow', pos=4)
    # text(0.15,.15, substitute(paste(bold(nBags))), cex=1.5, col='white')
    # text(-0.05,.10, "50-lb bags of fertilizer*", cex=0.9, col='yellow', pos=4)
    # text(.68,.2, "which is equal to:", cex=0.9, col='yellow', pos=4)
    # text(0.85,0.15, substitute(paste(bold(sqftlawns))), cex=1.5, col='white')
    # text(.68,.10, "sq. ft. of land fertilized**",cex=0.9, col='yellow', pos=4) 
    # text(-0.05,.050, "* Based on fertilizer", cex=0.65, col='yellow', pos=4)
    # text(-0.05,.025, "with 10% nitrogen content", cex=0.65, col='yellow', pos=4)
    # text(0.68,0.05,"** Using 1-lb of nitrogen", cex=0.65, col='yellow', pos=4)
    # text(0.68,0.025,"per 1000 sq. ft.", cex=0.65, col='yellow', pos=4)
    # text(0.5,1.0,"https://connect.fisheries.noaa.gov/ANRC/", cex=0.75, col='blue', pos=4)
    QR1=png::readPNG("linkQR.png")
    # rasterImage(QR1,0.8,0.7,1,0.9)
    text(0,0.45, "Nitrogen removed =", cex=1.1, col='yellow', pos=4)
    text(0.2,.37, prettyNum(nBags, big.mark = ",", scientific = FALSE), cex=1.5, col='white')
    text(0,.30, "50-lb bags of fertilizer*", cex=1.1, col='yellow', pos=4)
    text(.65,.45, "Which is equal to:", cex=1.1, col='yellow', pos=4)
    text(0.8,0.37, prettyNum(sqftlawns, big.mark = ",", scientific = FALSE), cex=1.5, col='white')
    text(.6,.30, "sq. ft. of land fertilized**",cex=1.1, col='yellow', pos=4) 
    text(0,.050, "* Based on fertilizer with 10% nitrogen content", cex=0.65, col='yellow', pos=4)
    text(0,.025, "** Using 1-lb of nitrogen per 1000 sq. ft.", cex=0.65, col='yellow', pos=4)
    text(0.5,0.95,"https://connect.fisheries.noaa.gov/ANRC/", cex=0.75, col='blue', pos=4)
    mtext(input$farmname, side=3, line=-1, outer=T, cex=ifelse(nchar(input$farmname)<40,1.3,1), font=2, col='black') #FF9900 #3399FF #003366 #003399
    rasterImage(QR1,0.85,0.7,1,0.9)
    Z
    # dev.off()
    # })
  }
  # Output Components
  output$nutbplot <- 
    renderPlot({
      Nplot()
    })
  output$nutbplot2 <- 
    renderPlot({
      Pplot()
    })
  output$mytable <-
    renderTable(
      table(),
      rownames = TRUE
    )
  output$mytable2 <-
    renderTable({
      esttable()
    })
  output$fertplot <- 
    renderPlot({
      fertilplot()
    })
  
  
  ## save infographic to file
  output$download <- downloadHandler(
    filename = paste0("Infographic_",Sys.Date(),".png"),
    content = function(file) {
      png(file, width = 1000,
          height = 1000,
          res = 200)
      # fertilplot()
      infoplot()
      dev.off()
    }) 
  
  
  # downaload handler - save the image
  # output$save_myPlot_hidden <- downloadHandler(
  #   filename = function() { 
  #     paste0("Infographic_", Sys.Date(), ".png") },
  #   content = function(file) {
  #     # get image code from URI
  #     plot_src <- gsub("^data.*base64,", "", input$plot_src)
  #     # decode the image code into the image
  #     plot_image <- image_read(base64_decode(plot_src))
  #     # save the image
  #     image_write(plot_image, file)
  #   })
  # 
  # # plot
  # output$fertplot <-
  #   renderPlot({
  #     fertilplot()
  #   })
  
  # }
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
  
  ### Screenshot function ###
  # observeEvent(input$go, {
  #   screenshot()
  # })
  
  output$downloader <- 
    downloadHandler(
      paste0(Sys.Date(),"_Oyster_Farm_Nitrogen_Report.pdf"),
      content = 
        function(file)
        {
          rmarkdown::render(
            input = "report.Rmd",
            output_file = "built_report.pdf",
            params = list(
              SeedOnly=input$seedonly,
              NurseryLocDiff=input$nurseryloc,
              table = table(),
              Nplot = Nplot(),
              Pplot = Plot(),
              Location=input$projloc, 
              Units=input$units, 
              gear=input$gear, 
              ploidy=input$ploidy, 
              spatsize=input$spatSizeOut,
              size=input$sizeOut,
              Farm=input$farmname,
              spatNumber=input$spatNum,
              Number=input$HNum,
              Dates=input$Htime,
              spatLat=input$mymap_draw_new_feature$geometry$coordinates[[2]],
              spatLon=input$mymap_draw_new_feature$geometry$coordinates[[1]],
              nurseLat=input$spatmap_draw_new_feature$geometry$coordinates[[2]],
              nurseLon=input$spatmap_draw_new_feature$geometry$coordinates[[1]],
              HLat=input$spatonlymap_draw_new_feature$geometry$coordinates[[2]],
              HLon=input$spatonlymap_draw_new_feature$geometry$coordinates[[1]]
              )
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
