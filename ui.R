library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub_list14 = c('', 'Adelanto',  'Anaheim',  'Banning',  'Barstow', 'Beverly Hills/West Hollywood',  'Big Bear Lake',  'Blythe', 'Burbank/LA 2',  'Camarillo',  'Chino',  'City of Industry',  'Corona',  'Covina',  'Dana Point',  'Downey',  'DTLA',  'El Segundo',  'Fillmore',  'Glendale', 'Hemet',  'Irvine/Lake Forest',  'Irvine/SNA',  'Lake Arrowhead',  'Lake Elsinore',  'Lake Elsinore SE',  'Lancaster',  'Long Beach',  'Malibu',  'Menifee',  'Montclair',  'Moreno Valley',  'Ojai',  'Palm Desert',  'Palm Springs',  'Palmdale',  'Pasadena',  'Pinon Hills',  'Rancho Cucamonga',  'Riverside',  'Riverside SW',  'San Bernardino',  'San Fernando Valley 2',  'Santa Clarita',  'Santa Fe Springs',  'Santa Paula',  'Simi Valley',  'Temecula',  'Thousand Oaks',  'Torrance',  'Ventura',  'Victorville',  'Yucaipa',  'Yucca Valley')
sub_list97 = c('', 'Adelanto',  'Anaheim',  'Banning',  'Barstow', 'Beverly Hills/West Hollywood',  'Big Bear Lake',  'Blythe',  'Burbank/LA 1', 'City of Industry',  'Corona',  'Covina',  'Dana Point', 'DTLA', 'Fillmore',  'Glendale',  'Hawthorne',  'Hemet',  'Irvine/Lake Forest',  'Irvine/SNA',  'Lake Arrowhead',  'Lake Elsinore',  'Lancaster',  'Long Beach',  'Malibu',  'Montclair',  'Needles',  'Ojai',  'Oxnard',  'Palm Desert',  'Palm Springs',  'Palmdale',  'Pasadena',  'Perris', 'Riverside',  'San Bernardino',  'San Fernando Valley 1', 'Santa Clarita',  'Santa Paula',  'Simi Valley',  'South El Monte',  'Temecula',  'Thousand Oaks',  'Torrance',  'Ventura',  'Victorville', 'Yucca Valley')
shinyUI(tabPanel("Southern California Employment Centers", div(class="outer",
                                                              
                  tags$head(
                  # custom, taken from Shiny's "superZIP"
                            includeCSS("styles.css")
                            #includeScript("gomap.js")
                            ),
                                                                
                  leafletOutput("myMap", width="100%", height="100%"),
                                                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                                                                
                                              h2("Southern California Employment Centers"),
                                              textInput("zip", label="Zoom to 5-digit ZIP: ", value=90012),
                                              actionButton("recenter", label="Re-center"),
                                                                                
                                              selectInput("year", label="Select Timeframe", choices=list("", "1997", "2014"), selected=""),
                                                                                
                                              conditionalPanel("input.year == '1997' | input.year == '2014'", 
                                                               selectInput("cstopic", label = "Select Topic:", selected="",
                                                                           choices = list("", "Employment", "Specialization"))),
                                                                                                 
                                              conditionalPanel("input.cstopic == 'Employment' | input.cstopic == 'Specialization' ",
                                                              selectInput("cstype", label= "Select Category", selected="",
                                                                          choices = list("", "High Tech", "KIBS", "Creative Class", "Retail", "Industrial", "Total", "Highest Category"))),
                                              
                                              conditionalPanel("input.cstype != 'Highest Category' & input.cstype != '' & input.year == '1997'",
                                                               selectInput("ctr97", label=em("Select Subcenter for Detail"), selected="Irvine/SNA",
                                                                           choices = sub_list97)),
                                              
                                              conditionalPanel("input.cstype != 'Highest Category' & input.cstype != '' & input.year == '2014'",
                                                               selectInput("ctr14", label=em("Show Subcenter in Histogram"), selected="Irvine/SNA",
                                                                           choices = sub_list14)),
                                              
                                              conditionalPanel("input.cstype != '' ",
                                                               actionButton("csgo", label="Go/Refresh")),            
                                              # Generate Histogram
                                              plotOutput("hist", height = 225),
                                              h6(em("by the ", a("Metropolitan Futures Initiative", href="http://mfi.soceco.uci.edu", target="_blank"), "at UC-Irvine (2016). Webmap by ", a("Kevin Kane, PhD", href="http://www.kevinkane.org", target="_blank"), "and", a("UCI Data Science Initiative", href="http://datascience.uci.edu", target="_blank")))
                                              ),
                              absolutePanel(id = "controls", class="panel panel-default", fixed = TRUE,
                                            draggable=TRUE, top=110, left=10, right="auto", bottom="auto",
                                            width=150, height="auto",
                                            p("Data Notes:"),
                                            h6("-- 'View Changes' displays changing subcenter boundaries using blue for 1997 and yellow for 2014"),
                                            h6("-- 'View Data' displays various business statistics about each subcenter.  Select this for further analysis."),
                                            h6("-- Click Go/Refresh after making new selections to ensure correct map and legend are displayed, and to clear any error messages."),
                                            h6(textOutput("var_desc")),
                                            h6(textOutput("var_desc2")),
                                            h6("-- See the ", a("full report here.", href="http://mfi.soceco.uci.edu/category/quarterly-report/detecting-job-density-over-time/", target="_blank"))
                                            )
)))

