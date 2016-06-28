library(shiny)
library(leaflet)
library(sp)
library(maptools)

sub <- readShapePoly("subctr60")
dfsub <- data.frame(sub)
dfsub[dfsub==0] = NA
zips <- read.csv("ZIP_centroids.csv")

sub97 <- readShapePoly("centers97")
sub14 <- readShapePoly("centers14")


shinyServer(function(input, output) {

  # Grab ZIP code input
  center <- reactiveValues(xcoord=-118.239, ycoord=34.06583)
  observeEvent(input$recenter, {
    center$xcoord = zips$x_centr[zips$CODE==input$zip]
    center$ycoord = zips$y_centr[zips$CODE==input$zip]
  })
  
  # Grab Inputs 
  options = reactiveValues(choose="Em_all_14")
  observeEvent(input$csgo, {
    type_link = switch(input$cstype, "Highest Category"="_max_", "Total"="_all_", "KIBS"="_kibs_",
                       "Creative Class"="_crtv_", "Retail"="_ret_", "High Tech"="_tech_", "Industrial"="_ind_")
    
    options$choose = paste(substr(input$cstopic,1,2), type_link, substr(input$year,3,4), sep="")
  })

  # Grab whether qual or quant
  options2 = reactiveValues(choose="quant")
  observeEvent(input$csgo, {
    link = switch(input$cstype, "Highest Category"="qual", "Total"="quant", "KIBS"="quant",
                       "Creative Class"="quant", "Retail"="quant", "High Tech"="quant", "Industrial"="quant")
    options2$choose = link
  })
  
  finalMap <- reactive ({
    choice = dfsub[,grep(options$choose, colnames(dfsub))]
    if(options2$choose=="qual"){pal <- colorFactor("RdYlBu", choice, na.color="#FFFFFF")} 
      else {pal <- colorNumeric("Blues", choice, na.color="#B0171F")}
    
    # Create map 
    m = leaflet(sub) %>%  setView(lng=center$xcoord, lat=center$ycoord , zoom=10) %>% addTiles() %>%
    addPolygons(data=sub, stroke=T, weight=1.5, fillColor = ~pal(choice), color="black", fillOpacity=0.6, 
                opacity=1, popup=~subctrNAME, group="View Data") %>%
    addLegend("bottomleft", pal=pal, values=~choice, opacity=0.75, na.label=~paste('Not a subcenter in', input$year),
              title=~paste(input$year, input$cstype, input$cstopic, sep=" ")) %>%
    addPolygons(data=sub97, stroke=T, weight=1.5, color="black", fillColor="dodgerblue", fillOpacity=0.5, group="View Changes") %>%
    addPolygons(data=sub14, stroke=T, weight=1.5, color="black", fillColor="yellow", fillOpacity=0.5, group="View Changes") %>%
    addLayersControl(
      baseGroups = c("View Data", "View Changes"),
      options = layersControlOptions(collapsed = FALSE))
  })
  
  # Generate Map Output
  output$myMap = renderLeaflet(finalMap())
  
  # Generate Histogram
  observeEvent(input$csgo, {
  output$hist <- renderPlot({
    if(options2$choose=="qual"){return(NULL)}   else{ 
    data = dfsub[,grep(options$choose, colnames(dfsub))]
    if(input$year == '1997'){ctrselect <- input$ctr97} else {ctrselect <- input$ctr14}
    hist(data, xlab=NULL, breaks=12, col="dodgerblue",
         ylab="# of Subcenters", border="white", main=paste(input$year, input$cstype, input$cstopic, sep=" "))
    legend("topright", c(ctrselect), lwd=2, box.col="white")
    if(input$cstopic=="Specialization"){
      abline(v=1, lty=2)
      legend("topright", c(ctrselect, "1.0"), lwd=c(2,1), lty=c(1,2), box.col="white")}
    if(input$cstopic=="Employment"){
      abline(v=mean(data, na.rm=T), lty=2)
      legend("topright", c(ctrselect, "Subctr Avg"), lwd=c(2,1), lty=c(1,2), box.col="white")}
    abline(v=dfsub[,grep(options$choose, colnames(dfsub))][dfsub$subctrNAME==ctrselect], lwd=2) }
  })
  })

  # Add Topic Descriptions
  output$var_desc <- renderText({
    data_notes = switch(input$cstopic,
                       "Employment" = "is the total number of employees in the subcenter.",
                       "Specialization"= "displays the selected category's location quotient in each subcenter. A value above 1 indicates a high concentration of that industry relative to the whole region.")
    paste("-- ", input$cstopic, data_notes, sep=" ")
  })
  
  # Add Variable Description
  output$var_desc2 <- renderText({
    data_notes = switch(input$cstype,
                        "High Tech" = "spans several industries and includes tech manufacturing.",
                        "KIBS"= "stands for Knowledge-Intensive Business Services.",
                        "Creative Class" = "employment consists of arts, entertainment, recreation, and information.",
                        "Retail" = "selected.",
                        "Industrial" = "includes manufacturing and utilities.",
                        "Total" = "cannot be selected for SPECIALIZATION",
                        "Highest Category"= "is the industry (of the 5 options shown) with the highest employment or location quotient in each subcenter.")
    paste("-- ", input$cstype, data_notes, sep=" ")
  })
  
})

