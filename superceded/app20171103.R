# 20171004
# Updated 2017xxxx
# R Shiny script for web based user interface with PGSQL database.
# Written by Louis Tsiattalou for TradeDataVis project.
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# TODO
# Add selectors for dates
# Add selectors for port/country of origin?
# Add WHERE into portsumquery and countrysumquery for port/country of origin
# LEFT JOINs into portsumquery/countrysumquery for port/country abbreviations?
# Test SQL speed vs R speed

# SCRIPT START ###############################################################

# Load Packages

if(require("shiny") == FALSE) {install.packages("shiny")}
library("shiny")

if(require("dplyr") == FALSE) {install.packages("dplyr")}
library("dplyr")

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")

if(require("networkD3") == FALSE) {install.packages("networkD3")}
library("networkD3")

library("maptools")
library("maps")
library("ggmap")

# Load Prerequisite Static data - Ports, Comcodes, etc. ======================

setwd("~/R/ImportTool/")
pg = dbDriver("PostgreSQL")
tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

# Load Metadata
portcode <- dbGetQuery(tradedata, "SELECT * FROM port")
comcode <- dbGetQuery(tradedata, "SELECT * FROM comcode")
countrycode <- dbGetQuery(tradedata, "SELECT * FROM country")
desclookup <- c(portcode$portname,countrycode$countryname)
names(desclookup) <- c(portcode$portcode,countrycode$countrycode)
desclookup <- data.frame(keyName=names(desclookup), value=desclookup, row.names=NULL, stringsAsFactors = FALSE)
desclookup <- desclookup[desclookup$value != "",]

itemCount <- 5

# Note - no data loaded at this stage - we query what we need when we need it

# Create list of 2, 4, 6, 8 digit commodity codes
comcode_2 <- comcode[nchar(comcode$commoditycode) == 2,]
comcode_4 <- comcode[nchar(comcode$commoditycode) == 4,]
comcode_6 <- comcode[nchar(comcode$commoditycode) == 6,]
comcode_8 <- comcode[nchar(comcode$commoditycode) == 8,]

# Create month list
syrs <- as.character(sprintf("%02d",c(09:30)))
smths <- as.character(sprintf("%02d",c(1:12)))
dates <- character(0)
for (i in syrs){
  for (j in smths){
    dates <- c(dates, paste(j,"/20",i,sep=""))
  }
}

# UI =========================================================================

# Use a fluid Bootstrap layout
ui <- fluidPage(    
# Head Styles  
tags$head(tags$style(HTML("
    .progress-striped .bar {
                            background-color: #149bdf;
                            background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
                            background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                            background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                            background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                            background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                            -webkit-background-size: 40px 40px;
                            -moz-background-size: 40px 40px;
                            -o-background-size: 40px 40px;
                            background-size: 40px 40px;
                            }
                            "))),
  # Give the page a title
  titlePanel("UK Non-EU Exports flow diagram"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with four cascading inputs - don't allow "All" on 2-digit comcode
    sidebarPanel(
      selectizeInput("dateselect", "Period:",
                     choices=dates),
      selectizeInput("comcode2", "2-digit Commodity Code:", 
                  choices=c(comcode_2$commoditycode),
                  options = list(maxItems = itemCount)),
      selectizeInput("comcode4", "4-digit Commodity Code:",
                  choices=c("All", comcode_4$commoditycode),
                  options = list(maxItems = itemCount)),
      selectizeInput("comcode6", "6-digit Commodity Code:",
                  choices=c("All", comcode_6$commoditycode),
                  options = list(maxItems = itemCount)),
      selectizeInput("comcode8", "8-digit Commodity Code:",
                  choices=c("All", comcode_8$commoditycode),
                  options = list(maxItems = itemCount)),
      actionButton("queryButton", "Run Query"),
      hr(),
      helpText("Data obtained from HMRC's Trade Data - ", tags$a(href="www.uktradeinfo.com", "Source")),
      width = 3
    ),
    
    # Create a spot for the sankey diagram
    mainPanel(
      tabsetPanel(
        tabPanel("FLOW", sankeyNetworkOutput(outputId = "sankeyTrade")), 
        tabPanel("MAP", plotOutput(outputId = "worldMap"))
      )
    )
  )
)


# SERVER =====================================================================

server <- function(input, output, session) {
  
  sankeyData <- reactiveValues(links = NULL, nodes = NULL)
  mapData <- reactiveValues(mapWorld = NULL)
  
  # OBSERVE STATEMENTS FOR MODIFYING DROPDOWNS -------------------------------
  
  observe({
    comcode_2_selection <- input$comcode2
    browser()
    # Update Comcodes
    updateSelectizeInput(session,"comcode4", "4-digit Commodity Code:",
                      choices=c("All", comcode_4[comcode_4$parent %in% comcode_2_selection,"commoditycode"]),
                      options = list(maxItems = itemCount))
    browser()
    updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                      choices=c("All", comcode_6[comcode_6$parent %in% comcode_2_selection,"commoditycode"]),
                      options = list(maxItems = itemCount))
    browser()
    updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                      choices=c("All", comcode_8[comcode_8$parent %in% comcode_2_selection,"commoditycode"]),
                      options = list(maxItems = itemCount))
    browser()
  })
  
  # observe({
  #   comcode_4_selection <- input$comcode4
  #   print(paste("4:",comcode_4_selection))
  # 
  #   # Update Comcodes
  #   if (comcode_4_selection == "All"){
  #     updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
  #                       choices=c("All", comcode_6$commoditycode ))
  #     updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
  #                       choices=c("All", comcode_8$commoditycode))
  #   } else {
  #     updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
  #                       choices=c("All", comcode_6[comcode_6$parent %in% comcode_4_selection,"commoditycode"]))
  #     updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
  #                       choices=c("All", comcode_8[comcode_8$parent %in% comcode_4_selection,"commoditycode"]))
  #   }
  # })
  # 
  # observe({
  #   comcode_6_selection <- input$comcode6
  #   print(paste("6:",comcode_6_selection))
  # 
  #   # Update Comcodes
  #   if (comcode_6_selection == "All"){
  #     updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
  #                       choices=c("All", comcode_8$commoditycode))
  #   } else {
  #     updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
  #                       choices=c("All", comcode_8[comcode_8$parent %in% comcode_6_selection,"commoditycode"]))
  #   }
  # })
  # 
  observeEvent(input$queryButton,{
    input$queryButton
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Generating Visualisations", value = 1)
    
    
    # Use comcodes on selectors to build plot
    isolate({
      
      # Control handling for comcode selectors
      comcode2query = input$comcode2
      if (input$comcode4 == "All") {comcode4query = "__"} else {comcode4query = input$comcode4}
      if (input$comcode6 == "All") {comcode6query = "__"} else {comcode6query = input$comcode6}
      if (input$comcode8 == "All") {comcode8query = "__"} else {comcode8query = input$comcode8}
      
      # This is kind of a funny way of doing things, but simply pasting the strings
      # together and taking the last 8 characters works quickly, easily and cleanly.
      comcodequery = paste(comcode2query, comcode4query, comcode6query, comcode8query, sep = "")
      comcodequery = substr(comcodequery, nchar(comcodequery)-7, nchar(comcodequery))
      
      portsumquery = paste("SELECT country_alpha_coo_imp,comcode,sum(value) FROM imports ",
                           "WHERE (comcode SIMILAR TO '",
                           comcodequery,
                           "') AND (account_date = '",
                           input$dateselect,
                           "') GROUP BY comcode,country_alpha_coo_imp",
                           sep = "")
      
      countrysumquery = paste("SELECT comcode,port_alpha,sum(value) FROM imports ",
                              "WHERE (comcode SIMILAR TO '",
                              comcodequery,
                              "') AND (account_date = '",
                              input$dateselect,
                              "') GROUP BY comcode,port_alpha",
                              sep = "")
      
      # Create Links + Nodes DF
      progress$set(detail = "Querying Country -> Comcode data")
      portsum = dbGetQuery(tradedata, portsumquery)
      
      progress$set(detail = "Querying Comcode -> Port data")
      countrysum = dbGetQuery(tradedata, countrysumquery)
      
      progress$set(detail = "Clean + Shape Data")
      colnames(portsum) = c("source","target","value")
      colnames(countrysum) = c("source","target","value")
      links = rbind(portsum,countrysum)
      nodes = data.frame(unique(c(links$source,links$target)),stringsAsFactors = FALSE)
      colnames(nodes) = "name"
      
      # SANKEY SPECIFIC -------------------------------------------------------
      
      # Replace links source, target columns with IDs specified in nodes.
      # Match to row number in nodes (which is uniquely indexed!)
      # Note - must be zero indexed, hence match - 1
      links$source = vapply(links$source, function(x){
        x = match(x,nodes[,1])-1
      }, double(1))
      
      links$target = vapply(links$target, function(x){
        x = match(x,nodes[,1])-1
      }, double(1))
    
      # Replace node codes for country and port with full name
      nodes$name = vapply(nodes$name,function(x){
        replacement = desclookup[match(x,desclookup$keyName),"value"]
        if (is.na(replacement) == FALSE){
          x = replacement
          if(nchar(x) > 30){x = substr(x,1,30)}}
        else {x}
        return(x)
      }, character(1))
      
      # WORLDMAP SPECIFIC -----------------------------------------------------
      
      mapWorld <- map_data("world")
      
      # Get map_data World names from iso codes using iso.expand
      portsum_countries <- iso.expand(unique(portsum$source))
      
      # Special Case - add serbia if XS is used - iso.expand only considers RS as Serbia
      if ("XS" %in% unique(portsum$source)) {portsum_countries = c(portsum_countries, "Serbia")}
      portsum_countries <- tibble(portsum_countries, iso.alpha(portsum_countries))
      colnames(portsum_countries) <- c("name","code")
      portsum_countries[portsum_countries$name == "Serbia","code"] = "XS"
      
      # Aggregate by country
      portsum_countrytotal <- portsum[,c("source","value")] %>% group_by(source) %>% summarise(value = sum(value))
      # Match plot-compatible names to iso codes
      portsum_countrytotal <- left_join(portsum_countrytotal,portsum_countries, by=c("source" = "code"))
      # Join values to mapWorld for plotting
      portsum_countrytotal <- tibble(portsum_countrytotal$name,portsum_countrytotal$value)
      colnames(portsum_countrytotal) <- c("region","value")
      mapWorld <- left_join(mapWorld,portsum_countrytotal)
      
      
    # End Isolate
    })

  # Now modify reactive variables with output from isolate() to trigger plot renders.
    sankeyData$links <- links
    sankeyData$nodes <- nodes
    mapData$mapWorld <- mapWorld
  
  })
  
  
  # Fill in the sankey diagram ================================================   
  output$sankeyTrade <- renderSankeyNetwork({
  
  # Suppress output if nothing has been selected yet
    if (input$queryButton == 0) return()

    sankeyNetwork(sankeyData$links, sankeyData$nodes,
                  "source", "target", "value", "name",
                  fontSize = 12, nodeWidth = 30)
    })
  
  output$worldMap <- renderPlot({
    if (input$queryButton == 0) return()
    
    worldmapPlot <- ggplot()
    worldmapPlot <- ggplot(data = mapData$mapWorld, aes(x=long,y=lat,group=group))
    worldmapPlot <- worldmapPlot + geom_polygon(colour="grey60", fill="grey80")
    worldmapPlot <- worldmapPlot + geom_polygon(aes(fill=value), colour = "grey60")
    worldmapPlot <- worldmapPlot + scale_fill_gradient(trans = "log10")
    worldmapPlot <- worldmapPlot + coord_fixed(1.3)
    worldmapPlot
  })
  
  }



# SHINY CALL =================================================================

shinyApp(ui = ui, server = server)

# END SCRIPT #################################################################

# JUNKYARD ###################################################################

# Disconnect All Database Cons
# lapply(dbListConnections(pg), dbDisconnect)