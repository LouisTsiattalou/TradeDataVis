# 20171004
# Updated 2017xxxx
# R Shiny script for web based user interface with PGSQL database.
# Written by Louis Tsiattalou for TradeDataVis project.
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# SCRIPT START ###############################################################

# Load Packages --------------------------------------------------------------

if(require("readr") == FALSE) {install.packages("readr")}
library("readr")

if(require("shiny") == FALSE) {install.packages("shiny")}
library("shiny")

if(require("shinyjs") == FALSE) {install.packages("shinyjs")}
library("shinyjs")

if(require("dplyr") == FALSE) {install.packages("dplyr")}
library("dplyr")

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")

if(require("networkD3") == FALSE) {install.packages("networkD3")}
library("networkD3")

if(require("maptools") == FALSE) {install.packages("maptools")}
library("maptools")

if(require("maps") == FALSE) {install.packages("maps")}
library("maps")

if(require("DT") == FALSE) {install.packages("DT")}
library("DT")

if(require("ggmap") == FALSE) {install.packages("ggmap")}
library("ggmap")

if(require("leaflet") == FALSE) {install.packages("leaflet")}
library("leaflet")


# Load Prerequisite Static data - Ports, Comcodes, etc. ======================

setwd("~/R/ImportTool/")
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2],
                      host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])

# Load Metadata
portcode <- dbGetQuery(tradedata, "SELECT * FROM port")
comcode <- dbGetQuery(tradedata, "SELECT * FROM comcode")
countrycode <- dbGetQuery(tradedata, "SELECT * FROM country")

### Factor enables multiple search terms in comcode lookup tab
comcodelookup <- tibble(commoditycode = as.factor(comcode$commoditycode), description = comcode$description)

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
syrs <- as.character(sprintf("%02d",c(09:50)))
smths <- as.character(sprintf("%02d",c(1:12)))
dates <- character(0)
for (i in syrs){
  for (j in smths){
    dates <- c(dates, paste(j,"/20",i,sep=""))
  }
}

# UI ==========================================================================

ui <- navbarPage(
  
  # Navbar Title
  title = "UK Trade Data Visualisation",
  
  # COMMODITY CODE LOOKUP -----------------------------------------------------
  
  tabPanel("Commodity Code Lookup",
           tags$i("Perform a fuzzy search on Commodity Codes using the search box at the top!"),
           hr(),
           dataTableOutput("ComcodeLookup")
           ),
  
  # NON-EU IMPORTS ------------------------------------------------------------
  
  tabPanel("Non-EU Imports",
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
    
    # Generate a row with a sidebar
    fluidRow(      
    
    # Define date selectors and four cascading inputs - don't allow "All" on 2-digit comcode
      column(3,
        selectizeInput("datestart", "Period Start:",
                     choices=dates),
        selectizeInput("dateend", "Period End:",
                       choices=dates)
        ),
      column(3,
        selectizeInput("comcode2", "2-digit Commodity Code:",
                    selected = "01",
                    choices=c(comcode_2$commoditycode),
                    options = list(maxItems = 5)),
        selectizeInput("comcode4", "4-digit Commodity Code:",
                    selected = "All",
                    choices=c("All", comcode_4$commoditycode),
                    options = list(maxItems = 5))
      ),
      column(3,
        selectizeInput("comcode6", "6-digit Commodity Code:",
                    selected = "All",
                    choices=c("All", comcode_6$commoditycode),
                    options = list(maxItems = 5)),
        selectizeInput("comcode8", "8-digit Commodity Code:",
                    selected = "All",
                    choices=c("All", comcode_8$commoditycode),
                    options = list(maxItems = 5))
      ),
      column(3,
        radioButtons("impexpSelect", label = NULL,
                     choices = c("Imports","Exports")),
        actionButton("queryButton", "Run Query")
      ),
      hr()
    ),
    
    # Create comcode legend
    fluidRow(
      column(12,
        dataTableOutput("ComcodeLegend")
      ),
      hr()
    ),
    
    # Create slider/unit bar
    fluidRow(
      column(6,
        selectizeInput("dateSlider", label = "Select Month",
                    choices = c("All", dates))
             ),
      column(6,
        radioButtons("unitSelect", label = "Choose Units", inline = TRUE,
                     choices = c("Value (GBP)", "Weight (KG)", "Price Per Kilo GBP/KG")))
    ),
    
    # Create a spot for the plots
    fluidRow(
      column(12,
        tabsetPanel(
          tabPanel("FLOW", sankeyNetworkOutput(outputId = "sankeyTrade")), 
          tabPanel("MAP", leafletOutput(outputId = "worldMap"))
        )
      )
    )
  ),
  # Enable ShinyJS support for cleaner on-click and disable features.
  shinyjs::useShinyjs()
)


# SERVER ======================================================================

server <- function(input, output, session) {
  
  queryData <- reactiveValues(portsumraw = NULL, countrysumraw = NULL)
  comcodeLegendData <- reactiveValues(comcodelegend = NULL)
  sankeyData <- reactiveValues(links = NULL, nodes = NULL)
  mapData <- reactiveValues(mapWorld = NULL)
  
  # SERVER SIDE COMMODITY CODE LOOKUP -----------------------------------------
  output$ComcodeLookup = renderDataTable(comcodelookup,
                                   filter = "top",
                                   rownames = FALSE,
                                   colnames = c("Commodity Code", "Description"),
                                   options = list(
                                   #  dom = "t", # disable search bar at top
                                     pageLength = 25, # set number of elements on page
                                     columnDefs = list(list(width = "150px", targets = 0))
                                     )
                                  )
  
  # SHINYJS ONCLICK STATEMENTS -----------------------------------------------
  
  shinyjs::onclick("comcode2", {updateSelectizeInput(session, "comcode2", selected = "")})
  shinyjs::onclick("comcode4", {updateSelectizeInput(session, "comcode4", selected = "")})
  shinyjs::onclick("comcode6", {updateSelectizeInput(session, "comcode6", selected = "")})
  shinyjs::onclick("comcode8", {updateSelectizeInput(session, "comcode8", selected = "")})
  shinyjs::onclick("dateSlider", {updateSelectizeInput(session, "dateSlider", selected = "")})
  
  # OBSERVE STATEMENTS FOR MODIFYING DROPDOWNS -------------------------------
  
  observe({
    comcode_2_selection <- input$comcode2

    # Update Comcodes
    updateSelectizeInput(session,"comcode4", "4-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", comcode_4[comcode_4$parent %in% comcode_2_selection,"commoditycode"]),
                      options = list(maxItems = 5))
    updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", comcode_6[comcode_6$parent %in% comcode_2_selection,"commoditycode"]),
                      options = list(maxItems = 5))
    updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", comcode_8[comcode_8$parent %in% comcode_2_selection,"commoditycode"]),
                      options = list(maxItems = 5))
  })

  observe({
    comcode_4_selection <- input$comcode4
    print(paste("4:",comcode_4_selection))

    # Update Comcodes
    if (is.null(comcode_4_selection) == FALSE) {
      if ("All" %in% comcode_4_selection){
        updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_6$commoditycode ),
                          options = list(maxItems = 5))
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_8$commoditycode),
                          options = list(maxItems = 5))
      } else {
        updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_6[comcode_6$parent %in% comcode_4_selection,"commoditycode"]),
                          options = list(maxItems = 5))
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_8[comcode_8$parent %in% comcode_4_selection,"commoditycode"]),
                          options = list(maxItems = 5))
      }
    }
  })

  observe({
    comcode_6_selection <- input$comcode6
    print(paste("6:",comcode_6_selection))
    
    # Update Comcodes
    if (is.null(comcode_6_selection) == FALSE) {
      if ("All" %in% comcode_6_selection){
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_8$commoditycode),
                          options = list(maxItems = 5))
      } else {
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_8[comcode_8$parent %in% comcode_6_selection,"commoditycode"]),
                          options = list(maxItems = 5))
      }
    }
  })

  observeEvent(input$queryButton,{
    input$queryButton
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Generating Visualisations", value = 1)
    
    
    # Use selectors information to build plot
    isolate({
      
      # Control handling for comcode selectors
      comcode2query = input$comcode2
      if ("All" %in% input$comcode4) {comcode4query = "__"} else {comcode4query = input$comcode4}
      if ("All" %in% input$comcode6) {comcode6query = "__"} else {comcode6query = input$comcode6}
      if ("All" %in% input$comcode8) {comcode8query = "__"} else {comcode8query = input$comcode8}
      
      # This is kind of a funny way of doing things, but simply pasting the strings
      # together and taking the last 8 characters works quickly, easily and cleanly.
      comcodequery = paste(comcode2query, comcode4query, comcode6query, comcode8query, sep = "")
      comcodequery = substr(comcodequery, nchar(comcodequery)-7, nchar(comcodequery))
      
      # Obtain date range
      daterangequery <- dates[match(input$datestart,dates):match(input$dateend,dates)]
      
      # Update dateSlider with daterangequery
      updateSelectizeInput(session,"dateSlider",
                           selected = "All",
                           choices=c("All", daterangequery))
      
      # First line of query dependent on Import or Export - parametrize to select*sumquery vars.
      
      if (input$impexpSelect == "Imports"){
        selectportsumquery <- "SELECT coo_alpha,comcode,account_date,sum(value),sum(quantity_1) FROM imports "
        selectcountrysumquery <- "SELECT comcode,port_alpha,account_date,sum(value),sum(quantity_1) FROM imports "
      }
      else if (input$impexpSelect == "Exports"){
        selectportsumquery <- "SELECT comcode,coo_alpha,account_date,sum(value),sum(quantity_1) FROM exports "
        selectcountrysumquery <- "SELECT port_alpha,comcode,account_date,sum(value),sum(quantity_1) FROM exports "
      }
      
      portsumquery = paste(selectportsumquery,
                           "WHERE (comcode SIMILAR TO '(",
                           paste(comcodequery,collapse = "|"),
                           ")') AND (account_date IN ('",
                           paste(daterangequery, collapse = "', '"),
                           "')) GROUP BY comcode,coo_alpha,account_date",
                           sep = "")
      
      countrysumquery = paste(selectcountrysumquery,
                              "WHERE (comcode SIMILAR TO '(",
                              paste(comcodequery,collapse = "|"),
                              ")') AND (account_date IN ('",
                              paste(daterangequery, collapse = "', '"),
                              "')) GROUP BY comcode,port_alpha,account_date",
                              sep = "")
      
      # Query data
      progress$set(detail = "Querying Country -> Comcode data")
      portsumraw <- dbGetQuery(tradedata, portsumquery)
      
      progress$set(detail = "Querying Comcode -> Port data")
      countrysumraw <- dbGetQuery(tradedata, countrysumquery)
      
      if (input$impexpSelect == "Imports") {
        colnames(portsumraw) = c("country","comcode","month","price", "weight")
        colnames(countrysumraw) = c("comcode","port","month","price", "weight")
      } else if (input$impexpSelect == "Exports") {
        colnames(portsumraw) = c("comcode","country","month","price", "weight")
        colnames(countrysumraw) = c("port","comcode","month","price", "weight")
      }
      
      portsumraw$country[is.na(portsumraw$country)] <- "Unknown Country" # blank country = <NA>
      countrysumraw$port[countrysumraw$port == ""] <- "Unknown Port" # blank port = ""
      
      
      # End Isolate
      })
    
    queryData$portsumraw <- portsumraw
    queryData$countrysumraw <- countrysumraw
    
  })
  
  # ||||||||||||
  # CHAINS WITH
  # ||||||||||||
  
  observe({
    # Only run after query button is pressed.
    if (input$queryButton == 0) return()
    
    # Dependencies - changes to Date Slider and Unit Selector
    input$dateSlider
    input$unitSelect
    
    # Prepare portsum and countrysum into appropriate format for rest of app
    # Based on date and unit, selected from fluidrow beneath comcode legend
    
    # Select correct month
    if (input$dateSlider == "All") {
      portsum <- queryData$portsumraw %>% select(-month)
      countrysum <- queryData$countrysumraw %>% select(-month)
      portsum <- portsum %>% group_by(country,comcode) %>% summarise(price = sum(price), weight = sum(weight))
      countrysum <- countrysum %>% group_by(comcode,port) %>% summarise(price = sum(price), weight = sum(weight))
    } else {
      portsum <- queryData$portsumraw %>% filter(month == input$dateSlider) %>% select(-month)
      countrysum <- queryData$countrysumraw %>% filter(month == input$dateSlider) %>% select(-month)
    }

    
    # Select correct unit
    if (input$unitSelect == "Value (GBP)"){
      portsum <- portsum %>% select(-weight)
      countrysum <- countrysum %>% select(-weight)
      
    } else if (input$unitSelect == "Weight (KG)"){
      portsum <- portsum %>% select(-price)
      countrysum <- countrysum %>% select(-price)
      
    } else if (input$unitSelect == "Price Per Kilo GBP/KG"){
      portsum$value <- portsum$price / portsum$weight
      portsum <- portsum %>% select(-c(price,weight))
      countrysum$value <- countrysum$price / countrysum$weight
      countrysum <- countrysum %>% select(-c(price,weight))
    }
    
    # At this point there should be two string and one numeric vector in both portsum
    # and countrysum dataframes. Now rename that numeric vector, which is the unit used,
    # to value.
    
    colnames(portsum)[colnames(portsum) %in% c("price","weight")] <- "value"
    colnames(countrysum)[colnames(countrysum) %in% c("price","weight")] <- "value"
    
    # Ungroup the data frames.
    portsum <- ungroup(portsum)
    countrysum <- ungroup(countrysum)
    
    # Clean and Shape Data --------------------------------------------------
    
    isolate({
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Generating Visualisations", value = 1)
      progress$set(detail = "Clean + Shape Data")
      
      
      # Create Links & Nodes dataframe. ---------------------------------------
      
      link_portsum <- portsum
      colnames(link_portsum) <- c("source","target","value")
      link_countrysum <- countrysum
      colnames(link_countrysum) <- c("source","target","value")
      
      links <- bind_rows(link_portsum,link_countrysum)
      nodes <- data.frame(unique(c(links$source,links$target)),stringsAsFactors = FALSE)
      colnames(nodes) = "name"
      
      
      # COMCODE LEGEND SPECIFIC -----------------------------------------------
      
      # Create df - list of commodity codes displayed. Match description to second col
      comcodelegend <- tibble(commoditycode = unique(portsum$comcode))
      comcodelegend <- left_join(comcodelegend, comcodelookup, by = "commoditycode")
      
      
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
      
      #browser()
      
      # Get map_data World names from iso codes using iso.expand
      portsum_countries <- iso.expand(unique(portsum$country[portsum$country != "Unknown Country"]))
      
      # Special Case - add serbia if XS is used - iso.expand only considers RS as Serbia
      if ("XS" %in% unique(portsum$country)) {portsum_countries = c(portsum_countries, "Serbia")}
      portsum_countries <- tibble(portsum_countries, iso.alpha(portsum_countries))
      colnames(portsum_countries) <- c("name","code")
      portsum_countries[portsum_countries$name == "Serbia","code"] = "XS"
      
      # Aggregate by country
      portsum_countrytotal <- portsum[,c("country","value")] %>% group_by(country) %>% summarise(value = sum(value))
      # Match plot-compatible names to iso codes
      portsum_countrytotal <- left_join(portsum_countrytotal,portsum_countries, by=c("country" = "code"))
      # Join values to mapWorld for plotting
      portsum_countrytotal <- tibble(country = portsum_countrytotal$name,value = portsum_countrytotal$value)
      mapWorld <- left_join(mapWorld,portsum_countrytotal, by = c("region" = "country"))
      
      
      # If using GGPlot, mapWorld DF is sufficient. If using Leaflet, need SpatialPolygons object.
      
      mapWorld_relevant <- mapWorld[!is.na(mapWorld$value),]
      rownames(mapWorld_relevant) <- NULL
      
      # turn into SpatialPolygons
      sp_mapWorld = lapply(unique(mapWorld_relevant$group), function(x) {
        latlonmatrix = as.matrix(mapWorld_relevant[mapWorld_relevant$group == x, c("long", "lat")])
        countryPolygons = Polygons(list(Polygon(latlonmatrix)), ID = x)
        return(countryPolygons)
        return(latlonmatrix)
      })
      
      dataPolygons = SpatialPolygonsDataFrame(SpatialPolygons(sp_mapWorld),
                                              distinct(bind_cols(
                                                region = mapWorld_relevant$region,
                                                group = mapWorld_relevant$group,
                                                value = mapWorld_relevant$value)),
                                              match.ID = FALSE)
    # End Isolate
    })
    
    
    # Now modify reactive variables with output from isolate() to trigger plot renders.
    comcodeLegendData$comcodelegend <- comcodelegend
    sankeyData$links <- links
    sankeyData$nodes <- nodes
    mapData$mapWorld <- mapWorld
    mapData$dataPolygons <- dataPolygons
  })
  
  # Fill in the comcode legend ================================================
  
  output$ComcodeLegend = renderDataTable({
    if (input$queryButton == 0) return()
    
    datatable(comcodeLegendData$comcodelegend,
              rownames = FALSE,
              colnames = c("Commodity Code", "Description"),
              options = list(
                dom = "tp", # disable search bar at top
                pageLength = 5, # set number of elements on page
                columnDefs = list(list(width = "150px", targets = 0)))
    )}
  )
  
  # Fill in the plots =========================================================   
  output$sankeyTrade <- renderSankeyNetwork({
  
  # Suppress output if nothing has been selected yet
    if (input$queryButton == 0) return()

    sankeyNetwork(sankeyData$links, sankeyData$nodes,
                  "source", "target", "value", "name",
                  fontSize = 12, nodeWidth = 30)
  })
  
  output$worldMap <- renderLeaflet({
    if (input$queryButton == 0) return()
    
    # worldmapPlot <- ggplot()
    # worldmapPlot <- ggplot(data = mapData$mapWorld, aes(x=long,y=lat,group=group))
    # worldmapPlot <- worldmapPlot + geom_polygon(colour="grey60", fill="grey80")
    # worldmapPlot <- worldmapPlot + geom_polygon(aes(fill=value), colour = "grey60")
    # worldmapPlot <- worldmapPlot + scale_fill_gradient(trans = "log10")
    # worldmapPlot <- worldmapPlot + coord_fixed(1.3)
    # worldmapPlot
    
    pal <- colorNumeric(palette = "inferno",
                        domain = 0:max(mapData$dataPolygons$value),
                        reverse = TRUE)
    
    value_popup <- paste0("<strong>Country: </strong>", 
                          mapData$dataPolygons$region, 
                          "<br><strong>Value: </strong>", 
                          mapData$dataPolygons$value)
    
    leaflet(data = mapData$dataPolygons) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(mapData$dataPolygons$value),
                  smoothFactor = 0.5,
                  weight = 1,
                  color = "#000000",
                  fillOpacity = 0.7,
                  popup = value_popup) %>% 
      addLegend(pal = pal,
                values = 0:max(mapData$dataPolygons$value), 
                opacity = 0.7, 
                title = "Colour Scale",
                position = "bottomright")
    
  })
  
} # Close Server Function



# SHINY CALL =================================================================

shinyApp(ui = ui, server = server)

# END SCRIPT #################################################################

# JUNKYARD ###################################################################

# Disconnect All Database Cons
# pg = dbDriver("PostgreSQL");lapply(dbListConnections(pg), dbDisconnect)