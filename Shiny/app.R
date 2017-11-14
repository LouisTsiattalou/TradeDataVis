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

# Load Packages --------------------------------------------------------------

if(require("shiny") == FALSE) {install.packages("shiny")}
library("shiny")

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
pg = dbDriver("PostgreSQL")
tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

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
        actionButton("queryButton", "Run Query"),
        hr(),
        helpText("Data obtained from HMRC's Trade Data - ", tags$a(href="www.uktradeinfo.com", "Source"))
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
    
    # Create a spot for the plots
    fluidRow(
      column(12,
        tabsetPanel(
          tabPanel("FLOW", sankeyNetworkOutput(outputId = "sankeyTrade")), 
          tabPanel("MAP", leafletOutput(outputId = "worldMap"))
        )
      )
    )
  )
)


# SERVER =====================================================================

server <- function(input, output, session) {
  
  comcodeLegendData <- reactiveValues(comcodelegend = NULL)
  sankeyData <- reactiveValues(links = NULL, nodes = NULL)
  mapData <- reactiveValues(mapWorld = NULL)
  
  # SERVER SIDE COMMODITY CODE LOOKUP
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
    
    
    # Use comcodes on selectors to build plot
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
      #browser()
      
      # Obtain date range
      daterangequery <- dates[match(input$datestart,dates):match(input$dateend,dates)]
      
      portsumquery = paste("SELECT cod_alpha,comcode,sum(value) FROM imports ",
                           "WHERE (comcode SIMILAR TO '(",
                           paste(comcodequery,collapse = "|"),
                           ")') AND (account_date IN ('",
                           paste(daterangequery, collapse = "', '"),
                           "')) GROUP BY comcode,cod_alpha",
                           sep = "")
      
      countrysumquery = paste("SELECT comcode,port_alpha,sum(value) FROM imports ",
                              "WHERE (comcode SIMILAR TO '(",
                              paste(comcodequery,collapse = "|"),
                              ")') AND (account_date IN ('",
                              paste(daterangequery, collapse = "', '"),
                              "')) GROUP BY comcode,port_alpha",
                              sep = "")
      
      # Query data
      progress$set(detail = "Querying Country -> Comcode data")
      portsum = dbGetQuery(tradedata, portsumquery)
      
      progress$set(detail = "Querying Comcode -> Port data")
      countrysum = dbGetQuery(tradedata, countrysumquery)
      
      # Clean and Shape
      progress$set(detail = "Clean + Shape Data")
      colnames(portsum) = c("source","target","value")
      colnames(countrysum) = c("source","target","value")
      portsum$source[is.na(portsum$source)] <- "Unknown Country" # blank country = <NA>
      countrysum$target[countrysum$target == ""] <- "Unknown Port" # blank port = ""
      
      # Create Links & Nodes dataframe.
      links = rbind(portsum,countrysum)
      nodes = data.frame(unique(c(links$source,links$target)),stringsAsFactors = FALSE)
      colnames(nodes) = "name"
      
      
      # COMCODE LEGEND SPECIFIC -----------------------------------------------
      
      # Create df - list of commodity codes displayed. Match description to second col
      comcodelegend <- tibble(commoditycode = unique(portsum$target))
      comcodelegend <- left_join(comcodelegend,comcodelookup,by = "commoditycode")
      
      
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
      portsum_countries <- iso.expand(unique(portsum$source[portsum$source != "Unknown Country"]))
      
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
      
      # If using GGPlot, mapWorld is sufficient. If using Leaflet, need SpatialPolygons object.
      
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