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

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")

if(require("networkD3") == FALSE) {install.packages("networkD3")}
library("networkD3")

# Load Prerequisite Static data - Ports, Comcodes, etc. ======================

setwd("~/R/ImportTool/")
pg = dbDriver("PostgreSQL")
tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

# Load Metadata
portcode <- dbGetQuery(tradedata, "SELECT * FROM port")
comcode <- dbGetQuery(tradedata, "SELECT * FROM comcode")
countrycode <- dbGetQuery(tradedata, "SELECT * FROM country")

# Note - no data loaded at this stage - we query what we need when we need it

# Create list of 2, 4, 6, 8 digit commodity codes
comcode_2 <- comcode[nchar(comcode$commoditycode) == 2,]
comcode_4 <- comcode[nchar(comcode$commoditycode) == 4,]
comcode_6 <- comcode[nchar(comcode$commoditycode) == 6,]
comcode_8 <- comcode[nchar(comcode$commoditycode) == 8,]

# UI =========================================================================

# Use a fluid Bootstrap layout
ui <- fluidPage(    
    
  # Give the page a title
  titlePanel("UK Non-EU Exports flow diagram"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectizeInput("comcode2", "2-digit Commodity Code:", 
                  choices=c("All", comcode_2$commoditycode )),
      selectizeInput("comcode4", "4-digit Commodity Code:", 
                  choices=c("All", comcode_4$commoditycode)),
      selectizeInput("comcode6", "6-digit Commodity Code:", 
                  choices=c("All", comcode_6$commoditycode)),
      selectizeInput("comcode8", "8-digit Commodity Code:", 
                  choices=c("All", comcode_8$commoditycode),
                  options=list(maxItems = 12000)),
      actionButton("queryButton", "Run Query"),
      hr(),
      helpText("Data obtained from HMRC's Trade Data - "),
      tags$a(href="www.uktradeinfo.com", "Source")
    ),
    
    # Create a spot for the sankey diagram
    mainPanel(
      plotOutput("sankeyTrade")  
    )
  )
)


# SERVER =====================================================================

server <- function(input, output, session) {
  
  sankeyData <- reactiveValues(links = NULL, nodes = NULL)
  
  # OBSERVE STATEMENTS FOR MODIFYING DROPDOWNS -------------------------------
  
  observe({
    comcode_2_selection <- input$comcode2
      
    if (comcode_2_selection == "All"){
      updateSelectizeInput(session,"comcode4", "4-digit Commodity Code:", 
                        choices=c("All", comcode_4$commoditycode ))
      updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:", 
                        choices=c("All", comcode_6$commoditycode ))
      updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:", 
                        choices=c("All", comcode_8$commoditycode),
                        options=list(maxItems = 12000))
    } else {
      updateSelectizeInput(session,"comcode4", "4-digit Commodity Code:", 
                        choices=c("All", comcode_4[comcode_4$parent == comcode_2_selection,"commoditycode"]))
      updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:", 
                        choices=c("All", comcode_6[comcode_6$parent == comcode_2_selection,"commoditycode"]))
      updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:", 
                        choices=c("All", comcode_8[comcode_8$parent == comcode_2_selection,"commoditycode"]),
                        options=list(maxItems = 12000))
    }
  })
  
  observe({
    comcode_4_selection <- input$comcode4
    
    if (comcode_4_selection == "All"){
      updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:", 
                        choices=c("All", comcode_6$commoditycode ))
      updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:", 
                        choices=c("All", comcode_8$commoditycode),
                        options=list(maxItems = 12000))
    } else {
      updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:", 
                        choices=c("All", comcode_6[comcode_6$parent == comcode_4_selection,"commoditycode"]))
      updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:", 
                        choices=c("All", comcode_8[comcode_8$parent == comcode_4_selection,"commoditycode"]),
                        options=list(maxItems = 12000))
    }
  })
  
  observe({
    comcode_6_selection <- input$comcode6
    
    if (comcode_6_selection == "All"){
      updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:", 
                        choices=c("All", comcode_8$commoditycode),
                        options=list(maxItems = 12000))
    } else {
      updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:", 
                        choices=c("All", comcode_8[comcode_8$parent == comcode_6_selection,"commoditycode"]),
                        options=list(maxItems = 12000))
    }
  })
  
  # ACTION BUTTON PRESS -------------------------------------------------------
  # organise Links, Nodes, Sources and Targets for Sankey Diagram
  observeEvent(input$queryButton,{
    if (input$comcode2 == "All") {comcode2query = "__"} else {comcode2query = input$comcode2}
    if (input$comcode4 == "All") {comcode4query = "__"} else {comcode4query = input$comcode4}
    if (input$comcode6 == "All") {comcode6query = "__"} else {comcode6query = input$comcode6}
    if (input$comcode8 == "All") {comcode8query = "__"} else {comcode8query = input$comcode8}
    
    comcodequery = paste(comcode2query, comcode4query, comcode6query, comcode8query)
    
    portsumquery = paste("SELECT flag_alpha,comcode,sum(value) FROM imports ",
                         "WHERE comcode SIMILAR TO '",
                         comcodequery,
                         "' GROUP BY comcode,flag_alpha",
                         sep = "")
    
    flagsumquery = paste("SELECT comcode,port_alpha,sum(value) FROM imports ",
                         "WHERE comcode SIMILAR TO '",
                         comcodequery,
                         "' GROUP BY comcode,port_alpha",
                         sep = "")
    
    # Create Links DF
    portsum = dbGetQuery(tradedata, portsumquery)
    flagsum = dbGetQuery(tradedata, flagsumquery)
    colnames(portsum) = c("source","target","value")
    colnames(flagsum) = c("source","target","value")
    links = rbind(portsum,flagsum)
    
    # Create Links DF
    nodes = data.frame(unique(c(links$source,links$target)),stringsAsFactors = FALSE)
    
    # Replace links source, target columns with IDs specified in nodes.
    # Match to row number in nodes (which is uniquely indexed!)
    # Note - must be zero indexed, hence match - 1
    links$source = vapply(links$source, function(x){
      x = match(x,nodes[,1])-1
    }, double(1))
    
    links$target = vapply(links$target, function(x){
      x = match(x,nodes[,1])-1
    }, double(1))
    
    sankeyData$links <- links
    sankeyData$nodes <- nodes
    
  })

  # Fill in the sankey diagram ================================================
  output$sankeyTrade <- renderPlot({
    if (is.null(sankeyData$links)) return()
    sankeyNetwork(sankeyData$links, sankeyData$nodes, "source", "target", "value", fontSize = 12, nodeWidth = 30)
  })
  
  }



# SHINY CALL =================================================================

shinyApp(ui = ui, server = server)

# END SCRIPT #################################################################

# JUNKYARD ###################################################################

# Annual archives ------------------------------------------------------------
# Special Annual Cases -------------------------------------------------------
# Unzip monthly files --------------------------------------------------------
# Cleanup ====================================================================
# Print info =================================================================