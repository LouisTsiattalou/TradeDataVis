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

if(require("shinyWidgets") == FALSE) {install.packages("shinyWidgets")}
library("shinyWidgets")

if(require("shinycssloaders") == FALSE) {install.packages("shinycssloaders")}
library("shinycssloaders")

if(require("dplyr") == FALSE) {install.packages("dplyr")}
library("dplyr")

if(require("purrr") == FALSE) {install.packages("purrr")}
library("purrr")

if(require("devtools") == FALSE) {install.packages("devtools")}
library("devtools")

# The development version of ggplot2 is necessary for the plotly time series plots to render correctly
# install_github("tidyverse/ggplot2")

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")

if(require("networkD3") == FALSE) {install.packages("networkD3")}
library("networkD3")

if(require("rgeos") == FALSE) {install.packages("rgeos")}
library("rgeos")

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

if(require("plotly") == FALSE) {install.packages("plotly")}
library("plotly")

if(require("scales") == FALSE) {install.packages("scales")}
library("scales")

if(require("shinythemes") == FALSE) {install.packages("shinythemes")}
library("shinythemes")

if(require("pool") == FALSE) {install.packages("pool")}
library("pool")

# Function Definitions -------------------------------------------------------

# Descendants - obtain all descendants of a vector of commodity codes.
# Tested on "01" and "02", takes 0.25 secs for 500 descendants. Quick!
descendants <- function(data, code) {
    # Strip 8-digit codes (these are leaf nodes)
    code <- code[nchar(code) < 8]
    if (length(code) == 0) {
        return()
    } else {
        # get all the children's indices
        f <- data$parent %in% code
        # get current children
        children <- data$commoditycode[f]
        return(c(children, descendants(data, children)))
    }
}

# Load Prerequisite Static data - Ports, Comcodes, etc. ======================
# Use pool instead of dbConnect
# setwd("~/R/ImportTool/Shiny/")
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)

#tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2], host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])
tradedata <- dbPool(
    drv = pg,
    user = dbenv[1,2],
    password = dbenv[2,2],
    host = dbenv[3,2],
    port = dbenv[4,2],
    dbname = dbenv[5,2]
)

# onStop(function() {
#     poolClose(tradedata)
# })


# Load Metadata
conn <- poolCheckout(tradedata)

portcode <- dbGetQuery(conn, "SELECT * FROM port")
comcode <- dbGetQuery(conn, "SELECT * FROM comcode")
countrycode <- dbGetQuery(conn, "SELECT * FROM country")

poolReturn(conn)

# Order Ascending
portcode <- portcode %>% arrange(portname)
comcode <- comcode %>% arrange(commoditycode)
countrycode <- countrycode %>% arrange(countryname)

# Strip Minor Seaports
# 1. If the record's portcode has many portnames to a single portcode, strip those
#    without the first 3 letters being capitals (as this denotes the main port for the portcode).
# 2. If the code isn't duplicated, keep the record.
portcode <- portcode %>% filter((toupper(substr(portname,1,3)) == substr(portname,1,3) & 
                                  (duplicated(portcode) | duplicated(portcode, fromLast = TRUE)) == TRUE
                                )
                                | (duplicated(portcode) | duplicated(portcode, fromLast = TRUE)) == FALSE)

### Factor enables multiple search terms in comcode lookup tab
comcodelookup <- tibble(commoditycode = as.factor(comcode$commoditycode), description = comcode$description)

desclookup <- c(portcode$portname,countrycode$countryname)
names(desclookup) <- c(portcode$portcode,countrycode$countrycode)
desclookup <- data.frame(keyName=names(desclookup), value=desclookup, row.names=NULL, stringsAsFactors = FALSE)
desclookup <- desclookup[desclookup$value != "",]

itemCount <- 5

# Note - no data loaded at this stage - we query what we need when we need it

# Create list of 2, 4, 6, 8 digit commodity codes. Sort by string ascending.
comcode_2 <- comcode[nchar(comcode$commoditycode) == 2,]
comcode_4 <- comcode[nchar(comcode$commoditycode) == 4,]
comcode_6 <- comcode[nchar(comcode$commoditycode) == 6,]
comcode_8 <- comcode[nchar(comcode$commoditycode) == 8,]

# Create month list
# Two and a half month gap between current date and data contained in App.
curyr <- as.numeric(substr(as.character(Sys.Date()),3,4))
curmth <- as.numeric(substr(as.character(Sys.Date()),6,7))
curday <- as.numeric(substr(as.character(Sys.Date()),9,10))
dates <- character(0)

# Current Year
for (j in curmth:1) {
  dates <- c(dates, paste0(as.character(curyr), "-", as.character(sprintf("%02d", j)) ))
}

# All Other Years
syrs <- as.character(sprintf("%02d",c(curyr-1:09)))
smths <- as.character(sprintf("%02d",c(12:1)))
for (i in syrs){
  for (j in smths){
    dates <- c(dates, paste0("20", i, "-", j))
  }
}
# 2 and a bit months time lag: pop off first two/three elements depending on day.
dates <- if (curday > 15) {
              dates[3:length(dates)]
          } else {
              dates[4:length(dates)]    
          }


# UI ==========================================================================
# Set plot animation colour
options(spinner.color="#0dc5c1")

ui <- navbarPage(theme = shinytheme("flatly"), inverse = TRUE,
  
  # Navbar Title
  title = "UK Trade Data Visualisation",
  
  # COMMODITY CODE LOOKUP -----------------------------------------------------
  
  tabPanel("Commodity Code Lookup",
           HTML("<div class=\"alert alert-dismissible alert-warning\">
                    <strong>Alert:</strong> This application is currently in beta. There may be bugs and unexpected behaviour as you use the application. Please help us improve the application by clicking the <i>Feedback</i> tab at the top of the page. <br>
                Additionally, please only use in Chrome. Internet Explorer and Microsoft Edge are unsupported.
                </div>"),
           tags$i("Perform a fuzzy search on Commodity Codes using the search box at the top!"),
           hr(),
           dataTableOutput("ComcodeLookup") %>% withSpinner(type=6)
           ),
  
  # NON-EU TRADE --------------------------------------------------------------
  
  tabPanel("Non-EU Trade",
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
      column(2,
        selectizeInput("datestart", "Period Start:",
                     choices=dates),
        selectizeInput("dateend", "Period End:",
                       choices=dates)
        ),
      column(2,
        selectizeInput("countryselect", "Country:",
                       choices=c("All",countrycode$countryname)),
        selectizeInput("portselect", "Port:",
                       choices=c("All",portcode$portname))
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
      column(2,
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
      column(2,
        tags$label("Select Month:"),     
        checkboxInput("dateSliderAll", label = "All", value = TRUE)
             ),
      column(4,
        shinyjs::disabled(sliderTextInput("dateSlider", label = NULL, grid = TRUE, force_edges = TRUE,
                                   choices = c(dates)))
             ),
      column(6,
        tags$label("Select Units:"),
        radioButtons("unitSelect", label = NULL, inline = TRUE,
                     choices = c("Value (GBP)", "Weight (KG)", "Price Per Kilo (GBP/KG)")))
    ),
    
    # Create a spot for the plots
    fluidRow(
      column(12,
        tabsetPanel(
          tabPanel("FLOW", sankeyNetworkOutput(outputId = "sankeyTrade") %>% withSpinner(type=6)), 
          tabPanel("MAP", leafletOutput(outputId = "worldMap", height = 600) %>% withSpinner(type=6)),
          tabPanel("TIME SERIES",
            tabsetPanel(
              tabPanel("By Commodity Code", plotlyOutput(outputId = "tsByComcode") %>% withSpinner(type=6)),
              tabPanel("By Country", plotlyOutput(outputId = "tsByCountry") %>% withSpinner(type=6)),
              tabPanel("By Port", plotlyOutput(outputId = "tsByPort") %>% withSpinner(type=6))
            )
          )
        )
      )
    ),
    
    # Create a spot for the download button
    fluidRow(
      column(10),
      column(2, downloadButton("dataDownload", "Download"))
    )
  ),
  
  # EU TRADE -------------------------------------------------------------------
  
  tabPanel("EU Trade",
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
      column(2,
        selectizeInput("eudatestart", "Period Start:",
                     choices=dates),
        selectizeInput("eudateend", "Period End:",
                       choices=dates)
        ),
      column(2,
        selectizeInput("eucountryselect", "Country:",
                       choices=c("All",countrycode$countryname))
        ),
      column(3,
        selectizeInput("eucomcode2", "2-digit Commodity Code:",
                    selected = "01",
                    choices=c(comcode_2$commoditycode),
                    options = list(maxItems = 5)),
        selectizeInput("eucomcode4", "4-digit Commodity Code:",
                    selected = "All",
                    choices=c("All", comcode_4$commoditycode),
                    options = list(maxItems = 5))
      ),
      column(3,
        selectizeInput("eucomcode6", "6-digit Commodity Code:",
                    selected = "All",
                    choices=c("All", comcode_6$commoditycode),
                    options = list(maxItems = 5)),
        selectizeInput("eucomcode8", "8-digit Commodity Code:",
                    selected = "All",
                    choices=c("All", comcode_8$commoditycode),
                    options = list(maxItems = 5))
      ),
      column(2,
        radioButtons("euimpexpSelect", label = NULL,
                     choices = c("Imports","Exports")),
        actionButton("euqueryButton", "Run Query")
      ),
      hr()
    ),
    
    # Create comcode legend
    fluidRow(
      column(12,
        dataTableOutput("euComcodeLegend")
      ),
      hr()
    ),
    
    # Create slider/unit bar
    fluidRow(
      column(2,
        tags$label("Select Month:"),     
        checkboxInput("eudateSliderAll", label = "All", value = TRUE)
             ),
      column(4,
        shinyjs::disabled(sliderTextInput("eudateSlider", label = NULL, grid = TRUE, force_edges = TRUE,
                                   choices = c(dates)))
             ),
      column(6,
        tags$label("Select Units:"),
        radioButtons("euunitSelect", label = NULL, inline = TRUE,
#                     choices = c("Value (GBP)", "Weight (KG)", "Price Per Kilo (GBP/KG)", "Number of Consignments")))
                     choices = c("Value (GBP)","Number of Consignments")))
    ),
    
    # Create a spot for the plots
    fluidRow(
      column(12,
        tabsetPanel(
          tabPanel("FLOW", sankeyNetworkOutput(outputId = "eusankeyTrade") %>% withSpinner(type=6)), 
          tabPanel("MAP", leafletOutput(outputId = "euworldMap", height = 600) %>% withSpinner(type=6)),
          tabPanel("TIME SERIES",
            tabsetPanel(
              tabPanel("By Commodity Code", plotlyOutput(outputId = "eutsByComcode") %>% withSpinner(type=6)),
              tabPanel("By Country", plotlyOutput(outputId = "eutsByCountry") %>% withSpinner(type=6))
            )
          )
        )
      )
    ),
    
    # Create a spot for the download button
    fluidRow(
      column(10),
      column(2, downloadButton("euDataDownload", "Download"))
    )
  ),
  # Feedback Form (I suggest you leave this part folded...)
  tabPanel("Feedback",
           HTML("
               <div id=\"wufoo-mmr67cc1bqdty6\">
                        Fill out my <a href=\"https://fsaanalytics.wufoo.co.uk/forms/mmr67cc1bqdty6\">online form</a>.
               </div>
                 <div id=\"wuf-adv\" style=\"font-family:inherit;font-size: small;color:#a7a7a7;text-align:center;display:block;\">The easy to use <a href=\"http://wufoo.co.uk/form-builder/\">Wufoo form builder</a> helps you make forms easy, fast, and fun.</div>
                                                                                                                                                                                                                                                            <script type=\"text/javascript\">var mmr67cc1bqdty6;(function(d, t) {
                                                                                                                                                                                                                                                                var s = d.createElement(t), options = {
                                                                                                                                                                                                                                                                    'userName':'fsaanalytics',
                                                                                                                                                                                                                                                                    'formHash':'mmr67cc1bqdty6',
                                                                                                                                                                                                                                                                    'autoResize':true,
                                                                                                                                                                                                                                                                    'height':'1262',
                                                                                                                                                                                                                                                                    'async':true,
                                                                                                                                                                                                                                                                    'host':'wufoo.co.uk',
                                                                                                                                                                                                                                                                    'header':'show',
                                                                                                                                                                                                                                                                    'ssl':true};
                                                                                                                                                                                                                                                                s.src = ('https:' == d.location.protocol ? 'https://' : 'http://') + 'www.wufoo.co.uk/scripts/embed/form.js';
                                                                                                                                                                                                                                                                s.onload = s.onreadystatechange = function() {
                                                                                                                                                                                                                                                                    var rs = this.readyState; if (rs) if (rs != 'complete') if (rs != 'loaded') return;
                                                                                                                                                                                                                                                                    try { mmr67cc1bqdty6 = new WufooForm();mmr67cc1bqdty6.initialize(options);mmr67cc1bqdty6.display(); } catch (e) {}};
                                                                                                                                                                                                                                                                var scr = d.getElementsByTagName(t)[0], par = scr.parentNode; par.insertBefore(s, scr);
                                                                                                                                                                                                                                                            })(document, 'script');</script>"
           )
        ),
  # Enable ShinyJS support for cleaner on-click and disable features.
  shinyjs::useShinyjs()
)


# SERVER ======================================================================

server <- function(input, output, session) {
  
  queryData <- reactiveValues(dataraw = NULL, portsumraw = NULL, countrysumraw = NULL)
  comcodeLegendData <- reactiveValues(comcodelegend = NULL)
  sankeyData <- reactiveValues(links = NULL, nodes = NULL)
  mapData <- reactiveValues(mapWorld = NULL)
  timeseriesData <- reactiveValues(byComcode = NULL, byCountry = NULL, byPort = NULL)

  nullDataframe <- reactiveValues(nullDataframe = NULL, comcodequery = NULL)

  euQueryData <- reactiveValues(euDataRaw = NULL)
  euComcodeLegendData <- reactiveValues(comcodelegend = NULL)
  euSankeyData <- reactiveValues(links = NULL, nodes = NULL)
  euMapData <- reactiveValues(mapWorld = NULL)
  euTimeseriesData <- reactiveValues(byComcode = NULL, byCountry = NULL)
 
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

  shinyjs::onclick("eucomcode2", {updateSelectizeInput(session, "eucomcode2", selected = "")})
  shinyjs::onclick("eucomcode4", {updateSelectizeInput(session, "eucomcode4", selected = "")})
  shinyjs::onclick("eucomcode6", {updateSelectizeInput(session, "eucomcode6", selected = "")})
  shinyjs::onclick("eucomcode8", {updateSelectizeInput(session, "eucomcode8", selected = "")})
  
  observeEvent(input$dateSliderAll, {
      if (input$dateSliderAll == FALSE) {
          shinyjs::enable("dateSlider")
      } else {
          shinyjs::disable("dateSlider")
      }
  })
  
  observeEvent(input$eudateSliderAll, {
      if (input$eudateSliderAll == FALSE) {
          shinyjs::enable("eudateSlider")
      } else {
          shinyjs::disable("eudateSlider")
      }
  })
  
  # SERVER (NON-EU) ==========================================================
  
  # OBSERVE STATEMENTS FOR MODIFYING DROPDOWNS -------------------------------
  observe({
    allDescendants <- descendants(comcode,input$comcode2)

    # Update Comcodes
    updateSelectizeInput(session,"comcode4", "4-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", sort(allDescendants[nchar(allDescendants) == 4])),
                      options = list(maxItems = 5))
    updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", sort(allDescendants[nchar(allDescendants) == 6])),
                      options = list(maxItems = 5))
    updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", sort(allDescendants[nchar(allDescendants) == 8])),
                      options = list(maxItems = 5))
  })

  observe({
    allDescendants <- descendants(comcode,input$comcode4)
    comcode_4_selection <- input$comcode4
    
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
                          choices=c("All", sort(allDescendants[nchar(allDescendants) == 6])),
                          options = list(maxItems = 5))
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", sort(allDescendants[nchar(allDescendants) == 8])),
                          options = list(maxItems = 5))
      }
    }
  })

  observe({
    allDescendants <- descendants(comcode,input$comcode6)
    comcode_6_selection <- input$comcode6
    
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
                          choices=c("All", sort(allDescendants[nchar(allDescendants) == 8])),
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
      
      # Set nullDataframe flag to FALSE
      nullDataframe$nullDataframe <- FALSE
      
      # Control handling for comcode selectors
      comcode2query = input$comcode2
      if ("All" %in% input$comcode4 | is.null(input$comcode4)) {comcode4query = "__"} else {comcode4query = input$comcode4}
      if ("All" %in% input$comcode6 | is.null(input$comcode6)) {comcode6query = "__"} else {comcode6query = input$comcode6}
      if ("All" %in% input$comcode8 | is.null(input$comcode8)) {comcode8query = "__"} else {comcode8query = input$comcode8}
      
      # This is kind of a funny way of doing things, but simply pasting the strings
      # together and taking the last 8 characters works quickly, easily and cleanly.
      comcodequery = paste(comcode2query, comcode4query, comcode6query, comcode8query, sep = "")
      comcodequery = substr(comcodequery, nchar(comcodequery)-7, nchar(comcodequery))
      
      # Country and Port Queries
      if ("All" %in% input$portselect) {
        portquery <- portcode$portcode
      } else {
        portquery <- portcode %>% filter(portname %in% input$portselect) %>% pull(portcode)
      }
      
      if ("All" %in% input$countryselect){
        countryquery <- countrycode$countrycode
      } else {
        countryquery <- countrycode %>% filter(countryname %in% input$countryselect) %>% pull(countrycode)
      }
          
      # Obtain date range
      daterangequery <- rev(dates[match(input$dateend,dates):match(input$datestart,dates)])
      # Update dateSlider with daterangequery
      updateSliderTextInput(session,"dateSlider",
                           choices=daterangequery)
      # Convert to Query Format
      daterangequery <- paste0(substr(daterangequery,6,7),
                               "/",
                               substr(daterangequery,1,4))
 
      # First line of query dependent on Import or Export - parametrize to select*sumquery vars.
      
      if (input$impexpSelect == "Imports"){
        selectquery <- "SELECT coo_alpha,comcode,port_alpha,account_date,sum(value),sum(quantity_1) FROM imports "
        wherecountrycondition <- ")') AND (coo_alpha SIMILAR TO '("
        groupbyquery <- "GROUP BY coo_alpha,comcode,port_alpha,account_date"
      }
      else if (input$impexpSelect == "Exports"){
        selectquery <- "SELECT port_alpha,comcode,cod_alpha,account_date,sum(value),sum(quantity_1) FROM exports "
        wherecountrycondition <- ")') AND (cod_alpha SIMILAR TO '("
        groupbyquery <- "GROUP BY cod_alpha,comcode,port_alpha,account_date"
      }

      dataquery = paste0(selectquery,
                         "WHERE (comcode SIMILAR TO '(",
                         paste(comcodequery,collapse = "|"),
                         ")') AND (port_alpha SIMILAR TO '(",
                         paste(portquery,collapse = "|"), 
                         wherecountrycondition, # This depends on cod/coo_alpha!
                         paste(countryquery,collapse = "|"), 
                         ")') AND (account_date IN ('",
                         paste(daterangequery, collapse = "', '"),
                         "'))",
                         groupbyquery) # import = coo_alpha, export = cod_alpha!
                         
      progress$set(detail = "Querying Data from Database")

      conn <- poolCheckout(tradedata)
      dataraw <- dbGetQuery(conn, dataquery)
      poolReturn(conn)

      # Break out of observeEvent if query returns no values (ie, df == dim 0,0)
      if (dim(dataraw)[1] == 0) {
        # Set nullDataframe flag to TRUE to stop downstream reactivity
        nullDataframe$nullDataframe <- TRUE
        nullDataframe$comcodequery <- paste(gsub("_","",comcodequery),collapse = ",")
        isolate({
            showModal(modalDialog(title = "Alert!",
                                  paste0("No ",
                                         input$impexpSelect,
                                         " for Date Range ",
                                         input$datestart, " - ", input$dateend,
                                         " and Commodity Code(s) ",
                                         nullDataframe$comcodequery,
                                         ".")), session)
            print(paste0("No ",
                         input$impexpSelect,
                         " for Date Range ",
                         input$datestart, " - ", input$dateend,
                         " and Commodity Code(s) ",
                         nullDataframe$comcodequery,
                         "."))
        })
        req(FALSE)
      }

      # Set up correct colnames and create portsum/countrysum dataframes from dataraw
      if (input$impexpSelect == "Imports") {
        colnames(dataraw) = c("country","comcode","port","month","price", "weight")
        portsumraw <- dataraw %>% select(country,comcode,month,price,weight) %>% group_by(country,comcode,month) %>% summarise(price = sum(price), weight = sum(weight))
        countrysumraw <- dataraw %>% select(comcode,port,month,price,weight) %>% group_by(comcode,port,month) %>% summarise(price = sum(price), weight = sum(weight))
      } else if (input$impexpSelect == "Exports") {
        colnames(dataraw) = c("port","comcode","country","month","price", "weight")
        portsumraw <- dataraw %>% select(comcode,country,month,price,weight) %>% group_by(comcode,country,month) %>% summarise(price = sum(price), weight = sum(weight))
        countrysumraw <- dataraw %>% select(port,comcode,month,price,weight) %>% group_by(port,comcode,month) %>% summarise(price = sum(price), weight = sum(weight))
      }
      
      portsumraw <- ungroup(portsumraw)
      countrysumraw <- ungroup(countrysumraw)
    
      # Transform month back to readable format 
      portsumraw$month <- paste0(substr(portsumraw$month,4,7),
                                "-",
                                substr(portsumraw$month,1,2))
      countrysumraw$month <- paste0(substr(countrysumraw$month,4,7),
                                "-",
                                substr(countrysumraw$month,1,2))
      portsumraw$country[is.na(portsumraw$country)] <- "Unknown Country" # blank country = <NA>
      countrysumraw$port[countrysumraw$port == ""] <- "Unknown Port" # blank port = ""

      # End Isolate
      })
    
    queryData$dataraw <- dataraw
    queryData$portsumraw <- portsumraw
    queryData$countrysumraw <- countrysumraw

  })
  
  # ||||||||||||
  # CHAINS WITH
  # ||||||||||||
  
  observe({
    # Conditions for observe statement to run
    if (input$queryButton == 0) return()
    req(input$dateSlider)
    if (nullDataframe$nullDataframe == TRUE) {
      # Break out of reactive chain
      req(FALSE)
    }
    
    # Dependencies - changes to Date Slider and Unit Selector
    input$dateSlider
    input$unitSelect
    
    # Prepare portsum and countrysum into appropriate format for rest of app
    # Based on date and unit, selected from fluidrow beneath comcode legend
    
    # Select correct month
    if (input$dateSliderAll == TRUE) {
      portsum <- queryData$portsumraw %>% select(-month)
      countrysum <- queryData$countrysumraw %>% select(-month)
      if (input$impexpSelect == "Imports") {
        portsum <- portsum %>% group_by(country,comcode) %>% summarise(price = sum(price), weight = sum(weight))
        countrysum <- countrysum %>% group_by(comcode,port) %>% summarise(price = sum(price), weight = sum(weight))
      } else if (input$impexpSelect == "Exports") {
        portsum <- portsum %>% group_by(comcode,country) %>% summarise(price = sum(price), weight = sum(weight))
        countrysum <- countrysum %>% group_by(port,comcode) %>% summarise(price = sum(price), weight = sum(weight))
      }
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
      
    } else if (input$unitSelect == "Price Per Kilo (GBP/KG)"){
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
    
    # Check again if, after sorting, we're dealing with a blank df.
    if (dim(portsum)[1] == 0) {
      nullDataframe$nullDataframe <- TRUE
      isolate({
        showModal(modalDialog(title = "Alert!",
                              paste0("No ",
                                     input$impexpSelect,
                                     " for ",
                                     input$dateSlider,
                                     "."),
                              easyClose = TRUE,
                              footer = NULL), session)
      })
      # Break out of reactive chain
      req(FALSE)
    }
    
     
    # Clean and Shape Data --------------------------------------------------
    
    isolate({
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Generating Visualisations", value = 1)
      progress$set(detail = "Clean + Shape Data")
      
      
      # COMCODE LEGEND SPECIFIC -----------------------------------------------
      
      # Create df - list of commodity codes displayed. Match description to second col
      comcodelegend <- tibble(commoditycode = unique(portsum$comcode))
      comcodelegend <- left_join(comcodelegend, comcodelookup, by = "commoditycode") %>% arrange(commoditycode)
      
      
      # SANKEY SPECIFIC -------------------------------------------------------
      
      # Create Links & Nodes dataframe.
      
      link_portsum <- portsum
      colnames(link_portsum) <- c("source","target","value")
      link_countrysum <- countrysum
      colnames(link_countrysum) <- c("source","target","value")
      
      links <- bind_rows(link_portsum,link_countrysum)
      nodes <- data.frame(unique(c(links$source,links$target)),stringsAsFactors = FALSE)
      colnames(nodes) = "name"
      
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
      portsum_countries <- iso.expand(unique(portsum$country[portsum$country != "Unknown Country"]))
      
      # Special Case - replace China's iso.expand entry as it is not recognised by iso.alpha
      if ("(^China(?!:Hong Kong|:Macao))|(^Paracel Islands)" %in% portsum_countries) {
         portsum_countries[match("(^China(?!:Hong Kong|:Macao))|(^Paracel Islands)", portsum_countries)] <- "China" 
      }
      
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
      
      # TIME SERIES SPECIFIC --------------------------------------------------
      
      # > We have to have month information, which means portsum/countrysum aren't sufficient.
      # > We must use the queryData reactive portsumraw/countrysumraw and use dplyr on that.
      # > Unit selections are slightly different too - price per kilo must be summed by
      #   comcode, by port, and by country then divided for each month.
      
      # Select correct unit
      if (input$unitSelect == "Value (GBP)"){
        byComcode <- queryData$portsumraw %>% select(month,comcode,price)
        byCountry <- queryData$portsumraw %>% select(month,country,price)
        byPort <- queryData$countrysumraw %>% select(month,port,price)
        
      } else if (input$unitSelect == "Weight (KG)"){
        byComcode <- queryData$portsumraw %>% select(month,comcode,weight)
        byCountry <- queryData$portsumraw %>% select(month,country,weight)
        byPort <- queryData$countrysumraw %>% select(month,port,weight)
        
      }
      
      # Special case for Price Per Kilo
      if (input$unitSelect == "Price Per Kilo (GBP/KG)"){
        byComcode <- queryData$portsumraw %>%
                       select(month,comcode,price,weight) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight)) %>%
                       group_by(month,comcode) %>% summarise(value = sum(value))
        byCountry <- queryData$portsumraw %>%
                       select(month,country,price,weight) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight)) %>%
                       group_by(month,country) %>% summarise(value = sum(value))
        byPort <- queryData$countrysumraw %>%
                    select(month,port,price,weight) %>%
                    mutate(value = price/weight) %>%
                    select(-c(price,weight)) %>%
                    group_by(month,port) %>% summarise(value = sum(value))
      } else { 
        # else statement required for non-PricePerKilo options
        colnames(byComcode)[colnames(byComcode) %in% c("price","weight")] <- "value"
        colnames(byCountry)[colnames(byCountry) %in% c("price","weight")] <- "value"
        colnames(byPort)[colnames(byPort) %in% c("price","weight")] <- "value"
        
        if (input$dateSliderAll != TRUE){
          byComcode <- byComcode %>% filter(month == input$dateSlider)
          byCountry <- byCountry %>% filter(month == input$dateSlider)
          byPort <- byPort %>% filter(month == input$dateSlider)
        }
        
        # Obtain long format dataframe for time series plot
        byComcode <- byComcode %>% group_by(month,comcode) %>% summarise(value = sum(value))
        byCountry <- byCountry %>% group_by(month,country) %>% summarise(value = sum(value))
        byPort <- byPort %>% group_by(month,port) %>% summarise(value = sum(value))
      }
      
      # Ungroup the data frames.
      byComcode <- ungroup(byComcode)
      byCountry <- ungroup(byCountry)
      byPort <- ungroup(byPort)
      
      # Replace country/port codes with full names.
      byCountry <- byCountry %>%
                     left_join(desclookup, by = c("country" = "keyName")) %>%
                     select(-country) %>%
                     rename(value = value.x, country = value.y)
      
      byPort <- byPort %>%
        left_join(desclookup, by = c("port" = "keyName")) %>%
        select(-port) %>%
        rename(value = value.x, port = value.y)
      
    # End Isolate
    })
    
    
    # Now modify reactive variables with output from isolate() to trigger plot renders.
    comcodeLegendData$comcodelegend <- comcodelegend
    sankeyData$links <- links
    sankeyData$nodes <- nodes
    mapData$mapWorld <- mapWorld
    mapData$dataPolygons <- dataPolygons
    timeseriesData$byComcode <- byComcode
    timeseriesData$byCountry <- byCountry
    timeseriesData$byPort <- byPort
    
    
  })
  
  # Fill in the comcode legend ================================================
  
  output$ComcodeLegend = renderDataTable({
    if (input$queryButton == 0) return()
    req(!nullDataframe$nullDataframe)
    
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
  
  # SANKEY --------------------------------------------------------------------
  
  output$sankeyTrade <- renderSankeyNetwork({
  
  # Suppress output if nothing has been selected yet
    if (input$queryButton == 0) return()
    req(!nullDataframe$nullDataframe)
    
    sankeyNetwork(sankeyData$links, sankeyData$nodes,
                  "source", "target", "value", "name",
                  fontSize = 12, nodeWidth = 30)
  })
  
  
  # MAP -----------------------------------------------------------------------
  
  output$worldMap <- renderLeaflet({
    if (input$queryButton == 0) return()
    req(!nullDataframe$nullDataframe)
    
    pal <- colorNumeric(palette = "inferno",
                        domain = 0:max(mapData$dataPolygons$value),
                        reverse = TRUE)
    
    value_popup <- paste0(ifelse(input$impexpSelect == "Imports", "<strong>Country of Origin: </strong>","<strong>Country of Dispatch: </strong>"), 
                          mapData$dataPolygons$region, 
                          "<br><strong>Value: </strong>", 
                          mapData$dataPolygons$value)
    
    leaflet(data = mapData$dataPolygons) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      # addTiles() %>%
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
                position = "topright")
    
  })
  
  
  # TIME SERIES ---------------------------------------------------------------
  
  output$tsByComcode <- renderPlotly({
      nbars <- length(timeseriesData$byComcode$comcode)
      ggplotly(
          ggplot(data = timeseriesData$byComcode) + 
          geom_col(aes(month,value,fill=comcode), show.legend = TRUE) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$unitSelect, " \n"),
               fill = "Commodity Codes") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })
  
  output$tsByCountry <- renderPlotly({
      nbars <- length(timeseriesData$byCountry$country)
      ggplotly(
          ggplot(data = timeseriesData$byCountry) + 
          geom_col(aes(month,value,fill=country), show.legend = TRUE) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$unitSelect, " \n"),
               fill = "Countries") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })
  
  output$tsByPort <- renderPlotly({
      nbars <- length(timeseriesData$byPort$port)
      ggplotly(
          ggplot(data = timeseriesData$byPort) + 
          geom_col(aes(month,value,fill=port), show.legend = TRUE) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$unitSelect, " \n"),
               fill = "Ports") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })


  # DATA DOWNLOAD ------------------------------------------------------------
    output$dataDownload <- downloadHandler(
        filename = function() {
        "TradeDataVisNonEUExtract.csv"
        },
        content = function(file) {
            downloadfile <- queryData$dataraw %>%
                left_join(comcodelookup, by=c("comcode" = "commoditycode")) %>%
                left_join(portcode, by=c("port" = "portcode")) %>%
                left_join(countrycode, by=c("country" = "countrycode"))
            downloadfile <- downloadfile %>% select(comcode,port,portname,type,country,countryname,month,price,weight,description)
            write.csv(downloadfile, file, row.names = FALSE)
        }
    )



    
  # SERVER (EU) ==============================================================
  
  # OBSERVE STATEMENTS FOR MODIFYING DROPDOWNS -------------------------------
  observe({
    allDescendants <- descendants(comcode,input$eucomcode2)
    
    # Update Comcodes
    updateSelectizeInput(session,"eucomcode4", "4-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", sort(allDescendants[nchar(allDescendants) == 4])),
                      options = list(maxItems = 5))
    updateSelectizeInput(session,"eucomcode6", "6-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", sort(allDescendants[nchar(allDescendants) == 6])),
                      options = list(maxItems = 5))
    updateSelectizeInput(session,"eucomcode8", "8-digit Commodity Code:",
                      selected = "All",
                      choices=c("All", sort(allDescendants[nchar(allDescendants) == 8])),
                      options = list(maxItems = 5))
  })

  observe({
    allDescendants <- descendants(comcode,input$eucomcode4)
    eu_comcode_4_selection <- input$eucomcode4
    
    # Update Comcodes
    if (is.null(eu_comcode_4_selection) == FALSE) {
      if ("All" %in% eu_comcode_4_selection){
        updateSelectizeInput(session,"eucomcode6", "6-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_6$commoditycode ),
                          options = list(maxItems = 5))
        updateSelectizeInput(session,"eucomcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_8$commoditycode),
                          options = list(maxItems = 5))
      } else {
        updateSelectizeInput(session,"eucomcode6", "6-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", sort(allDescendants[nchar(allDescendants) == 6])),
                          options = list(maxItems = 5))
        updateSelectizeInput(session,"eucomcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", sort(allDescendants[nchar(allDescendants) == 8])),
                          options = list(maxItems = 5))
      }
    }
  })

  observe({
    allDescendants <- descendants(comcode,input$eucomcode6)
    eu_comcode_6_selection <- input$eucomcode6
    
    # Update Comcodes
    if (is.null(eu_comcode_6_selection) == FALSE) {
      if ("All" %in% eu_comcode_6_selection){
        updateSelectizeInput(session,"eucomcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcode_8$commoditycode),
                          options = list(maxItems = 5))
      } else {
        updateSelectizeInput(session,"eucomcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", sort(allDescendants[nchar(allDescendants) == 8])),
                          options = list(maxItems = 5))
      }
    }
  })

  observeEvent(input$euqueryButton,{
    input$euqueryButton
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Generating Visualisations", value = 1)
    
    
    # Use selectors information to build plot
    isolate({
      
      # Set nullDataframe flag to FALSE
      nullDataframe$nullDataframe <- FALSE
      
      # Control handling for comcode selectors
      eucomcode2query = input$eucomcode2
      if ("All" %in% input$eucomcode4 | is.null(input$eucomcode4)) {eucomcode4query = "__"} else {eucomcode4query = input$eucomcode4}
      if ("All" %in% input$eucomcode6 | is.null(input$eucomcode6)) {eucomcode6query = "__"} else {eucomcode6query = input$eucomcode6}
      if ("All" %in% input$eucomcode8 | is.null(input$eucomcode8)) {eucomcode8query = "__"} else {eucomcode8query = input$eucomcode8}
      
      # This is kind of a funny way of doing things, but simply pasting the strings
      # together and taking the last 8 characters works quickly, easily and cleanly.
      eucomcodequery = paste0(eucomcode2query, eucomcode4query, eucomcode6query, eucomcode8query)
      eucomcodequery = substr(eucomcodequery, nchar(eucomcodequery)-7, nchar(eucomcodequery))
      
      if ("All" %in% input$eucountryselect){
        eucountryquery <- countrycode$countrycode
      } else {
        eucountryquery <- countrycode %>% filter(countryname %in% input$eucountryselect) %>% pull(countrycode)
      }
      
      # Obtain date range
      eudaterangequery <- rev(dates[match(input$eudateend,dates):match(input$eudatestart,dates)])
      # Update dateSlider with daterangequery
      updateSliderTextInput(session,"eudateSlider",
                           choices=eudaterangequery)
      # Transform to EU Query Format
      eudaterangequery <- paste0("0",
                                 substr(eudaterangequery,1,4),
                                 substr(eudaterangequery,6,7))
      
      # First line of query dependent on Import or Export
      if (input$euimpexpSelect == "Imports") {
        euselectquery <- "SELECT smk_cod_alpha, smk_comcode, smk_period_reference, sum(smk_no_of_consignments), sum(smk_stat_value), sum(smk_nett_mass) FROM arrivals "
        eugroupbyquery <- "GROUP BY smk_cod_alpha,smk_comcode,smk_period_reference"
      } else if (input$euimpexpSelect == "Exports") {
        euselectquery <- "SELECT smk_comcode, smk_cod_alpha, smk_period_reference, sum(smk_no_of_consignments), sum(smk_stat_value), sum(smk_nett_mass) FROM dispatches "
        eugroupbyquery <- "GROUP BY smk_comcode,smk_cod_alpha,smk_period_reference"
      } 
      
      eudataquery = paste0(euselectquery,
                         "WHERE (smk_comcode SIMILAR TO '(",
                         paste(eucomcodequery,collapse = "|"),
                         ")') AND (smk_cod_alpha SIMILAR TO '(",
                         paste(eucountryquery,collapse = "|"), 
                         ")') AND (smk_period_reference IN ('",
                         paste(eudaterangequery, collapse = "', '"),
                         "')) ",
                         eugroupbyquery)
     
      # Query data
      progress$set(detail = "Querying Data from Database")
      
      conn <- poolCheckout(tradedata)
      euDataRaw <- dbGetQuery(conn, eudataquery)
      poolReturn(conn)
      
      # Break out of observeEvent if query returns no values (ie, df == dim 0,0)
      if (dim(euDataRaw)[1] == 0) {
        # Set nullDataframe flag to TRUE to stop downstream reactivity
        nullDataframe$nullDataframe <- TRUE
        nullDataframe$comcodequery <- paste(gsub("_","",eucomcodequery),collapse = ",")
        isolate({
            showModal(modalDialog(title = "Alert!",
                                  paste0("No ",
                                         input$euimpexpSelect,
                                         " for Date Range ",
                                         input$eudatestart, " - ", input$eudateend,
                                         " and Commodity Code(s) ",
                                         nullDataframe$comcodequery,
                                         ".")), session)
        })
        req(FALSE)
      }
      
      if (input$euimpexpSelect == "Imports") {
        colnames(euDataRaw) = c("country","comcode","month", "consignments", "price", "weight")
      } else if (input$euimpexpSelect == "Exports") {
        colnames(euDataRaw) = c("comcode","country","month", "consignments", "price", "weight")
      }
     
      # Transform month back to readable format 
      euDataRaw$month <- paste0(substr(euDataRaw$month,2,5),
                                "-",
                                substr(euDataRaw$month,6,7))
      # Handle NAs
      euDataRaw$country[is.na(euDataRaw$country)] <- "Unknown Country" # blank country = <NA>

      # End Isolate
      })
    
    euQueryData$euDataRaw <- euDataRaw 

  })
  
  # ||||||||||||
  # CHAINS WITH
  # ||||||||||||
  
  observe({
    # Conditions for observe statement to run
    if (input$euqueryButton == 0) return()
    req(input$eudateSlider)
    if (nullDataframe$nullDataframe == TRUE) {
      # Break out of reactive chain
      req(FALSE)
    }
       
    # Dependencies - changes to Date Slider and Unit Selector
    input$eudateSlider
    input$euunitSelect
    
    # Prepare euData into appropriate format for rest of app
    # Based on date and unit, selected from fluidrow beneath comcode legend
    
    # Select correct month
    if (input$eudateSliderAll == TRUE) {
      euData <- euQueryData$euDataRaw %>% select(-month)
      if (input$euimpexpSelect == "Imports") {
        euData <- euData %>% group_by(country,comcode) %>% summarise(consignments = sum(consignments), price = sum(price), weight = sum(weight))
      } else if (input$euimpexpSelect == "Exports") {
        euData <- euData %>% group_by(comcode,country) %>% summarise(consignments = sum(consignments), price = sum(price), weight = sum(weight))
      }
    } else {
      euData <- euQueryData$euDataRaw %>% filter(month == input$eudateSlider) %>% select(-month)
    }
    
    # Select correct unit
    if (input$euunitSelect == "Value (GBP)"){
      euData <- euData %>% select(-c(weight,consignments))
      
    } else if (input$euunitSelect == "Weight (KG)"){
      euData <- euData %>% select(-c(price,consignments))
      
    } else if (input$euunitSelect == "Price Per Kilo (GBP/KG)"){
      euData$value <- euData$price / euData$weight
      euData <- euData %>% select(-c(price,weight,consignments))
      
    } else if (input$euunitSelect == "Number Of Consignments"){
      euData$value <- euData %>% select(-c(price,weight))
    }
    
    # At this point there should be two string and one numeric vector in the euData
    # dataframe. Now rename that numeric vector, which is the unit used, to value.
    
    colnames(euData)[colnames(euData) %in% c("price","weight","consignments")] <- "value"
    
    # Ungroup the data frame.
    euData <- ungroup(euData)
    
    # Check again if, after sorting, we're dealing with a blank df.
    if (dim(euData)[1] == 0) {
      nullDataframe$nullDataframe <- TRUE
      isolate({
        showModal(modalDialog(title = "Alert!",
                              paste0("No ",
                                     input$euimpexpSelect,
                                     " for ",
                                     input$eudateSlider,
                                     "."),
                              easyClose = TRUE,
                              footer = NULL), session)
      })
      # Break out of reactive chain
      req(FALSE)
    }
     
    # Clean and Shape Data --------------------------------------------------
    
    isolate({
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Generating Visualisations", value = 1)
      progress$set(detail = "Clean + Shape Data")
      
      
      # COMCODE LEGEND SPECIFIC -----------------------------------------------
      
      # Create df - list of commodity codes displayed. Match description to second col
      comcodelegend <- tibble(commoditycode = unique(euData$comcode))
      comcodelegend <- left_join(comcodelegend, comcodelookup, by = "commoditycode") %>% arrange(commoditycode)
      
      
      # SANKEY SPECIFIC -------------------------------------------------------
      
      # Create Links & Nodes dataframe.
      links <- euData
      colnames(links) <- c("source","target","value")
      nodes <- data.frame(unique(c(links$source,links$target)),stringsAsFactors = FALSE)
      colnames(nodes) = "name"
      
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
      euData_countries <- iso.expand(unique(euData$country[euData$country != "Unknown Country"]))
      
      # Special Case - add serbia if XS is used - iso.expand only considers RS as Serbia
      if ("XS" %in% unique(euData$country)) {euData_countries = c(euData_countries, "Serbia")}
      euData_countries <- tibble(euData_countries, iso.alpha(euData_countries))
      colnames(euData_countries) <- c("name","code")
      euData_countries[euData_countries$name == "Serbia","code"] = "XS"
      
      # Aggregate by country
      euData_countrytotal <- euData[,c("country","value")] %>% group_by(country) %>% summarise(value = sum(value))
      # Match plot-compatible names to iso codes
      euData_countrytotal <- left_join(euData_countrytotal,euData_countries, by=c("country" = "code"))
      # Join values to mapWorld for plotting
      euData_countrytotal <- tibble(country = euData_countrytotal$name,value = euData_countrytotal$value)
      mapWorld <- left_join(mapWorld,euData_countrytotal, by = c("region" = "country"))
      
      
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
      
      # TIME SERIES SPECIFIC --------------------------------------------------
      
      # > We have to have month information, which means portsum/countrysum aren't sufficient.
      # > We must use the queryData reactive portsumraw/countrysumraw and use dplyr on that.
      # > Unit selections are slightly different too - price per kilo must be summed by
      #   comcode, by port, and by country then divided for each month.
      
      # Select correct unit
      if (input$euunitSelect == "Value (GBP)"){
        byComcode <- euQueryData$euDataRaw %>% select(month,comcode,price)
        byCountry <- euQueryData$euDataRaw %>% select(month,country,price)
        
      } else if (input$euunitSelect == "Weight (KG)"){
        byComcode <- euQueryData$euDataRaw %>% select(month,comcode,weight)
        byCountry <- euQueryData$euDataRaw %>% select(month,country,weight)

      } else if (input$euunitSelect == "Number of Consignments") {
        byComcode <- euQueryData$euDataRaw %>% select(month,comcode,consignments)
        byCountry <- euQueryData$euDataRaw %>% select(month,country,consignments)
      }
      
      # Special case for Price Per Kilo
      if (input$euunitSelect == "Price Per Kilo (GBP/KG)"){
        byComcode <- euQueryData$euDataRaw %>%
                       select(month,comcode,price,weight) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight)) %>%
                       group_by(month,comcode) %>% summarise(value = sum(value))
        byCountry <- euQueryData$euDataRaw %>%
                       select(month,country,price,weight) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight)) %>%
                       group_by(month,country) %>% summarise(value = sum(value))
      } else { 
        # else statement required for non-PricePerKilo options
        colnames(byComcode)[colnames(byComcode) %in% c("price","weight","consignments")] <- "value"
        colnames(byCountry)[colnames(byCountry) %in% c("price","weight","consignments")] <- "value"
        
        if (input$eudateSliderAll != TRUE) {
          byComcode <- byComcode %>% filter(month == input$eudateSlider)
          byCountry <- byCountry %>% filter(month == input$eudateSlider)
        }
        
        # Obtain long format dataframe for time series plot
        byComcode <- byComcode %>% group_by(month,comcode) %>% summarise(value = sum(value))
        byCountry <- byCountry %>% group_by(month,country) %>% summarise(value = sum(value))
      }
      
      # Ungroup the data frames.
      byComcode <- ungroup(byComcode)
      byCountry <- ungroup(byCountry)
      
      # Replace country codes with full names.
      byCountry <- byCountry %>%
                     left_join(desclookup, by = c("country" = "keyName")) %>%
                     select(-country) %>%
                     rename(value = value.x, country = value.y)
      
    # End Isolate
    })
    
    
    # Now modify reactive variables with output from isolate() to trigger plot renders.
    euComcodeLegendData$comcodelegend <- comcodelegend
    euSankeyData$links <- links
    euSankeyData$nodes <- nodes
    euMapData$mapWorld <- mapWorld
    euMapData$dataPolygons <- dataPolygons
    euTimeseriesData$byComcode <- byComcode
    euTimeseriesData$byCountry <- byCountry
    
    
  })
  
  # Fill in the comcode legend ================================================
  
  output$euComcodeLegend = renderDataTable({
    if (input$euqueryButton == 0) return()
    req(!nullDataframe$nullDataframe)
    
    datatable(euComcodeLegendData$comcodelegend,
              rownames = FALSE,
              colnames = c("Commodity Code", "Description"),
              options = list(
                dom = "tp", # disable search bar at top
                pageLength = 5, # set number of elements on page
                columnDefs = list(list(width = "150px", targets = 0)))
    )}
  )
  
  # Fill in the plots =========================================================   
  
  # SANKEY --------------------------------------------------------------------
  
  output$eusankeyTrade <- renderSankeyNetwork({
  
  # Suppress output if nothing has been selected yet
    if (input$euqueryButton == 0) return()
    req(!nullDataframe$nullDataframe)
    
    sankeyNetwork(euSankeyData$links, euSankeyData$nodes,
                  "source", "target", "value", "name",
                  fontSize = 12, nodeWidth = 30)
  })
  
  
  # MAP -----------------------------------------------------------------------
  
  output$euworldMap <- renderLeaflet({
    if (input$euqueryButton == 0) return()
    req(!nullDataframe$nullDataframe)
    
    pal <- colorNumeric(palette = "inferno",
                        domain = 0:max(euMapData$dataPolygons$value),
                        reverse = TRUE)
    
    value_popup <- paste0("<strong>Country of Dispatch: </strong>", 
                          euMapData$dataPolygons$region, 
                          "<br><strong>Value: </strong>", 
                          euMapData$dataPolygons$value)
    
    leaflet(data = euMapData$dataPolygons) %>%
      setView(lng = 21.22574, lat = 48.2361, zoom = 4) %>%
      # addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(abs(euMapData$dataPolygons$value)),
                  smoothFactor = 0.5,
                  weight = 1,
                  color = "#000000",
                  fillOpacity = 0.7,
                  popup = value_popup) %>% 
      addLegend(pal = pal,
                values = 0:max(euMapData$dataPolygons$value), 
                opacity = 0.7, 
                title = "Colour Scale",
                position = "topright")
    
  })
  
  
  # TIME SERIES ---------------------------------------------------------------
  
  output$eutsByComcode <- renderPlotly({
      nbars <- length(euTimeseriesData$byComcode$comcode)
      ggplotly(
          ggplot(data = euTimeseriesData$byComcode) + 
          geom_col(aes(month,value,fill=comcode), show.legend = TRUE) +
          labs(x = paste(substr(input$euimpexpSelect,1,nchar(input$euimpexpSelect)-1),"Month"),
               y = paste(input$euunitSelect, " \n"),
               fill = "Commodity Codes") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })
  
  output$eutsByCountry <- renderPlotly({
      nbars <- length(euTimeseriesData$byCountry$country)
      ggplotly(
          ggplot(data = euTimeseriesData$byCountry) + 
          geom_col(aes(month,value,fill=country), show.legend = TRUE) +
          labs(x = paste(substr(input$euimpexpSelect,1,nchar(input$euimpexpSelect)-1),"Month"),
               y = paste(input$euunitSelect, " \n"),
               fill = "Countries") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })


  # DATA DOWNLOAD ------------------------------------------------------------
    output$euDataDownload <- downloadHandler(
        filename = function() {
        "TradeDataVisEUExtract.csv"
        },
        content = function(file) {
            eudownloadfile <- euQueryData$euDataRaw %>%
                left_join(comcodelookup, by=c("comcode" = "commoditycode")) %>%
                left_join(countrycode, by=c("country" = "countrycode"))
            eudownloadfile <- eudownloadfile %>% select(comcode,country,countryname,month,price,consignments,description)
            write.csv(eudownloadfile, file, row.names = FALSE)
        }
  )
    
# Close Server Function  
}



# SHINY CALL =================================================================

shinyApp(ui = ui, server = server)

# END SCRIPT #################################################################

# JUNKYARD ###################################################################

# Disconnect All Database Cons
# pg = dbDriver("PostgreSQL");lapply(dbListConnections(pg), dbDisconnect)
