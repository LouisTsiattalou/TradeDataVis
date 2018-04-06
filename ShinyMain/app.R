# 20171004
# Updated 2017xxxx
# R Shiny script for web based user interface with PGSQL database.
# Written by Louis Tsiattalou for TradeDataVis project.
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# SCRIPT START ###############################################################

# Load Packages --------------------------------------------------------------

if(require("shiny") == FALSE) {install.packages("shiny")}
library("shiny")

if(require("shinyjs") == FALSE) {install.packages("shinyjs")}
library("shinyjs")

if(require("shinyWidgets") == FALSE) {install.packages("shinyWidgets")}
library("shinyWidgets")

if(require("shinycssloaders") == FALSE) {install.packages("shinycssloaders")}
library("shinycssloaders")

if(require("tidyverse") == FALSE) {install.packages("tidyverse")}
library("tidyverse")

if(require("devtools") == FALSE) {install.packages("devtools")}
library("devtools")

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")

# The development version of ggplot2 is necessary for the plotly time series plots to render correctly
# install_github("tidyverse/ggplot2")
library("ggplot2")

# Same for pool - otherwise it's all over the place with my app
# install_github("rstudio/pool")
library("pool")

# Same for networkD3 - to get modal dialogs working
# install_github("christophergandrud/networkD3")
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

if(require("xlsx") == FALSE) {install.packages("xlsx")}
library("xlsx")

# Function Definitions -------------------------------------------------------

# Descendants - obtain all descendants of a vector of commodity codes.
# Tested on "01" and "02", takes 0.25 secs for 500 descendants. Quick!
descendants <- function(data, codes) {
    # Strip 8-digit codes (these are leaf nodes)
    codes <- codes[nchar(codes) < 8]
    if (length(codes) == 0) {
        return()
    } else {
        # get all the children's indices
        f <- data$parent %in% codes
        # get current children
        children <- data$commoditycode[f]
        return(c(children, descendants(data, children)))
    }
}

# comcodeshort - Gets comcode from comcode - description format
comcodeshort <- function(long) {
    short <- substr(long, 1, str_locate(long, " - ")[,1] - 1)
    return(short)
}

# comcodelong - Gets comcode - description from comcode
comcodelong <- function(short) {
    long <- paste(short, comcode$description[match(short, comcode$commoditycode)], sep = " - ")
    return(long)
}

# Load Prerequisite Static data - Ports, Comcodes, etc. ======================
# Use pool instead of dbConnect
# setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/ShinyMain/")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)

# pg <- dbDriver("PostgreSQL")
# tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2], host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])
tradedata <- dbPool(
    drv = RPostgreSQL::PostgreSQL(max.con=40),
    user = dbenv[1,2],
    password = dbenv[2,2],
    host = dbenv[3,2],
    port = dbenv[4,2],
    dbname = dbenv[5,2],
    minSize = 3,
    idleTimeout = 1200000  # 20 minutes 
)

onStop(function() {
    poolClose(tradedata)
})


# Load Metadata
# conn <- poolCheckout(tradedata)

portcode <- dbGetQuery(tradedata, "SELECT * FROM port")
comcode <- dbGetQuery(tradedata, "SELECT * FROM comcode")
countrycode <- dbGetQuery(tradedata, "SELECT * FROM country")

# poolReturn(conn)

# Order Ascending
portcode <- portcode %>% arrange(portname)
comcode <- comcode %>% arrange(commoditycode)
countrycode <- countrycode %>% arrange(countryname)

# Remove Duplicate Countrycodes
countrycode <- countrycode[!duplicated(countrycode$countrycode),]
eucountrycode <- countrycode %>% filter(eu == TRUE)
noneucountrycode <- countrycode %>% filter(non_eu == TRUE)

# Factor enables multiple search terms in comcode lookup tab
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
  dates <- c(dates, paste0(20, as.character(curyr), "-", as.character(sprintf("%02d", j)) ))
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

  # WELCOME PAGE --------------------------------------------------------------
  tabPanel("Welcome",
           tags$h1("Welcome to the Trade Data Visualisation Application!"),
           tags$hr(),
           HTML("<div class=\"alert alert-dismissible alert-danger\">
                    <strong>Alert:</strong> This application is currently in beta. You are <i>very likely</i> to encounter bugs and unexpected behaviour as you use the application. Please help us improve the application by clicking the <i>Feedback</i> tab at the top of the page. <br>
                Please only use in Chrome. Internet Explorer and Microsoft Edge are unsupported.
                Lastly, if the app keeps crashing, please close the application for 15 minutes and retry.
                </div>"),
           tags$hr(),
           tags$h3("Introduction"),
           tags$p("This application was born out of a requirement to analyse HMRC trade data in a much more robust way. The previous method involved downloading and unzipping numerous nested trade data archives from ", tags$a(href = "https://uktradeinfo.com", "UKTradeInfo"), ", importing it into Excel, performing endless VLOOKUPs to find what commodities, ports and countries you were actually looking at... It took a very long time to conduct even basic analysis."),
           tags$p("This application aims to fix this by providing three key visualisations: ",
                  tags$ul(
                           tags$li("Sankey Diagram"),
                           tags$li("World Choropleth Map"),
                           tags$li("Bar Charts by Comcode/Country/Port")
                       )
                  ),
           tags$p("These interactive visualisations provide a level of detail that is very difficult to get quickly using Excel or other end-user analysis tools. The Sankey Diagram is a network diagram describing the flow of absolute quantities (Price and Weight for non-EU, Price and Consignments for EU) between countries, commodities and ports; you can hover over a network relationship to obtain the value. The World Choropleth Map is a colour map of the world showing where our imports/exports are coming in from; click a country for more information. Lastly, the Bar Charts are interactive and show the proportion of the imports/exports contributed by each country, commodity and port. However, since port data is not available in the EU source data, ports are not shown in visualisations for EU imports/exports."),
           tags$hr(),
           tags$h3("How to Use the Application"),
           tags$p("To use the application, first obtain the commodity codes you wish to find in the ", tags$b("Comcode Lookup"), " tab. There are search functions that are designed to quickly find the codes you need. Then go to either the EU/Non-EU Trade tabs and fill in the options at the top of the page. ", tags$b("You can type in values, or select them from the dropdowns."), "Once you have done this, hit the ", tags$i("\"Run Query\""), " button. All the data will be fetched from the database, and visualisations generated."),
           tags$p("Once the visualisations show up, you will see:",
                  tags$ul(
                           tags$li("A mini Commodity Code Lookup table, showing", tags$i("only the commodity codes currently in play.")),
                           tags$li("A row containing a Date Slider and a Unit Selector. This will enable you to filter the query you've already made to show the appropriate unit and enable you to see the evolution of trade data activity in time by clicking and dragging the date slider."),
                           tags$li("A tab selection panel enabling you to switch between the interactive visualisations."),
                           tags$li("A Download Button, which downloads the results of the unfiltered query you just made by clicking \"Run Query\".")
                           ),
                  "You can also run another query by changing the selectors at the top and clicking Run Query again."),
           tags$p("I aim to continue developing this application to maximise its value to the organisation. ", tags$b("Please help in this effort by filling in the Feedback tab"), " after having used the app for a while. This will help me to see exactly where the app needs improvement, and will speed up the iteration process for the application greatly!"),
           tags$hr(),
           tags$h3("Known Bugs"),
                  tags$p(tags$u("Screen Grey-out"),
                  tags$br(),
                  "If you haven't run a successful query yet in your session and you make a query with no underlying data, it will display a message saying \"No Data Found\", which you can exit from and continue working. However, it will not show after a successful query has been made. Since it does not show, you can't exit from it, so the screen is essentially frozen from that point on.",
                  tags$br(),
                  tags$b("Solution: "), "Simply reload the App.",
                  tags$br(),
                  tags$br(),
                  tags$u("Small Visualisations/Data Tables not loading"),
                  tags$br(),
                  tags$b("Solution: "), "Use Chrome.",
                  tags$br(),
                  tags$br(),
                  tags$u("Sankey Diagram only shows lines, not nodes"),
                  tags$br(),
                  tags$b("Solution: "), "Too many countries/comcodes/ports. Narrow your query down."),
           tags$hr(),
           tags$h3("About"),
           tags$p("Github:", tags$a(href = "https://github.com/fsa-analytics/TradeDataVis", "FSA Analytics Github"),
                  tags$br(),
                  "Version Number:", "0.1.2",
                  tags$br(),
                  "Contact:", tags$a(href = "mailto:louis.tsiattalou@food.gov.uk", "Louis Tsiattalou")
                  ),
           tags$p("This application was developed by Louis Tsiattalou (Operational Research Fast Stream) at the Food Standards Agency; with assistance from Arthur Lugtigheid (Data Science) and Tim Johnston (Operational Research). Alpha-stage QA was performed by Harry Grantham-Hill (Operational Research).")
           ),
  
  # QUERY PARAMETERS-----------------------------------------------------
  
  tabPanel("Query Panel",
    # Query Options
    fluidRow(      
        
    # Define date selectors and four cascading inputs - don't allow "All" on 2-digit comcode
      column(3,
        selectizeInput("datestart", "Period Start:",
                       choices=dates),
        selectizeInput("dateend", "Period End:",
                       choices=dates),
        selectizeInput("countryselect", "Country:",
                       selected = "All",
                       choices=list(`All` = "All",
                                    `Non-EU` = noneucountrycode$countryname,
                                    `EU` = eucountrycode$countryname),
                       options = list(maxItems = 20)),
        selectizeInput("portselect", "Port:",
                       selected = "All",
                       choices=c("All",portcode$portname),
                       options = list(maxItems = 20))
        ),
      column(9,
        selectizeInput("comcode2", "2-digit Commodity Code:",
                       selected = "All",
                       width = "100%",
                       choices=c("All", comcodelong(comcode_2$commoditycode)),
                       options = list(maxItems = 10)),
        selectizeInput("comcode4", "4-digit Commodity Code:",
                       selected = "All",
                       width = "100%",
                       choices=c("All", comcodelong(comcode_4$commoditycode)),
                       options = list(maxItems = 10)),
        selectizeInput("comcode6", "6-digit Commodity Code:",
                       selected = "All",
                       width = "100%",
                       choices=c("All", comcodelong(comcode_6$commoditycode)),
                       options = list(maxItems = 10)),
        selectizeInput("comcode8", "8-digit Commodity Code:",
                       selected = "All",
                       width = "100%",
                       choices=c("All", comcodelong(comcode_8$commoditycode)),
                       options = list(maxItems = 10))
      ),
      column(2,
        radioButtons("impexpSelect", label = NULL, inline = TRUE,
                     choices = c("Imports","Exports")),
        actionButton("queryButton", "Run Query"),
        br(),
        br(),
        actionButton("queryClear", "Clear Selections")
      )
    ),
    hr(),
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
                     choices = c("Price (GBP)", "Weight (KG)", "Price Per Kilo (GBP/KG)")))
    ),
    
    # Create a spot for the plots
    fluidRow(
      column(12,
        tabsetPanel(
          tabPanel("FLOW",
            tabsetPanel(
              tabPanel("Full", sankeyNetworkOutput(outputId = "sankeyTrade") %>% withSpinner(type=6)), 
              tabPanel("Country & Port Only", sankeyNetworkOutput(outputId = "pcSankeyTrade") %>% withSpinner(type=6))
            )
          ),
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
      column(2, downloadButton("dataDownload", "Download Query Data"))
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
#                     choices = c("Price (GBP)", "Weight (KG)", "Price Per Kilo (GBP/KG)", "Number of Consignments")))
                     choices = c("Price (GBP)","Number of Consignments")))
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
      column(2, downloadButton("euDataDownload", "Download Query Data"))
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
  
  queryData <- reactiveValues(dataraw = NULL, portsumraw = NULL, countrysumraw = NULL, comcodesumraw = NULL)
  comcodeLegendData <- reactiveValues(comcodelegend = NULL)
  sankeyData <- reactiveValues(links = NULL, nodes = NULL, pclinks = NULL, pcnodes = NULL)
  mapData <- reactiveValues(mapWorld = NULL)
  timeseriesData <- reactiveValues(byComcode = NULL, byCountry = NULL, byPort = NULL)

  nullDataframe <- reactiveValues(nullDataframe = NULL, eunullDataframe = NULL, comcodequery = NULL)

  euQueryData <- reactiveValues(euDataRaw = NULL)
  euComcodeLegendData <- reactiveValues(comcodelegend = NULL)
  euSankeyData <- reactiveValues(links = NULL, nodes = NULL)
  euMapData <- reactiveValues(mapWorld = NULL)
  euTimeseriesData <- reactiveValues(byComcode = NULL, byCountry = NULL)

  # SERVER SIDE COMMODITY CODE LOOKUP -----------------------------------------
   output$ComcodeLookup = renderDataTable(comcodelookup,
                                          # filter = "top",
                                          rownames = FALSE,
                                          colnames = c("Commodity Code", "Description"),
                                          class = "cell-border stripe",
                                          options = list(
                                            #  dom = "t", # disable search bar at top
                                            pageLength = 25, # set number of elements on page
                                            language = list(search = "Search Comcodes and Descriptions:"), # Change Search Text.
                                            columnDefs = list(list(width = "150px", targets = 0))
                                            )
                                          )
  
  # SHINYJS ONCLICK STATEMENTS -----------------------------------------------
  shinyjs::onclick("countryselect", {updateSelectizeInput(session, "countryselect", selected = "")})
  shinyjs::onclick("portselect", {updateSelectizeInput(session, "portselect", selected = "")})
  shinyjs::onclick("comcode2", {updateSelectizeInput(session, "comcode2", selected = "")})
  shinyjs::onclick("comcode4", {updateSelectizeInput(session, "comcode4", selected = "")})
  shinyjs::onclick("comcode6", {updateSelectizeInput(session, "comcode6", selected = "")})
  shinyjs::onclick("comcode8", {updateSelectizeInput(session, "comcode8", selected = "")})

  # DATESLIDER OPTIONS ---------------------------------------------------------

  # Hide Dateslider options if the same date is picked.
  observe({
    input$datestart
    input$dateend

    if (input$datestart == input$dateend) {
      shinyjs::disable("dateSliderAll")
    } else {
      shinyjs::enable("dateSliderAll")
    }
  })
  
  observeEvent(input$dateSliderAll, {
    if (!input$dateSliderAll) {
      shinyjs::enable("dateSlider")
    } else {
      shinyjs::disable("dateSlider")
    }
  })
  
  # DROPDOWN CASCADING =========================================================
  observe({
    comcode_2_selection <- comcodeshort(input$comcode2)
    allDescendants <- descendants(comcode, comcode_2_selection)

    # Update Comcodes
    if (is.null(comcode_2_selection) == FALSE) {
      if ("All" %in% comcode_2_selection){
        updateSelectizeInput(session,"comcode4", "4-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(comcode_4$commoditycode)),
                          options = list(maxItems = 10))
        updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(comcode_6$commoditycode)),
                          options = list(maxItems = 10))
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(comcode_8$commoditycode)),
                          options = list(maxItems = 10))
      } else {
        updateSelectizeInput(session,"comcode4", "4-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 4]))),
                          options = list(maxItems = 10))
        updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 6]))),
                          options = list(maxItems = 10))
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 8]))),
                          options = list(maxItems = 10))
      }
    }
  })

  observe({
    comcode_4_selection <- comcodeshort(input$comcode4)
    allDescendants <- descendants(comcode, comcode_4_selection)
    
    # Update Comcodes
    if (is.null(comcode_4_selection) == FALSE) {
      if ("All" %in% comcode_4_selection){
        updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(comcode_6$commoditycode)),
                          options = list(maxItems = 10))
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(comcode_8$commoditycode)),
                          options = list(maxItems = 10))
      } else {
        updateSelectizeInput(session,"comcode6", "6-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 6]))),
                          options = list(maxItems = 10))
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 8]))),
                          options = list(maxItems = 10))
      }
    }
  })

  observe({
    comcode_6_selection <- comcodeshort(input$comcode6)
    allDescendants <- descendants(comcode, comcode_6_selection) 
    
    # Update Comcodes
    if (is.null(comcode_6_selection) == FALSE) {
      if ("All" %in% comcode_6_selection){
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(comcode_8$commoditycode)),
                          options = list(maxItems = 10))
      } else {
        updateSelectizeInput(session,"comcode8", "8-digit Commodity Code:",
                          selected = "All",
                          choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 8]))),
                          options = list(maxItems = 10))
      }
    }
  })

  # QUERY EXECUTION ============================================================
  
  observeEvent(input$queryButton,{
    input$queryButton
    
    # Use selectors information to build plot
    isolate({

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Processing Selections", value = 0.25)

      # Set nullDataframe flag to FALSE
      nullDataframe$nullDataframe <- FALSE
      
      # Control handling for comcode selectors
      if ("All" %in% input$comcode2 | is.null(input$comcode2)) {comcode2query = "__"} else {comcode2query = comcodeshort(input$comcode2)}
      if ("All" %in% input$comcode4 | is.null(input$comcode4)) {comcode4query = "__"} else {comcode4query = comcodeshort(input$comcode4)}
      if ("All" %in% input$comcode6 | is.null(input$comcode6)) {comcode6query = "__"} else {comcode6query = comcodeshort(input$comcode6)}
      if ("All" %in% input$comcode8 | is.null(input$comcode8)) {comcode8query = "__"} else {comcode8query = comcodeshort(input$comcode8)}
      
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
          

      # NON-EU SPECIFIC --------------------------------------------------------
      
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
                         "')) ",
                         groupbyquery) # import = coo_alpha, export = cod_alpha!
                         
      progress$set(message = "Querying Non-EU Data", value = 0.5)
      
      # Run Query
      dataraw <- dbGetQuery(tradedata, dataquery)

      # Break out of observeEvent if query returns no values (ie, df == dim 0,0)
      if (dim(dataraw)[1] == 0) {
        # Set nullDataframe flag to TRUE to stop downstream reactivity
        nullDataframe$nullDataframe <- TRUE
        nullDataframe$comcodequery <- paste(gsub("_","",comcodequery),collapse = ",")
        isolate({
            showModal(modalDialog(title = "Alert!",
                                  paste0("No Non-EU ",
                                         input$impexpSelect,
                                         " for Date Range ",
                                         input$datestart, " - ", input$dateend,
                                         " and Commodity Code(s) ",
                                         nullDataframe$comcodequery,
                                         ".")), session)
        })
      } else {
          
        # Transform month back to readable format 
        dataraw$account_date <- paste0(substr(dataraw$account_date,4,7),
                                       "-",
                                       substr(dataraw$account_date,1,2))

        # Set up correct colnames and create portsum/countrysum dataframes from dataraw
        if (input$impexpSelect == "Imports") {
            colnames(dataraw) = c("country","comcode","port","month","price", "weight")
            dataraw$country[is.na(dataraw$country)] <- "Unknown Country" # blank country = <NA>
            dataraw$port[dataraw$port == ""] <- "Unknown Port" # blank port = ""

            portsumraw <- dataraw %>%
                select(country,comcode,month,price,weight) %>%
                group_by(country,comcode,month) %>%
                summarise(price = sum(price), weight = sum(weight))
            countrysumraw <- dataraw %>%
                select(comcode,port,month,price,weight) %>%
                group_by(comcode,port,month) %>%
                summarise(price = sum(price), weight = sum(weight))
            comcodesumraw <- dataraw %>%
                select(country,port,month,price,weight) %>%
                group_by(country,port,month) %>%
                summarise(price = sum(price), weight = sum(weight))

        } else if (input$impexpSelect == "Exports") {
            colnames(dataraw) = c("port","comcode","country","month","price", "weight")
            dataraw$country[is.na(dataraw$country)] <- "Unknown Country" # blank country = <NA>
            dataraw$port[dataraw$port == ""] <- "Unknown Port" # blank port = ""

            portsumraw <- dataraw %>%
                select(comcode,country,month,price,weight) %>%
                group_by(comcode,country,month) %>%
                summarise(price = sum(price), weight = sum(weight))
            countrysumraw <- dataraw %>%
                select(port,comcode,month,price,weight) %>%
                group_by(port,comcode,month) %>%
                summarise(price = sum(price), weight = sum(weight))
            comcodesumraw <- dataraw %>%
                select(port,country,month,price,weight) %>%
                group_by(port,country,month) %>%
                summarise(price = sum(price), weight = sum(weight))
        }

        portsumraw <- ungroup(portsumraw)
        countrysumraw <- ungroup(countrysumraw)
        comcodesumraw <- ungroup(comcodesumraw)

      }



      # EU SPECIFIC ------------------------------------------------------------
      
      # Set nullDataframe flag to FALSE
      nullDataframe$eunullDataframe <- FALSE
      
      # Obtain date range
      eudaterangequery <- rev(dates[match(input$dateend,dates):match(input$datestart,dates)])
      # Update dateSlider with daterangequery
      updateSliderTextInput(session,"eudateSlider",
                           choices=eudaterangequery)
      # Transform to EU Query Format
      eudaterangequery <- paste0("0",
                                 substr(eudaterangequery,1,4),
                                 substr(eudaterangequery,6,7))
      
      # First line of query dependent on Import or Export
      if (input$impexpSelect == "Imports") {
        euselectquery <- "SELECT smk_cod_alpha, smk_comcode, smk_period_reference, sum(smk_no_of_consignments), sum(smk_stat_value), sum(smk_nett_mass) FROM arrivals "
        eugroupbyquery <- "GROUP BY smk_cod_alpha,smk_comcode,smk_period_reference"
      } else if (input$impexpSelect == "Exports") {
        euselectquery <- "SELECT smk_comcode, smk_cod_alpha, smk_period_reference, sum(smk_no_of_consignments), sum(smk_stat_value), sum(smk_nett_mass) FROM dispatches "
        eugroupbyquery <- "GROUP BY smk_comcode,smk_cod_alpha,smk_period_reference"
      } 
      
      eudataquery = paste0(euselectquery,
                         "WHERE (smk_comcode SIMILAR TO '(",
                         paste(comcodequery,collapse = "|"),
                         ")') AND (smk_cod_alpha SIMILAR TO '(",
                         paste(countryquery,collapse = "|"), 
                         ")') AND (smk_period_reference IN ('",
                         paste(eudaterangequery, collapse = "', '"),
                         "')) ",
                         eugroupbyquery)

      progress$set(message = "Querying EU Data", value = 0.75)

      # Query Data
      euDataRaw <- dbGetQuery(tradedata, eudataquery)

      # Break out of observeEvent if query returns no values (ie, df == dim 0,0)
      if (dim(euDataRaw)[1] == 0) {
        # Set nullDataframe flag to TRUE to stop downstream reactivity
        nullDataframe$eunullDataframe <- TRUE
        nullDataframe$comcodequery <- paste(gsub("_","",comcodequery),collapse = ",")
        isolate({
            showModal(modalDialog(title = "Alert!",
                                  paste0("No EU ",
                                         input$impexpSelect,
                                         " for Date Range ",
                                         input$datestart, " - ", input$dateend,
                                         " and Commodity Code(s) ",
                                         nullDataframe$comcodequery,
                                          ".")), session)
        })
      } else {

          if (input$impexpSelect == "Imports") {
              colnames(euDataRaw) = c("country","comcode","month", "consignments", "price", "weight")
          } else if (input$impexpSelect == "Exports") {
              colnames(euDataRaw) = c("comcode","country","month", "consignments", "price", "weight")
          }
          
          # Transform month back to readable format 
          euDataRaw$month <- paste0(substr(euDataRaw$month,2,5),
                                    "-",
                                    substr(euDataRaw$month,6,7))
          # Handle NAs
          euDataRaw$country[is.na(euDataRaw$country)] <- "Unknown Country" # blank country = <NA>
      }

      # End Isolate
      })
    
    queryData$dataraw <- dataraw
    queryData$portsumraw <- portsumraw
    queryData$countrysumraw <- countrysumraw
    queryData$comcodesumraw <- comcodesumraw
    euQueryData$euDataRaw <- euDataRaw 

  })
      

  # NON-EU DATA SHAPING AND PLOTTING ===========================================

  observe({
    # FILTER DATE/UNIT IN DATA -------------------------------------------------
      
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
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Clean and Shape Data", value = 1)

    # Prepare portsum, countrysum and comcodesum into appropriate format for rest of app
    # Based on date and unit, selected from fluidrow beneath comcode legend
    
    # Select correct month
    if (input$dateSliderAll == TRUE) {
      portsum <- queryData$portsumraw %>% select(-month)
      countrysum <- queryData$countrysumraw %>% select(-month)
      comcodesum <- queryData$comcodesumraw %>% select(-month)
      if (input$impexpSelect == "Imports") {
        portsum <- portsum %>% group_by(country,comcode) %>% summarise(price = sum(price), weight = sum(weight))
        countrysum <- countrysum %>% group_by(comcode,port) %>% summarise(price = sum(price), weight = sum(weight))
        comcodesum <- comcodesum %>% group_by(country,port) %>% summarise(price = sum(price), weight = sum(weight))
      } else if (input$impexpSelect == "Exports") {
        portsum <- portsum %>% group_by(comcode,country) %>% summarise(price = sum(price), weight = sum(weight))
        countrysum <- countrysum %>% group_by(port,comcode) %>% summarise(price = sum(price), weight = sum(weight))
        comcodesum <- comcodesum %>% group_by(port,country) %>% summarise(price = sum(price), weight = sum(weight))
      }
    } else {
      portsum <- queryData$portsumraw %>% filter(month == input$dateSlider) %>% select(-month)
      countrysum <- queryData$countrysumraw %>% filter(month == input$dateSlider) %>% select(-month)
      comcodesum <- queryData$comcodesumraw %>% filter(month == input$dateSlider) %>% select(-month)
    }
    
    # Select correct unit
    if (input$unitSelect == "Price (GBP)"){
      portsum <- portsum %>% select(-weight)
      countrysum <- countrysum %>% select(-weight)
      comcodesum <- comcodesum %>% select(-weight)
      
    } else if (input$unitSelect == "Weight (KG)"){
      portsum <- portsum %>% select(-price)
      countrysum <- countrysum %>% select(-price)
      comcodesum <- comcodesum %>% select(-price)
      
    } else if (input$unitSelect == "Price Per Kilo (GBP/KG)"){
      # For map:
      portcomcodesum <- portsum %>%
                            select(country, price, weight) %>%
                            group_by(country) %>%
                            summarise(price = sum(price), weight = sum(weight)) %>%
                            mutate(value = price/weight) %>%
                            select(country,value)
      # For sankey:
      portsum$value <- portsum$price / portsum$weight
      portsum <- portsum %>% select(-c(price,weight))
      countrysum$value <- countrysum$price / countrysum$weight
      countrysum <- countrysum %>% select(-c(price,weight))
      comcodesum$value <- comcodesum$price / comcodesum$weight
      comcodesum <- comcodesum %>% select(-c(price,weight))
    }
    
    # At this point there should be two string and one numeric vector in all sum 
    # dataframes. Now rename that numeric vector, which is the unit used, to value.
    
    colnames(portsum)[colnames(portsum) %in% c("price","weight")] <- "value"
    colnames(countrysum)[colnames(countrysum) %in% c("price","weight")] <- "value"
    colnames(comcodesum)[colnames(comcodesum) %in% c("price","weight")] <- "value"
    
    # Ungroup the data frames.
    portsum <- ungroup(portsum)
    countrysum <- ungroup(countrysum)
    comcodesum <- ungroup(comcodesum)
    
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
    
     
    # CLEAN AND SHAPE DATA --------------------------------------------------
    
    isolate({
      
      # COMCODE LEGEND SPECIFIC -----------------------------------------------
      
      # Create df - list of commodity codes displayed. Match description to second col
      comcodelegend <- tibble(commoditycode = unique(portsum$comcode))
      comcodelegend <- left_join(comcodelegend, comcodelookup, by = "commoditycode") %>% arrange(commoditycode)
      
      
      # FULL SANKEY SPECIFIC --------------------------------------------------
      
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

      # COUNTRY -> PORT SANKEY SPECIFIC ---------------------------------------

      pclinks <- comcodesum
      colnames(pclinks) <- c("source","target","value")
      pcnodes <- data.frame(unique(c(pclinks$source,pclinks$target)),stringsAsFactors = FALSE)
      colnames(pcnodes) = "name"
      
      # Replace pclinks source, target columns with IDs specified in pcnodes.
      # Match to row number in pcnodes (which is uniquely indexed!)
      # Note - must be zero indexed, hence match - 1
      pclinks$source = vapply(pclinks$source, function(x){
        x = match(x,pcnodes[,1])-1
      }, double(1))
      
      pclinks$target = vapply(pclinks$target, function(x){
        x = match(x,pcnodes[,1])-1
      }, double(1))
      
      # Replace node codes for country and port with full name
      pcnodes$name = vapply(pcnodes$name,function(x){
        replacement = desclookup[match(x,desclookup$keyName),"value"]
        if (is.na(replacement) == FALSE){
          x = replacement
          if(nchar(x) > 30){x = substr(x,1,30)}}
        else {x}
        return(x)
      }, character(1))

      # WORLDMAP SPECIFIC -----------------------------------------------------
      mapWorld <- map_data("world")
       
      # Convert mapWorld region to iso codes
      mapWorld$region <- iso.alpha(mapWorld$region)

      # Special cases
      ## Nigeria: maps package doesn't distinguish Niger and Nigeria...
      mapWorld$region[map_data("world")$region == "Nigeria"] <- "NG"
      ## replace RS (Serbia mapWorld) with XS (Serbia countrycode)
      mapWorld$region <- mapWorld$region %>% str_replace("RS","XS")

      # If an absolute quantity is selected, use portsum, strip commodities and aggregate.
      # If Price Per Kilo is selected, use portcomcodesum, which is already in our preferred format.
      if (input$unitSelect == "Price Per Kilo (GBP/KG)") {
          portsum_countrytotal <- portcomcodesum
      } else {
          portsum_countrytotal <- portsum[,c("country","value")] %>% group_by(country) %>% summarise(value = sum(value))
      }
      
      # Join values to mapWorld for plotting
      mapWorld <- left_join(mapWorld, portsum_countrytotal, by = c("region" = "country"))
     
      mapWorld <- left_join(mapWorld, countrycode, by = c("region" = "countrycode")) 
      mapWorld <- mapWorld %>% select(-region)
      mapWorld <- mapWorld %>% rename(region = "countryname")
      
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
      if (input$unitSelect == "Price (GBP)"){
        byComcode <- queryData$dataraw %>% select(month,comcode,price)
        byCountry <- queryData$dataraw %>% select(month,country,price)
        byPort <- queryData$dataraw %>% select(month,port,price)
        
      } else if (input$unitSelect == "Weight (KG)"){
        byComcode <- queryData$dataraw %>% select(month,comcode,weight)
        byCountry <- queryData$dataraw %>% select(month,country,weight)
        byPort <- queryData$dataraw %>% select(month,port,weight)
        
      }
      
      # Special case for Price Per Kilo
      if (input$unitSelect == "Price Per Kilo (GBP/KG)"){
        if (input$dateSliderAll != TRUE) {
          byComcode <- queryData$dataraw %>% filter(month == input$dateSlider)
          byCountry <- queryData$dataraw %>% filter(month == input$dateSlider)
          byPort <- queryData$dataraw %>% filter(month == input$dateSlider)
        } else {
          byComcode <- queryData$dataraw
          byCountry <- queryData$dataraw
          byPort <- queryData$dataraw
        }
        
        byComcode <- byComcode %>%
                       select(month,comcode,price,weight) %>%
                       group_by(month,comcode) %>%
                       summarise(price = sum(price), weight = sum(weight)) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight))
        byCountry <- byCountry %>%
                       select(month,country,price,weight) %>%
                       group_by(month,country) %>%
                       summarise(price = sum(price), weight = sum(weight)) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight))
        byPort <- byPort %>%
                       select(month,port,price,weight) %>%
                       group_by(month,port) %>%
                       summarise(price = sum(price), weight = sum(weight)) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight))
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
    
    # TRIGGER PLOT RENDERING ---------------------------------------------------
    # This is done right at the end of the observe() function, before all our
    # lovely datasets disappear into the aether...!
    comcodeLegendData$comcodelegend <- comcodelegend
    sankeyData$links <- links
    sankeyData$nodes <- nodes
    sankeyData$pclinks <- pclinks
    sankeyData$pcnodes <- pcnodes
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
  
  # COUNTRY -> PORT SANKEY -----------------------------------------------------
  
  output$pcSankeyTrade <- renderSankeyNetwork({
  
  # Suppress output if nothing has been selected yet
    if (input$queryButton == 0) return()
    req(!nullDataframe$nullDataframe)
    
    sankeyNetwork(sankeyData$pclinks, sankeyData$pcnodes,
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

    # Format legend figures
    if (input$unitSelect == "Price (GBP)"){
      legendModifier <- labelFormat(prefix = "£")
      legendTitle <- "Price"
    } else if (input$unitSelect == "Weight (KG)"){
      legendModifier <- labelFormat(suffix = " kg")
      legendTitle <- "Weight"
    } else if (input$unitSelect == "Price Per Kilo (GBP/KG)"){
      legendModifier <- labelFormat(prefix = "£", suffix = "/kg")
      legendTitle <- "Price/Weight"
    }
    
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
                title = legendTitle,
                labFormat = legendModifier,
                position = "topright")
    
  })
  
  
  # TIME SERIES ---------------------------------------------------------------
  
  output$tsByComcode <- renderPlotly({
      if (length(unique(timeseriesData$byComcode$month)) < 7) {colposition = "dodge"} else {colposition = "stack"}
      nbars <- length(timeseriesData$byComcode$comcode)
      ggplotly(
          ggplot(data = timeseriesData$byComcode) + 
          geom_col(aes(month,value,fill=comcode), show.legend = TRUE, position = colposition) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$unitSelect, " \n"),
               fill = "Commodity Codes") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })
  
  output$tsByCountry <- renderPlotly({
      if (length(unique(timeseriesData$byCountry$month)) < 7) {colposition = "dodge"} else {colposition = "stack"}
      nbars <- length(timeseriesData$byCountry$country)
      ggplotly(
          ggplot(data = timeseriesData$byCountry) + 
          geom_col(aes(month,value,fill=country), show.legend = TRUE, position = colposition) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$unitSelect, " \n"),
               fill = "Countries") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })
  
  output$tsByPort <- renderPlotly({
      if (length(unique(timeseriesData$byPort$month)) < 7) {colposition = "dodge"} else {colposition = "stack"}
      nbars <- length(timeseriesData$byPort$port)
      ggplotly(
          ggplot(data = timeseriesData$byPort) + 
          geom_col(aes(month,value,fill=port), show.legend = TRUE, position = colposition) +
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
            # "TradeDataVisNonEUExtract.csv"
            "TradeDataVisNonEUExtract.xlsx"
        },
        content = function(file) {
            downloadfile <- queryData$dataraw %>%
                left_join(comcodelookup, by=c("comcode" = "commoditycode")) %>%
                left_join(portcode, by=c("port" = "portcode")) %>%
                left_join(countrycode, by=c("country" = "countrycode"))
            downloadfile <- downloadfile %>% select("Commodity Code" = comcode, "Port Code" = port, "Port Name" = portname, "Port Type" = type, "Country Code" = country, "Country Name" = countryname, "Month" = month, "Value (£)" = price, "Weight (kg)" = weight, "Commodity Description" = description)
            #write.csv(downloadfile, file, row.names = FALSE)
            write.xlsx2(downloadfile, file, sheetName = "Non-EU Trade Data", row.names = FALSE)
        }
    )



    
  # EU DATA SHAPING AND PLOTTING ===============================================
  
  observe({
    # Conditions for observe statement to run
    if (input$queryButton == 0) return()
    req(input$eudateSlider)
    if (nullDataframe$eunullDataframe == TRUE) {
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
      if (input$impexpSelect == "Imports") {
        euData <- euData %>% group_by(country,comcode) %>% summarise(consignments = sum(consignments), price = sum(price), weight = sum(weight))
      } else if (input$impexpSelect == "Exports") {
        euData <- euData %>% group_by(comcode,country) %>% summarise(consignments = sum(consignments), price = sum(price), weight = sum(weight))
      }
    } else {
      euData <- euQueryData$euDataRaw %>% filter(month == input$eudateSlider) %>% select(-month)
    }
    
    # Select correct unit
    if (input$euunitSelect == "Price (GBP)"){
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
      nullDataframe$eunullDataframe <- TRUE
      isolate({
        showModal(modalDialog(title = "Alert!",
                              paste0("No ",
                                     input$impexpSelect,
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
      
      # Convert mapWorld region to iso codes
      mapWorld$region <- iso.alpha(mapWorld$region)
      
      # Aggregate by country
      euData_countrytotal <- euData[,c("country","value")] %>% group_by(country) %>% summarise(value = sum(value))
      
      # Join values to mapWorld for plotting
      mapWorld <- left_join(mapWorld,euData_countrytotal, by = c("region" = "country"))
      
      mapWorld <- left_join(mapWorld, countrycode, by = c("region" = "countrycode")) 
      mapWorld <- mapWorld %>% select(-region)
      mapWorld <- mapWorld %>% rename(region = "countryname")
      
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
      if (input$euunitSelect == "Price (GBP)"){
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
    if (input$queryButton == 0) return()
    req(!nullDataframe$eunullDataframe)
    
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
    if (input$queryButton == 0) return()
    req(!nullDataframe$eunullDataframe)
    
    sankeyNetwork(euSankeyData$links, euSankeyData$nodes,
                  "source", "target", "value", "name",
                  fontSize = 12, nodeWidth = 30)
  })
  
  
  # MAP -----------------------------------------------------------------------
  
  output$euworldMap <- renderLeaflet({
    if (input$queryButton == 0) return()
    req(!nullDataframe$eunullDataframe)
    
    pal <- colorNumeric(palette = "inferno",
                        domain = 0:max(euMapData$dataPolygons$value),
                        reverse = TRUE)
    
    value_popup <- paste0("<strong>Country of Dispatch: </strong>", 
                          euMapData$dataPolygons$region, 
                          "<br><strong>Value: </strong>", 
                          euMapData$dataPolygons$value)
    
    # Format legend
    if (input$euunitSelect == "Price (GBP)"){
      eulegendModifier <- labelFormat(prefix = "£")
      eulegendTitle <- "Price"
    } else if (input$euunitSelect == "Weight (KG)"){
      eulegendModifier <- labelFormat(suffix = " kg")
      eulegendTitle <- "Weight"
    } else if (input$euunitSelect == "Price Per Kilo (GBP/KG)"){
      eulegendModifier <- labelFormat(prefix = "£", suffix = "/kg")
      eulegendTitle <- "Price/Weight"
    } else if (input$euunitSelect == "Number of Consignments"){
      eulegendModifier <- labelFormat(suffix = " Con.")
      eulegendTitle <- "# Consignments"
    }

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
                title = eulegendTitle,
                labFormat = eulegendModifier,
                position = "topright")
    
  })
  
  
  # TIME SERIES ---------------------------------------------------------------
  
  output$eutsByComcode <- renderPlotly({
      nbars <- length(euTimeseriesData$byComcode$comcode)
      if (length(unique(euTimeseriesData$byComcode$month)) < 7) {colposition = "dodge"} else {colposition = "stack"}
      ggplotly(
          ggplot(data = euTimeseriesData$byComcode) + 
          geom_col(aes(month,value,fill=comcode), show.legend = TRUE, position = colposition) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$euunitSelect, " \n"),
               fill = "Commodity Codes") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })
  
  output$eutsByCountry <- renderPlotly({
      if (length(unique(euTimeseriesData$byCountry$month)) < 7) {colposition = "dodge"} else {colposition = "stack"}
      nbars <- length(euTimeseriesData$byCountry$country)
      ggplotly(
          ggplot(data = euTimeseriesData$byCountry) + 
          geom_col(aes(month,value,fill=country), show.legend = TRUE, position = colposition) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$euunitSelect, " \n"),
               fill = "Countries") + 
          scale_y_continuous(labels = comma) + 
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })


  # DATA DOWNLOAD ------------------------------------------------------------
    output$euDataDownload <- downloadHandler(
        filename = function() {
            #"TradeDataVisEUExtract.csv"
            "TradeDataVisEUExtract.xlsx"
        },
        content = function(file) {
            eudownloadfile <- euQueryData$euDataRaw %>%
                left_join(comcodelookup, by=c("comcode" = "commoditycode")) %>%
                left_join(countrycode, by=c("country" = "countrycode"))
            eudownloadfile <- eudownloadfile %>% select("Commodity Code" = comcode, "Country Code" = country, "Country Name" = countryname, "Month" = month, "Value (£)" = price, "Number of Consignments" = consignments, "Commodity Description" = description)
            #write.csv(eudownloadfile, file, row.names = FALSE)
            write.xlsx2(eudownloadfile, file, sheetName = "EU Trade Data", row.names = FALSE)
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
