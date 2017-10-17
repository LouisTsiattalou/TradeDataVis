# 20171004
# Updated 2017xxxx
# R Shiny script for web based user interface with PGSQL database.
# Written by Louis Tsiattalou for TradeDataVis project.
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# TODO

# SCRIPT START ###############################################################

# Load Packages

if(require("shiny") == FALSE) {install.packages("shiny")}
library("shiny")

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")


# UI =========================================================================

# Use a fluid Bootstrap layout
ui <- fluidPage(    
    
  # Give the page a title
  titlePanel("UK Non-EU Exports flow diagram"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Region:", 
                  choices=colnames(WorldPhones)),
      hr(),
      helpText("Data obtained from HMRC's Trade Data - "),
      tags$a(href="www.uktradeinfo.com", "Source")
    ),
    
    # Create a spot for the sankey diagram
    mainPanel(
      plotOutput("phonePlot")  
    )
    
  )
)



# SERVER =====================================================================

server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    barplot(WorldPhones[,input$region]*1000, 
            main=input$region,
            ylab="Number of Telephones",
            xlab="Year")
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