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

ui <- "x"



# SERVER =====================================================================

server <- "x"




# SHINY CALL =================================================================

shinyApp(ui = ui, server = server)


# END SCRIPT #################################################################

# JUNKYARD ###################################################################

# Annual archives ------------------------------------------------------------
# Special Annual Cases -------------------------------------------------------
# Unzip monthly files --------------------------------------------------------
# Cleanup ====================================================================
# Print info =================================================================