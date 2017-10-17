# 20171017
# Updated 2017xxxx
# Query template for extracting Trade Data from Postgres.
# Written by Louis Tsiattalou for TradeDataVis project.
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# TODO

# SCRIPT START ###############################################################

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")
if(require("XLConnect") == FALSE) {install.packages("XLConnect")}
library("XLConnect")

setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/")
suppressWarnings(dir.create(paste(getwd(), "/dbextract", sep = "")))
setwd("dbextract")

# DEFINE CONNECTION ----------------------------------------------------------

pg = dbDriver("PostgreSQL")
tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

# DEFINE QUERY AND EXECUTE ---------------------------------------------------

# sqlquery <- "SELECT exports.comcode,
#                     comcode.description,
#                     exports.cod_alpha,
#                     exports.account_date,
#                     exports.port_alpha,
#                     exports.value,
#                     exports.quantity_1
#               FROM exports
#               LEFT JOIN comcode ON exports.comcode = comcode.commoditycode
#               WHERE account_date LIKE '%2016%' AND comcode LIKE '03______';"
sqlquery <- "SELECT exports.comcode,
                    comcode.description,
                    exports.cod_alpha,
                    exports.account_date,
                    exports.port_alpha,
                    exports.value,
                    exports.quantity_1
              FROM exports
              LEFT JOIN comcode ON exports.comcode = comcode.commoditycode
              WHERE account_date LIKE '%2016%' AND comcode LIKE '0301____';"


data <- dbGetQuery(tradedata,sqlquery)

# Write to CSV
# write.csv(data,"querydata.csv")

# Write to Workbook

wkb <- loadWorkbook("extract.xlsx", create = TRUE)
createSheet(wkb, "PostgreSQL Extract")
writeWorksheet(wkb, data, sheet = "PostgreSQL Extract")
saveWorkbook(wkb)
