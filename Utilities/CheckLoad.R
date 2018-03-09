# 20180306
# Script to check a detabase has been loaded with all monthly HMRC data. Then plot the result. 

# Written by Louis Tsiattalou for TradeDataVis application
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# SCRIPT START ################################################################

# Library Import --------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(ggplot2)
library(RPostgreSQL)
setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/")

# Connect to DB ---------------------------------------------------------------
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env.example", delim = "=", col_names = FALSE, trim_ws = TRUE)
tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2],
                       host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])

# Import Data into R ----------------------------------------------------------

# Imports
sqlquery <- "SELECT DISTINCT account_date, COUNT(account_date) AS records FROM imports GROUP BY account_date"
imports <- dbGetQuery(tradedata, sqlquery)
imports$account_date <- paste0(substr(imports$account_date, 4, 7),substr(imports$account_date, 1, 2))
imports <- imports %>%
  rename(period = account_date) %>% 
  arrange(period)

# Exports
sqlquery <- "SELECT DISTINCT account_date, COUNT(account_date) AS records FROM exports GROUP BY account_date"
exports <- dbGetQuery(tradedata, sqlquery)
exports$account_date <- paste0(substr(exports$account_date, 4, 7), substr(exports$account_date, 1, 2))
exports <- exports %>%
  rename(period = account_date) %>% 
  arrange(period)

# Arrivals
sqlquery <- "SELECT DISTINCT smk_period_reference, COUNT(smk_period_reference) AS records FROM arrivals GROUP BY smk_period_reference"
arrivals <- dbGetQuery(tradedata, sqlquery)
arrivals_useful <- arrivals %>%
  filter(substr(smk_period_reference, 1, 3) == "020") %>%
  rename(period = smk_period_reference)
arrivals_useful$period <- arrivals_useful$period %>% substr(2,7)

# Dispatches
sqlquery <- "SELECT DISTINCT smk_period_reference, COUNT(smk_period_reference) AS records FROM dispatches GROUP BY smk_period_reference"
dispatches <- dbGetQuery(tradedata, sqlquery)
dispatches_useful <- dispatches %>%
  filter(substr(smk_period_reference, 1, 3) == "020") %>%
  rename(period = smk_period_reference)
dispatches_useful$period <- dispatches_useful$period %>% substr(2,7)

# Join Data --------------------------------------------------------------------
ukTradeRecords <- imports
ukTradeRecords <- ukTradeRecords %>% 
  left_join(exports, by = "period", suffix = c("Import", "Export")) %>%
  left_join(arrivals_useful, by = "period") %>%
  left_join(dispatches_useful, by = "period", suffix = c("Arrival", "Dispatch"))
colnames(ukTradeRecords) <- stringr::str_replace(colnames(ukTradeRecords), "records","")
ukTradeRecordsLong <- ukTradeRecords %>% gather("type", "records", Import:Dispatch)

# Plot Data --------------------------------------------------------------------
ukTradeRecordsLong <- ukTradeRecordsLong %>% filter(!substr(period, 1, 4) == "2008")
xScale <- unique(ukTradeRecordsLong$period)[seq(1, length(unique(ukTradeRecordsLong$period)) + 1, 3)]
ggplot(data = ukTradeRecordsLong, aes(x = period, y = records, colour = type, group = type)) +
  geom_line(size = 1) +
  scale_x_discrete(name = "Period", breaks = xScale) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Number of Records", title = "UK Trade Activity over time by Record Count in HMRC Data")
