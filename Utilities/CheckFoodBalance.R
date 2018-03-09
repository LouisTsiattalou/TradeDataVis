# Connect to DB
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2],
                       host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])

# Define food commodities
foodcommodities <- paste0(as.character(sprintf("%02d", c(1:23))),"______")

# Imports
sqlquery <- paste0("SELECT DISTINCT account_date, SUM(quantity_1) AS records FROM imports WHERE comcode SIMILAR TO '(",
                   paste0(foodcommodities, collapse = "|"),
                   ")' GROUP BY account_date")
imports <- dbGetQuery(tradedata, sqlquery)
imports$account_date <- paste0(substr(imports$account_date, 4, 7),substr(imports$account_date, 1, 2))
imports <- imports %>%
  rename(period = account_date) %>% 
  arrange(period)

# Exports
sqlquery <- paste0("SELECT DISTINCT account_date, SUM(quantity_1) AS records FROM exports WHERE comcode SIMILAR TO '(",
                   paste0(foodcommodities, collapse = "|"),
                   ")' GROUP BY account_date")
exports <- dbGetQuery(tradedata, sqlquery)
exports$account_date <- paste0(substr(exports$account_date, 4, 7), substr(exports$account_date, 1, 2))
exports <- exports %>%
  rename(period = account_date) %>% 
  arrange(period)

# Arrivals
sqlquery <- paste0("SELECT DISTINCT smk_period_reference, SUM(smk_stat_value) AS records FROM arrivals WHERE smk_comcode SIMILAR TO '(",
                   paste0(foodcommodities, collapse = "|"),
                   ")' GROUP BY smk_period_reference")
arrivals <- dbGetQuery(tradedata, sqlquery)
arrivals_useful <- arrivals %>%
  filter(substr(smk_period_reference, 1, 3) == "020") %>%
  rename(period = smk_period_reference)
arrivals_useful$period <- arrivals_useful$period %>% substr(2,7)

# Dispatches
sqlquery <- paste0("SELECT DISTINCT smk_period_reference, SUM(smk_stat_value) AS records FROM dispatches WHERE smk_comcode SIMILAR TO '(",
                   paste0(foodcommodities, collapse = "|"),
                   ")' GROUP BY smk_period_reference")
dispatches <- dbGetQuery(tradedata, sqlquery)
dispatches_useful <- dispatches %>%
  filter(substr(smk_period_reference, 1, 3) == "020") %>%
  rename(period = smk_period_reference)
dispatches_useful$period <- dispatches_useful$period %>% substr(2,7)

# Join Data
ukTradeRecords <- imports
ukTradeRecords <- ukTradeRecords %>% 
  left_join(exports, by = "period", suffix = c("Import", "Export")) %>%
  left_join(arrivals_useful, by = "period") %>%
  left_join(dispatches_useful, by = "period", suffix = c("Arrival", "Dispatch"))
colnames(ukTradeRecords) <- stringr::str_replace(colnames(ukTradeRecords), "records","")
ukTradeRecordsLong <- ukTradeRecords %>% gather("type", "records", Import:Dispatch)

# Plot Data
ukTradeRecordsLong <- ukTradeRecordsLong %>% filter(!substr(period, 1, 4) %in% c("2008","2009"))
xScale <- unique(ukTradeRecordsLong$period)[seq(1, length(unique(ukTradeRecordsLong$period)) + 1, 3)]
ggplot(data = ukTradeRecordsLong, aes(x = period, y = records, colour = type, group = type)) +
  geom_line(size = 1) +
  scale_x_discrete(name = "Period", breaks = xScale) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Value of Transactions", title = "UK Food Trade Activity over time by Transaction Value in HMRC Data")
