# 20170904
# Updated 20170921
# Script to write Trade Data to a PostgreSQL database.
# Base Script by David Lee at DEFRA
# Adapted by Louis Tsiattalou for Imports Tool
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# TODO
# >importers/exporters files need to be entered after comcode issue sorted
# >fix code description merge - not working atm
# >check overwrite enabled on codes - not currently enabled

# SCRIPT START ###############################################################

# Library import and constants ===============================================

# install.packages("RPostgreSQL")
# install.packages("tidyr")
library('RPostgreSQL')
library('tidyr')
library('readr')

# Database Driver
setwd("~/R/ImportTool/")
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2],
                       host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])

setwd("~/R/ImportTool/")
suppressWarnings(dir.create(paste(getwd(), "/datafiles", sep = "")))
setwd("datafiles")

start <- Sys.time()
errors <- character()

# make names db safe: no '.' or other illegal characters,
# all lower case and unique
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

files <- c("SMKA12", "SMKE19", "SMKI19", "SMKX46", "SMKM46", "SESX16", "SESM16")
names(files) <- c("control", "exp", "imp", "disp", "arr", "dispest", "arrest")

eutradecols <- c("SMK-COMCODE","SMK-RECORD-TYPE","SMK-COD-SEQ","SMK-COD-ALPHA","SMK-TRADE-IND","SMK-COO-SEQ","SMK-COO-ALPHA","SMK-NATURE-OF-TRANSACTION","SMK-MODE-OF-TRANSPORT","SMK-PERIOD-REFERENCE","SMK-SUITE INDICATOR","SMK-SITC","SMK-IP-COMCODE","SMK-NO-OF-CONSIGNMENTS","SMK-STAT-VALUE","SMK-NETT-MASS","SMK-SUPP-UNIT")
noneuexportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","VALUE","QUANTITY-1","QUANTITY-2","INDUSTRIAL-PLANT-COMCODE")
noneuimportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","COO-SEQUENCE","COO-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","COUNTRY-SEQUENCE-COO-IMP","COUNTRY-ALPHA-COO-IMP","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","CB-CODE","VALUE","QUANTITY-1","QUANTITY-2")
controlfilecols <- c("MK-COMCODE","MK-INTRA-EXTRA-IND","MK-INTRA-MMYY-ON","MK-INTRA-MMYY-OFF","MK-EXTRA-MMYY-ON","MK-EXTRA-MMYY-OFF","MK-NON-TRADE-ID","MK-SITC-NO","MK-SITC-IND","MK-SITC-CONV-A","MK-SITC-CONV-B","MK-CN-Q2","MK-SUPP-ARRIVALS","MK-SUPP-DESPATCHES","MK-SUPP-IMPORTS","MK-SUPP-EXPORTS","MK-SUB-GROUP-ARR","MK-ITEM-ARR","MK-SUB-GROUP-DESP","MK-ITEM-DESP","MK-SUB-GROUP-IMP","MK-ITEM-IMP","MK-SUB-GROUP-EXP","MK-ITEM-EXP","MK-QTY1-ALPHA","MK-QTY2-ALPHA","MK-COMMODITY-ALPHA-1")


# Read Dataframes from file ==================================================

dispatches <- read.table(paste(files["disp"], "0901", sep = ""), sep = "|", skip = 1, col.names = eutradecols, fill = TRUE, colClasses = "character")
colnames(dispatches) = dbSafeNames(colnames(dispatches))
dispatches <- dispatches[-length(dispatches$smk_comcode),]
dispatches$smk_comcode <- substr(dispatches$smk_comcode, 1, 8)

arrivals <- read.table(paste(files["arr"], "0901", sep = ""), sep = "|", skip = 1, col.names = eutradecols, fill = TRUE, colClasses = "character")
colnames(arrivals) = dbSafeNames(colnames(arrivals))
arrivals <- arrivals[-length(arrivals$smk_comcode),]
arrivals$smk_comcode <- substr(arrivals$smk_comcode, 1, 8)

exports <- read.table(paste(files["exp"], "0901", sep = ""), sep = "|", skip = 1, col.names = noneuexportcols, fill = TRUE, colClasses = "character")
colnames(exports) = dbSafeNames(colnames(exports))
exports <- exports[-length(exports$comcode),]
exports$comcode <- substr(exports$comcode, 1, 8)

imports <- read.table(paste(files["imp"], "0901", sep = ""), sep = "|", skip = 1, col.names = noneuimportcols, fill = TRUE, colClasses = "character")
colnames(imports) = dbSafeNames(colnames(imports))
imports <- imports[-length(imports$mk_comcode),]
imports$comcode <- substr(imports$comcode, 1, 8)

control <- read.table(paste(files["control"], "1206", sep = ""), sep = "|", skip = 1, col.names = controlfilecols, fill = TRUE, colClasses = "character")
colnames(control) = dbSafeNames(colnames(control))
control <- control[-length(control$mk_comcode),]
control$mk_comcode <- substr(control$mk_comcode, 1, 8)


# Create tables ===============================================================

dbWriteTable(tradedata,'dispatches', dispatches, row.names=FALSE)
dbGetQuery(tradedata, "delete from dispatches")
dbGetQuery(tradedata, "drop index if exists idx_comcode_disp")
dbGetQuery(tradedata, "drop index if exists idx_period_disp")
dbGetQuery(tradedata, "alter table dispatches alter column smk_no_of_consignments type bigint using (smk_no_of_consignments::bigint)")
dbGetQuery(tradedata, "alter table dispatches alter column smk_nett_mass type bigint using (smk_nett_mass::bigint)")
dbGetQuery(tradedata, "alter table dispatches alter column smk_stat_value type bigint using (smk_stat_value::bigint)")
dbGetQuery(tradedata, "alter table dispatches alter column smk_supp_unit type bigint using (smk_supp_unit::bigint)")

dbWriteTable(tradedata,'arrivals', arrivals, row.names=FALSE)
dbGetQuery(tradedata, "delete from arrivals")
dbGetQuery(tradedata, "drop index if exists idx_comcode_arr")
dbGetQuery(tradedata, "drop index if exists idx_period_arr")
dbGetQuery(tradedata, "alter table arrivals alter column smk_no_of_consignments type bigint using (smk_no_of_consignments::bigint)")
dbGetQuery(tradedata, "alter table arrivals alter column smk_nett_mass type bigint using (smk_nett_mass::bigint)")
dbGetQuery(tradedata, "alter table arrivals alter column smk_stat_value type bigint using (smk_stat_value::bigint)")
dbGetQuery(tradedata, "alter table arrivals alter column smk_supp_unit type bigint using (smk_supp_unit::bigint)")

dbWriteTable(tradedata,'exports', exports, row.names=FALSE)
dbGetQuery(tradedata, "delete from exports")
dbGetQuery(tradedata, "drop index if exists idx_comcode_exp")
dbGetQuery(tradedata, "drop index if exists idx_period_exp")
dbGetQuery(tradedata, "alter table exports alter column value type bigint using (value::bigint)")
dbGetQuery(tradedata, "alter table exports alter column quantity_1 type bigint using (quantity_1::bigint)")
dbGetQuery(tradedata, "alter table exports alter column quantity_2 type bigint using (quantity_2::bigint)")

dbWriteTable(tradedata, 'imports', imports, row.names=FALSE)
dbGetQuery(tradedata, "delete from imports")
dbGetQuery(tradedata, "drop index if exists idx_comcode_imp")
dbGetQuery(tradedata, "drop index if exists idx_period_imp")
dbGetQuery(tradedata, "alter table imports alter column value type bigint using (value::bigint)")
dbGetQuery(tradedata, "alter table imports alter column quantity_1 type bigint using (quantity_1::bigint)")
dbGetQuery(tradedata, "alter table imports alter column quantity_2 type bigint using (quantity_2::bigint)")

dbWriteTable(tradedata, 'control', control, row.names=FALSE)
dbGetQuery(tradedata, "delete from control")
dbGetQuery(tradedata, "SET client_encoding = 'LATIN1'")
try({dbGetQuery(tradedata, "alter table control add constraint control_pkey PRIMARY KEY (mk_comcode)")})


# Write to the five database tables ========================================

yrs <- as.character(sprintf("%02d", c(09:17)))
mths <- as.character(sprintf("%02d",c(1:12)))

for (i in yrs) {
  for (j in mths) {
    
    print(paste("Processing dispatches: ", i, j))
    
    tryCatch({
      dispatches <- read.table(paste(files["disp"], i, j, sep = ""), sep = "|", skip = 1, col.names = eutradecols, fill = TRUE, colClasses = "character")
      colnames(dispatches) = dbSafeNames(colnames(dispatches))
      dispatches <- dispatches[-length(dispatches$smk_comcode),]
      dispatches$smk_comcode <- substr(dispatches$smk_comcode, 1, 8)
      dbWriteTable(tradedata,'dispatches', dispatches, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["disp"], i, j, sep = ""))}
    )
    
    
    print(paste("Processing arrivals: ", i, j))
    
    tryCatch({
      arrivals <- read.table(paste(files["arr"], i, j, sep = ""), sep = "|", skip = 1, col.names = eutradecols, fill = TRUE, colClasses = "character")
      colnames(arrivals) = dbSafeNames(colnames(arrivals))
      arrivals <- arrivals[-length(arrivals$smk_comcode),]
      arrivals$smk_comcode <- substr(arrivals$smk_comcode, 1, 8)
      dbWriteTable(tradedata,'arrivals', arrivals, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["arr"], i, j, sep = ""))}
    )
    
    
    print(paste("Processing exports: ", i, j))
    
    tryCatch({
      exports <- read.table(paste(files["exp"], i, j, sep = ""), sep = "|", skip = 1, col.names = noneuexportcols, fill = TRUE, colClasses = "character")
      colnames(exports) = dbSafeNames(colnames(exports))
      exports <- exports[-length(exports$comcode),]
      exports$comcode <- substr(exports$comcode, 1, 8)
      dbWriteTable(tradedata,'exports', exports, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["exp"], i, j, sep = ""))}
    )
    
    
    print(paste("Processing imports: ", i, j))
    
    tryCatch({
      imports <- read.table(paste(files["imp"], i, j, sep = ""), sep = "|", skip = 1, col.names = noneuimportcols, fill = TRUE, colClasses = "character")
      colnames(imports) = dbSafeNames(colnames(imports))
      imports <- imports[-length(imports$comcode),]
      imports$comcode <- substr(imports$comcode, 1, 8)
      dbWriteTable(tradedata,'imports', imports, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["imp"], i, j, sep = ""))}
    )
    
    # Extra processing necessary for commodity codes due to extra delimiter entry before 2012.
    # Merges two "description" columns at end of dataframe.
    # Also handled by single SQL line-by-line queries to maintain integrity of primary key
    # Encoding errors exist for SMKA >= 1201 - encoding forced to LATIN1 in table definition above.
    
    print(paste("Processing commodity codes: ", i, j))
    
    tryCatch({
      control <- read.table(paste(files["control"], i, j, sep = ""), sep = "|", skip = 1, quote = NULL, fill = TRUE, strip.white = TRUE, colClasses = "character")
      fixedCols <- length(controlfilecols)
      actualCols <- ncol(control)
      if (fixedCols != actualCols) {
        control <- unite(control, "newcol", "V27", "V28", sep = "")}
      safecontrolfilecols <- dbSafeNames(controlfilecols)
      colnames(control) <- safecontrolfilecols
      control <- control[-length(control$mk_comcode), 1:fixedCols]
      control$mk_comcode <- substr(control$mk_comcode, 1, 8)
      control$mk_commodity_alpha_1 <- trimws(control$mk_commodity_alpha_1, "both")
      control$mk_commodity_alpha_1 <- gsub("\'", "\'\'", control$mk_commodity_alpha_1)
      control$mk_commodity_alpha_1 <- iconv(control$mk_commodity_alpha_1, from = "UTF-8", to = "LATIN1", sub = "/")
      #      dbWriteTable(tradedata,'control', control, row.names=FALSE, append = TRUE)
      for (k in 1:length(control$mk_comcode)) {
        valstring1 <- paste(sapply(control[k,], paste, ", \'", sep = "\'"), sep = "", collapse = "")
        valstring2 <- paste("\'", substr(valstring1, 1, nchar(valstring1)-3), sep = "")
        sqlQuery <- paste(
          "INSERT INTO control VALUES (",
          valstring2,
          ") ON CONFLICT (mk_comcode) DO UPDATE SET mk_commodity_alpha_1 = ", 
          #        valstring2,
          paste("\'", control$mk_commodity_alpha_1[k], "\'", sep = ""),
          ";", sep = "")
        dbGetQuery(tradedata,sqlQuery)
      }
    }, error = function(e){errors <<- c(errors, paste(files["control"], i, j, sep = ""))}
    )   
    
  }
}

# Recreate Indices
print("Creating indices: Dispatches")
dbGetQuery(tradedata, "create index idx_comcode_disp on dispatches (smk_comcode, smk_cod_alpha, smk_no_of_consignments, smk_stat_value, smk_nett_mass)")
dbGetQuery(tradedata, "create index idx_period_disp on dispatches (smk_period_reference, smk_cod_alpha, smk_no_of_consignments, smk_stat_value, smk_nett_mass)")
print("Creating indices: Arrivals")
dbGetQuery(tradedata, "create index idx_comcode_arr on arrivals (smk_comcode, smk_cod_alpha, smk_no_of_consignments, smk_stat_value, smk_nett_mass)")
dbGetQuery(tradedata, "create index idx_period_arr on arrivals (smk_period_reference, smk_cod_alpha, smk_no_of_consignments, smk_stat_value, smk_nett_mass)")
print("Creating indices: Exports")
dbGetQuery(tradedata, "create index idx_comcode_exp on exports (comcode, cod_alpha, port_alpha, quantity_1, quantity_2)")
dbGetQuery(tradedata, "create index idx_period_exp on exports (account_date, cod_alpha, port_alpha, quantity_1, quantity_2)")
print("Creating indices: Imports")
dbGetQuery(tradedata, "create index idx_comcode_imp on imports (comcode, cod_alpha, port_alpha, quantity_1, quantity_2)")
dbGetQuery(tradedata, "create index idx_period_imp on imports (account_date, cod_alpha, port_alpha, quantity_1, quantity_2)")


end <- Sys.time()

print(paste("Time taken:", end - start))
print("The following files were not found:")
print(errors)