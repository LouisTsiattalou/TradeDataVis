# 20171004
# Updated 20171005
# Script to create a tidier, more relevant, more complete Comcode table from
# EUROSTAT Commodity Nomenclature file, and existing control table build from
# HMRC Data..

# Written by Louis Tsiattalou for TradeDataVis application
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# TODO

# SCRIPT START ################################################################

# LIBRARY IMPORT AND CONSTANTS ================================================

if(require("readr") == FALSE) {install.packages("readr")}
library("readr")

if(require("dplyr") == FALSE) {install.packages("dplyr")}
library("dplyr")

if(require("stringr") == FALSE) {install.packages("stringr")}
library("stringr")

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")

if(require("readxl") == FALSE) {install.packages("readxl")}
library("readxl")

# make names db safe: no '.' or other illegal characters,
# all lower case and unique
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/")
pg = dbDriver("PostgreSQL")
tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")


# COMCODE TABLE BUILD =========================================================

# Read Data using readr and tidy as appropriate -------------------------------
# Note that this data comes in semi-colon delimited format. Despite CSV in url...

CNURL <- "http://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=ACT_OTH_CLS_DLD&StrNom=CN_2017&StrFormat=CSV&StrLanguageCode=EN&IntKey=&IntLevel=&bExport="
CN <- read_csv2(CNURL)
CN <- tibble(CommodityCode = CN$Code_1,Parent = CN$Parent_1,Description = CN[[8]])
colnames(CN) <- c("CommodityCode", "Parent", "Description")
CN$CommodityCode <- gsub(" ", "", CN$CommodityCode)
CN$Parent <- gsub(" ", "", CN$Parent)
CN <- CN[is.na(CN$CommodityCode) == FALSE,]

# store indices for Section Characters (roman numerals)
Sections <- tibble(Section = CN$CommodityCode[grep("^.*(I|V|X).*$",CN$CommodityCode)],
                  Code = CN$CommodityCode[grep("^.*(I|V|X).*$",CN$CommodityCode)+1],
                  Description = CN$Description[grep("^.*(I|V|X).*$",CN$CommodityCode)])

control <- dbGetQuery(tradedata, "SELECT mk_comcode,mk_commodity_alpha_1 FROM public.control")
control <- tibble(commoditycode = control$mk_comcode, description = control$mk_commodity_alpha_1)
control <- arrange(control,commoditycode)

# Create a complete Codes and Parents column ----------------------------------

# Extracts numeric comcodes (IE: not section level).
codesonly <- CN$CommodityCode[grepl("^.*(I|V|X).*$",CN$CommodityCode) == FALSE]
codesonly <- unique(c(codesonly, control$commoditycode))

# Commodity Nomenclature codes contain a ragged hierarchy. This means that
# a child-parent relationship can span levels in the hierarchy. Therefore it
# isn't simply a case of chopping off the last two digits of a code gives the
# parent's code - it may not exist! The code below generates a parent through
# recursive removal of last two digits in the commodity code.
# Once all the parents can be found within the commodity code vector, create tibble.
parents <- substr(codesonly,1, nchar(codesonly)-2)
thisrecur <- parents
lastrecur <- rep("",times=length(parents))

while (sum(sort(thisrecur) != sort(lastrecur)) != 0) {
  lastrecur <- thisrecur
  thisrecur <- vapply(thisrecur,function(x){
    if (x %in% codesonly == FALSE){
      x <- substr(x,1,nchar(x)-2)
    } else {
      return(x)
    }
  }, character(1), USE.NAMES = FALSE)
}
#names(thisrecur) = NULL # for some reason lastrecur becomes names in thisrecur...
parents <- thisrecur

codeandparent <- tibble(commoditycode = codesonly, parent = parents)

# Joining data ----------------------------------------------------------------

colnames(CN) <- dbSafeNames(colnames(CN))
colnames(codeandparent) <- dbSafeNames(colnames(codeandparent))
colnames(control) <- dbSafeNames(colnames(control))

# joins new parent vector to tibble, merges cols.
CN <- full_join(CN, codeandparent, by = "commoditycode")
CN$parent.z <- ifelse(is.na(CN$parent.x), CN$parent.y, CN$parent.x)
CN <- tibble(commoditycode = CN$commoditycode, parent = CN$parent.z, description = CN$description)

# joins descriptions from control file to tibble, merges cols
CN <- full_join(CN, control, by = "commoditycode")
CN <- arrange(CN, commoditycode)
CN$description.z <- ifelse(is.na(CN$description.x), CN$description.y, CN$description.x)
CN <- tibble(commoditycode = CN$commoditycode, parent = CN$parent, description = CN$description.z)

# remember the indices object? we can now put section numbers back in the right place
# using the data stored in that tibble! In reverse order so add_row doesn't get confused...

for (i in length(Sections$Section):1){
  CN <- add_row(CN, commoditycode = Sections$Section[i], 
                parent = "", 
                description = Sections$Description[i],
                .before = grep(paste("^", Sections$Code[i], "$", sep=""),CN$commoditycode))
}

CN <- CN[!duplicated(CN$commoditycode),]

# creates new table, adds to db -----------------------------------------------

dbWriteTable(tradedata, 'comcode', CN, row.names=FALSE)
dbSendQuery(tradedata, "delete from comcode")
dbSendQuery(tradedata, "SET client_encoding = 'LATIN1'")
try(dbSendQuery(tradedata, "alter table comcode add constraint control_pkey PRIMARY KEY (CommodityCode)"))

dbWriteTable(tradedata,'comcode', CN, row.names=FALSE, append = TRUE)


# PORTCODE TABLE BUILD ========================================================

portcodeURL <- "https://www.uktradeinfo.com/CodesAndGuides/Documents/Port_Codes.xls"
portcodeFN <- "portcode.xls"
download.file(portcodeURL, portcodeFN, mode = "wb")

Airportcode <- read_excel(portcodeFN,
                       sheet = "Airport codes",
                       range = "A2:B100",
                       col_names = c("portname","portcode"))
Airportcode$type <- "Airport"

Seaportcode <- read_excel(portcodeFN,
                          sheet = "Seaport codes",
                          range = "A2:B300",
                          col_names = c("portname","portcode"))
Seaportcode$type <- "Seaport"

portcode <- rbind(Airportcode, Seaportcode)
portcode <- portcode[is.na(portcode$portname) == FALSE,]
colnames(portcode) <- dbSafeNames(colnames(portcode))

dbWriteTable(tradedata, 'port', portcode, row.names=FALSE)
dbSendQuery(tradedata, "delete from port")
dbWriteTable(tradedata,'port', portcode, row.names=FALSE, append = TRUE)

unlink(portcodeFN)


# COUNTRYCODE TABLE BUILD =====================================================

countrycodeURL <- "https://www.uktradeinfo.com/CodesAndGuides/Documents/Country_alpha.xls"
countrycodeFN <- "countrycode.xls"
download.file(countrycodeURL, countrycodeFN, mode = "wb")
countrycode <- read_excel(countrycodeFN,
                          sheet = "Country Codes",
                          range = "A2:B300",
                          col_names = c("countryname","countrycode"))
countrycode <- countrycode[is.na(countrycode$countryname) == FALSE,]
colnames(countrycode) <- dbSafeNames(colnames(countrycode))

dbWriteTable(tradedata, 'country', countrycode, row.names=FALSE)
dbSendQuery(tradedata, "delete from country")
dbWriteTable(tradedata,'country', countrycode, row.names=FALSE, append = TRUE)

unlink(countrycodeFN)


# SCRIPT END ##################################################################