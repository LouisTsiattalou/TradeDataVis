# 20171004
# Updated 20171212
# Script to create a tidier, more relevant, more complete Comcode table from
# EUROSTAT Commodity Nomenclature file, and existing control table build from
# HMRC Data..

# Written by Louis Tsiattalou for TradeDataVis application
# Github: https://github.com/LouisTsiattalou/TradeDataVis

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

setwd("~/Documents/R/ImportTool/")
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2],
                       host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])


# COMCODE TABLE BUILD =========================================================

# Read Data using readr and tidy as appropriate -------------------------------
# Note that this data comes in semi-colon delimited format. Despite CSV in url...

CNURL <- "http://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=ACT_OTH_CLS_DLD&StrNom=CN_2017&StrFormat=CSV&StrLanguageCode=EN&IntKey=&IntLevel=&bExport="
#CN <- read_csv2(CNURL)
CN <- read_csv2("CNCodes.csv")
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

thisrecur <- vapply(thisrecur,function(x){
    if (x %in% codesonly == FALSE){
      x <- substr(x,1,nchar(x)-2)
    } else {
      return(x)
    }
  }, character(1), USE.NAMES = FALSE)

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
#portcode$portname <- toupper(portcode$portname)
colnames(portcode) <- dbSafeNames(colnames(portcode))

# OBTAIN LAT/LONG COORDINATES -------------------------------------------------

# Set URL and FileName
portlatlonURL <- "http://www.unece.org/fileadmin/DAM/cefact/locode/loc171csv.zip"
portlatlonFN <- "datafiles/LatLonPorts.zip"

# Download ZIP to datafiles, obtain list of items
download.file(portlatlonURL,portlatlonFN)
portlatlonlist <- unzip(portlatlonFN, list=TRUE)

# Unzip to datafiles, paste csvs together
unzip(portlatlonFN)
unlink(portlatlonFN)

# Read data and reshape
portlatlon <- portlatlonlist$Name[grepl("CodeList",portlatlonlist$Name)] %>% # Take Data only
  lapply(read_csv, col_names = FALSE) %>% # read them into R
  bind_rows %>% # Paste them together
  filter(X2 == "GB") %>% # Take only United Kingdom ports (GB)
  select(choices = c(X3,X5,X11)) # Select code, name, and lat/lon location

# Now have GB ports, by portcode,portname,lat/lon.
colnames(portlatlon) <- c("portcode","portname","latlon")

portlatlon <- portlatlon %>%
  mutate(lat = substr(latlon,1,4)) %>% # Obtain Latitude
  mutate(long = substr(latlon,nchar(latlon)-6,nchar(latlon)-1)) %>% # Obtain Longitude
  filter(!is.na(latlon))

# Convert to Numeric Vectors and Divide by 100 to get decimal form
portlatlon$lat <- as.numeric(portlatlon$lat) / 100
portlatlon$long <- as.numeric(portlatlon$long) / 100

# Multiply - North = +1, South = -1, West = -1, East = +1
portlatlon$lat <- ifelse(grepl("N", portlatlon$latlon), portlatlon$lat*1, portlatlon$lat*-1)
portlatlon$long <- ifelse(grepl("E", portlatlon$latlon), portlatlon$long*1, portlatlon$long*-1)

# Remove latlon column.
portlatlon <- select(portlatlon, -latlon)

# At this point, I realised that UN Locode had lots of ports missing that we actually needed.
# Big ones too - Felixstowe for example!
# I manually entered these into missingports.csv, which we append here and left-join into the dataframe
missingports <- read_csv("missingports.csv") %>% select(portname.x,portcode,lat,long)
portlatlon <- full_join(portlatlon, missingports, by = c("portname" = "portname.x"))

# Now merge into lat/long/portcode columns and reorder
portlatlon$lat <- ifelse(is.na(portlatlon$lat.x), portlatlon$lat.y, portlatlon$lat.x)
portlatlon$long <- ifelse(is.na(portlatlon$long.x), portlatlon$long.y, portlatlon$long.x)
portlatlon$portcode <- ifelse(is.na(portlatlon$portcode.x), portlatlon$portcode.y, portlatlon$portcode.x)
portlatlon <- portlatlon %>% select(portname, portcode, lat, long)

# Set portname to uppercase for joining
portlatlon$portname <- toupper(portlatlon$portname)

# Delete unzipped files
unlink(portlatlonlist$Name)

# JOIN LAT/LONG TO PORTCODE ---------------------------------------------------
# <<This section is incomplete>> 
# TODO Find a way to match UN/LOCODE to HMRC Ports. Maybe manually.
portcode2 <- left_join(portcode,portlatlon, by = "portcode")
portcode2 <- portcode2 %>% select(portname = portname.x, portcode, type, lat, long)

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
