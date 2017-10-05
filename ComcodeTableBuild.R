# 20171004
# Updated 20171005
# Script to create a tidier, more relevant, more complete Comcode table from
# EUROSTAT Commodity Nomenclature file, and existing control table build from
# HMRC Data..

# Written by Louis Tsiattalou for TradeDataVis application
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# TODO
# > source from InitialiseDB.R
# > Read mk_comcode, mk_commodity_alpha_1 from control and include in parent
#   logic.

# SCRIPT START ###############################################################

# Library import and constants ===============================================

library(readr)
library(dplyr)
library(stringr)
library(RPostgreSQL)

# make names db safe: no '.' or other illegal characters,
# all lower case and unique
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

pg = dbDriver("PostgreSQL")
tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

# Read Data using readr and tidy as appropriate ==============================

CN <- read_csv("CN2017.csv")
CN <- tibble(CommodityCode = CN$Code_1,Parent = CN$Parent_1,Description = CN[[8]])
colnames(CN) <- c("CommodityCode", "Parent", "Description")
CN$CommodityCode <- gsub(" ", "", CN$CommodityCode)
CN$Parent <- gsub(" ", "", CN$Parent)
CN <- CN[is.na(CN$CommodityCode) == FALSE,]

control <- as_tibble(dbGetQuery(tradedata, "SELECT mk_comcode,mk_commodity_alpha_1 FROM public.control"))
control <- arrange(control,mk_comcode)

# Create a complete Codes and Parents column ===========================================

# Extracts numeric comcodes (IE: not section level).
codesonly <- CN$CommodityCode[grepl("^.*(I|V|X).*$",CN$CommodityCode) == FALSE]
codesonly <- c(codesonly, unique(control$mk_comcode))

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
  }, character(1))
}
names(thisrecur) = NULL # for some reason lastrecur becomes names in thisrecur...
parents <- thisrecur

codeandparent <- tibble(CommodityCode = codesonly, Parent = parents)

# joins new parent vector to tibble, merges cols.
CN <- full_join(CN, codeandparent, by = "CommodityCode")
CN$Parent.z <- ifelse(is.na(CN$Parent.x),CN$Parent.y,CN$Parent.x)
CN <- tibble(CommodityCode = CN$CommodityCode, Parent = CN$Parent.z, Description = CN$Description)
colnames(CN) <- dbSafeNames(colnames(CN))

# creates new table, adds to db

dbWriteTable(tradedata, 'comcode', CN5, row.names=FALSE)
dbSendQuery(tradedata, "delete from comcode")
dbSendQuery(tradedata, "SET client_encoding = 'LATIN1'")
try(dbSendQuery(tradedata, "alter table comcode add constraint control_pkey PRIMARY KEY (CommodityCode)"))

dbWriteTable(tradedata,'comcode', CN5, row.names=FALSE, append = TRUE)


## Padding to uniform 8 char IDs
#CN3 <- CN2
#CN3$Code <- vapply(CN2$Code, function(x){
#  if(grepl("^.*(I|V|X).*$",x) == FALSE){
#    x <- str_pad(x, width=8, side="right", pad="0")
#  } else {x}
#}, character(1))