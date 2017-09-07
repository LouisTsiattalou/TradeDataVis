# 20170904
# Script to write Trade Data to a PostgreSQL database.
# Written by David Lee at DEFRA
# Adapted by Louis Tsiattalou for Imports Tool

# TODO
# >importers/exporters files need to be entered after comcode issue sorted

# SCRIPT START ###############################################################

install.packages("RPostgreSQL")
library('RPostgreSQL')

start <- Sys.time()


# make names db safe: no '.' or other illegal characters,
# all lower case and unique
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

# database driver
pg = dbDriver("PostgreSQL")

files <- c("SMKA12", "SMKE19", "SMKI19", "SMKX46", "SMKM46", "SESX16", "SESM16")
names(files) <- c("control", "exp", "imp", "disp", "arr", "dispest", "arrest")

disp <- "SMKX46"
arr <- "SMKM46"

eutradecols <- c("SMK-COMCODE","SMK-RECORD-TYPE","SMK-COD-SEQ","SMK-COD-ALPHA","SMK-TRADE-IND","SMK-COO-SEQ","SMK-COO-ALPHA","SMK-NATURE-OF-TRANSACTION","SMK-MODE-OF-TRANSPORT","SMK-PERIOD-REFERENCE","SMK-SUITE INDICATOR","SMK-SITC","SMK-IP-COMCODE","SMK-NO-OF-CONSIGNMENTS","SMK-STAT-VALUE","SMK-NETT-MASS","SMK-SUPP-UNIT")
noneuexportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","VALUE","QUANTITY-1","QUANTITY-2","INDUSTRIAL-PLANT-COMCODE")
noneuimportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","COO-SEQUENCE","COO-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","COUNTRY-SEQUENCE-COO-IMP","COUNTRY-ALPHA-COO-IMP","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","CB-CODE","VALUE","QUANTITY-1","QUANTITY-2")
controlfilecols <- c("MK-COMCODE","MK-INTRA-EXTRA-IND","MK-INTRA-MMYY-ON","MK-INTRA-MMYY-OFF","MK-EXTRA-MMYY-ON","MK-EXTRA-MMYY-OFF","MK-NON-TRADE-ID","MK-SITC-NO","MK-SITC-IND","MK-SITC-CONV-A","MK-SITC-CONV-B","MK-CN-Q2","MK-SUPP-ARRIVALS","MK-SUPP-DESPATCHES","MK-SUPP-IMPORTS","MK-SUPP-EXPORTS","MK-SUB-GROUP-ARR","MK-ITEM-ARR","MK-SUB-GROUP-DESP","MK-ITEM-DESP","MK-SUB-GROUP-IMP","MK-ITEM-IMP","MK-SUB-GROUP-EXP","MK-ITEM-EXP","MK-QTY1-ALPHA","MK-QTY2-ALPHA","MK-COMMODITY-ALPHA-1")

dispatches <- read.table(paste(files["disp"], "0901", sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["disp"], "0901", sep = "")))-2, col.names = eutradecols)
colnames(dispatches) = dbSafeNames(colnames(dispatches))

arrivals <- read.table(paste(files["arr"], "0901", sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["arr"], "0901", sep = "")))-2, col.names = eutradecols)
colnames(arrivals) = dbSafeNames(colnames(arrivals))

exports <- read.table(paste(files["exp"], "0901", sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["exp"], "0901", sep = "")))-2, col.names = noneuexportcols)
colnames(exports) = dbSafeNames(colnames(exports))

imports <- read.table(paste(files["imp"], "0901", sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["imp"], "0901", sep = "")))-2, col.names = noneuimportcols)
colnames(imports) = dbSafeNames(colnames(imports))

#control <- read.table(paste(files["control"], "1206", sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["control"], "1206", sep = "")))-2, col.names = controlfilecols)
#colnames(control) = dbSafeNames(colnames(control))


# Local Postgres.app database; no password by default
# Of course, you fill in your own database information here.
tradedata = dbConnect(pg, user="postgres", password="postgres",
                host="localhost", port=5432, dbname="tradedata")

dbWriteTable(tradedata,'dispatches', dispatches, row.names=FALSE)
dbSendQuery(tradedata, "delete from dispatches")

dbWriteTable(tradedata,'arrivals', arrivals, row.names=FALSE)
dbSendQuery(tradedata, "delete from arrivals")

dbWriteTable(tradedata,'exports', exports, row.names=FALSE)
dbSendQuery(tradedata, "delete from exports")

dbWriteTable(tradedata, 'imports', imports, row.names=FALSE)
dbSendQuery(tradedata, "delete from imports")

#dbWriteTable(tradedata, 'control', control, row.names=FALSE)
#dbSendQuery(tradedata, "delete from control")

yrs <- as.character(sprintf("%02d", c(9:16)))
mths <- as.character(sprintf("%02d",c(1:12)))

for (i in yrs) {
  for (j in mths) {
    
    print(paste("Processing dispatches: ", i,j))
    
    dispatches <- read.table(paste(files["disp"], i, j, sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["disp"], i, j, sep = "")))-2, col.names = eutradecols)
    colnames(dispatches) = dbSafeNames(colnames(dispatches))
    dbWriteTable(tradedata,'dispatches', dispatches, row.names=FALSE, append = TRUE)

    print(paste("Processing arrivals: ", i,j))
    
    arrivals <- read.table(paste(files["arr"], i, j, sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["arr"], i, j, sep = "")))-2, col.names = eutradecols)
    colnames(arrivals) = dbSafeNames(colnames(arrivals))
    dbWriteTable(tradedata,'arrivals', arrivals, row.names=FALSE, append = TRUE)
    
    print(paste("Processing exports: ", i,j))
    
    exports <- read.table(paste(files["exp"], i, j, sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["exp"], i, j, sep = "")))-2, col.names = noneuexportcols)
    colnames(exports) = dbSafeNames(colnames(exports))
    dbWriteTable(tradedata,'exports', exports, row.names=FALSE, append = TRUE)
    
    print(paste("Processing imports: ", i,j))
    
    imports <- read.table(paste(files["imp"], i, j, sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["imp"], i, j, sep = "")))-2, col.names = noneuimportcols)
    colnames(imports) = dbSafeNames(colnames(imports))
    dbWriteTable(tradedata,'imports', imports, row.names=FALSE, append = TRUE)

#    control <- read.table(paste(files["control"], i, j, sep = ""), sep = "|", skip = 1, nrows = length(readLines(paste(files["control"], i, j, sep = "")))-2, col.names = controlfilecols)
#    colnames(control) = dbSafeNames(colnames(control))
#    dbWriteTable(tradedata,'control', control, row.names=FALSE, append = TRUE)
        
  }
}

end <- Sys.time()

print(paste("Time taken:", end - start))