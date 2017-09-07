setwd("./Dropbox/Work/Downloads")


# Download annual archives
years <- c(2009:2015)

for (i in years ) {
  download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKE19_", i , "archive.zip", sep = ""), paste("SMKE19_", i, "archive.zip", sep = ""))
  download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKI19_", i , "archive.zip", sep = ""), paste("SMKI19_", i, "archive.zip", sep = ""))
  download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKX46_", i , "archive.zip", sep = ""), paste("SMKX46_", i, "archive.zip", sep = ""))
  download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKM46_", i , "archive.zip", sep = ""), paste("SMKM46_", i, "archive.zip", sep = ""))
  download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKA12_", i , "archive.zip", sep = ""), paste("SMKA12_", i, "archive.zip", sep = ""))
  download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SIAI11_", i , "archive.zip", sep = ""), paste("SIAI11_", i, "archive.zip", sep = ""))
  download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SESX16_", i , "archive.zip", sep = ""), paste("SESX16_", i, "archive.zip", sep = ""))
  download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SESM16_", i , "archive.zip", sep = ""), paste("SESM16_", i, "archive.zip", sep = ""))
  }

# Unzip annual archives
for (i in years) {
  unzip(paste("SMKE19_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKI19_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKX46_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKM46_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKA12_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SIAI11_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SESX16_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SESM16_", i, "archive.zip", sep = ""), exdir = getwd())
  }


# Unzip monthly files
syrs <- as.character(sprintf("%02d",c(9:15)))
smths <- as.character(sprintf("%02d",c(1:12)))

for (i in syrs) {
  for (j in smths) {
    unzip(paste("SMKE19", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SMKI19", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SMKX46", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SMKM46", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SMKA12", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SIAI11", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SESX16", i, j, ".zip", sep = ""), exdir = getwd())   
    unzip(paste("SESM16", i, j, ".zip", sep = ""), exdir = getwd())    
  }
}


# Clean up: delete the zipfiles
zipfiles <- list.files(getwd(), pattern = ".zip")
sapply(zipfiles, unlink)


# Make all uppercase
datafiles <- list.files(getwd())
sapply(datafiles, FUN = function(up) {
  file.rename(from = up, to = toupper(up))
})


# Remove .txt suffix
datafiles <- list.files(getwd())
sapply(datafiles, FUN = function(txt) {
  file.rename(from = txt, to = sub(pattern = ".TXT", replacement = "", txt))
})

# Fix ~1 for Feb 2009
datafiles <- list.files(getwd())
sapply(datafiles, FUN = function(txt) {
  file.rename(from = txt, to = sub(pattern = "~1", replacement = "0902", txt))
})

# Fix V2 in May/June 2014
datafiles <- list.files(getwd())
sapply(datafiles, FUN = function(txt) {
  file.rename(from = txt, to = sub(pattern = "V2", replacement = "", txt))
})





#non_eu_export_cols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","VALUE","QUANTITY-1","QUANTITY-2","INDUSTRIAL-PLANT-COMCODE")
#non_eu_import_cols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","COO-SEQUENCE","COO-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","COUNTRY-SEQUENCE-COO-IMP","COUNTRY-ALPHA-COO-IMP","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","CB-CODE","VALUE","QUANTITY-1","QUANTITY-2")
#eu_trade_cols <- c("SMK-COMCODE","SMK-RECORD-TYPE","SMK-COD-SEQ","SMK-COD-ALPHA","SMK-TRADE-IND","SMK-COO-SEQ","SMK-COO-ALPHA","SMK-NATURE-OF-TRANSACTION","SMK-MODE-OF-TRANSPORT","SMK-PERIOD-REFERENCE","SMK-SUITE INDICATOR","SMK-SITC","SMK-IP-COMCODE","SMK-NO-OF-CONSIGNMENTS","SMK-STAT-VALUE","SMK-NETT-MASS","SMK-SUPP-UNIT")
#control_file_cols <- c("MK-COMCODE","MK-INTRA-EXTRA-IND","MK-INTRA-MMYY-ON","MK-INTRA-MMYY-OFF","MK-EXTRA-MMYY-ON","MK-EXTRA-MMYY-OFF","MK-NON-TRADE-ID","MK-SITC-NO","MK-SITC-IND","MK-SITC-CONV-A","MK-SITC-CONV-B","MK-CN-Q2","MK-SUPP-ARRIVALS","MK-SUPP-DESPATCHES","MK-SUPP-IMPORTS","MK-SUPP-EXPORTS","MK-SUB-GROUP-ARR","MK-ITEM-ARR","MK-SUB-GROUP-DESP","MK-ITEM-DESP","MK-SUB-GROUP-IMP","MK-ITEM-IMP","MK-SUB-GROUP-EXP","MK-ITEM-EXP","MK-QTY1-ALPHA","MK-QTY2-ALPHA","MK-COMMODITY-ALPHA-1")

#exports <- read.table("SMKE191501", skip = 1, nrows = length(readLines("SMKE191501"))-2, sep = "|", col.names = non_eu_export_cols)
#imports <- read.table("SMKI191501", skip = 1, nrows = length(readLines("SMKI191501"))-2, sep = "|", col.names = non_eu_import_cols)
#arrivals <- read.table("smkm461501.txt", skip = 1, nrows = length(readLines("smkm461501.txt"))-2, sep = "|", col.names = eu_trade_cols)
#dispatches <- read.table("SMKX461501", skip = 1, nrows = length(readLines("SMKX461501"))-2, sep = "|", col.names = eu_trade_cols)
