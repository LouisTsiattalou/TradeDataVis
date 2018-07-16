# 20171211
# Script to update the database with monthly data for a specified month
# Written By Louis Tsiattalou for Trade Data Visualisation project
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# SCRIPT START ###############################################################

library('RPostgreSQL')
library('tidyr')
library('readr')

# Constant setup =============================================================

start <- Sys.time()
errors <- character()

# Set working directory
setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/")
suppressWarnings(dir.create(paste0(getwd(), "/datafiles")))
setwd("datafiles")

# Connect to Database
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2],
                       host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])

# dbSafeNames function definition
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

## Set Year and Month
### Note: There is a 2 month time lag for when data comes out.
### Always check uktradeinfo.com/Statistics/Documents for new data.

syr <- "18"
smth <- "05"

# Data Structure constants
files <- c("SMKA12", "SMKE19", "SMKI19", "SMKX46", "SMKM46", "SESX16", "SESM16")
names(files) <- c("control", "exp", "imp", "disp", "arr", "dispest", "arrest")

eutradecols <- c("SMK-COMCODE","SMK-RECORD-TYPE","SMK-COD-SEQ","SMK-COD-ALPHA","SMK-TRADE-IND","SMK-COO-SEQ","SMK-COO-ALPHA","SMK-NATURE-OF-TRANSACTION","SMK-MODE-OF-TRANSPORT","SMK-PERIOD-REFERENCE","SMK-SUITE INDICATOR","SMK-SITC","SMK-IP-COMCODE","SMK-NO-OF-CONSIGNMENTS","SMK-STAT-VALUE","SMK-NETT-MASS","SMK-SUPP-UNIT")
noneuexportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","VALUE","QUANTITY-1","QUANTITY-2","INDUSTRIAL-PLANT-COMCODE")
noneuimportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","COO-SEQUENCE","COO-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","COUNTRY-SEQUENCE-COO-IMP","COUNTRY-ALPHA-COO-IMP","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","CB-CODE","VALUE","QUANTITY-1","QUANTITY-2")
controlfilecols <- c("MK-COMCODE","MK-INTRA-EXTRA-IND","MK-INTRA-MMYY-ON","MK-INTRA-MMYY-OFF","MK-EXTRA-MMYY-ON","MK-EXTRA-MMYY-OFF","MK-NON-TRADE-ID","MK-SITC-NO","MK-SITC-IND","MK-SITC-CONV-A","MK-SITC-CONV-B","MK-CN-Q2","MK-SUPP-ARRIVALS","MK-SUPP-DESPATCHES","MK-SUPP-IMPORTS","MK-SUPP-EXPORTS","MK-SUB-GROUP-ARR","MK-ITEM-ARR","MK-SUB-GROUP-DESP","MK-ITEM-DESP","MK-SUB-GROUP-IMP","MK-ITEM-IMP","MK-SUB-GROUP-EXP","MK-ITEM-EXP","MK-QTY1-ALPHA","MK-QTY2-ALPHA","MK-COMMODITY-ALPHA-1")


## File Downloads =============================================================
tryCatch({ suppressWarnings(
    download.file(paste0("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/smke19", syr, smth, ".zip"), paste0("smke19", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smke19", syr, smth, ".zip"))})

tryCatch({ suppressWarnings(
    download.file(paste0("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/smki19", syr, smth, ".zip"), paste0("smki19", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smki19", syr, smth, ".zip"))})

tryCatch({ suppressWarnings(
    download.file(paste0("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/smkx46", syr, smth, ".zip"), paste0("smkx46", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smkx46", syr, smth, ".zip"))})

tryCatch({ suppressWarnings(
    download.file(paste0("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/smkm46", syr, smth, ".zip"), paste0("smkm46", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smkm46", syr, smth, ".zip"))})

tryCatch({ suppressWarnings(
    download.file(paste0("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/smka12", syr, smth, ".zip"), paste0("smka12", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smka12", syr, smth, ".zip"))})

# Unzip monthly files --------------------------------------------------------
unzip(paste0("smke19", syr, smth, ".zip"), exdir = getwd())
unzip(paste0("smki19", syr, smth, ".zip"), exdir = getwd())
unzip(paste0("smkx46", syr, smth, ".zip"), exdir = getwd())
unzip(paste0("smkm46", syr, smth, ".zip"), exdir = getwd())
unzip(paste0("smka12", syr, smth, ".zip"), exdir = getwd())


# Cleanup --------------------------------------------------------------------

# Delete the zipfiles
zipfiles <- list.files(getwd(), pattern = ".zip")
sapply(zipfiles, unlink)

# Make all uppercase
datafiles <- list.files(getwd())
datafiles <- sapply(datafiles, FUN = function(up) {
  file.rename(from = up, to = toupper(up))
})

# Remove .txt suffix
datafiles <- list.files(getwd())
datafiles <- sapply(datafiles, FUN = function(txt) {
  file.rename(from = txt, to = sub(pattern = ".TXT", replacement = "", txt))
})


# Load to Database =============================================================

# Check if Data Present in Databases, load if not. -----------------------------
# This is done by checking whether there are 50+ records for that month already present in the database.

# DISPATCHES
dispQuery <- paste0("SELECT COUNT(smk_period_reference) FROM dispatches WHERE smk_period_reference = '020", syr, smth, "'")
dispexisting <- dbGetQuery(tradedata, dispQuery)

if (dispexisting[1,1] <= 50) {

    print(paste("Loading dispatches:", syr, smth))
    
    dispatches <- read.table(paste(files["disp"], syr, smth, sep = ""), sep = "|", skip = 1, col.names = eutradecols, fill = TRUE, colClasses = "character")
    colnames(dispatches) = dbSafeNames(colnames(dispatches))
    dispatches <- dispatches[-length(dispatches$smk_comcode),]
    dispatches$smk_comcode <- substr(dispatches$smk_comcode, 1, 8)

    tryCatch({
        dbWriteTable(tradedata,'dispatches', dispatches, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["disp"], syr, smth, sep = ""))})
    
} else {
    print(paste0("Dispatches 20", syr, "/", smth, " data exists in database"))
}

# ARRIVALS
arrQuery <- paste0("SELECT COUNT(smk_period_reference) FROM arrivals WHERE smk_period_reference = '020", syr, smth, "'")
arrexisting <- dbGetQuery(tradedata, arrQuery)

if (arrexisting[1,1] <= 50) {
     
    print(paste("Loading arrivals:", syr, smth))

    arrivals <- read.table(paste(files["arr"], syr, smth, sep = ""), sep = "|", skip = 1, col.names = eutradecols, fill = TRUE, colClasses = "character")
    colnames(arrivals) = dbSafeNames(colnames(arrivals))
    arrivals <- arrivals[-length(arrivals$smk_comcode),]
    arrivals$smk_comcode <- substr(arrivals$smk_comcode, 1, 8)

    tryCatch({
        dbWriteTable(tradedata,'arrivals', arrivals, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["arr"], syr, smth, sep = ""))})
   
} else {
    print(paste0("Arrivals 20", syr, "/", smth, " data exists in database"))
}

# EXPORTS
expQuery <- paste0("SELECT COUNT(account_date) FROM exports WHERE account_date= '", smth,"/20", syr, "'")
expexisting <- dbGetQuery(tradedata, expQuery)

if (expexisting[1,1] <= 50) {
    
    print(paste("Loading exports:", syr, smth))

    exports <- read.table(paste(files["exp"], syr, smth, sep = ""), sep = "|", skip = 1, col.names = noneuexportcols, fill = TRUE, colClasses = "character")
    colnames(exports) = dbSafeNames(colnames(exports))
    exports <- exports[-length(exports$comcode),]
    exports$comcode <- substr(exports$comcode, 1, 8)

    tryCatch({
        dbWriteTable(tradedata,'exports', exports, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["exp"], syr, smth, sep = ""))})
    
} else {
    print(paste0("Exports 20", syr, "/", smth, " data exists in database"))
}

# IMPORTS
impQuery <- paste0("SELECT COUNT(account_date) FROM imports WHERE account_date= '", smth,"/20", syr, "'")
impexisting <- dbGetQuery(tradedata, impQuery)

if (impexisting[1,1] <= 50) {
    
    print(paste("Processing imports:", syr, smth))
    imports <- read.table(paste(files["imp"], syr, smth, sep = ""), sep = "|", skip = 1, col.names = noneuimportcols, fill = TRUE, colClasses = "character")
    colnames(imports) = dbSafeNames(colnames(imports))
    imports <- imports[-length(imports$comcode),]
    imports$comcode <- substr(imports$comcode, 1, 8)

    tryCatch({
        dbWriteTable(tradedata,'imports', imports, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["imp"], syr, smth, sep = ""))})
} else {
    print(paste0("Imports 20", syr, "/", smth, " data exists in database"))
}


# Note that there is no need to check for existing data, as this is already UPSERTing line by line.

print(paste("Loading commodity codes: ", syr, smth))

tryCatch({
    control <- read.table(paste(files["control"], syr, smth, sep = ""), sep = "|", skip = 1, quote = NULL, fill = TRUE, strip.white = TRUE, colClasses = "character")
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
    for (k in 1:length(control$mk_comcode)) {
        valstring1 <- paste(sapply(control[k,], paste, ", \'", sep = "\'"), sep = "", collapse = "")
        valstring2 <- paste("\'", substr(valstring1, 1, nchar(valstring1)-3), sep = "")
        sqlQuery <- paste(
            "INSERT INTO control VALUES (",
            valstring2,
            ") ON CONFLICT (mk_comcode) DO UPDATE SET mk_commodity_alpha_1 = ", 
            paste("\'", control$mk_commodity_alpha_1[k], "\'", sep = ""),
            ";", sep = "")
        dbGetQuery(tradedata,sqlQuery)
        }
    }, error = function(e){errors <<- c(errors, paste(files["control"], syr, smth, sep = ""))}
)   

# Now clear out datafiles/ ---------------------------------------------------
datafiles <- list.files(getwd())
sapply(datafiles, unlink)

# Print info =================================================================
end <- Sys.time()

print(paste("Time taken:", end - start))
print("The following files were not found:")
print(errors)

# END SCRIPT #################################################################
