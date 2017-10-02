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

control <- read.table(paste(files["control"], "1206", sep = ""), sep = "|", skip = 1, col.names = controlfilecols, fill = TRUE, colClasses = "character")
colnames(control) = dbSafeNames(colnames(control))
control <- control[-length(control$mk_comcode),]
control$mk_comcode <- substr(control$mk_comcode, 1, 8)

pg = dbDriver("PostgreSQL")

tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

dbWriteTable(tradedata, 'control', control, row.names=FALSE)
dbSendQuery(tradedata, "SET client_encoding = 'LATIN1'")
dbSendQuery(tradedata, "delete from control")

yrs <- as.character(sprintf("%02d", c(09:17)))
mths <- as.character(sprintf("%02d",c(01:12)))

errors <- character()

for (i in yrs) {
  for (j in mths) {
    
    print(paste("Processing commodity codes: ", i, j))
    
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
  }
}
