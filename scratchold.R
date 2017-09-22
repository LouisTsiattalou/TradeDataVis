pg = dbDriver("PostgreSQL")

tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

dbWriteTable(tradedata, 'control', control, row.names=FALSE)
dbSendQuery(tradedata, "delete from control")

yrs <- as.character(sprintf("%02d", c(09:17)))
mths <- as.character(sprintf("%02d",c(01:12)))

errors <- character(0)

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
