pg = dbDriver("PostgreSQL")

tradedata = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

dbWriteTable(tradedata, 'control', control, row.names=FALSE)
dbSendQuery(tradedata, "delete from control")

yrs <- as.character(sprintf("%02d", c(17:09)))
mths <- as.character(sprintf("%02d",c(12:01)))

errors <- character(0)

for (i in yrs) {
  for (j in mths) {
    
    print(paste("Processing commodity codes: ", i, j))
    
    tryCatch({
      control <- read.table(paste(files["control"], i, j, sep = ""), sep = "|", skip = 1, fill = TRUE, colClasses = "character")
      fixedCols <- length(controlfilecols)
      actualCols <- ncol(control)
      if (fixedCols != actualCols) {
        control <- unite(control, "newcol", "V27", "V28", sep = "")}
      colnames(control) <- controlfilecols
      colnames(control) = dbSafeNames(colnames(control))
      control <- control[-length(control$mk_comcode), 1:fixedCols]
      control$mk_comcode <- substr(control$mk_comcode, 1, 8)
      control$mk_commodity_alpha_1 <- trimws(control$mk_commodity_alpha_1, "r")
      dbWriteTable(tradedata,'control', control, row.names=FALSE, append = TRUE)
    }, error = function(e){errors <<- c(errors, paste(files["control"], i, j, sep = ""))}
    )  

  }
}
