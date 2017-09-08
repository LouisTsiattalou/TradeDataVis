# 20170904
# Script to create postgreSQL tables for import data
# Written by Louis Tsiattalou for Imports Tool

# TODO


# SCRIPT START ###############################################################
#install.packages("RPostgreSQL")
library('RPostgreSQL')

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
# Local Postgres database
con = dbConnect(pg, user="postgres", password="postgres",
                      host="localhost", port=5432, dbname="tradedata")

eutradecols <- c("SMK-COMCODE","SMK-RECORD-TYPE","SMK-COD-SEQ","SMK-COD-ALPHA","SMK-TRADE-IND","SMK-COO-SEQ","SMK-COO-ALPHA","SMK-NATURE-OF-TRANSACTION","SMK-MODE-OF-TRANSPORT","SMK-PERIOD-REFERENCE","SMK-SUITE INDICATOR","SMK-SITC","SMK-IP-COMCODE","SMK-NO-OF-CONSIGNMENTS","SMK-STAT-VALUE","SMK-NETT-MASS","SMK-SUPP-UNIT")
noneuexportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","VALUE","QUANTITY-1","QUANTITY-2","INDUSTRIAL-PLANT-COMCODE")
noneuimportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","COO-SEQUENCE","COO-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","COUNTRY-SEQUENCE-COO-IMP","COUNTRY-ALPHA-COO-IMP","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","CB-CODE","VALUE","QUANTITY-1","QUANTITY-2")
controlfilecols <- c("MK-COMCODE","MK-INTRA-EXTRA-IND","MK-INTRA-MMYY-ON","MK-INTRA-MMYY-OFF","MK-EXTRA-MMYY-ON","MK-EXTRA-MMYY-OFF","MK-NON-TRADE-ID","MK-SITC-NO","MK-SITC-IND","MK-SITC-CONV-A","MK-SITC-CONV-B","MK-CN-Q2","MK-SUPP-ARRIVALS","MK-SUPP-DESPATCHES","MK-SUPP-IMPORTS","MK-SUPP-EXPORTS","MK-SUB-GROUP-ARR","MK-ITEM-ARR","MK-SUB-GROUP-DESP","MK-ITEM-DESP","MK-SUB-GROUP-IMP","MK-ITEM-IMP","MK-SUB-GROUP-EXP","MK-ITEM-EXP","MK-QTY1-ALPHA","MK-QTY2-ALPHA","MK-COMMODITY-ALPHA-1")

eutradecols <- dbSafeNames(eutradecols)
noneuexportcols <- dbSafeNames(noneuexportcols)
noneuimportcols <- dbSafeNames(noneuimportcols)
controlfilecols <- dbSafeNames(controlfilecols)


# SQL COMMAND DEFINITIONS _AND_ EXECUTIONS FOR EACH TABLE ######################################

# EU DISPATCHES ###############################################################

# specifies the details of the table
dispatches <- paste("CREATE TABLE dispatches
(
  ", eutradecols[1] ," character(9) NOT NULL,
  ", eutradecols[2] ," character(1),
  ", eutradecols[3] ," character(3),
  ", eutradecols[4] ," character(2),  
  ", eutradecols[5] ," character(1),
  ", eutradecols[6] ," character(3),
  ", eutradecols[7] ," character(2),
  ", eutradecols[8] ," character(3),
  ", eutradecols[9] ," character(3),
  ", eutradecols[10] ," character(7),
  ", eutradecols[11] ," character(3),
  ", eutradecols[12] ," character(5),
  ", eutradecols[13] ," character(9),
  ", eutradecols[14] ," bigint,
  ", eutradecols[15] ," bigint,
  ", eutradecols[16] ," bigint,
  ", eutradecols[17] ," bigint
)
WITH (
OIDS=FALSE
);
ALTER TABLE dispatches
OWNER TO postgres;")
#CONSTRAINT dispatches_pkey PRIMARY KEY (", eutradecols[1], ")
# sends the command and creates the table
dbGetQuery(con, dispatches)

# EU ARRIVALS #################################################################

# specifies the details of the table
arrivals <- paste("CREATE TABLE arrivals
                    (
                    ", eutradecols[1] ," character(9) NOT NULL,
                    ", eutradecols[2] ," character(1),
                    ", eutradecols[3] ," character(3),
                    ", eutradecols[4] ," character(2),  
                    ", eutradecols[5] ," character(1),
                    ", eutradecols[6] ," character(3),
                    ", eutradecols[7] ," character(2),
                    ", eutradecols[8] ," character(3),
                    ", eutradecols[9] ," character(3),
                    ", eutradecols[10] ," character(7),
                    ", eutradecols[11] ," character(3),
                    ", eutradecols[12] ," character(5),
                    ", eutradecols[13] ," character(9),
                    ", eutradecols[14] ," bigint,
                    ", eutradecols[15] ," bigint,
                    ", eutradecols[16] ," bigint,
                    ", eutradecols[17] ," bigint
                    )
                    WITH (
                    OIDS=FALSE
                    );
                    ALTER TABLE arrivals
                    OWNER TO postgres;")
# sends the command and creates the table
dbGetQuery(con, arrivals)

# NON-EU EXPORTS ##############################################################

# specifies the details of the table
exports <- paste("CREATE TABLE exports
                   (
                   ", noneuexportcols[1] ," character(9) NOT NULL,
                   ", noneuexportcols[2] ," character(5),
                   ", noneuexportcols[3] ," character(3),
                   ", noneuexportcols[4] ," character(3),  
                   ", noneuexportcols[5] ," character(2),
                   ", noneuexportcols[6] ," character(7),
                   ", noneuexportcols[7] ," character(3),
                   ", noneuexportcols[8] ," character(3),
                   ", noneuexportcols[9] ," character(3),
                   ", noneuexportcols[10] ," character(2),
                   ", noneuexportcols[11] ," character(1),
                   ", noneuexportcols[12] ," character(3),
                   ", noneuexportcols[13] ," character(3),
                   ", noneuexportcols[14] ," character(2),
                   ", noneuexportcols[15] ," character(3),
                   ", noneuexportcols[16] ," character(3),
                   ", noneuexportcols[17] ," character(3),
                   ", noneuexportcols[18] ," character(3),
                   ", noneuexportcols[19] ," bigint,
                   ", noneuexportcols[20] ," bigint,
                   ", noneuexportcols[21] ," bigint,  
                   ", noneuexportcols[22] ," character(15)
                   )
                   WITH (
                   OIDS=FALSE
                   );
                   ALTER TABLE exports
                   OWNER TO postgres;")
# sends the command and creates the table
dbGetQuery(con, exports)


# NON-EU IMPORTS ##############################################################

# specifies the details of the table
imports <- paste("CREATE TABLE imports
                 (
                 ", noneuimportcols[1] ," character(9) NOT NULL,
                 ", noneuimportcols[2] ," character(5),
                 ", noneuimportcols[3] ," character(3),
                 ", noneuimportcols[4] ," character(3),  
                 ", noneuimportcols[5] ," character(2),
                 ", noneuimportcols[6] ," character(3),
                 ", noneuimportcols[7] ," character(2),
                 ", noneuimportcols[8] ," character(7),
                 ", noneuimportcols[9] ," character(3),
                 ", noneuimportcols[10] ," character(3),
                 ", noneuimportcols[11] ," character(3),
                 ", noneuimportcols[12] ," character(2),
                 ", noneuimportcols[13] ," character(3),
                 ", noneuimportcols[14] ," character(2),
                 ", noneuimportcols[15] ," character(1),
                 ", noneuimportcols[16] ," character(3),
                 ", noneuimportcols[17] ," character(3),
                 ", noneuimportcols[18] ," character(2),
                 ", noneuimportcols[19] ," character(3),
                 ", noneuimportcols[20] ," character(3),
                 ", noneuimportcols[21] ," character(3),  
                 ", noneuimportcols[22] ," character(3),
                 ", noneuimportcols[23] ," character(3),
                 ", noneuimportcols[24] ," bigint,
                 ", noneuimportcols[25] ," bigint,  
                 ", noneuimportcols[26] ," bigint
                 )
                 WITH (
                 OIDS=FALSE
                 );
                 ALTER TABLE imports
                 OWNER TO postgres;")
# sends the command and creates the table
dbGetQuery(con, imports)