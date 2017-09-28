# TradeDataVis

## Contents

1. Introduction 
2. Live R Scripts
3. Complexities
4. ToDo


## Introduction

Welcome to my TradeDataVis project. This project is an online tool developed in Shiny and R, running off a PostgreSQL database, which visualises the UK's import/export of food products to non-EU and EU countries. The underlying data can be found by visiting the [UKTradeInfo](http://uktradeinfo.com) website.


## Live R Scripts

### Data Cleaning and Setup
#### Unzip.R
This is an R Script which automatically downloads and unzips trade data from the UKTradeInfo website.

#### Importers.R?
This code creates a dataframe containing correct codes and cleaned importers/exporters data. This is old and needs to be looked at and incorporated into `Make2015db.R` and `Unzip.R`.

### Data Loading
#### Make2015db.R
A PostgreSQL database must be defined prior to running this script. It loads the unzipped files from the working directory used in `Unzip.R` into a PostgreSQL database defined in the script. Note that the tables need not be defined before the running of this script, the postgres database just needs to be defined and the correct details entered.


## Complexities

### Commodity Codes
Commodity Codes Control Files (SMKA_) contain some serious complexities. They are listed below in bulletted form. 
* Commodity Codes are obviously primary keys - you can't have the same commodity code contain completely different types of data! The way this is handled is that the commodity code is _added_ if it does not exist within the table. If it _already exists_, that entry is updated with the information in the current SMKA file. This method of adding/updating is referred to as `UPSERT` (portmanteau of update-insert). This has to be done using line-by-line SQL queries, as R's `DBI` package does not support UPSERT operations. As we consider SMKA files sequentially from 200901, we always have the most up-to-date description for each commodity code.
* SMKA files prior to 201201 have the SUB unicode character in one of the commodity code descriptions. All data analysis tools use this character as the EOF marker - stopping the dataload! This is an outstanding issue.
* Older pre-2012 SMKA files also split the description up with a | delimiter after it reaches a certain character limit for god knows what reason. SQL table limits pre-2012 maybe? I don't know. I do know that it's annoying to deal with. There's some lines which merge the final two columns in the data frame if they exceed the number of columns in the new data format to homogenise the data structures so everything can be loaded into the same table.
* Lastly, since the descriptions contain both " and ' chars, quoting is set to null for the `read.table` load. Apostrophes are all converted to double apostrophes `''` during the data cleaning routine, as SQL statements rely on the ' char for denoting strings!

## ToDo
* Adapt `Unzip.R` and `Make2015db.R` to include importers/exporters data.
* Solve error for old SMKA including the sub character for some reason.
* Develop shiny app.