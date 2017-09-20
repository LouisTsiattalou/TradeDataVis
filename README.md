# TradeDataVis

## Contents

1. Introduction 
2. Live R Scripts
3. ToDo
4. 


## Introduction

Welcome to my TradeDataVis project. This project is an online tool developed in Shiny and R, running off a PostgreSQL database, which visualises the UK's import/export of food products to non-EU and EU countries. The underlying data can be found by visiting the [UKTradeInfo]http://uktradeinfo.com website.

## Live R Scripts

### Data Cleaning and Setup
#### Unzip.R
This is an R Script which automatically downloads and unzips trade data from the UKTradeInfo website.

#### Importers.R?
This code creates a dataframe containing correct codes and cleaned importers/exporters data. This is old and needs to be looked at and incorporated into _Make2015db.R_ and _Unzip.R_.

### Data Loading
#### Make2015db.R
A PostgreSQL database must be defined prior to running this script. It loads the unzipped files from the working directory used in Unzip.R into a PostgreSQL database defined in the script. Note that the tables need not be defined before the running of this script, the postgres database just needs to be defined and the correct details entered.

## ToDo
* Adapt _Unzip.R_ and _Make2015db.R_ to include importers and codes data.
* Develop shiny app.



