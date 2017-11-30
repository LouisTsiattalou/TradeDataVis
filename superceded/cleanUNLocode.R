library(dplyr)
library(readr)

setwd("~/R/ImportTool/")

# Set URL and FileName
portlatlonURL <- "http://www.unece.org/fileadmin/DAM/cefact/locode/loc171csv.zip"
portlatlonFN <- "datafiles/LatLonPorts.zip"

# Download ZIP to datafiles, obtain list of items
download.file(portlatlonURL,portlatlonFN)
portlatlonlist <- unzip(portlatlonFN, list=TRUE)

# Unzip to datafiles, paste csvs together
unzip(portlatlonFN)
unlink(portlatlonFN)

# Read data and reshape
portlatlon <- portlatlonlist$Name[grepl("CodeList",portlatlonlist$Name)] %>% # Take Data only
    lapply(read_csv, col_names = FALSE) %>% # read them into R
    bind_rows %>% # Paste them together
    filter(X2 == "GB") %>% # Take only United Kingdom ports (GB)
    select(choices = c(X3,X5,X11)) # Select code, name, and lat/lon location

# Now have GB ports, by portcode,portname,lat/lon.
colnames(portlatlon) <- c("portcode","portname","latlon")

portlatlon <- portlatlon %>%
    mutate(lat = substr(latlon,1,4)) %>% # Obtain Latitude
    mutate(long = substr(latlon,nchar(latlon)-6,nchar(latlon)-1)) %>% # Obtain Longitude
    filter(!is.na(latlon))

# Convert to Numeric Vectors and Divide by 100 to get decimal form
portlatlon$lat <- as.numeric(portlatlon$lat) / 100
portlatlon$long <- as.numeric(portlatlon$long) / 100

# Multiply - North = +1, South = -1, West = -1, East = +1
portlatlon$lat <- ifelse(grep("N", portlatlon$latlon), portlatlon$lat*1, portlatlon$lat*-1)
portlatlon$long <- ifelse(grep("E", portlatlon$latlon), portlatlon$lat*1, portlatlon$lat*-1)

# Remove latlon column.
portlatlon <- select(portlatlon, -latlon)

# At this point, I realised that UN Locode had lots of ports missing that we actually needed.
# Big ones too - Felixstowe for example!
# I manually entered these into missingports.csv, which we append here and left-join into the dataframe
missingports <- read_csv("missingports.csv")
portlatlon <- 


# Delete unzipped files
unlink(portlatlonlist)


