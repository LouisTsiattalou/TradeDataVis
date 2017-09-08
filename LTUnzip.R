# 20170904
# Script to download and process hmrc trade data from uktradeinfo.com.
# Written by David Lee at DEFRA
# Adapted by Louis Tsiattalou for Imports Tool

# TODO
# Create importer/exporter data code

# SCRIPT START ###############################################################

# Constant setup =============================================================

# Set working directory
setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/")
suppressWarnings(dir.create(paste(getwd(), "datafiles", sep = "")))
setwd("datafiles")

start <- Sys.time()
errors <- character()

# Download annual archives
years <- c(2009:2017)


# File Downloads =============================================================
for (i in years ) {
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKE19_", i , "archive.zip", sep = ""), paste("SMKE19_", i, "archive.zip", sep = ""))
  )},error = function(e){errors <<- c(errors, paste("SMKE19_",i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKI19_", i , "archive.zip", sep = ""), paste("SMKI19_", i, "archive.zip", sep = ""))
  )},error = function(e){errors <<- c(errors, paste("SMKI19_",i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKX46_", i , "archive.zip", sep = ""), paste("SMKX46_", i, "archive.zip", sep = ""))
  )},error = function(e){errors <<- c(errors, paste("SMKX46_",i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKM46_", i , "archive.zip", sep = ""), paste("SMKM46_", i, "archive.zip", sep = ""))
  )},error = function(e){errors <<- c(errors, paste("SMKM46_",i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SMKA12_", i , "archive.zip", sep = ""), paste("SMKA12_", i, "archive.zip", sep = ""))
  )},error = function(e){errors <<- c(errors, paste("SMKA12_",i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SIAI11_", i , "archive.zip", sep = ""), paste("SIAI11_", i, "archive.zip", sep = ""))
  )},error = function(e){errors <<- c(errors, paste("SIAI11_",i,"archive.zip", sep = ""))})
  
#  tryCatch({ suppressWarnings(
#    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SESX16_", i , "archive.zip", sep = ""), paste("SESX16_", i, "archive.zip", sep = ""))
#  )},error = function(e){errors <<- c(errors, paste("SESX16_",i,"archive.zip", sep = ""))})
#  
#  tryCatch({ suppressWarnings(
#    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/SESM16_", i , "archive.zip", sep = ""), paste("SESM16_", i, "archive.zip", sep = ""))
#  )},error = function(e){errors <<- c(errors, paste("SESM16_",i,"archive.zip", sep = ""))})
  }

# Special Cases --------------------------------------------------------------

# 2011 SIAI missing the 11 in SIAI11_yyyyarchive
download.file("https://www.uktradeinfo.com/Statistics/Documents/SIAI_2011archive.zip", "SIAI11_2011archive.zip")

# 2016 all files have been split into half years for some reason. Need JulDec files too!
download.file("https://www.uktradeinfo.com/Statistics/Documents/SMKE19_2016archive_JulDec.zip", "SMKE19_2016archive_JulDec.zip")
download.file("https://www.uktradeinfo.com/Statistics/Documents/SMKI19_2016archive_JulDec.zip", "SMKI19_2016archive_JulDec.zip")
download.file("https://www.uktradeinfo.com/Statistics/Documents/SMKX46_2016archive_JulDec.zip", "SMKX46_2016archive_JulDec.zip")
download.file("https://www.uktradeinfo.com/Statistics/Documents/SMKM46_2016archive_JulDec.zip", "SMKM46_2016archive_JulDec.zip")
download.file("https://www.uktradeinfo.com/Statistics/Documents/SMKA12_2016archive_JulDec.zip", "SMKA12_2016archive_JulDec.zip")
#download.file("https://www.uktradeinfo.com/Statistics/Documents/SESX16_2016archive_JulDec.zip", "SESX16_2016archive_JulDec.zip")
#download.file("https://www.uktradeinfo.com/Statistics/Documents/SESM16_2016archive_JulDec.zip", "SESM16_2016archive_JulDec.zip")

# Unzip scripts ==============================================================

# Annual archives ------------------------------------------------------------
for (i in years) {
  unzip(paste("SMKE19_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKI19_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKX46_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKM46_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKA12_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SIAI11_", i, "archive.zip", sep = ""), exdir = getwd())
#  unzip(paste("SESX16_", i, "archive.zip", sep = ""), exdir = getwd())
#  unzip(paste("SESM16_", i, "archive.zip", sep = ""), exdir = getwd())
  }

# Special Annual Cases -------------------------------------------------------
  unzip("SMKE19_2016archive_JulDec.zip", exdir = getwd())
  unzip("SMKI19_2016archive_JulDec.zip", exdir = getwd())
  unzip("SMKX46_2016archive_JulDec.zip", exdir = getwd())
  unzip("SMKM46_2016archive_JulDec.zip", exdir = getwd())
  unzip("SMKA12_2016archive_JulDec.zip", exdir = getwd())
  unzip("SIAI11_2016archive_JulDec.zip", exdir = getwd())
#  unzip("SESX16_2016archive_JulDec.zip", exdir = getwd())
#  unzip("SESM16_2016archive_JulDec.zip", exdir = getwd())


# Unzip monthly files --------------------------------------------------------
syrs <- as.character(sprintf("%02d",c(9:17)))
smths <- as.character(sprintf("%02d",c(1:12)))

for (i in syrs) {
  for (j in smths) {
    unzip(paste("SMKE19", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SMKI19", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SMKX46", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SMKM46", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SMKA12", i, j, ".zip", sep = ""), exdir = getwd())
    unzip(paste("SIAI11", i, j, ".zip", sep = ""), exdir = getwd())
#    unzip(paste("SESX16", i, j, ".zip", sep = ""), exdir = getwd())   
#    unzip(paste("SESM16", i, j, ".zip", sep = ""), exdir = getwd())    
  }
}

# Convoluted way for special ~1 case in Feb 2009 - otherwise just ignored!
dirFeb09 <- "0902"
fullDirFeb09 <- paste(getwd(),"/", dirFeb09, sep = "")
suppressWarnings(dir.create(dirFeb09))
unzip(paste("SMKE190902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SMKI190902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SMKX460902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SMKM460902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SMKA120902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SIAI110902.zip", sep = ""), exdir = fullDirFeb09)
datafiles <- list.files(fullDirFeb09, full.names = TRUE)
# Double Sub - replace ~1 with 0902, and move to working directory. Then delete folder.
sapply(datafiles, FUN = function(txt) {
  file.rename(
    from = txt,
    to = sub(fullDirFeb09, getwd(), sub(pattern = "~1", replacement = "0902", txt))
    )
})
unlink(fullDirFeb09, recursive = TRUE)

# Cleanup =====================================================================

# Delete the zipfiles
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


# Print info =================================================================
end <- Sys.time()

print(paste("Time taken:", end - start))
print("The following files were not found:")
print(errors)

# END SCRIPT #################################################################