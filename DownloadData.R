# 20170904
# Updated 20171212
# Script to download and process hmrc trade data from uktradeinfo.com.
# Base script by David Lee at DEFRA
# Adapted by Louis Tsiattalou for Trade Data Visualisation project
# Github: https://github.com/LouisTsiattalou/TradeDataVis

# SCRIPT START ###############################################################

# Constant setup =============================================================

# Set working directory
setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/")
suppressWarnings(dir.create(paste(getwd(), "/datafiles", sep = "")))
setwd("datafiles")

start <- Sys.time()
errors <- character()

years <- c(2009:2018)
# File Downloads =============================================================
for (i in years ) {
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKE19_", i , "archive.zip", sep = ""), paste("SMKE19_", i, "archive.zip", sep = ""))
  )}, error = function(e){errors <<- c(errors, paste("SMKE19_", i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKI19_", i , "archive.zip", sep = ""), paste("SMKI19_", i, "archive.zip", sep = ""))
  )}, error = function(e){errors <<- c(errors, paste("SMKI19_", i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKX46_", i , "archive.zip", sep = ""), paste("SMKX46_", i, "archive.zip", sep = ""))
  )}, error = function(e){errors <<- c(errors, paste("SMKX46_", i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKM46_", i , "archive.zip", sep = ""), paste("SMKM46_", i, "archive.zip", sep = ""))
  )}, error = function(e){errors <<- c(errors, paste("SMKM46_", i,"archive.zip", sep = ""))})
  
  tryCatch({ suppressWarnings(
    download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKA12_", i , "archive.zip", sep = ""), paste("SMKA12_", i, "archive.zip", sep = ""))
  )}, error = function(e){errors <<- c(errors, paste("SMKA12_", i,"archive.zip", sep = ""))})

# 2016/17 all files have been split into half years for some reason. Need JulDec files too!
    
  if (i >= 2016) {
    tryCatch({ suppressWarnings(
      download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKE19_", i , "archive_JulDec.zip", sep = ""), paste("SMKE19_", i, "archive_JulDec.zip", sep = ""))
    )}, error = function(e){errors <<- c(errors, paste("SMKE19_", i,"archive_JulDec.zip", sep = ""))})
      
    tryCatch({ suppressWarnings(
      download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKI19_", i , "archive_JulDec.zip", sep = ""), paste("SMKI19_", i, "archive_JulDec.zip", sep = ""))
    )}, error = function(e){errors <<- c(errors, paste("SMKI19_", i,"archive_JulDec.zip", sep = ""))})
      
    tryCatch({ suppressWarnings(
      download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKX46_", i , "archive_JulDec.zip", sep = ""), paste("SMKX46_", i, "archive_JulDec.zip", sep = ""))
    )}, error = function(e){errors <<- c(errors, paste("SMKX46_", i,"archive_JulDec.zip", sep = ""))})
      
    tryCatch({ suppressWarnings(
      download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKM46_", i , "archive_JulDec.zip", sep = ""), paste("SMKM46_", i, "archive_JulDec.zip", sep = ""))
    )}, error = function(e){errors <<- c(errors, paste("SMKM46_", i,"archive_JulDec.zip", sep = ""))})
      
    tryCatch({ suppressWarnings(
      download.file(paste("https://www.uktradeinfo.com/Statistics/Documents/Data%20Downloads/SMKA12_", i , "archive_JulDec.zip", sep = ""), paste("SMKA12_", i, "archive_JulDec.zip", sep = ""))
    )}, error = function(e){errors <<- c(errors, paste("SMKA12_", i,"archive_JulDec.zip", sep = ""))})
       
  }    
}


# Unzip scripts ==============================================================

# Annual archives ------------------------------------------------------------
for (i in years) {
  unzip(paste("SMKE19_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKI19_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKX46_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKM46_", i, "archive.zip", sep = ""), exdir = getwd())
  unzip(paste("SMKA12_", i, "archive.zip", sep = ""), exdir = getwd())
  if (i >= 2016) {
    unzip(paste("SMKE19_", i, "archive_JulDec.zip", sep = ""), exdir = getwd())
    unzip(paste("SMKI19_", i, "archive_JulDec.zip", sep = ""), exdir = getwd())
    unzip(paste("SMKX46_", i, "archive_JulDec.zip", sep = ""), exdir = getwd())
    unzip(paste("SMKM46_", i, "archive_JulDec.zip", sep = ""), exdir = getwd())
    unzip(paste("SMKA12_", i, "archive_JulDec.zip", sep = ""), exdir = getwd())
  }
}

# Unzip monthly files --------------------------------------------------------

# Convoluted way for special ~1 case in Feb 2009 - otherwise just ignored!
# Has to be done first as Windows machines cry when they see a tilde.
dirFeb09 <- "0902"
fullDirFeb09 <- paste(getwd(),"/", dirFeb09, sep = "")
suppressWarnings(dir.create(dirFeb09))
unzip(paste("SMKE190902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SMKI190902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SMKX460902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SMKM460902.zip", sep = ""), exdir = fullDirFeb09)
unzip(paste("SMKA120902.zip", sep = ""), exdir = fullDirFeb09)

# Double Sub - replace ~1 with 0902, and move to working directory. Then delete folder.
datafiles <- list.files(fullDirFeb09, full.names = TRUE)
sapply(datafiles, FUN = function(txt) {
  file.rename(
    from = txt,
    to = sub(fullDirFeb09, getwd(), sub(pattern = "~1", replacement = "0902", txt))
    )
})
unlink(fullDirFeb09, recursive = TRUE)

# Normal Cases
syrs <- as.character(sprintf("%02d",c(9:18)))
smths <- as.character(sprintf("%02d",c(1:12)))

for (i in syrs) {
  for (j in smths) {
    if (paste0(i, j) != "0902") {
      unzip(paste0("SMKE19", i, j, ".zip"), exdir = getwd())
      unzip(paste0("SMKI19", i, j, ".zip"), exdir = getwd())
      unzip(paste0("SMKX46", i, j, ".zip"), exdir = getwd())
      unzip(paste0("SMKM46", i, j, ".zip"), exdir = getwd())
      unzip(paste0("SMKA12", i, j, ".zip"), exdir = getwd())
    }
  }
}


# Cleanup =====================================================================

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

# Fix ~1 for Feb 2009
datafiles <- list.files(getwd())
datafiles <- sapply(datafiles, FUN = function(txt) {
  file.rename(from = txt, to = sub(pattern = "~1", replacement = "0902", txt))
})

# Fix V2 in May/June 2014
datafiles <- list.files(getwd())
datafiles <- sapply(datafiles, FUN = function(txt) {
  file.rename(from = txt, to = sub(pattern = "V2", replacement = "", txt))
})

# Final check for missing files
datafiles <- list.files(getwd())
for (i in syrs) {
  for (j in smths) {
    if (! paste0("SMKE19",i,j) %in% datafiles) {print(paste0("SMKE19",i,j," is missing!"))}
    if (! paste0("SMKI19",i,j) %in% datafiles) {print(paste0("SMKI19",i,j," is missing!"))}
    if (! paste0("SMKX46",i,j) %in% datafiles) {print(paste0("SMKX46",i,j," is missing!"))}
    if (! paste0("SMKM46",i,j) %in% datafiles) {print(paste0("SMKM46",i,j," is missing!"))}
    if (! paste0("SMKA12",i,j) %in% datafiles) {print(paste0("SMKA12",i,j," is missing!"))}
  }
}

# Print info =================================================================
end <- Sys.time()

print(paste("Time taken:", end - start))
print("The following files were not found:")
print(errors)

# END SCRIPT #################################################################
