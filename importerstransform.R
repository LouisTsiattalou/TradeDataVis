#import
library(tidyr)
library(stringr)

#FUNCTION DEFINITIONS

# dbSafeNames - all lower case, unique, underscored. No illegal chars.
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

# CODES

codes <- read.delim("SMKA121706.txt",header = FALSE, quote = "", sep = "|", colClasses = c(rep("character",times = 30)))
codes$V27 <- trimws(codes$V27,which = "right")
codes <- codes[,c("V1","V27")]
codes <- codes[-c(1,length(codes$V1)),]
colnames(codes) <- c("Code","Description")
rownames(codes) <- NULL
codes$Code <- lapply(codes$Code,substr,start = 1, stop = 8)
codes$Description <- sapply(codes$Description, function(x){
  if( grepl("\"", x) & !grepl("ompensated", x) ){
    x <- sub(" \".*\"", "", x)
  } else {
    x
  }
})



#IMPORTERS

importers <- read.delim("importers1706..txt", header = FALSE, quote = "\"", na.strings=c("", "NA"), colClasses = c(rep("character",times = 59)))
importers <- importers[!is.na(importers$V1),]
importers <- importers[-c(2,4:8)]
colnames(importers) <- c("Period","Company","Postcode","Com1","Com2","Com3","Com4","Com5","Com6","Com7","Com8","Com9","Com10","Com11","Com12","Com13","Com14","Com15","Com16","Com17","Com18","Com19","Com20","Com21","Com22","Com23","Com24","Com25","Com26","Com27","Com28","Com29","Com30","Com31","Com32","Com33","Com34","Com35","Com36","Com37","Com38","Com39","Com40","Com41","Com42","Com43","Com44","Com45","Com46","Com47","Com48","Com49","Com50")
importers$Period <- as.character(importers$Period)
importers$Period <- lapply(importers$Period,function(x){paste(substr(x,1,4),substr(x,5,6),sep="-")})
importers <- separate(importers,"Period",c("Year","Month"),sep="-")
importers <- gather(importers, "Com1","Com2","Com3","Com4","Com5","Com6","Com7","Com8","Com9","Com10","Com11","Com12","Com13","Com14","Com15","Com16","Com17","Com18","Com19","Com20","Com21","Com22","Com23","Com24","Com25","Com26","Com27","Com28","Com29","Com30","Com31","Com32","Com33","Com34","Com35","Com36","Com37","Com38","Com39","Com40","Com41","Com42","Com43","Com44","Com45","Com46","Com47","Com48","Com49","Com50", key = "Company", value = "Code", na.rm = TRUE)
importers <- importers[-5]
importers <- importers[order(importers$Company),]
rownames(importers) <- NULL

# Proof importers commodity codes are insoluble - some are ambiguous as to
# which commodity code in codes it matches with (ie:two matches)
importers2 <- importers[nchar(importers$Code) > 2,]
#my_func <- function(x){n<-grep(x,codes$Code); codes$Code[n];print(c(x,codes$Code[n],n))}
my_func <- function(x){n<-grep(x,codes$Code)}
for(i in 1:length(importers2$Code)) {
  importers2$Code[i] <- my_func(importers2$Code[i])[1]
}
# Press esc before it finishes and look at results - more than one grep match
# between importers$Code and codes$Code. Ambiguity kills the ability to link.
# Possible solution - ask the hmrc data curator why this is?
# ANSWER - COMCODES ARE 8 DIGITS, CODES HAS TRAILING 0 ON EVERY COMCODE FOR SOME REASON


#EXPORTERS

exporters <- read.delim("exporters1706..txt", header = FALSE, quote = "\"", na.strings=c("", "NA"), colClasses = c(rep("character",times = 59)))
exporters <- exporters[!is.na(exporters$V1),]
exporters <- exporters[-c(2,4:8)]
colnames(exporters) <- c("Period","Company","Postcode","Com1","Com2","Com3","Com4","Com5","Com6","Com7","Com8","Com9","Com10","Com11","Com12","Com13","Com14","Com15","Com16","Com17","Com18","Com19","Com20","Com21","Com22","Com23","Com24","Com25","Com26","Com27","Com28","Com29","Com30","Com31","Com32","Com33","Com34","Com35","Com36","Com37","Com38","Com39","Com40","Com41","Com42","Com43","Com44","Com45","Com46","Com47","Com48","Com49","Com50")
exporters$Period <- as.character(exporters$Period)
exporters$Period <- lapply(exporters$Period,function(x){paste(substr(x,1,4),substr(x,5,6),sep="-")})
exporters <- separate(exporters,"Period",c("Year","Month"),sep="-")
exporters <- gather(exporters, "Com1","Com2","Com3","Com4","Com5","Com6","Com7","Com8","Com9","Com10","Com11","Com12","Com13","Com14","Com15","Com16","Com17","Com18","Com19","Com20","Com21","Com22","Com23","Com24","Com25","Com26","Com27","Com28","Com29","Com30","Com31","Com32","Com33","Com34","Com35","Com36","Com37","Com38","Com39","Com40","Com41","Com42","Com43","Com44","Com45","Com46","Com47","Com48","Com49","Com50", key = "Company", value = "Code", na.rm = TRUE)
exporters <- exporters[-5]
exporters <- exporters[order(exporters$Company),]
rownames(exporters) <- NULL