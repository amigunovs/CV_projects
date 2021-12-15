setwd("C:/Users/Андрей/Desktop")

if(!"pdftools" %in% installed.packages()) install.packages("pdftools")
if(!"magick" %in% installed.packages()) install.packages("magick")
if(!"stringr" %in% installed.packages()) install.packages("stringr")
if(!"readr" %in% installed.packages()) install.packages("readr")
if(!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if(!"xlsx" %in% installed.packages()) install.packages("xlsx")
if(!"splitstackshape" %in% installed.packages()) install.packages("splitstackshape")


if(!"splitstackshape" %in% search()) library("splitstackshape")
if(!"pdftools" %in% search()) library("pdftools")
if(!"magick" %in% search()) library("magick")
if(!"stringr" %in% search()) library("stringr")
if(!"readr" %in% search()) library("readr")
if(!"tidyverse" %in% search()) library("tidyverse")
if(!"xlsx" %in% search()) library("xlsx")

url <- "C:/Users/Андрей/Desktop/TSLA.pdf"

file <- pdf_text(url)


pdf_subset(url, pages = 68, output = "intro2.pdf")

file2 <- pdf_text("C:/Users/Андрей/Desktop/intro2.pdf")

filesplit <- c(unlist(strsplit(file2[[1]], "\n"))) 

for (i in seq_along(filesplit)) {
  
  filesplit[i] <- gsub(",", "", filesplit[i])
  filesplit[i] <- gsub("\\(", "-", filesplit[i])
  filesplit[i] <- gsub("\\)", "", filesplit[i])
  filesplit[i] <- gsub("\\$", "", filesplit[i])
}

file3 <- as.data.frame(filesplit)
file4 <- cSplit(file3, 'filesplit', sep="   ", type.convert=FALSE)

file4[5,3] <- file4[5,2]
file4[6,3] <- file4[6,2]
file4[6,2] <- file4[6,1]
file4[5:6,1] <- NA

frame <- as.data.frame(file4)
write.xlsx(frame, "file.xlsx", sheetName = "Sheet1", 
           col.names = FALSE, row.names = FALSE, append = TRUE, showNA = FALSE)

wb <- loadWorkbook("file.xlsx")
sheets <- getSheets(wb)
# autosize column widths
autoSizeColumn(sheets[[1]], colIndex=1)
saveWorkbook(wb,"file.xlsx")
