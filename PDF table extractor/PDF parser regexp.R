###### Libraries #####

setwd("C:/Users/Андрей/Desktop")

if(!"pdftools" %in% installed.packages()) install.packages("pdftools")
if(!"magick" %in% installed.packages()) install.packages("magick")
if(!"stringr" %in% installed.packages()) install.packages("stringr")
if(!"readr" %in% installed.packages()) install.packages("readr")
if(!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if(!"xlsx" %in% installed.packages()) install.packages("xlsx")

if(!"pdftools" %in% search()) library("pdftools")
if(!"magick" %in% search()) library("magick")
if(!"stringr" %in% search()) library("stringr")
if(!"readr" %in% search()) library("readr")
if(!"tidyverse" %in% search()) library("tidyverse")
if(!"xlsx" %in% search()) library("xlsx")

##### Selecting the file and pages #####

# Selecting the path to the PDF file

url <- "C:/Users/Андрей/Desktop/TSLA.pdf"

file <- pdf_text(url)

# Selecting the page that and copying it into a new PDF file 

pdf_subset(url, pages = 68, output = "intro2.pdf")

file2 <- pdf_text("C:/Users/Андрей/Desktop/intro2.pdf")


##### Splitting the data into a table #####


# Unlisting the file and creating an empty matrix as large as the desired table
filesplit <- c(unlist(strsplit(file2[[1]], "\n"))) 
transition <- matrix(NA, nrow = length(filesplit), ncol = 3)
# table <- matrix(NA, nrow = length(filesplit), ncol = 3)
filesplit_init <- filesplit

# Writing a loop that at first removes special symbols and afterwards finds the desired combination of symbols using regular expressions
# Afterwards the resuts are saved into the matrix created in the previous step

for (i in seq_along(filesplit)) {
  
  filesplit[i] <- gsub(",", "", filesplit[i])
  filesplit[i] <- gsub("\\(", "-", filesplit[i])
  filesplit[i] <- gsub("\\)", "", filesplit[i])
  filesplit[i] <- gsub("\\$", "", filesplit[i])
  
  col1 <- filesplit[i]
  fands <- str_match(str_squish(filesplit[i]),"-*\\d+ -*\\d+$")
  if(is.na(fands) == TRUE) {
    fands <- str_match(str_squish(filesplit[i]),"— -*\\d+$")
  }
  if(is.na(fands) == TRUE) {
    fands <- str_match(str_squish(filesplit[i]),"-*\\d+ —")
  }
  
  col2 <- str_match(str_squish(fands),"-*\\d+")
  col3 <- str_match(str_squish(fands),"-*\\d+$")
  
  if(is.na(col3) == FALSE) {
    col1 <- str_remove(col1, col3)
  }
  if(is.na(col2) == FALSE) {
    col1 <- str_remove(col1, col2)
  }
  
  transition[i,1] <- toString(col1)
  transition[i,2] <- as.numeric(col2)
  transition[i,3] <- as.numeric(col3)
}
# 
# for (i in length(filesplit)) {
#   
#   col1 <- str_remove(transition[i,1], transition[i,2])
#   col2 <- as.numeric(str_match(str_squish(transition[i,2]),"-*\\d+"))
#   
#   table[i,1] <- col1
#   table[i,2] <- col2
#   
# }
# 
# col1 <- str_remove(transition[30,1], transition[30,2])
# col1
# transition[30,1]
# transition[30,2]
# 
# for (i in 1:length(filesplit)){
#   
#   filesplit[i] <- gsub(",", "", filesplit[i])
#   filesplit[i] <- gsub("\\(", "-", filesplit[i])
#   filesplit[i] <- gsub("\\)", "", filesplit[i])
#   filesplit[i] <- gsub("\\$", "", filesplit[i])
#   
#   col1 <- str_match(str_squish(filesplit[i]),"\\D+")
#   col2 <- str_match(str_squish(filesplit[i]),"-*\\d+ -*\\d+$")
#   col3 <- as.numeric(str_match(str_squish(filesplit[i]),"-*\\d+$"))
#   
#   table[i, 1] <- col1
#   table[i, 2] <- col2
#   table[i, 3] <- col3
# }

# filesplit[40]
# a <- toString(filesplit[40])
# gsub(a, " ", "")
# a
# table_df <- as.data.frame(table)
# names(table_df) <- c("name", "2019", "2018")
# a <- unlist(table_df["name"])
# table_df["name"] <- gsub("$", "", unlist(table_df["name"]))

filesplit_init

# Now the matrix can be rewritten into a data frame format and the respective excel sheet containing the data can be created

frame <- as.data.frame(transition)
write.xlsx(frame, "file.xlsx", sheetName = "Sheet1", 
           col.names = FALSE, row.names = FALSE, append = TRUE, showNA = FALSE)

wb <- loadWorkbook("file.xlsx")
sheets <- getSheets(wb)
# autosize column widths
autoSizeColumn(sheets[[1]], colIndex=1)
saveWorkbook(wb,"file.xlsx")
