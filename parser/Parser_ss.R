# Libraries

if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"rvest" %in% installed.packages()) install.packages("rvest")
if(!"XML" %in% installed.packages()) install.packages("XML")
if(!"httr" %in% installed.packages()) install.packages("httr")
if(!"stringr" %in% installed.packages()) install.packages("stringr")
if(!"stringi" %in% installed.packages()) install.packages("stringi")

if(!"dplyr" %in% search()) library("dplyr")
if(!"rvest" %in% search()) library("rvest")
if(!"XML" %in% search()) library("XML")
if(!"httr" %in% search()) library("httr")
if(!"stringr" %in% search()) library("stringr")
if(!"stringi" %in% search()) library("stringi")

# Link with the apartments for sale 

link <- "https://www.ss.lv/lv/real-estate/flats/riga/all/sell/"

qi_webpage <- read_html(link)

# Extracting and tidying the first table page and preparing the data frame to append observations

qi_table <- html_nodes(qi_webpage, 'table')
qi1 <- html_table(qi_table, header = FALSE)[[1]]
qi1 <- qi1[-c(1,2, 10:length(names(qi1)))]
qi1 <- qi1[-c(1:5),]
qi1 <- qi1[-c((nrow(qi1)-5):nrow(qi1)),]
names(qi1) <- c("Description", "HoodStreet", "Rooms", "m_sq", "Floor", "Series", "Price")
ex <- qi1

# Further, when a user selects a page, the link consists of two parts with a page number in the middle:

first_part <- "https://www.ss.lv/lv/real-estate/flats/riga/all/sell/page"
second_part <- ".html"

# Since only the number changes in the link, we can create a loop that inserts a number between the two parts of the link,
# parses some data and appends it to the main data table created in the previous step (qi1).
# The webpage works in such a way that if "a" becomes bigger than the maximum amount of pages, the user is sent to the last existing page
# So, if there are 70 pages and "a" becomes 71, the code goes back to page 70 and extracts it again
# To avoid the infinite loop and extracting the same data twice, I write an if statement that stops the loop when it sees that the data starts repeating
# This method might have a drawback - if the table gets updated at the same time when the loop switches to the last page, the loop might give incorrect results at the end
# Yet, most of the data will be inserted correctly and the loop will stop. Of course, if something similar happens, the user can rerun the code again

a <- 1
for (i in 1:1000) {
  a <- a + 1
  link_c <- paste(first_part,a, second_part, sep="")
  qi_webpage <- read_html(link_c)
  qi_table <- html_nodes(qi_webpage, 'table')
  qi <- html_table(qi_table, header = FALSE)[[1]]
  qi <- qi[-c(1,2, 10:length(names(qi)))]
  qi <- qi[-c(1:5),]
  qi <- qi[-c((nrow(qi)-5):nrow(qi)),]
  names(qi) <- c("Description", "HoodStreet", "Rooms", "m_sq", "Floor", "Series", "Price")
  
  if (identical(qi, ex) == TRUE) {
    break
  }
  
  qi1 <- rbind(qi1, qi)
  
}

# This part contains data tidying and mostly cosmetical changes. 
# To my mind, it is done not very efficiently since too much information is set manually through the individual changes in each case, not with the general formula
# Thus, the code can be used only for taking the information from this concrete source and actually might stop working if ss.lv changes something in their webpage format.

qi2 <- qi1

b <- unlist(qi2["Price"])
b <- gsub("[^0-9.-]", "", b)
b <- as.numeric(b)
qi2["Price"] <- b

b <- unlist(qi2["m_sq"])
b <- as.numeric(b)
qi2["m_sq"] <- b

b <- unlist(qi2["Floor"])
b <- str_extract(b, "[^/]+")
b <- as.numeric(b)
qi2["Floor_nr"] <- b

b <- unlist(qi2["Floor"])
b <- str_extract(b, "/[0-9]*")
b <- str_extract(b, "[^/].*")
b <- as.numeric(b)
qi2["Max_floors"] <- b

b <- unlist(qi2["HoodStreet"])
b <- stri_trans_general(b, "Latin-ASCII")

# old2 <- c("centrs", "kalnes", "miju", "rjanu", "")
# new2 <- c("Sampeteris-Pleskodale", "Stacija-Tirgus", "Maskavas priekspilseta")
# for(i in seq_along(old2)) b <- gsub(old2[i], new2[i], b, fixed = TRUE)
# 

b <- gsub("centrs", "Centrs", b)
b <- gsub("TeikaM\\.", "Teikam", b)
b <- gsub("JuglaM\\.", "Juglam", b)
b <- gsub("ImantaM\\.", "Imantam", b)
b <- gsub("AgenskalnsM\\.", "Agenskalnsm", b)
b <- gsub("DzeguzkalnsM\\.","Dzeguzkalnsm", b )
qi2["HoodStreet"] <- b

b <- str_extract(b, "[A-Z][a-z]*")

old2 <- c("Sampeteris", "Stacija", "Maskavas")
new2 <- c("Sampeteris-Pleskodale", "Stacija-Tirgus", "Maskavas priekspilseta")
for(i in seq_along(old2)) b <- gsub(old2[i], new2[i], b, fixed = TRUE)

qi2["Hood"] <- b

b <- unlist(qi2["HoodStreet"])
b <- gsub("centrs", "Centrs", b)
c <- unlist(qi2["Hood"])
b <- str_remove(b, c)
b <- str_remove(b, "r-ns")
qi2["Street_a"] <- b

b <- str_extract(b, "[A-z]+")
b <- gsub("janvara", "13. janvara", b)
qi2["Street"] <- b

qi2 <- qi2[-c(2,5)]

# write.csv(qi2, file = "SS_parser.csv", append = FALSE, col.names = TRUE, row.names = FALSE)
