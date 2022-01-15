##### Libraries #####

if(!"readr" %in% installed.packages()) install.packages("readr")
if(!"sqldf" %in% installed.packages()) install.packages("sqldf")
if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"DBI" %in% installed.packages()) install.packages("DBI")


if(!"readr" %in% search()) library("readr")
if(!"sqldf" %in% search()) library("sqldf")
if(!"dplyr" %in% search()) library("dplyr")
if(!"DBI" %in% search()) library("DBI")

setwd("C:/Users/Андрей/Desktop")

data <- read.csv(file = "SS_parser.csv", header=TRUE, sep = ",")


# Since it was impossible to set a primary key based on a single column because values in each column are not unique, I create an id column for simplicity
# In SQL it is also possible to set the primary key as a combination of different columns, but in this particular case it is impossible
# Even if the address and floor are chosen as a primary key, it is still possible that there are two flats on one floor for sale which have the same house address
# The description might be the same as well, so adding it to the combination with the previous two columns might not help.

data$id <- NA
for (i in row_number(data$Description)) {
  data$id[i] <- i
}


#####
##### This function should open a usual SQL file, however, the due to the encoding of the .sql data file the function might fail.
# In this example I do not use the sql file but a csv file instead

# getSQL <- function(filepath){
#   con = file(filepath, "r", encoding = "WINDOWS-1252")
#   sql.string <- ""
#   
#   while (TRUE){
#     line <- readLines(con, n = 1)
#     
#     if ( length(line) == 0 ){
#       break
#     }
#     
#     line <- gsub("\\t", " ", line)
#     
#     if(grepl("--",line) == TRUE){
#       line <- paste(sub("--","/*",line),"*/")
#     }
#     
#     sql.string <- paste(sql.string, line)
#   }
#   
#   close(con)
#   return(sql.string)
# }

#####

# The sqldf package allows the user to perform the sql language in R to work with data frames
# To my mind, this is a convenient alternative and addition to the dplyr package if the user knows sql

##### Example 1 #####
# How many LT projects are being sold?
LT_proj <- sqldf("SELECT * FROM data WHERE Series = 'LT proj.' ")

# The same can be obtained with the dplyr package
LT_proj_dplyr <- data %>%
  filter(Series == "LT proj." )

##### Example 2 ##### 
# How many flats are sold in each hood?
# Assuming that the description is unique (and can be set as a primary key)
Hoods <- sqldf("SELECT hood, count(description) FROM data GROUP BY hood")


##### Example 3 #####
# Sometimes it is needed to run a nested query (query with a subquery)
# What is the average area of the 10 cheapest flats located in the center of Riga?
m_sq_cheapest <- sqldf("SELECT AVG(m_sq) FROM data WHERE id IN (SELECT id FROM data WHERE hood = 'Centrs' ORDER BY Price ASC limit 10) ")

# In case I would be interested in names/descriptions of the 10 cheapest flats in the center of Riga, I would try to avoid ORDER BY ... limit approach, or use it differently
# because the 10th and 11th cheapest flats in the center might have the same price, so 1 description would be lost, even though it still fits the choice.
# For now the 10th cheapest flat in the center costs 18000 and there are no flats sold for the same price, so the code returns only 10 observations

names_cheapest <- sqldf("SELECT id, Description, Price FROM data WHERE hood = 'Centrs' AND Price <= 
           (SELECT Price FROM data WHERE hood = 'Centrs' AND Price IN 
           (SELECT Price FROM data WHERE hood = 'Centrs' ORDER BY Price ASC limit 10) 
           ORDER BY Price DESC limit 1) 
           ORDER BY Price ASC")

# But, if I add a "magical flat" with the same price located in the center of Riga and run the code once again:

new_row <- c('magical_flat', 10, 10, 110, 18000, 5, 5, 'Centrs', 'Valdemara 1b', 'Valdemara', 2096) 
data <- as.data.frame(rbind(data, new_row))
data[c(2:3, 5:7, 11)] <- as.numeric(unlist(data[c(2:3, 5:7,11)]))


names_cheapest_magical <- sqldf("SELECT id, Description, Price FROM data WHERE hood = 'Centrs' AND Price <= 
           (SELECT Price FROM data WHERE hood = 'Centrs' AND Price IN 
           (SELECT Price FROM data WHERE hood = 'Centrs' ORDER BY Price ASC limit 10) 
           ORDER BY Price DESC limit 1) 
           ORDER BY Price ASC")

# Now it can be perfectly seen that the code returns 11 observations instead of 18 because the 10th and 11th cheapest price has the same value
# Even if we are interested in 10 cheapest flats in the center of Riga, it is unclear which flat to consider the 10th cheapest and 11th cheapest, so the best option would be returning both

#####
# con <- dbConnect(RSQLite::SQLite(), ":memory:")
# dbWriteTable(con, "data", data)
# dbReadTable(con, "data")
# 
# 
# dbExecute(con, "INSERT INTO data VALUES ('magical_flat', '10', '10', '110', '18000', '5', '5', 'Centrs', 'Valdemara 1b', 'Valdemara', '2096') ")
# 
# names_cheapest <- sqldf("SELECT id, Description, Price FROM data WHERE hood = 'Centrs' AND Price <= 
#            (SELECT Price FROM data WHERE hood = 'Centrs' AND Price IN 
#            (SELECT Price FROM data WHERE hood = 'Centrs' ORDER BY Price ASC limit 10) 
#            ORDER BY Price DESC limit 1) 
#            ORDER BY Price ASC")



##### Example 4 #####
# SQL allows to easily join multiple tables as well. The dplyr package also has functions that allow to join datasets, however, as I know, it is quite limited
# The problem with the dplyr package is related to its limited ability to join only 2 dataframes simultaneously. Although the user can write some function, e.g.:

megajoin_6 <- function(join_parameter, df1, df2, df3, df4, df5, df6){
  megajoined <- left_join(df1, df2, by=join_parameter)
  if(missing(df3) == FALSE){
    megajoined <- left_join(megajoined, df3, by=join_parameter)
  }
  if(missing(df4) == FALSE){
    megajoined <- left_join(megajoined, df4, by=join_parameter)
  }
  if(missing(df5) == FALSE){
    megajoined <- left_join(megajoined, df5, by=join_parameter)
  }
  if(missing(df6) == FALSE){
    megajoined <- left_join(megajoined, df6, by=join_parameter)
  }
  return(megajoined)
}

# This function is able to left join up to 6 datasets, however, it is limited as well because the join parameter (key) should have the same name everywhere
# I think it is still possible to join two tables where the key names are different with usual dplyr package, but it is easier to apply SQL if there are multiple tables
# However, in SQL the user can specify the join type, the tables and the column names that should match by table.variable = table2.variable2 



# Usually, to receive queries faster, the data is stored in multiple tables which usually makes it more efficient
# This dataset is not large in observations and the number of variables, however, let us assume that it is
# To show how the join functions work, I can split the dataframe with 11 variables into several dataframes
# Of course, each table should contain the key, on which the dataframes will be joined
# To make it more interesting, I will delete 100 different random rows in each table except the first (data_1)

data_1 <- data[c(11, 3:5)]

data_2 <- data[c(11, 1:2, 6:7)]
index2 <- sample(nrow(data_2), 100) 
data_2 <- data_2[-index2, ]

data_3 <- data[c(11, 8:10)]
index3 <- sample(nrow(data_3), 100)
data_3 <- data_3[-index3, ]

# Also, I will rename the id columns to different names

names(data_2)[1] <- "key2"
names(data_3)[1] <- "key3"

# Now, different join functions can be tested

# Inner join - returns only "complete" observations (if an observation is missing in any of the joined tables, it is excluded; no NAs)
inner_join <- sqldf("SELECT * FROM data_1 INNER JOIN data_2 ON data_1.id = data_2.key2 INNER JOIN data_3 ON data_1.id = data_3.key3")
View(inner_join)

# Left join - takes the first table as a "base" and binds observations from other tables to it (might have NAs)
left_join <- sqldf("SELECT * FROM data_1 LEFT JOIN data_2 ON data_1.id = data_2.key2 LEFT JOIN data_3 ON data_1.id = data_3.key3")
View(left_join)
# Some values have NAs
# Right join works the same way, but it takes the last table as a "base". 
# However, this library does not support it, as well as it does not support the outer join

# Natural join in this case does not work because to fulfill this function, the key names should match.
# Usually this function does not duplicate the keys and finds them itself (the user does not have to specify ON clause)
# natural_join <- sqldf("SELECT * FROM data_1 NATURAL JOIN data_2  NATURAL JOIN data_3 ")
