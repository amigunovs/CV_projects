  ##### Libraries #####
  
  if(!"eurostat" %in% installed.packages()) install.packages("eurostat")
  if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
  if(!"tidyr" %in% installed.packages()) install.packages("tidyr")
  if(!"plm" %in% installed.packages()) install.packages("plm")
  if(!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
  if(!"sqldf" %in% installed.packages()) install.packages("sqldf")
  if(!"readxl" %in% installed.packages()) install.packages("readxl")
  if(!"fastmatch" %in% installed.packages()) install.packages("fastmatch")
  if(!"ggpubr" %in% installed.packages()) devtools::install_github("kassambara/ggpubr")
  
  
  if(!"eurostat" %in% search()) library("eurostat")
  if(!"dplyr" %in% search()) library("dplyr")
  if(!"tidyr" %in% search()) library("tidyr")
  if(!"plm" %in% search()) library("plm")
  if(!"ggplot2" %in% search()) library("ggplot2")
  if(!"sqldf" %in% search()) library("sqldf")
  if(!"readxl" %in% search()) library("readxl")
  if(!"fastmatch" %in% search()) library("fastmatch")
  if(!"ggpubr" %in% search()) library("ggpubr")
  
  
  setwd("C:/Users/andrejs.migunovs/Documents/Darba faili/data science/R/debt level")
  
  ##### Functions #####
  
  
  ##### Getting the data #####
  
  # Deficit
  trade_data = get_eurostat("teina230", update_cache = TRUE)
  
  trade_data <- trade_data %>%
    filter(unit == "MIO_EUR") %>%
    filter(geo != "EU27_2020")
  
  trade_data <- trade_data[, !names(trade_data) %in% c("na_item", "sector", "unit")] 
  
  # Population
  pop_data = get_eurostat("tps00001", update_cache = TRUE)
  
  pop_data <- pop_data[, !names(pop_data) %in% c("indic_de")] 
  names(pop_data)[fmatch("values", names(pop_data)) ] <- "population"
  names(trade_data)[fmatch("values", names(trade_data))] <- "debt"
  
  # Country codes
  
  country_codes <- read_excel("kodi.xlsx", 1)
  country_codes <- country_codes[-c(fmatch("code3", names(country_codes)))]
  names(country_codes)[fmatch("valstis", names(country_codes))] <- "country"
  names(country_codes)[fmatch("code2", names(country_codes))] <- "code"
  
  ##### Joining the data #####
  pop_data <- pop_data %>%
    unite("join", geo:time, remove = FALSE)
  
  trade_data <- trade_data %>%
    unite("join", geo:time, remove = FALSE)
  
  pop_def <- left_join(trade_data, pop_data, by="join")
  pop_def <- pop_def[, !names(pop_def) %in% c("time.y", "geo.y")] 
  
  pop_def <- pop_def %>%
    mutate(date = as.Date(time.x, format = "%Y.%m.%d"))
  
  pop_def <- pop_def %>%
    filter(date >= "2019-01-01")
  
  pop_def_p <- pdata.frame(pop_def, index = c("geo.x", "date"), drop.index = FALSE)
  names(pop_def_p)
  
  pop_def_p <- pop_def_p[, !names(pop_def_p) %in% c("join", "time.x")] 
  #View(pop_def_p)
  ##### Coping with different frequencies #####
  
  for (i in 1:nrow(pop_def_p)) {
    if (is.na(pop_def_p[i,fmatch("population", names(pop_def_p))]) == TRUE) {
      pop_def_p[i,fmatch("population", names(pop_def_p))] <- pop_def_p[i-1, fmatch("population", names(pop_def_p))]
    } else {
      next
    }
  }
  
  pop_def_p$debt_pc <- pop_def_p$debt * 1000000 / pop_def_p$population
  pop_def_p <- separate(data = pop_def_p, col = date, into = c("year", "month", "day"), sep = "-", remove = FALSE, convert = FALSE, extra = "warn", fill = "warn")
  pop_def_p$month <- as.numeric(pop_def_p$month)
  pop_def_p <- pop_def_p%>%
    group_by(geo.x) %>%
    mutate(debt_pc_g = debt_pc - dplyr::lag(debt_pc),
           debt_pc_l = dplyr::lag(debt_pc),
           debt_pc_gp = (debt_pc/dplyr::lag(debt_pc) - 1) * 100)
  
  latest <- pop_def_p %>%
    filter(year == max(pop_def_p$year))
  
  latest <- latest %>%
    filter(month == max(latest$month))
  
  names(latest)[fmatch("geo.x", names(latest))] <- "code"
  
  latest1 <- left_join(latest, country_codes, by="code")
  
  latest <- as.data.frame(latest1)
  remove(latest1)
  #View(latest)
  
  for (i in 1:nrow(latest)) {
    if (is.na(latest[i,fmatch("country", names(latest))]) == TRUE) {
      if (latest[i, fmatch("code", names(latest))] == "EL") {
        latest[i,fmatch("country", names(latest))] <- "Grieíija"
      } else {
        latest[i, fmatch("country", names(latest))] <- latest[i, fmatch("code", names(latest))]}
    } else {
      next
    }
  }
  
  ##### Visualization #####
  
  latest <- latest %>%
    arrange(desc(debt_pc))
  
  latest$country <- factor(latest$country, levels = latest$country, ordered = TRUE)
  names(latest)[fmatch("country", names(latest))] <- "Valsts"
  names(latest)[fmatch("debt_pc", names(latest))] <- "Parâds"
  
  # debt_per_capita <- ggplot(latest, aes(x=Valsts, y=Parâds, fill = factor(ifelse(Valsts == "Latvija", "Citas", "Latvija")))) + 
  #   geom_bar(stat = "identity", show.legend = FALSE) +
  #   scale_fill_manual(name = "Valsts", values = c("red", "#0057B8")) +
  #   ggtitle("ES valstu parâds uz iedzîvotâju, eiro") +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle=75, vjust=0.6))
  
  # debt_per_capita
  
  debt_per_capita <- ggplot(latest) + 
    geom_bar(aes(x=Valsts, y=Parâds),stat="identity", fill = factor(ifelse(latest$Valsts == "Latvija", "black", "#0057B8"))) +
    geom_path(aes(x=Valsts, y=debt_pc_gp * mean(Parâds)/10) , stat="identity", group = 1, colour = "red", size = 1.2 ) + 
    scale_y_continuous(sec.axis = sec_axis(~./ mean(latest$Parâds)*10)) +
    ggtitle("ES valstu parâds uz iedzîvotâju un izmaiòas pret iepriekðçjo ceturksni (euro, %)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=75, vjust=0.6))
  
  # geom_text(aes(label=Rate, x=Year, y=Rate*max(df$Response)), colour="black")+
  # geom_text(aes(label=Response, x=Year, y=0.95*Response), colour="black")+
  
  
  ##### Debt as a percentage of GDP #####
  
  # Getting the table
  trade_data = get_eurostat("teina230", update_cache = TRUE)
  
  trade_data <- trade_data %>%
    filter(unit == "PC_GDP") %>%
    filter(geo != "EU27_2020")
  
  trade_data <- trade_data[, !names(trade_data) %in% c("na_item", "sector", "unit") ]
  names(trade_data)[fmatch("values", names(trade_data))] <- "debt_GDP"
  
  # Joining the country codes and splitting the date
  table2 <- sqldf("SELECT * FROM trade_data INNER JOIN country_codes on trade_data.geo = country_codes.code")
  table2 <- table2[, !names(table2) %in% c("geo")]
  table2 <- separate(data = table2, col = time, into = c("year", "month", "day"), sep = "-", remove = FALSE, convert = FALSE, extra = "warn", fill = "warn")
  
  table2 <- table2%>%
    group_by(country) %>%
    mutate(debt_GDP_g = debt_GDP - dplyr::lag(debt_GDP),
           debt_GDP_l = dplyr::lag(debt_GDP),
           debt_GDP_gp = (debt_GDP/dplyr::lag(debt_GDP) - 1) * 100)
  
  # Filtering the latest data
  table2 <- table2 %>%
    filter(year == max(table2$year))
  
  table2 <- table2 %>%
    filter(month == max(table2$month))
  
  # Visualization 
  
  table2 <- table2%>%
    arrange(desc(debt_GDP))
  
  table2$country <- factor(table2$country, levels = table2$country)
  names(table2)[fmatch("country", names(table2))] <- "Valsts"
  names(table2)[fmatch("debt_GDP", names(table2))] <- "Parâds"
  
  debt_GDP <- ggplot(table2, aes(x=Valsts, y=Parâds, fill = factor(ifelse(Valsts == "Latvija", "Citas", "Latvija")))) + 
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "Valsts", values = c("red", "#0057B8")) +
    ggtitle("ES valstu parâds no IKP, %") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle=75, vjust=0.6))
  
  
  debt_GDP <- ggplot(table2) + 
    geom_bar(aes(x=Valsts, y=Parâds),stat="identity", fill = factor(ifelse(table2$Valsts == "Latvija", "black", "#0057B8"))) +
    geom_path(aes(x=Valsts, y=debt_GDP_g * mean(Parâds)/10) , stat="identity", group = 1, colour = "red", size = 1.2 ) + 
    scale_y_continuous(sec.axis = sec_axis(~./ mean(table2$Parâds)*10)) +
    ggtitle("ES valstu parâds no IKP un izmaiòas pret iepriekðçjo cetursni (% no IKP, procentpunkti") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=75, vjust=0.6))
  
  debt_GDP
  
  
  ##### Results #####
  
  # Period
  table2[1,fmatch("time", names(table2))]
  
  # Debt per capita
  debt_per_capita
  
  # Debt as a percentage of GDP
  debt_GDP
  
  
  multi.page <- ggarrange(debt_per_capita, debt_GDP, nrow=2, ncol=1) # for one plot per page
  multi.page[[1]] # for seeing the first plot
  ggexport(multi.page, filename="debt_level_output.pdf")
  
  
