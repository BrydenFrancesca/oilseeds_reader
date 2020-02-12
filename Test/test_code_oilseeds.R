setwd("L:/Prices/Dashboards/Oilseeds/Test")

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(zoo, dplyr, readxl, data.table, stringr, reshape2, data.table, pdftools,
               shiny, writexl, DT, tidyr, tibble, lubridate)

#Read in PDF
raw_pdf_oilseed <- pdf_data("test_input.pdf")[[1]]

# ##Find date on PDF
date_row <- raw_pdf_oilseed[grep("Farmers", raw_pdf_oilseed$text),4]
data_date <- raw_pdf_oilseed %>% filter(y == as.numeric(date_row))
##Identify numbers; take max as year and min as day
year <- as.numeric(data_date$text) %>% na.omit()
day <- min(year)
year <- max(year)
  #Identify text which is month name
month <- as.character(data_date[which(data_date$text %in% month.name), 6])
month <- which(month.name == month)
data_date <- as.Date(paste(year, month, day, sep = "-"))

##Data processing
  ##Identify columns of interest
new_oilseed <- raw_pdf_oilseed %>% 
  ##Remove commentary rows
  filter(y > as.numeric(raw_pdf_oilseed[grep("levies", raw_pdf_oilseed$text), 4])) %>%
  filter(y < as.numeric(raw_pdf_oilseed[grep("co-op", raw_pdf_oilseed$text), 4]))
  
##Find column headers which are not repeated
find_cols1 <- new_oilseed %>% filter(text %in% c("Â£/t", "13%", "Barley", "Oilseed", "micronising", "Beans"))
find_cols1 <- dplyr::pull(find_cols1, x)
  
##Find column headers which are duplicated; take the last time the data says "wheat" and "peas"
find_cols_wheat <- new_oilseed %>% filter(text %in% c("Wheat"))
find_cols_wheat <- max(dplyr::pull(find_cols_wheat, x))
find_cols_peas <- new_oilseed %>% filter(text %in% c("Peas"))
find_cols_peas <- max(dplyr::pull(find_cols_peas, x))
  
#Combine all of the header column values and remove duplicates
find_cols <- unique(c(find_cols1, find_cols_peas, find_cols_wheat)-2)
  
###Split into columns
oilseed <-  new_oilseed %>% mutate(col = cut(x, breaks = c(10, find_cols, Inf))) %>%
  arrange(col, y) %>%
  group_by(col, y) %>%
  mutate(text = paste(text, collapse = " ")) %>%
  ungroup() %>%
  select(y, text, col) %>%
  unique() %>%
  spread(col, text) %>%
  select(-y)
  
#Create column names
colnames(oilseed) <- c("del_month", "wheat_13_milling", "wheat_feed", "barley_feed", "oilseed_rape", "peas_micronising", "peas_feed", "beans")
  
#Create region column (take all values from the del_month column which are not months into a new column and fill values down.
oilseed <- oilseed %>% mutate(region = del_month)
oilseed[oilseed$region %in% month.abb, 9] <- NA
oilseed <- oilseed %>% fill(region) %>%
#filter to remove north-east scotland and remove column
  filter(region != "NORTH-EAST") %>%
  select(-region)
  
#Remove headers by filtering for months only in del_month column
oilseed$del_month <- gsub("^(.{3}).*", "\\1", oilseed$del_month)
oilseed <- oilseed[oilseed$del_month %in% month.abb,]
  
#Add date column
oilseed <- oilseed %>% mutate(Date = data_date)
  
##Turn del_month into date
##In October-December January, Feb and potentially March data will be for the next year.
#In all other cases, it will be for the same year
  
if(month(data_date) %in% c(10, 11, 12)){
  oilseed <- oilseed %>% mutate(del_year = case_when(del_month %in% c("Jan", "Feb", "Mar") ~ year(data_date) + 1,
                                                     TRUE ~ year(data_date)))} else
                                                     {oilseed <- oilseed %>% mutate(del_year = year(data_date))}
oilseed <- oilseed %>% mutate(del_month = match(del_month,month.abb)) %>%
  mutate(del_date = as.Date(paste(del_year, del_month, "01", sep = "-"))) %>%
  select(-c(del_month, del_year))
  
##Put into tidy form
oilseed <- oilseed %>% gather("feedstuff", "price", -c(Date, del_date))
oilseed$price <- gsub("[^[:digit:]. ]", "", oilseed$price)
oilseed$price <- as.numeric(oilseed$price)
  
##Summarise prices as mean
oilseed <- oilseed %>% group_by(Date, del_date, feedstuff) %>%
    summarise(price = mean(price, na.rm = T)) %>%
    spread(feedstuff, price )

##Save file

file_path = paste0("L:/Prices/Dashboards/Oilseeds/Test/Test files/test_output_", today(), ".xlsx")
write_xlsx(list("weekly_prices" = oilseed), path = file_path)