setwd("L:/Prices/Dashboards/Oilseeds/Test")

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(zoo, dplyr, readxl, data.table, stringr, reshape2, data.table, pdftools,
               shiny, writexl, DT, tidyr, tibble, lubridate)

#Check backseries is available and read it in
backseries_available <- class(try(read_xlsx("../Data/oilseed_prices.xlsx", sheet = 1))) %>% 
  grepl(pattern = "try-error") %>%
  sum() == 0

#Check data is available and read it in
pdf_available <- class(try(pdf_data("error_check.pdf"))) %>% 
  grepl(pattern = "try-error") %>%
  sum() == 0


raw_pdf_oilseed <- pdf_data("error_check.pdf")[[1]]
date_row_identified <- try(sum(grepl("Farmers", raw_pdf_oilseed$text)) == 1)
date_row <- raw_pdf_oilseed[grep("Farmers", raw_pdf_oilseed$text),4]
data_date <- raw_pdf_oilseed %>% filter(y == as.numeric(date_row))
year <- as.numeric(data_date$text) %>% na.omit()
day <- min(year)
year <- max(year)
month <- as.character(data_date[which(data_date$text %in% month.name), 6])
month <- which(month.name == month)
data_date <- as.Date(paste(year, month, day, sep = "-"))

##Check date is available and in range
date_available <- try(is.Date(data_date))
date_range_correct <- try(data_date > "2019-01-01" & data_date < today())

##Data processing
##Identify columns of interest
new_oilseed <- raw_pdf_oilseed %>% 
  filter(y > as.numeric(raw_pdf_oilseed[grep("levies", raw_pdf_oilseed$text), 4])) %>%
  filter(y < as.numeric(raw_pdf_oilseed[grep("co-op", raw_pdf_oilseed$text), 4]))
find_cols1 <- new_oilseed %>% filter(text %in% c("£/t", "13%", "Barley", "Oilseed", "micronising", "Beans"))
find_cols1 <- dplyr::pull(find_cols1, x)
find_cols_wheat <- new_oilseed %>% filter(text %in% c("Wheat"))
find_cols_wheat <- max(dplyr::pull(find_cols_wheat, x))
find_cols_peas <- new_oilseed %>% filter(text %in% c("Peas"))
find_cols_peas <- max(dplyr::pull(find_cols_peas, x))

key_words_available <- try(sum(sum(grepl("£/t", new_oilseed$text)) != 0,
                               sum(grepl("13%", new_oilseed$text)) != 0,
                               sum(grepl("Barley", new_oilseed$text)) != 0,
                               sum(grepl("Oilseed", new_oilseed$text)) != 0,
                               sum(grepl("micronising", new_oilseed$text)) != 0,
                               sum(grepl("Beans", new_oilseed$text)) != 0,
                               sum(grepl("Peas", new_oilseed$text)) != 0,
                               sum(grepl("Wheat", new_oilseed$text)) != 0) == 8)

#split into columns
find_cols <- unique(c(find_cols1, find_cols_peas, find_cols_wheat)-2)
oilseed <-  new_oilseed %>% mutate(col = cut(x, breaks = c(10, find_cols, Inf))) %>%
  arrange(col, y) %>%
  group_by(col, y) %>%
  mutate(text = paste(text, collapse = " ")) %>%
  ungroup() %>%
  select(y, text, col) %>%
  unique() %>%
  spread(col, text) %>%
  select(-y)
##Check number of columns
structure_correct <- if(exists("oilseed") == F) {
  FALSE
  } else{ncol(oilseed) == 8}

colnames(oilseed) <- c("del_month", "wheat_13_milling", "wheat_feed", "barley_feed", "oilseed_rape", "peas_micronising", "peas_feed", "beans")
oilseed <- oilseed %>% mutate(region = del_month)
oilseed[oilseed$region %in% month.abb, 9] <- NA
oilseed <- oilseed %>% fill(region) %>%
  filter(region != "NORTH-EAST") %>%
  select(-region)

oilseed$del_month <- gsub("^(.{3}).*", "\\1", oilseed$del_month)

##Test if numbers are being removed when regions are removed
correct_rows_removed <- oilseed[!(oilseed$del_month %in% month.abb),]
correct_rows_removed <- gsub("13%", "", correct_rows_removed)
correct_rows_removed <- try(sum(grepl("[[:digit:]]", correct_rows_removed)) == 0)

oilseed <- oilseed[oilseed$del_month %in% month.abb,]

#Add date column
oilseed <- oilseed %>% mutate(Date = data_date)

if(month(data_date) %in% c(10, 11, 12)){
  oilseed <- oilseed %>% mutate(del_year = case_when(del_month %in% c("Jan", "Feb", "Mar") ~ year(data_date) + 1,
                                                     TRUE ~ year(data_date)))} else
                                                     {oilseed <- oilseed %>% mutate(del_year = year(data_date))}
oilseed <- oilseed %>% mutate(del_month = match(del_month,month.abb)) %>%
  mutate(del_date = as.Date(paste(del_year, del_month, "01", sep = "-"))) %>%
  select(-c(del_month, del_year))

##Check range of del_dates calculated appropriately
start_date <- as.Date(paste(year, month, "01", sep = "-"))
end_date <- as.Date(paste(year, month + 3, "01", sep = "-"))

del_date_range_correct <- try(min(oilseed$del_date) >= start_date & max(oilseed$del_date) <= end_date)

##Put into tidy form
oilseed <- oilseed %>% gather("feedstuff", "price", -c(Date, del_date))
oilseed$price <- gsub("[^[:digit:]. ]", "", oilseed$price)
oilseed$price <- as.numeric(oilseed$price)
oilseed <- oilseed %>% group_by(Date, del_date, feedstuff) %>%
  summarise(price = mean(price, na.rm = T)) %>%
  spread(feedstuff, price )

#Check no NA values generated
no_na_value <- try(mean(oilseed == na.omit(oilseed)) == 1)

##Generate output checker
output_checker <- tibble(backseries_available, pdf_available, date_row_identified, 
  date_available, date_range_correct,key_words_available, structure_correct, correct_rows_removed, 
  del_date_range_correct, no_na_value)

output_checker[grep("Error", output_checker)] <- FALSE


file_path = paste0("L:/Prices/Dashboards/Oilseeds/Test/Test files/error_check_output", today(), ".xlsx")
write_xlsx(list("prices" = output_checker), path = file_path)


