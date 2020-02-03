setwd("L:/Prices/Dashboards/PDF_reading")

if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(zoo, dplyr, readxl, data.table, stringr, reshape2, data.table, pdftools,
               shiny, writexl, DT, tidyr, tibble, lubridate)

library(zoo)
library(dplyr)
library(readxl)
library(data.table)
library(stringr)
library(reshape2)
library(data.table)
library(pdftools)
library(shiny)
library(writexl)
library(DT)
library(tidyr)
library(tibble)
library(lubridate)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Oilseeds reader"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file_oilseed", "Choose PDF of current month data",
                multiple = F,
                accept = c("pdf",
                           ".pdf")),
      
      
      checkboxInput("overwrite", label = "Overwrite current data?", value = F),
      checkboxInput("read_oilseed", label = "Read data only", value = F),
      hr(),
      textOutput("last_oilseed_date"),
      # Horizontal line ----
      hr(),
      hr(),
      
      ##download results
      conditionalPanel("input.read_oilseed == 0", downloadButton("dl_oilseed", "Download")),
      conditionalPanel("input.read_oilseed == 0",actionButton("submit_oilseed", "Submit"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Raw data",
                           # Output: Data file ----
                           DTOutput("oilseed_data")),
                  tabPanel("Weekly spot averages", DTOutput("weekly_output")),
                  tabPanel("Monthly oilseed averages", h4("Oilseed rape prices are calculated as an average of forward and spot prices for delivery in the named month"), hr(), DTOutput("monthly_output")),
                  tabPanel("Monthly pea and bean averages", h4("Pea and bean prices are calculated as an average of spot prices only"), hr(), DTOutput("monthly_pea_output"))
      )#end of tabset
    )#end of main panel
  )#end of sidebar layout
)#end of fluid page
# Define server logic to read selected file ----
server <- function(input, output) {
  
  ##Output datatable of raw data  
  output$oilseed_data <- renderDT({
    if(input$read_oilseed == F){
      req(input$file_oilseed)
      return(datatable(bound_oilseed_series()[order(bound_oilseed_series()$Date, decreasing = T),] %>% 
        mutate_at(vars(-Date, -del_date), funs(round(., 2))), 
        colnames = c("Date", "Delivery month", "Barley feed", "Beans", "Oilseed rape", "Feed peas", "Micronising peas", "Wheat, 13% milling", "Wheat feed")))}
    else{
      return(datatable(backseries_oilseed()[order(backseries_oilseed()$Date, decreasing = T),] %>% 
               mutate_at(vars(-Date, -del_date), funs(round(., 2))),
               colnames = c("Date", "Delivery month", "Barley feed", "Beans", "Oilseed rape", "Feed peas", "Micronising peas", "Wheat, 13% milling", "Wheat feed")))
    }
  })
  
  ##Output datatable of monthly oilseed averages  
  output$monthly_output <- renderDT({
    datatable(monthly_oilseed_averages()[order(monthly_oilseed_averages()$del_date, decreasing = F),], 
              colnames = c("Delivery month", "Barley feed", "Oilseed rape", "Wheat, 13% milling", "Wheat feed"))
  })
  
  ##Output datatable of weekly averages  
  output$weekly_output <- renderDT({
    datatable(weekly_oilseed_averages()[order(weekly_oilseed_averages()$Date, decreasing = F),] %>%
                mutate_at(vars(-Date, -del_date), funs(round(., 2))),
              colnames = c("Date", "Delivery month", "Barley feed", "Beans", "Oilseed rape", "Feed peas", "Micronising peas", "Wheat, 13% milling", "Wheat feed"))
  })
  
  #Output datatable of pea and bean averages
  output$monthly_pea_output <- renderDT({
    datatable(monthly_pea_averages()[order(monthly_pea_averages()$Date, decreasing = F),],
              colnames = c("Date", "Beans", "Feed peas", "Micronising peas"))
  })
 
  ##Load in backseries
  backseries_oilseed = reactive({
    data <- as_tibble(read_xlsx("Data/oilseed_prices.xlsx", sheet = 1))
    data$Date <- as.Date(data$Date)
    data$del_date <- as.Date(data$del_date)
    data
  })
  
  ##Text output showing date of last data uploaded
  output$last_oilseed_date = renderText({
    paste0("Last dataset uploaded: ", format(max(backseries_oilseed()$Date), "%d-%m-%Y"))
  })
  
  ##Read in pdf
    raw_pdf_oilseed = reactive({
    raw_pdf_oilseed <- pdf_data(input$file_oilseed$datapath)
    raw_pdf_oilseed <- as_tibble(raw_pdf_oilseed[[1]])
  })
  
  # ##Find date on PDF
  data_date = reactive({
    date_row <- raw_pdf_oilseed()[grep("Farmers", raw_pdf_oilseed()$text),4]
    data_date <- raw_pdf_oilseed() %>% filter(y == as.numeric(date_row))
    ##Identify numbers; take max as year and min as day
    year <- as.numeric(data_date$text) %>% na.omit()
    day <- min(year)
    year <- max(year)
    #Identify text which is month names
    month <- as.character(data_date[which(data_date$text %in% month.name), 6])
    month <- which(month.name == month)
    data_date <- as.Date(paste(year, month, day, sep = "-"))
    data_date
  })
  
  ##Data processing
  
  new_oilseed = reactive({
    ##Identify columns of interest
    new_oilseed <- raw_pdf_oilseed() %>% 
      ##Remove commentary rows
      filter(y > as.numeric(raw_pdf_oilseed()[grep("levies", raw_pdf_oilseed()$text), 4])) %>%
      filter(y < as.numeric(raw_pdf_oilseed()[grep("co-op", raw_pdf_oilseed()$text), 4]))
    
    ##Find column headers which are not repeated
    find_cols1 <- new_oilseed %>% filter(text %in% c("£/t", "13%", "Barley", "Oilseed", "micronising", "Beans"))
    find_cols1 <- dplyr::pull(find_cols1, x)

    ##Find column headers which are duplicated; second wheat and peas
    find_cols_wheat <- new_oilseed %>% filter(text %in% c("Wheat"))
    find_cols_wheat <- max(dplyr::pull(find_cols_wheat, x))
    find_cols_peas <- new_oilseed %>% filter(text %in% c("Peas"))
    find_cols_peas <- max(dplyr::pull(find_cols_peas, x))

    #Combine all of the columns
    find_cols <- c(find_cols1, find_cols_peas, find_cols_wheat)-2

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

    #Create region column, filter to remove north-east scotland and remove
    oilseed <- oilseed %>% mutate(region = del_month)
    oilseed[oilseed$region %in% month.abb, 9] <- NA
    oilseed <- oilseed %>% fill(region) %>%
      filter(region != "NORTH-EAST") %>%
      select(-region)

    #Remove headers by filtering for months only in del_month column
    oilseed$del_month <- gsub("^(.{3}).*", "\\1", oilseed$del_month)
    oilseed <- oilseed[oilseed$del_month %in% month.abb,]

     #Add date column
     oilseed <- oilseed %>% mutate(Date = data_date())
     
    ##Turn del_month into date
    ##In October-December January, Feb and potentially March data will be for the next year.
    #In all other cases, it will be for the same year
     
     if(month(data_date()) %in% c(10, 11, 12)){
      oilseed <- oilseed %>% mutate(del_year = case_when(del_month %in% c("Jan", "Feb", "Mar") ~ year(data_date()) + 1,
                                                          TRUE ~ year(data_date())))} else
       {oilseed <- oilseed %>% mutate(del_year = year(data_date()))}
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
  }) #end of reactive


  ##Bind data to backseries
  bound_oilseed_series = reactive({
    if(input$overwrite == F){
      if(T %in% grepl(data_date(), backseries_oilseed()$Date)){
        backseries_oilseed()}
      else{
        bound <- bind_rows(backseries_oilseed(), new_oilseed())
        bound <- bound[order(bound$Date),]
        bound
      }
    }
    else if(input$overwrite == T){
      bound <- backseries_oilseed() %>% dplyr::filter(Date != data_date())
      bound <- bind_rows(bound, new_oilseed())
      bound <- bound[order(bound$Date),]
      bound
    }
  })
  
  ##Select only weekly spot prices by removing any columns that give future dates of delivery
  weekly_oilseed_averages <- reactive({
    if(input$read_oilseed == F){
      validate(
        need(input$file_oilseed != "", "Please select a data set")
      )
      data1 = bound_oilseed_series()}
    else{
      data1 = backseries_oilseed()
    }
    
    data1 <- data1 %>%  
      group_by(Date) %>% 
      slice(which.min(del_date))
  })

  #Calculate averages for oilseed by delivery month
  monthly_oilseed_averages <- reactive({
    #Select bound series or back series as appropriate
    if(input$read_oilseed == F){
      validate(
        need(input$file_oilseed != "", "Please select a data set")
      )
    data1 = bound_oilseed_series()}
    else{
      data1 = backseries_oilseed()
    }
    ##Put data into tidy form and get averages by delivery month
    data1 = data1 %>% gather("feedstuff", "price", -c(Date, del_date))
     data1 = data1 %>% group_by(feedstuff, del_date) %>% summarise(price = round(mean(price, na.rm = T),2)) %>%
       spread(feedstuff, price) %>%
       select(-c(beans, peas_feed, peas_micronising))
        })
  
  #Calculate spot price averages for peas and beans by data month
  monthly_pea_averages = reactive({
    #Convert weekly dates to monthly dates and group by date
    weekly_oilseed_averages() %>% 
      ungroup() %>% 
      select(Date, peas_feed, beans, peas_micronising) %>%
      gather(oilseed, price, -Date) %>%
      mutate(Date = as.Date(paste0(format(Date, "%Y-%m"), "-01"))) %>%
      group_by(Date, oilseed) %>%
      summarise(price = mean(price, na.rm = T)) %>%
      spread(oilseed, price)
  })

  output$dl_oilseed <- downloadHandler(
    filename = function() { "oilseed_prices.xlsx"},
    content = function(file) {write_xlsx(list("Full series" = bound_oilseed_series(), "Weekly prices" = weekly_oilseed_averages(), "Monthly oilseed prices" = monthly_oilseed_averages(), "Monthly pea prices" = monthly_pea_averages()), path = file)}
  )
  
  # When the Submit button is clicked, save the data to server and let the user know
  observeEvent(input$submit_oilseed, {
    write_xlsx(list("Full series" = bound_oilseed_series(), "Weekly prices" = weekly_oilseed_averages(), "Monthly oilseed prices" = monthly_oilseed_averages(), "Monthly pea prices" = monthly_pea_averages()), path = "L:/Prices/Dashboards/PDF_reading/Data/oilseed_prices.xlsx")
    write_xlsx(list("Full series" = bound_oilseed_series(), "Weekly prices" = weekly_oilseed_averages(), "Monthly oilseed prices" = monthly_oilseed_averages()), path = "L:/Prices/API/Oilseeds/oilseed_prices.xlsx")
    showModal(modalDialog(
      title = "Data saved to app!",
      "To download an xlsx version of this data instead please select download"
    ))
  })
  
}
# Run the app ----
shinyApp(ui, server)
