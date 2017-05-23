library(shiny)
library(ggplot2)
library(zipcode)
library(maps)
library(sqldf)
library(shinydashboard)
library(leaflet)
library(squash)


data("zipcode")
merged_data = as.data.frame(read.csv('mergedDataDemo.csv'))
subscriber_data = na.omit(read.csv('subscriber_data_demo.csv'))

ui <- fluidPage(
  dashboardPage(skin = "purple",
    dashboardHeader(title = h3("CLO Donor Report")),
    dashboardSidebar(sidebarMenu(menuItem("Abstract", tabName = "text"), 
                                 menuItem("Donor Map", tabName = "usmap"),
                                 menuItem("Contribution Distribution", tabName = "contplot"),
                                 menuItem("Donation Frequency", tabName = "numdonations"),
                                 menuItem("Donor Retention", tabName = "retention"),
                                 menuItem("Donor Location Distributions", tabName = "hist"),
                                 menuItem("Donations by Month", tabName = "bymonth"),
                                 menuItem("Donor Case Studies", tabName = "cases"))),
    dashboardBody(
      tabItems(
      tabItem(h1("Investigating Trends in Pittsburgh CLO Donors"),h4("The goals of our project were to take the donation and ticketing data given to us and discover useful trends in the donation data, create models for predicting the potential donations of subscribers, and create models for predicting future donations of current donors, all to enhance the ability of the Pittsburgh CLO to make decisions regarding their fundraising strategies. This website is meant to serve as an interactive tool to help visualize the ticketing and donor data and provide new insights for the CLO to investigate. Note: this is a demo version of the website. All the data used for plots and maps has been randomly generated."), h5("This website and the statistical analysis on these pages was conducted as part of Carnegie Mellon Univeristy's 36-490 Undergraduate Research in Statistics Course, by Andrew Bryan, Naveen Shankar, Shichen Yang, Emily Helfer, and Apoorva Havanur, under the guidance of Professor Jordan Rodu."), tabName = "text"),
      tabItem(h3("Donor Locations by Zip Code Across the Country"), h4("The map shows clusters of zip codes in different areas in the United States. Click on a number to zoom into that area, and once you zoom to a single zip code, you can click on that zip code to show the number of donors and average contribution amount for that zip. 
"), h6(downloadLink("downloadDAtC", "Download the data behind this graph")), tabName = "usmap", 
              leafletOutput("usmap"),
              column(3, selectInput(inputId = "min_year_us", label = "Start Year", selected = min(merged_data$cont_yr), choices = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(4, selectInput(inputId = "max_year_us", label = "End Year", selected = max(merged_data$cont_yr), choice = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(5, numericInput(inputId = "zip_code", label = "Specific Zip Code", value = "None", min = 0)),
              column(6, numericInput(inputId = "contribution_us", label = "Maximum Contribution Amount", value = "None", min = 0)),
              column(7, numericInput(inputId = "min_contribution_us", label = "Minimum Contribution Amount", value = "None"))),
     
       tabItem(h3("Contribution Distribution Across Years"), h4("This histogram shows the distribution of donor amounts for a specified time and donation amount range. The x-axis is the amount donated to the CLO and the y-axis is the number of donors who donated that amount.
"), h6(downloadLink("downloadContPlot", "Download the data behind this graph")), tabName = "contplot",
              plotOutput("contplot"),
              column(3, selectInput(inputId = "min_year_cont", label = "Start Year", selected = min(merged_data$cont_yr), choices = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(4, selectInput(inputId = "max_year_cont", label = "End Year", selected = max(merged_data$cont_yr), choice = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(5, numericInput(inputId = "cont_max", label = "Show Contributions Under:", value = "None", min = 0)),
              column(6, numericInput(inputId = "cont_min", label = "Show Contributions Over:", value = "None"))),
      
      tabItem(h3("Frequency of Donations"), h4("The x-axis shows a donation frequency, and the y-axis the number of donors who have donated that frequently. For example, the base graph shows that most people donate once or twice, while practically no one donates more than 25 times.
"), h6(downloadLink("downloadFreqPlot", "Download the data behind this graph")), tabName = "numdonations", 
              plotOutput("numdonations"),
              column(3, selectInput(inputId = "min_year_don", label = "Start Year", selected = min(merged_data$cont_yr), choices = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(4, selectInput(inputId = "max_year_don", label = "End Year", selected = max(merged_data$cont_yr), choice = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(5, numericInput(inputId = "don_max", label = "Count Contributions Under:", value = "None", min = 0)),
              column(6, numericInput(inputId = "don_min", label = "Count Contributions Over:", value = "None"))),
      
      tabItem(h3("Donor Retention"), h4("This is a time series of the number of donors the CLO retained each year from 1995 to 2016. A donor is “retained” if the donated the current and previous year. There are options to specify certain contribution amounts, certain zip codes, and to also change to “donor retention percentage” instead of the raw amount of donors retained.
"), h6(downloadLink("downloadRetentionPlot", "Download the data behind this graph")), tabName = "retention",
              plotOutput("retention"),
              column(3, numericInput(inputId = "retention_num_min", label = "Count Donations Over ($):", value = "None", min = 0)),
              column(4, numericInput(inputId = "retention_num_max", label = "Count Donations Under ($):", value = "None", min = 0)),
              column(5, selectInput(inputId = "ret_zip_code", label = "Enter Specific Zip Code", choices = c("All Zip Codes", sort(unique(merged_data$clean_zips))), selected = "None")),
              column(6, checkboxInput(inputId = "percentage", label = "Show Retention Percentage?", value = FALSE))),
      
      
      tabItem(h3("Donor Distribution Across Zip Codes"), h4("The x-axis shows the number of donors in a zip code, and the y-axis shows the number of zip codes that have that many donors. For example, from 1994 to 2017, with a minimum contribution amount of $25 and a maximum of $100, most zip codes have 1 or 2 donors, while few zip codes have 25 or more donors."), h6(downloadLink("downloadZipPlot", "Download the data behind this graph")), tabName = "hist",
              plotOutput("hist"),
              column(3, selectInput(inputId = "min_year_hist", label = "Choose a start year", choices = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(4, selectInput(inputId = "max_year_hist", label = "Choose an end year", choices = min(merged_data$cont_yr):max(merged_data$cont_yr), selected = max(merged_data$cont_yr))),
              column(5, numericInput(inputId = "contribution_hist", label = "Maximum Contribution Amount", value = "None", min = 0)),
              column(6, numericInput(inputId = "min_contribution_hist", label = "Minimum Contribution Amount", value = "None"))),
      
      tabItem(h3("Donations by Month"), h4("This bar chart visualizes the amount of donation money the CLO received in each month for a given year range and a given contribution amount range.
"), h6(downloadLink("downloadByMonthPlot", "Download the data behind this graph")), tabName = "bymonth",
              plotOutput("bymonth"),
              column(3, selectInput(inputId = "min_year_month", label = "Choose a start year", choices = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(4, selectInput(inputId = "max_year_month", label = "Choose an end year", choice = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(5, numericInput(inputId = "contribution_month", label = "Maximum Contribution Amount", value = "None", min = 0)),
              column(6, numericInput(inputId = "min_contribution_month", label = "Minimum Contribution Amount", value = "None"))),
      
      tabItem(h3("Case Studies of Individual Donors"), h4("These graphs can take a little bit of time to load. The x-axis shows a year, the y-axis shows the total amount in dollars, the ticket sales are in green, and the donations are in red. Each point is sized by the number of donations or ticket purchases in that year. To see an aggregate plot of all donors who fit the selected specifications, click the show aggregate button.
"), h6(downloadLink("downloadCaseStudyPlot", "Download the data behind this graph")), tabName = "cases",
              plotOutput("cases"),
              column(3, selectInput(inputId = "donorYears", label = "Min. Years Donating", choice = 0:20)),
              column(4, selectInput(inputId = "ticketYears", label = "Min. Years Buying Tickets", choice = 0:9)),
              column(5,uiOutput("donorID")),
              column(6, selectInput(inputId = "casestartyear", label = "Start From Year", choice = min(merged_data$cont_yr):max(merged_data$cont_yr))),
              column(7, checkboxInput(inputId = "aggregate", label = "Show Aggregate Plot", value = FALSE)))
  ))))
  
server <- function(input, output, session) {
  output$downloadReport <- downloadHandler(filename = function () {"clo-report.pdf"}, content = function (file) {file.copy("CLOReport.pdf",file)})
  output$usmap <- renderLeaflet({
    if (is.na(input$contribution_us)) {
      merged_lim_data = merged_data
    }
    else {
      merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt <= as.numeric(input$contribution_us)),]
    }
    if (!is.na(input$min_contribution_us)) {
      merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt >= as.numeric(input$min_contribution_us)),]
    }
    if (is.na(input$zip_code)) {
      zip_lat = mean(zipcode$latitude, na.rm = TRUE)
      zip_long = mean(zipcode$longitude, na.rm = TRUE)
      zip_zoom = 3
    }
    else {
      zip_lat = zipcode$latitude[which(zipcode$zip==input$zip_code)]
      zip_long = zipcode$longitude[which(zipcode$zip==input$zip_code)]
      zip_zoom = 14
    }
    yearly_data = sqldf("SELECT SUM(Contribution_cont_amt) as total_cont, COUNT(customer_no) as total_cust, cont_yr as year, clean_zips as zip_code, lat, long FROM merged_lim_data GROUP BY clean_zips, cont_yr")
    year_data = as.data.frame(yearly_data[which(yearly_data$year >= input$min_year_us & yearly_data$year <= input$max_year_us),])
    new_year_data = sqldf("SELECT SUM(total_cont) as total_cont, SUM(total_cust) as total_cust, zip_code, lat, long FROM year_data GROUP BY zip_code")
    leaflet(data = new_year_data) %>% addTiles() %>% setView(lng = zip_long, lat = zip_lat, zoom = zip_zoom) %>%
    addCircleMarkers(~long, ~lat, opacity = (sqrt(new_year_data$total_cust/max(new_year_data$total_cust))+0.5)/(1.5), popup = c(paste0("Number of Donors: ", new_year_data$total_cust, ", Average Contribution: $", round(new_year_data$total_cont/new_year_data$total_cust, digit = 2), sep= '\n')), label = ~as.character(paste0("zip code: ",zip_code)),  clusterOptions = markerClusterOptions(weight = 1.5))
  })
  output$downloadDAtC <- downloadHandler(
    filename = function () 
      {"zip_donor_data.csv"}, 
    content = function (file) 
      {if (is.na(input$contribution_us)) {
        merged_lim_data = merged_data
      }
      else {
        merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt <= as.numeric(input$contribution_us)),]
      }
      yearly_data = sqldf("SELECT SUM(Contribution_cont_amt) as total_cont, COUNT(customer_no) as total_cust, cont_yr as year, clean_zips as zip_code, lat, long FROM merged_lim_data GROUP BY clean_zips, cont_yr")
      year_data = as.data.frame(yearly_data[which(yearly_data$year >= input$min_year_us & yearly_data$year <= input$max_year_us),])
      new_year_data = sqldf("SELECT SUM(total_cont) as total_cont, SUM(total_cust) as total_cust, zip_code, lat, long FROM year_data GROUP BY zip_code")
      write.csv(new_year_data, file)
      })
  
  output$contplot <- renderPlot({
    merged_lim_data = merged_data[which(as.numeric(merged_data$cont_yr) >= input$min_year_cont & as.numeric(merged_data$cont_yr) <= input$max_year_cont),]
    if (!is.na(input$cont_max)) {
      merged_lim_data = merged_lim_data[which(merged_lim_data$Contribution_cont_amt <= input$cont_max),]
    }
    if (!is.na(input$cont_min)) {
      merged_lim_data = merged_lim_data[which(merged_lim_data$Contribution_cont_amt >= input$cont_min),]
    }
    p <- qplot(Contribution_cont_amt, data=merged_lim_data, geom="histogram", fill = I("#5D00C6"))
    p <- p + (labs(title = "Distribution of Contribution Amount", x = "Contribution Amount",
    y = "Number of Donors Donating that Amount"))
    p <- p + theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  plot.title = element_blank(),
                  plot.background = element_rect(fill = "transparent",colour = NA),
                  legend.background = element_blank())
    p
  }, bg = "transparent")
  output$downloadContPlot <- downloadHandler(
    filename = function () 
    {"contribution_amount.csv"}, 
    content = function (file) 
    {merged_lim_data = merged_data[which(as.numeric(merged_data$cont_yr) >= input$min_year_cont & as.numeric(merged_data$cont_yr) <= input$max_year_cont),]
    if (!is.na(input$cont_max)) {
      merged_lim_data = merged_lim_data[which(merged_lim_data$Contribution_cont_amt <= input$cont_max),]
    }
      write.csv(merged_lim_data, file)
    })
    
  output$numdonations <- renderPlot({
    merged_lim_data = merged_data[which(as.numeric(merged_data$cont_yr) >= input$min_year_don & as.numeric(merged_data$cont_yr) <= input$max_year_don),]
    if (!is.na(input$don_max)) {
      merged_lim_data = merged_lim_data[which(merged_lim_data$Contribution_cont_amt <= input$don_max),]
    }
    if (!is.na(input$don_min)) {
      merged_lim_data = merged_lim_data[which(merged_lim_data$Contribution_cont_amt >= input$don_min),]
    }
    num_times_donating = sqldf("SELECT COUNT(customer_no) as times_donating, customer_no FROM merged_lim_data GROUP BY customer_no")
    p <- qplot(times_donating, data=num_times_donating, geom="histogram", fill = I("#5D00C6"))
    p <- p + (labs(title = "Number of Times Donating", x = "Number of Donations",
                   y = "Number of Donors Donating that Frequently"))
    p <- p + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank(),
                   plot.title = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   legend.background = element_blank())
    p
  }, bg = "transparent")
  output$downloadFreqPlot <- downloadHandler(
    filename = function () 
    {"donation_frequency.csv"}, 
    content = function (file) 
    {merged_lim_data = merged_data[which(as.numeric(merged_data$cont_yr) >= input$min_year_don & as.numeric(merged_data$cont_yr) <= input$max_year_don),]
    if (!is.na(input$don_max)) {
      merged_lim_data = merged_lim_data[which(merged_lim_data$Contribution_cont_amt <= input$don_max),]
    }
    num_times_donating = sqldf("SELECT COUNT(customer_no) as times_donating, customer_no FROM merged_lim_data GROUP BY customer_no")
    write.csv(num_times_donating, file)
    })
  
  output$retention <- renderPlot({
    if (is.na(input$retention_num_min)) {
      min_val = 0
    }
    else {
      min_val = input$retention_num_min
    }
    if (is.na(input$retention_num_max)) {
      max_val = max(merged_data$Contribution_cont_amt)
    }
    else {
      max_val = input$retention_num_max
    }
    merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt >= min_val & merged_data$Contribution_cont_amt <= max_val),]
    if (input$ret_zip_code != "All Zip Codes") {
      merged_lim_data = merged_lim_data[which(merged_lim_data$clean_zips == input$ret_zip_code),]
    }
    retained = c()
    denoms = c()
    for (i in (min(merged_lim_data$cont_yr)+1):(max(merged_lim_data$cont_yr))) {
      current_year_data = merged_lim_data[which(merged_lim_data$cont_yr == i),]
      prev_year_data = merged_lim_data[which(merged_lim_data$cont_yr == i-1),]
      curr_donors = unique(current_year_data$customer_no)
      prev_donors = unique(prev_year_data$customer_no)
      all = union(curr_donors, prev_donors)
      r = intersect(curr_donors, prev_donors)
      if (input$percentage == TRUE) {
        val = length(r)/(length(prev_donors)+0.0000001)*100
      }
      else {
        val = length(r)
      }
      retained = c(retained, val)
    }
    years = (min(merged_lim_data$cont_yr)+1):(max(merged_lim_data$cont_yr))
    df1 = data.frame(cbind(years, retained))
    colnames(df1) = c("year", "retained")
    if (2017 %in% df1$year) {
      df1 = df1[-(which(df1$year == 2017)),] 
    }
    p <- ggplot(df1, aes(x=year,y=(retained), col = I("#5D00C6"))) + geom_point()
    p <- p + geom_line()
    p <- p + theme(panel.border = element_blank(),
                   panel.background = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA)
    )
    p <- if (input$percentage == TRUE) {
      p + labs(title = "Percentage of Donors Retained Each Year", x = "Year", y = "% of Donors")
      }
    else {
      p + labs(title = "Number of Donors Retained Each Year", x = "Year", y = "Number of Donors")
    }
    p
  }, bg = "transparent")
  
  output$downloadRetentionPlot <- downloadHandler(
    filename = function () 
    {"donor_retention.csv"}, 
    content = function (file) 
    {if (is.na(input$retention_num_min)) {
      min_val = 0
    }
      else {
        min_val = input$retention_num_min
      }
      if (is.na(input$retention_num_max)) {
        max_val = max(merged_data$Contribution_cont_amt)
      }
      else {
        max_val = input$retention_num_max
      }
      merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt >= min_val & merged_data$Contribution_cont_amt <= max_val),]
      if (input$ret_zip_code != "All Zip Codes") {
        merged_lim_data = merged_lim_data[which(merged_lim_data$clean_zips == input$ret_zip_code),]
      }
      retained = c()
      denoms = c()
      for (i in (min(merged_lim_data$cont_yr)+1):(max(merged_lim_data$cont_yr))) {
        current_year_data = merged_lim_data[which(merged_lim_data$cont_yr == i),]
        prev_year_data = merged_lim_data[which(merged_lim_data$cont_yr == i-1),]
        curr_donors = unique(current_year_data$customer_no)
        prev_donors = unique(prev_year_data$customer_no)
        all = union(curr_donors, prev_donors)
        r = intersect(curr_donors, prev_donors)
        if (input$percentage == TRUE) {
          val = length(r)/(length(prev_donors)+0.0000001)*100
        }
        else {
          val = length(r)
        }
        retained = c(retained, val)
      }
      years = (min(merged_lim_data$cont_yr)+1):(max(merged_lim_data$cont_yr))
      df1 = data.frame(cbind(years, retained))
      colnames(df1) = c("year", "retained")
      if (2017 %in% df1$year) {
        df1 = df1[-(which(df1$year == 2017)),] 
      }
      write.csv(df1, file)}
      )
  
  output$hist <- renderPlot({
    if (is.na(input$contribution_hist)) {
      merged_lim_data = merged_data
    }
    else {
      merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt <= input$contribution_hist),]
    }
    if (!is.na(input$min_contribution_hist)) {
      merged_lim_data = merged_lim_data[which(merged_lim_data$Contribution_cont_amt >= input$min_contribution_hist),]
    }
    yearly_data = sqldf("SELECT SUM(Contribution_cont_amt) as total_cont, COUNT(customer_no) as total_cust, cont_yr as year, clean_zips as zip_code, lat, long FROM merged_lim_data GROUP BY clean_zips, cont_yr")
    year_data = as.data.frame(yearly_data[which(yearly_data$year >= input$min_year_hist & yearly_data$year <= input$max_year_hist),])
    p <- qplot(total_cust, data=year_data, geom="histogram", binwidth = 1, fill = I("#5D00C6"))
    p <- p + (labs(title = "Donors Per Zip Code", x = "Number of Donors in Zip Code",
                                  y = "Number of Zip Codes"))
    p <- p + theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank(),
               plot.title = element_blank(),
               plot.background = element_rect(fill = "transparent",colour = NA),
               legend.background = element_blank())
    p
    }, bg = "transparent")
  
  output$downloadZipPlot <- downloadHandler(
    filename = function () 
    {"zip_donor_distribution.csv"}, 
    content = function (file) 
    {if (is.na(input$contribution_hist)) {
      merged_lim_data = merged_data
    }
      else {
        merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt <= input$contribution_hist),]
      }
      yearly_data = sqldf("SELECT SUM(Contribution_cont_amt) as total_cont, COUNT(customer_no) as total_cust, cont_yr as year, clean_zips as zip_code, lat, long FROM merged_lim_data GROUP BY clean_zips, cont_yr")
      year_data = as.data.frame(yearly_data[which(yearly_data$year >= input$min_year_hist & yearly_data$year <= input$max_year_hist),])
      write.csv(year_data, file)
    })

  output$bymonth <- renderPlot({
    if (is.na(input$contribution_month)) {
      merged_lim_data = merged_data
    }
    else {
      merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt <= input$contribution_month),]
    }
    if (!is.na(input$min_contribution_month)) {
      merged_lim_data = merged_lim_data[which(merged_lim_data$Contribution_cont_amt >= input$min_contribution_month),]
    }
    monthly_data = sqldf("SELECT SUM(Contribution_cont_amt) as total_cont, COUNT(customer_no) as total_cust, cont_yr as year, cont_month as month, clean_zips, lat, long FROM merged_lim_data GROUP BY clean_zips, cont_yr, cont_month")
    month_year_data = as.data.frame(monthly_data[which(monthly_data$year >= input$min_year_month & monthly_data$year <= input$max_year_month),])
    temp_df = sqldf("SELECT SUM(total_cont) as total_cont, month FROM month_year_data GROUP BY month ORDER BY month ASC")
    if (nrow(temp_df) != 12) {
      temp_df = rbind(temp_df, data.frame(total_cont = 0, month =c(1:12)[-which(c(1:12) %in% temp_df$month)]))
      temp_df = temp_df[order(temp_df$month),]
    }
    p <- ggplot(temp_df, aes(x = factor(month.abb[month]), y = total_cont)) + geom_bar(stat = "sum", fill = "#5D00C6")
    p <- p + labs(title = "Total Donations By Month ($)", x = "Month", y = "Total Amount of Donations")
    p <- p + scale_x_discrete(limits = month.abb)
    p <- p + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_rect(fill = "transparent",colour = NA),
                       plot.title = element_blank(),
                       legend.position="none")
    p
  }, bg = "transparent")
  
  output$downloadByMonthPlot <- downloadHandler(
    filename = function () 
    {"by_month_distribution.csv"}, 
    content = function (file) 
    {if (is.na(input$contribution_month)) {
      merged_lim_data = merged_data
      }
      else {
        merged_lim_data = merged_data[which(merged_data$Contribution_cont_amt <= input$contribution_month),]
      }
      monthly_data = sqldf("SELECT SUM(Contribution_cont_amt) as total_cont, COUNT(customer_no) as total_cust, cont_yr as year, cont_month as month, clean_zips, lat, long FROM merged_lim_data GROUP BY clean_zips, cont_yr, cont_month")
      month_year_data = as.data.frame(monthly_data[which(monthly_data$year >= input$min_year_month & monthly_data$year <= input$max_year_month),])
      temp_df = sqldf("SELECT SUM(total_cont) as total_cont, month FROM month_year_data GROUP BY month ORDER BY month ASC")
      if (nrow(temp_df) != 12) {
        temp_df = rbind(temp_df, data.frame(total_cont = 0, month =c(1:12)[-which(c(1:12) %in% temp_df$month)]))
        temp_df = temp_df[order(temp_df$month),]
      }
      write.csv(temp_df, file)
    })
  
  output$donorID <- renderUI({
    if (input$aggregate == FALSE) {
    donor_by_year = sqldf('SELECT SUM(Contribution_cont_amt) as cont_amt, customer_no, cont_yr FROM merged_data GROUP BY customer_no, cont_yr')
    subscriber_by_year = sqldf('SELECT SUM(tkt_value) as paid_amt, customer_no, year FROM subscriber_data GROUP BY customer_no, year')
    donors_years = sqldf('SELECT customer_no, COUNT(cont_yr) as year_count FROM donor_by_year GROUP BY customer_no')
    subscriber_years = sqldf('SELECT customer_no, COUNT(year) as year_count FROM subscriber_by_year GROUP BY customer_no')
    if (as.numeric(input$donorYears) == 0) {
      selected_donors = as.numeric(donors_years$customer_no)
    }
    else {
      selected_donors = as.numeric(donors_years$customer_no[which(donors_years$year_count >= as.numeric(input$donorYears))])
    }
    if (as.numeric(input$ticketYears) == 0){
      selected_subscribers = as.numeric(subscriber_years$customer_no)
    }
    else {
      selected_subscribers = as.numeric(subscriber_years$customer_no[which(subscriber_years$year_count >= as.numeric(input$ticketYears))])
    }
    updated_customers = intersect(selected_donors, selected_subscribers)
    selectInput("selectDonorsID", "Customer Number", choices = sort(unique(updated_customers)), selected = min(updated_customers))
  }})
  
  output$cases <- renderPlot({
    donor_by_year = sqldf('SELECT SUM(Contribution_cont_amt) as cont_amt, customer_no, cont_yr FROM merged_data GROUP BY customer_no, cont_yr')
    subscriber_by_year = sqldf('SELECT SUM(tkt_value) as paid_amt, customer_no, year FROM subscriber_data GROUP BY customer_no, year')
    if (!is.null(input$selectDonorsID)) {
      if (input$aggregate == FALSE) {
        donor_by_year = donor_by_year[which(as.numeric(donor_by_year$customer_no) == as.numeric(input$selectDonorsID)),]
        donor_by_year = sqldf("SELECT SUM(cont_amt) as cont_amt, cont_yr, COUNT(cont_amt) as yr_cont FROM donor_by_year GROUP BY cont_yr")
        subscriber_by_year = subscriber_by_year[which(as.numeric(subscriber_by_year$customer_no)== as.numeric(input$selectDonorsID)),]
        subscriber_by_year = sqldf("SELECT SUM(paid_amt) as cont_amt, year, COUNT(paid_amt) as amt_cont FROM subscriber_by_year GROUP BY year")
      }
      else {
        donors_years = sqldf('SELECT customer_no, COUNT(cont_yr) as year_count FROM donor_by_year GROUP BY customer_no')
        subscriber_years = sqldf('SELECT customer_no, COUNT(year) as year_count FROM subscriber_by_year GROUP BY customer_no')
        if (as.numeric(input$donorYears) == 0) {
          selected_donors = as.numeric(as.character(donors_years$customer_no))
        }
        else {
          selected_donors = as.numeric(as.character(donors_years$customer_no[which(donors_years$year_count >= as.numeric(input$donorYears))]))
        }
        if (as.numeric(as.character(input$ticketYears)) == 0){
          selected_subscribers = as.numeric(as.character(subscriber_years$customer_no))
        }
        else {
          selected_subscribers = as.numeric(as.character(subscriber_years$customer_no[which(as.numeric(subscriber_years$year_count) >= as.numeric(as.character(input$ticketYears)))]))
        }
        updated_customers = intersect(selected_donors, selected_subscribers)
        if (input$aggregate == FALSE) {
          donor_by_year = donor_by_year[which(donor_by_year$customer_no %in% updated_customers),]
          subscriber_by_year = subscriber_by_year[which(subscriber_by_year$customer_no %in% updated_customers),]
        }
        donor_by_year = sqldf("SELECT SUM(cont_amt) as cont_amt, cont_yr, COUNT(cont_amt) as yr_cont FROM donor_by_year GROUP BY cont_yr")
        subscriber_by_year = sqldf("SELECT SUM(paid_amt) as cont_amt, year, COUNT(paid_amt) as amt_cont FROM subscriber_by_year GROUP BY year")
      }
      df1 = data.frame(cbind(
        c(as.numeric(donor_by_year$cont_yr), as.numeric(subscriber_by_year$year)), 
        c(as.numeric(as.character((donor_by_year$cont_amt))), as.numeric(as.character(subscriber_by_year$cont_amt))), 
        c(as.numeric(donor_by_year$yr_cont), as.numeric(subscriber_by_year$amt_cont)), 
        c(rep("amount donated", length(donor_by_year$cont_yr)), rep("ticket sales", length(subscriber_by_year$year)))))
      colnames(df1) <- c("year","amount", "count", "type")
      df1$amount = as.numeric(as.character(df1$amount))
      df1$year = as.numeric(as.character(df1$year))
      if (2017 %in% df1$year) {
        df1 = df1[-(which(df1$year == 2017)),] 
      }
      start_year = min(as.numeric(df1$year))
      if (!is.na(input$casestartyear)) {
        start_year = input$casestartyear
      }
      df1 <- df1[which(as.numeric(df1$year) >= start_year),]
      p <- ggplot(df1, aes(x=year,y=amount,group = type, col=type)) + geom_point(aes(size = count), show.legend = FALSE)
      p <- p + geom_line()
      p <- if (input$aggregate == FALSE) {
        p + labs(title = paste("Total Donations and Ticket Payments by Year for Customer ",
                                  input$selectDonorsID), x = "Year", y = "Total Amount ($)", subtitle = "Points sized by number of ticket purchases/donations")
      }
      else {
        p + labs(title = "Aggreate Donations and Ticket Sales By Year", x = "Year", y = "Total Amount ($)", subtitle = "Points sized by number of ticket purchases/donations")
      }
      p <- p + theme(panel.border = element_blank(),
                     panel.background = element_blank(),
                     legend.background = element_blank(),
                     plot.background = element_rect(fill = "transparent",colour = NA)
      )
      p
      }
      
  }, bg = "transparent")
  
  output$downloadCaseStudyPlot <- downloadHandler(
    filename = function () 
    {if (input$aggregate) {
      "aggregate_case_study.csv"}
    else if (!is.null(input$selectDonorsID)) {
       paste0(input$selectDonorsID, "_case_study.csv") }
    else {
        "case_study.csv"
      }
     }, 
    content = function(file) {
    donor_by_year = sqldf('SELECT SUM(Contribution_cont_amt) as cont_amt, customer_no, cont_yr FROM merged_data GROUP BY customer_no, cont_yr')
    subscriber_by_year = sqldf('SELECT SUM(tkt_value) as paid_amt, customer_no, year FROM subscriber_data GROUP BY customer_no, year')
    if (!is.null(input$selectDonorsID)) {
      if (input$aggregate == FALSE) {
        donor_by_year = donor_by_year[which(as.numeric(donor_by_year$customer_no) == as.numeric(input$selectDonorsID)),]
        donor_by_year = sqldf("SELECT SUM(cont_amt) as cont_amt, cont_yr, COUNT(cont_amt) as yr_cont FROM donor_by_year GROUP BY cont_yr")
        subscriber_by_year = subscriber_by_year[which(as.numeric(subscriber_by_year$customer_no)== as.numeric(input$selectDonorsID)),]
        subscriber_by_year = sqldf("SELECT SUM(paid_amt) as cont_amt, year, COUNT(paid_amt) as amt_cont FROM subscriber_by_year GROUP BY year")
      }
      else {
        donors_years = sqldf('SELECT customer_no, COUNT(cont_yr) as year_count FROM donor_by_year GROUP BY customer_no')
        subscriber_years = sqldf('SELECT customer_no, COUNT(year) as year_count FROM subscriber_by_year GROUP BY customer_no')
        if (as.numeric(input$donorYears) == 0) {
          selected_donors = as.numeric(donors_years$customer_no)
        }
        else {
          selected_donors = as.numeric(donors_years$customer_no[which(donors_years$year_count >= as.numeric(input$donorYears))])
        }
        if (as.numeric(input$ticketYears) == 0){
          selected_subscribers = as.numeric(subscriber_years$customer_no)
        }
        else {
          selected_subscribers = as.numeric(subscriber_years$customer_no[which(subscriber_years$year_count >= as.numeric(input$ticketYears))])
        }
        updated_customers = intersect(selected_donors, selected_subscribers)
        donor_by_year = donor_by_year[which(donor_by_year$customer_no %in% updated_customers),]
        donor_by_year = sqldf("SELECT SUM(cont_amt) as cont_amt, cont_yr, COUNT(cont_amt) as yr_cont FROM donor_by_year GROUP BY cont_yr")
        subscriber_by_year = subscriber_by_year[which(subscriber_by_year$customer_no %in% updated_customers),]
        subscriber_by_year = sqldf("SELECT SUM(paid_amt) as cont_amt, year, COUNT(paid_amt) as amt_cont FROM subscriber_by_year GROUP BY year")
      }
      df1 = data.frame(cbind(
        c(as.numeric(donor_by_year$cont_yr), as.numeric(subscriber_by_year$year)), 
        c(as.numeric(as.character((donor_by_year$cont_amt))), as.numeric(as.character(subscriber_by_year$cont_amt))), 
        c(as.numeric(donor_by_year$yr_cont), as.numeric(subscriber_by_year$amt_cont)), 
        c(rep("amount donated", length(donor_by_year$cont_yr)), rep("ticket sales", length(subscriber_by_year$year)))))
      colnames(df1) <- c("year","amount", "count", "type")
      df1$amount = as.numeric(as.character(df1$amount))
      df1$year = as.numeric(as.character(df1$year))
      if (2017 %in% df1$year) {
        df1 = df1[-(which(df1$year == 2017)),] 
      }
      start_year = min(as.numeric(df1$year))
      if (!is.na(input$casestartyear)) {
        start_year = input$casestartyear
      }
      df1 <- df1[which(as.numeric(df1$year) >= start_year),]}
      write.csv(df1, file)
      }
    )
}

shinyApp(ui = ui, server = server)