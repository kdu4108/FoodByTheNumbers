# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)

#regional plot
regions <- unique(Brown_Datathon_Store_Overall$region)
west_breakfast <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="West" & Brown_Datathon_Store_Overall$shift_bin=="Breakfast",]
average_spent_west_breakfast <- sum(west_breakfast$total_sales)/sum(west_breakfast$number_of_tickets)
west_lunch <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="West" & Brown_Datathon_Store_Overall$shift_bin=="Lunch",]
average_spent_west_lunch <- sum(west_lunch$total_sales)/sum(west_lunch$number_of_tickets)
west_dinner <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="West" & Brown_Datathon_Store_Overall$shift_bin=="Dinner",]
average_spent_west_dinner <- sum(west_dinner$total_sales)/sum(west_dinner$number_of_tickets)
west_latenight <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="West" & Brown_Datathon_Store_Overall$shift_bin=="Late Night",]
average_spent_west_latenight <- sum(west_latenight$total_sales)/sum(west_latenight$number_of_tickets)

midwest_breakfast <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Midwest" & Brown_Datathon_Store_Overall$shift_bin=="Breakfast",]
average_spent_midwest_breakfast <- sum(midwest_breakfast$total_sales)/sum(midwest_breakfast$number_of_tickets)
midwest_lunch <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Midwest" & Brown_Datathon_Store_Overall$shift_bin=="Lunch",]
average_spent_midwest_lunch <- sum(midwest_lunch$total_sales)/sum(midwest_lunch$number_of_tickets)
midwest_dinner <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Midwest" & Brown_Datathon_Store_Overall$shift_bin=="Dinner",]
average_spent_midwest_dinner <- sum(midwest_dinner$total_sales)/sum(midwest_dinner$number_of_tickets)
midwest_latenight <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Midwest" & Brown_Datathon_Store_Overall$shift_bin=="Late Night",]
average_spent_midwest_latenight <- sum(midwest_latenight$total_sales)/sum(midwest_latenight$number_of_tickets)

northeast_breakfast <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Northeast" & Brown_Datathon_Store_Overall$shift_bin=="Breakfast",]
average_spent_northeast_breakfast <- sum(northeast_breakfast$total_sales)/sum(northeast_breakfast$number_of_tickets)
northeast_lunch <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Northeast" & Brown_Datathon_Store_Overall$shift_bin=="Lunch",]
average_spent_northeast_lunch <- sum(northeast_lunch$total_sales)/sum(northeast_lunch$number_of_tickets)
northeast_dinner <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Northeast" & Brown_Datathon_Store_Overall$shift_bin=="Dinner",]
average_spent_northeast_dinner <- sum(northeast_dinner$total_sales)/sum(northeast_dinner$number_of_tickets)
northeast_latenight <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Northeast" & Brown_Datathon_Store_Overall$shift_bin=="Late Night",]
average_spent_northeast_latenight <- sum(northeast_latenight$total_sales)/sum(northeast_latenight$number_of_tickets)

south_breakfast <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="South" & Brown_Datathon_Store_Overall$shift_bin=="Breakfast",]
average_spent_south_breakfast <- sum(south_breakfast$total_sales)/sum(south_breakfast$number_of_tickets)
south_lunch <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="South" & Brown_Datathon_Store_Overall$shift_bin=="Lunch",]
average_spent_south_lunch <- sum(south_lunch$total_sales)/sum(south_lunch$number_of_tickets)
south_dinner <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="South" & Brown_Datathon_Store_Overall$shift_bin=="Dinner",]
average_spent_south_dinner <- sum(south_dinner$total_sales)/sum(south_dinner$number_of_tickets)
south_latenight <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="South" & Brown_Datathon_Store_Overall$shift_bin=="Late Night",]
average_spent_south_latenight <- sum(south_latenight$total_sales)/sum(south_latenight$number_of_tickets)

Breakfast <- c(average_spent_west_breakfast, average_spent_midwest_breakfast, average_spent_northeast_breakfast, average_spent_south_breakfast)
Lunch <- c(average_spent_west_lunch, average_spent_midwest_lunch, average_spent_northeast_lunch, average_spent_south_lunch)
Dinner <- c(average_spent_west_dinner, average_spent_midwest_dinner, average_spent_northeast_dinner, average_spent_south_dinner)
Latenight <- c(average_spent_west_latenight, average_spent_midwest_latenight, average_spent_northeast_latenight, average_spent_south_latenight)
df_region <- c("West", "Midwest", "Northeast", "South")
df <- data.frame(df_region, Breakfast, Lunch, Dinner, Latenight)
df_melt <- melt(df, id.vars="df_region")

# weekday plot
salesByTypeAndDay = Brown_Datathon_Store_by_Category[, c(3, 5, 6)]
salesByTypeAndDay$day <- weekdays(as.Date(salesByTypeAndDay$date))
daysList <- unique(salesByTypeAndDay$day)
itemTypeList <- unique(salesByTypeAndDay$item_category)
# 
table <- data.frame()
for (i in daysList){
  for (j in itemTypeList){
    salesIJ <- salesByTypeAndDay[salesByTypeAndDay$day==i & salesByTypeAndDay$item_category==j,]
    oneDayTypeSale <- sum(salesIJ[, c(3)])
    table[i, j] <- oneDayTypeSale/10000
  }
}
table$dayofweek <- daysList
table2 <- melt(table, id.vars = "dayofweek")
table2$cat <- ''
table2[table2$variable == "food",]$cat <- "Food"
table2[table2$variable != "food",]$cat <- "Alcohol"

#year plot
salesByDate <- Brown_Datathon_Store_by_Category[, c(3, 5, 6)]
dateList <- unique(salesByDate$date)
dailyTable <- data.frame()
for (i in dateList){
  for (j in itemTypeList){
    salesOneDate <- salesByDate[salesByDate$date==i & salesByDate$item_category==j,]
    oneDateSales <- sum(salesOneDate[, c(3)])
    dailyTable[j, i] <- oneDateSales
  }
}
shinyServer(function(input, output) {
  output$plot3 <- renderPlotly({
    start <- input$date
    end <- input$date2
    diff <-difftime(as.Date(end), as.Date(start))
    as.numeric(gsub("([0-9]+).*$", "\\1", diff))
    startIndex <- difftime(as.Date(start), as.Date("2017-01-01")) + 1
    endIndex <- startIndex + diff
    dailyTable2 <<- dailyTable[, c(startIndex:endIndex)]
    print(dailyTable2)
    dailyTable2$Type <- itemTypeList
    dailyTable3 <- melt(dailyTable2, id.vars = "Type")
    q <- ggplot(dailyTable3, aes(x=variable, y=value, fill=Type)) +
      geom_bar(stat="identity") +
      ggtitle("Total Revenue in 2017") +
      xlab("Date") +
      ylab("Total Revenue ($)") +
      theme_bw()
    q <- ggplotly(q)
  })
  output$distPlot <- renderPlotly({
    g <- ggplot(df_melt, aes(df_region, value, fill=variable)) + 
      geom_bar(stat="identity") + xlab("Region") + ylab("Average Spent per Ticket") + 
      title("Average Spent per Ticket by Meal and Region") + 
      guides(fill=guide_legend(title="")) + 
      ggtitle("Average Spent per Ticket by Region and Meal") + theme_bw()
    g <- ggplotly(g)
  })
  
  output$plot2 <- renderPlotly({
    p <- ggplot(table2, aes(x=cat, y=value, fill=variable)) + 
      geom_bar(stat="identity", position="stack") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("Total Revenue in 2017 Per Day of the Week") +
      guides(fill=guide_legend(title="")) + 
      xlab("") + ylab("Total Revenue in 2017 ($10000)") + 
      facet_grid(~ dayofweek) + theme_bw()
    p <- ggplotly(p)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(Brown_Datathon_Store_by_Category)
  })
  
})

# # This is the server logic for a Shiny web application.
# # You can find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com
# #
# 
# library(shiny)
# library(ggplot2)
# library(reshape2)
# library(plotly)
# library(lubridate)
# 
# 
# #regional plot
# regions <- unique(Brown_Datathon_Store_Overall$region)
# west_breakfast <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="West" & Brown_Datathon_Store_Overall$shift_bin=="Breakfast",]
# average_spent_west_breakfast <- sum(west_breakfast$total_sales)/sum(west_breakfast$number_of_tickets)
# west_lunch <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="West" & Brown_Datathon_Store_Overall$shift_bin=="Lunch",]
# average_spent_west_lunch <- sum(west_lunch$total_sales)/sum(west_lunch$number_of_tickets)
# west_dinner <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="West" & Brown_Datathon_Store_Overall$shift_bin=="Dinner",]
# average_spent_west_dinner <- sum(west_dinner$total_sales)/sum(west_dinner$number_of_tickets)
# west_latenight <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="West" & Brown_Datathon_Store_Overall$shift_bin=="Late Night",]
# average_spent_west_latenight <- sum(west_latenight$total_sales)/sum(west_latenight$number_of_tickets)
# 
# midwest_breakfast <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Midwest" & Brown_Datathon_Store_Overall$shift_bin=="Breakfast",]
# average_spent_midwest_breakfast <- sum(midwest_breakfast$total_sales)/sum(midwest_breakfast$number_of_tickets)
# midwest_lunch <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Midwest" & Brown_Datathon_Store_Overall$shift_bin=="Lunch",]
# average_spent_midwest_lunch <- sum(midwest_lunch$total_sales)/sum(midwest_lunch$number_of_tickets)
# midwest_dinner <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Midwest" & Brown_Datathon_Store_Overall$shift_bin=="Dinner",]
# average_spent_midwest_dinner <- sum(midwest_dinner$total_sales)/sum(midwest_dinner$number_of_tickets)
# midwest_latenight <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Midwest" & Brown_Datathon_Store_Overall$shift_bin=="Late Night",]
# average_spent_midwest_latenight <- sum(midwest_latenight$total_sales)/sum(midwest_latenight$number_of_tickets)
# 
# northeast_breakfast <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Northeast" & Brown_Datathon_Store_Overall$shift_bin=="Breakfast",]
# average_spent_northeast_breakfast <- sum(northeast_breakfast$total_sales)/sum(northeast_breakfast$number_of_tickets)
# northeast_lunch <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Northeast" & Brown_Datathon_Store_Overall$shift_bin=="Lunch",]
# average_spent_northeast_lunch <- sum(northeast_lunch$total_sales)/sum(northeast_lunch$number_of_tickets)
# northeast_dinner <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Northeast" & Brown_Datathon_Store_Overall$shift_bin=="Dinner",]
# average_spent_northeast_dinner <- sum(northeast_dinner$total_sales)/sum(northeast_dinner$number_of_tickets)
# northeast_latenight <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="Northeast" & Brown_Datathon_Store_Overall$shift_bin=="Late Night",]
# average_spent_northeast_latenight <- sum(northeast_latenight$total_sales)/sum(northeast_latenight$number_of_tickets)
# 
# south_breakfast <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="South" & Brown_Datathon_Store_Overall$shift_bin=="Breakfast",]
# average_spent_south_breakfast <- sum(south_breakfast$total_sales)/sum(south_breakfast$number_of_tickets)
# south_lunch <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="South" & Brown_Datathon_Store_Overall$shift_bin=="Lunch",]
# average_spent_south_lunch <- sum(south_lunch$total_sales)/sum(south_lunch$number_of_tickets)
# south_dinner <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="South" & Brown_Datathon_Store_Overall$shift_bin=="Dinner",]
# average_spent_south_dinner <- sum(south_dinner$total_sales)/sum(south_dinner$number_of_tickets)
# south_latenight <- Brown_Datathon_Store_Overall[Brown_Datathon_Store_Overall$region=="South" & Brown_Datathon_Store_Overall$shift_bin=="Late Night",]
# average_spent_south_latenight <- sum(south_latenight$total_sales)/sum(south_latenight$number_of_tickets)
# 
# Breakfast <- c(average_spent_west_breakfast, average_spent_midwest_breakfast, average_spent_northeast_breakfast, average_spent_south_breakfast)
# Lunch <- c(average_spent_west_lunch, average_spent_midwest_lunch, average_spent_northeast_lunch, average_spent_south_lunch)
# Dinner <- c(average_spent_west_dinner, average_spent_midwest_dinner, average_spent_northeast_dinner, average_spent_south_dinner)
# Latenight <- c(average_spent_west_latenight, average_spent_midwest_latenight, average_spent_northeast_latenight, average_spent_south_latenight)
# df_region <- c("West", "Midwest", "Northeast", "South")
# df <- data.frame(df_region, Breakfast, Lunch, Dinner, Latenight)
# df_melt <- melt(df, id.vars="df_region")
# 
# # # weekday plot
# salesByTypeAndDay = Brown_Datathon_Store_by_Category[, c(3, 5, 6)]
# salesByTypeAndDay$day <- weekdays(as.Date(salesByTypeAndDay$date))
# # daysList <- unique(salesByTypeAndDay$day)
# itemTypeList <- unique(salesByTypeAndDay$item_category)
# # 
# # table <- data.frame()
# # for (i in daysList){
# #   for (j in itemTypeList){
# #     salesIJ <- salesByTypeAndDay[salesByTypeAndDay$day==i & salesByTypeAndDay$item_category==j,]
# #     oneDayTypeSale <- sum(salesIJ[, c(3)])
# #     table[j, i] <- oneDayTypeSale
# #   }
# # }
# # table$Type <- itemTypeList
# # table2 <- melt(table, id.vars = "Type")
# 
# #yearly plot
# 
# salesByDate <- Brown_Datathon_Store_by_Category[, c(3, 5, 6)]
# dateList <- unique(salesByDate$date)
# dailyTable <- data.frame()
# for (i in dateList){
#   for (j in itemTypeList){
#     salesOneDate <- salesByDate[salesByDate$date==i & salesByDate$item_category==j,]
#     oneDateSales <- sum(salesOneDate[, c(3)])
#     dailyTable[j, i] <- oneDateSales
#   }
# }
# print(dailyTable)
# # dailyTable <- reactive({
# #   dailyTable[, c(input$date:input$date2)]
# # })
# 
# 
# 
# 
# shinyServer(function(input, output) {
#   
#   # output$distPlot <- renderPlotly({
#   #   g <- ggplot(df_melt, aes(df_region, value, fill=variable)) + geom_bar(stat="identity") + xlab("Region") + ylab("Average Spent per Ticket") + title("Average Spent per Ticket by Meal and Region") + guides(fill=guide_legend(title="Meal")) + ggtitle("Average Spent per Ticket by Region and Meal")
#   #   g <- ggplotly(g)
#   # })
#   # 
#   # output$plot2 <- renderPlotly({
#   #   p <- ggplot(table2, aes(x=variable, y=value, fill=Type)) +
#   #     geom_bar(stat="identity", position="dodge") +
#   #     ggtitle("Total Revenue in 2017 Per Day of the Week") +
#   #     xlab("Day of the Week") +
#   #     ylab("Total Revenue in 2017 ($)") +
#   #     theme_bw()
#   #   p <- ggplotly(p)
#   # })
#   
#   output$plot3 <- renderPlotly({
#     start <- input$date
#     end <- input$date2
#     diff <-difftime(as.Date(end), as.Date(start))
#     as.numeric(gsub("([0-9]+).*$", "\\1", diff))
#     startIndex <- difftime(as.Date(start), as.Date("2017-01-01"))
#     endIndex <- startIndex + diff
#     dailyTable2 <<- dailyTable[, c(startIndex:endIndex)]
#     print(dailyTable2)
#     dailyTable2$Type <- itemTypeList
#     dailyTable3 <- melt(dailyTable2, id.vars = "Type")
#     q <- ggplot(dailyTable3, aes(x=variable, y=value, fill=Type)) +
#       geom_bar(stat="identity") +
#       ggtitle("Total Revenue in 2017") +
#       xlab("Date") +
#       ylab("Total Revenue ($)") +
#       theme_bw()
#     q <- ggplotly(q)
#   })
# 
# 
# })
