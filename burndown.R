library(ggplot2)
library(lubridate)
library(gh)

write.table(
  format(
    head(
      my_table[order(-my_table$months), c("number", "title", "months")], n = 100
    ),
    digits = 2
  ), "longest_open_issues.csv", row.names = F, quote = F, sep = "\t"
)

# Define the UI
ui <- bootstrapPage(
  textInput('orgrepo', 'Enter org name, then slash, then repo name', ''),
  submitButton(text = "Submit"),
  plotOutput('plot1'),
  plotOutput('plot2'),
  dataTableOutput('table')
)

# Define the server code
server <- function (input, output) {
  table = reactive({
    res = c()
    data = NULL
    len = 1
    page = 1
    while (len != 0) {
      result = gh_rate_limit()
      if (result$remaining == 0) {
        time = as.numeric(difftime(Sys.time(), result$reset, units = "secs"))
        print(-time)
        Sys.sleep(-time)
      }
      data = gh(paste("GET /repos/", input$orgrepo, "/issues?state=all"), page = page)
      page = page + 1
      res = c(res, data)
      len = length(data)
      print(page)
    }

    print('done')

    title = sapply(res, function (row) {
      row[['title']]
    })
    closed_at = sapply(res, function (row) {
      row[['closed_at']]
    })
    created_at = sapply(res, function (row) {
      row[['created_at']]
    })
    number = sapply(res, function (row) {
      row[['number']]
    })

    start < -ymd_hms(created_at)
    end < -sapply(closed_at, function (x) {
      ifelse(is.null(x), ymd_hms(x), x)
    })
    start_date < -as.Date(start)
    end_date < -as.Date(end)
    months < -as.numeric(end_date - start_date) / 30

    my_table = data.frame(start, end, title, start_date, end_date, months, number)
  })

  output$plot1 <- renderPlot({
    if (input$orgrepo != '') {
      ggplot(my_table) + geom_linerange(aes(
          ymin = end_date,
          ymax = start_date,
          x = months,
          color = number,
          size = I(1),
          text = title
        )) +
        scale_color_gradientn(colours = rainbow(5)) +
        theme_bw() +
        theme(panel.grid = element_blank()) +
        xlab("Months taken") +
        ylab("Time taken to close issue") +
        ggtitle("Issues completed over time")
    } else {
      title('Waiting for input...')
    }
  })

  output$plot2 <- renderPlot({
    if (input$orgrepo != '') {
      ggplot(my_table) + geom_linerange(aes(
          ymin = end_date,
          ymax = start_date,
          x = number,
          color = months,
          text = title,
          size = I(1)
        )) +
        scale_color_gradientn(colours = rainbow(5)) +
        theme_bw() +
        theme(panel.grid = element_blank()) +
        xlab("Issue number") +
        ylab("Time taken to close issue") +
        ggtitle("Issues completed over time")
    } else {
      title('Waiting for input...')
    }
  })

  output$table <- renderDataTable(
    format(head(my_table[order(-my_table$months), c("number", "title", "months")], n = 100), digits = 2)
  )
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)



## Set factor level to order the activities on the plot

# p<-ggplot(my_table) + geom_linerange(aes(
#   ymin = end_date,
#   ymax = start_date,
#   x = months,
#   color = number,
#   size = I(1),
#   text=title
# )) +
#   scale_color_gradientn(colours = rainbow(5)) +
#   theme_bw() +
#   theme(panel.grid = element_blank()) +
#   xlab("Months taken") +
#   ylab("Time taken to close issue") +
#   ggtitle("Issues completed over time")


# ggsave("issues.png",p)
# l<-plotly::ggplotly(p)
# htmlwidgets::saveWidget(l, "index.html")




# p<-ggplot(my_table) + geom_linerange(aes(
#   ymin = end_date,
#   ymax = start_date,
#   x = number,
#   color = months,
#   text = title,
#   size = I(1)
# )) +
#   scale_color_gradientn(colours = rainbow(5)) +
#   theme_bw() +
#   theme(panel.grid = element_blank()) +
#   xlab("Issue number") +
#   ylab("Time taken to close issue") +
#   ggtitle("Issues completed over time")



# l<-plotly::ggplotly(p)
# ggsave("issues2.png",p)
# htmlwidgets::saveWidget(l, "index2.html")



# p <- ggplot(my_table)+geom_point(aes(
#   y = months,
#   x = end_date,
#   color = number,
#   text=title,
#   size = I(1)
# )) +
#   scale_color_gradientn(colours = rainbow(5)) +
#   theme_bw() +
#   theme(panel.grid = element_blank()) +
#   xlab("Issue number") +
#   ylab("Months taken to finish") +
#   ggtitle("Issues completed over time")


# l<-plotly::ggplotly(p)
# ggsave("issues3.png",p)
# htmlwidgets::saveWidget(l, "index3.html")



# p <- ggplot(my_table)+geom_point(aes(
#   y = number,
#   x = end_date,
#   color = months,
#   text=title,
#   size = I(1)
# )) +
#   scale_color_gradientn(colours = rainbow(5)) +
#   theme_bw() +
#   theme(panel.grid = element_blank()) +
#   xlab("End date") +
#   ylab("Issue number") +
#   ggtitle("Issues completed over time")


# l<-plotly::ggplotly(p)
# ggsave("issues4.png")
# htmlwidgets::saveWidget(l, "index4.html")


# p<-qplot(months, data = my_table) +
#   scale_y_log10() +
#   theme_bw() +
#   theme(panel.grid = element_blank())

# l<-plotly::ggplotly(p)
# ggsave("distribution.png",p)
# htmlwidgets::saveWidget(l, "distibution.html")


# my_table[is.na(my_table)] <- Sys.time()
# days <- as.Date(seq(min(my_table$start), Sys.time(), 86400))

# total <- rowSums(outer(days, my_table$start, ">") & outer(days, my_table$end, "<"))
# my_table2 <- data.frame(days, total)


# p<-qplot(days, total, data = my_table2) +
#   geom_line() +
#   theme_bw() +
#   theme(panel.grid = element_blank()) +
#   ylab("Issues open") +
#   ggtitle("Total issues open at any given time")

# l<-plotly::ggplotly(p)
# ggsave("burndown.png",p)
# htmlwidgets::saveWidget(l, "burndown.html")
