library(ggplot2)
library(lubridate)
library(gh)
library(shiny)


global_size = 10


# Define the UI
ui <- bootstrapPage(
  textInput("orgrepo", "Enter org name, then slash, then repo name", ""),
  submitButton(text = "Submit"),
  fluidRow(column(8,tabsetPanel(type = "tabs",
              tabPanel("Plot 1", plotOutput("plot1")),
              tabPanel("Plot 2", plotOutput("plot2")),
              tabPanel("Plot 3", plotOutput("plot3")),
              tabPanel("Plot 4", plotOutput("plot4")),
              tabPanel("Plot 5", plotOutput("plot5")),
              tabPanel("Plot 6", plotOutput("plot6")),
              tabPanel("Table", tableOutput("table")))
  ))
)

# Define the server code
server <- function(input, output) {
  fetchGithubTable = reactive({
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
      data = gh(paste("GET /repos/", input$orgrepo, "/issues?state=all", sep = ""),
        page = page)
      page = page + 1
      res = c(res, data)
      len = length(data)
      print(page)
    }

    print("done")

    title = sapply(res, function(row) {
      row[["title"]]
    })
    closed_at = sapply(res, function(row) {
      row[["closed_at"]]
    })
    created_at = sapply(res, function(row) {
      row[["created_at"]]
    })
    number = sapply(res, function(row) {
      row[["number"]]
    })

    start = ymd_hms(created_at)
    end = sapply(closed_at, function(x) {
      ifelse(is.null(x), ymd_hms(x), x)
    })
    start_date = as.Date(start)
    end_date = as.Date(end)
    months = as.numeric(end_date - start_date)/30

    data.frame(start, end, title, start_date, end_date, months, number)
  })



  output$plot1 <- renderPlot({
    if (input$orgrepo != "") {
      my_table = fetchGithubTable()
      ggplot(my_table) + geom_linerange(aes(ymin = end_date, ymax = start_date,
        x = months, color = number, size = I(1), text = title)) + scale_color_gradientn(colours = rainbow(5)) +
        theme_bw() + theme(panel.grid = element_blank()) + xlab("Months taken") +
        ylab("Time taken to close issue") + ggtitle("Issues completed over time")++  theme_classic(base_size = global_size)
    } else {
      title("Waiting for input...")
    }
  })

  output$plot2 <- renderPlot({
    if (input$orgrepo != "") {
      my_table = fetchGithubTable()
      ggplot(my_table) + geom_linerange(aes(ymin = end_date, ymax = start_date,
        x = number, color = months, text = title, size = I(1))) + scale_color_gradientn(colours = rainbow(5)) +
        theme_bw() + theme(panel.grid = element_blank()) + xlab("Issue number") +
        ylab("Time taken to close issue") + ggtitle("Issues completed over time")
    } else {
      title("Waiting for input...")
    }
  })


  output$plot3 <- renderPlot({
    if (input$orgrepo != "") {
      my_table = fetchGithubTable()
      ggplot(my_table) + geom_point(aes(y = months, x = end_date, color = number,
        text = title, size = I(1))) + scale_color_gradientn(colours = rainbow(5)) +
        theme_bw() + theme(panel.grid = element_blank()) + xlab("Issue number") +
        ylab("Months taken to finish") + ggtitle("Issues completed over time")
    } else {
      title("Waiting for input...")
    }
  })



  output$plot4 <- renderPlot({
    if (input$orgrepo != "") {
      my_table = fetchGithubTable()
      ggplot(my_table) + geom_point(aes(y = number, x = end_date, color = months,
        text = title, size = I(1))) + scale_color_gradientn(colours = rainbow(5)) +
        theme_bw() + theme(panel.grid = element_blank()) + xlab("End date") +
        ylab("Issue number") + ggtitle("Issues completed over time")
    } else {
      title("Waiting for input...")
    }
  })

  output$plot5 <- renderPlot({
    if (input$orgrepo != "") {
      my_table = fetchGithubTable()
      qplot(months, data = my_table) +
        scale_y_log10() +
        theme_bw() +
        theme(panel.grid = element_blank()) +
        xlab("Months taken to complete") +
        ylab("#issues that took this long to complete") +
        ggtitle("Binned distribution of time taken to complete issues")
    } else {
      title("Waiting for input...")
    }
  })


  output$plot6 <- renderPlot({
    if (input$orgrepo != "") {
      my_table = fetchGithubTable()
      my_table$start = as.POSIXct(my_table$start)
      my_table$end = as.POSIXct(my_table$end)
      my_table[is.na(my_table)] <- as.Date(Sys.time())
      days <- as.Date(seq(min(my_table$start), Sys.time(), 86400))

      total <- rowSums(outer(days, my_table$start, ">") & outer(days, my_table$end,
        "<"))
      my_table2 <- data.frame(days, total)


      qplot(days, total, data = my_table2) + geom_line() + theme_bw() + theme(panel.grid = element_blank()) +
        ylab("Issues open") + ggtitle("Total issues open at any given time")
    } else {
      title("Waiting for input...")
    }
  })




  output$table <- renderDataTable({
    if (input$orgname) {
      my_table = fetchGithubTable()
      format(head(my_table[order(-my_table$months), c("number", "title", "months")],
        n = 100), digits = 2)
    } else {
      data.frame()
    }
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)

