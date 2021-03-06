library(ggplot2)
library(plyr)
library(lubridate)
library(gh)
library(shiny)
library(plotly)

readRenviron(".Renviron")

global_size <- 18


# Define the UI
ui <- fluidPage(
  titlePanel("Issue burndown charts"),
  a(href = "https://github.com/cmdcolin/shinyburndown", "Source code repo"),
  br(), textInput(
    "orgrepo", "Enter org/repo e.g. GMOD/bam-js. Note that repos with many issues will take awhile to complete and may exhaust API call limits",
    "GMOD/bam-js"
  ),
  textInput(
    "token", "Optional custom token. Please use if submitting large calls jobs. See https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token turn on \"repos\" access",
    ""
  ),
  submitButton(text = "Submit"),
  fluidRow(
    column(
      8, tabsetPanel(
        type = "tabs", tabPanel(
          "Plot 1", div(
            p("Note: hover over top of bar for tooltip"),
            downloadButton("exportpdf1", label = "Export PDF"),
            downloadButton("exportpng1", label = "Export PNG"),
            plotlyOutput("plot1")
          )
        ),
        tabPanel(
          "Plot 2", div(
            p("Note: hover over top of bar for tooltip"),
            downloadButton("exportpdf2", label = "Export PDF"),
            downloadButton("exportpng2", label = "Export PNG"),
            plotlyOutput("plot2")
          )
        ),
        tabPanel(
          "Plot 3", div(
            downloadButton("exportpdf3", label = "Export PDF"),
            downloadButton("exportpng3", label = "Export PNG"),
            plotlyOutput("plot3")
          )
        ),
        tabPanel(
          "Plot 4", div(
            downloadButton("exportpdf4", label = "Export PDF"),
            downloadButton("exportpng4", label = "Export PNG"),
            plotlyOutput("plot4")
          )
        ),
        tabPanel(
          "Plot 5", div(
            downloadButton("exportpdf5", label = "Export PDF"),
            downloadButton("exportpng5", label = "Export PNG"),
            plotlyOutput("plot5")
          )
        ),
        tabPanel(
          "Plot 6", div(
            downloadButton("exportpdf6", label = "Export PDF"),
            downloadButton("exportpng6", label = "Export PNG"),
            plotlyOutput("plot6")
          )
        ),
        tabPanel(
          "Plot 7", div(
            downloadButton("exportpdf7", label = "Export PDF"),
            downloadButton("exportpng7", label = "Export PNG"),
            plotlyOutput("plot7")
          )
        ),
        tabPanel("Longest open issues", dataTableOutput("table"))
      )
    )
  )
)

# Define the server code
server <- function(input, output) {
  fetchGithubTable <- reactive({
    if (input$orgrepo == "") {
      data.frame(
        start = character(),
        end = character(),
        title = character(),
        start_date = character(),
        end_date = character(),
        months = character(),
        issue_number = character(),
        stringsAsFactors = F
      )
    } else {
      res <- c()
      data <- NULL
      len <- 1
      page <- 1
      while (len != 0) {
        if (input$token != "") {
          result <- gh_rate_limit(.token = input$token)
        } else {
          result <- gh_rate_limit()
        }
        if (result$remaining == 0) {
          time <- as.numeric(difftime(Sys.time(), result$reset, units = "secs"))
          print(-time)
          Sys.sleep(-time)
        }
        if (input$token != "") {
          data <- gh(
            paste("GET /repos/", input$orgrepo, "/issues?state=all", sep = ""),
            page = page, .token = input$token
          )
        } else {
          data <- gh(
            paste("GET /repos/", input$orgrepo, "/issues?state=all", sep = ""),
            page = page
          )
        }
        page <- page + 1
        res <- c(res, data)
        len <- length(data)
        print(page)
      }

      print("done")

      title <- sapply(
        res, function(row) {
          row[["title"]]
        }
      )
      closed_at <- sapply(
        res, function(row) {
          row[["closed_at"]]
        }
      )
      created_at <- sapply(
        res, function(row) {
          row[["created_at"]]
        }
      )
      issue_number <- sapply(
        res, function(row) {
          row[["number"]]
        }
      )

      start <- ymd_hms(created_at)
      end <- sapply(
        closed_at, function(x) {
          ifelse(
            is.null(x),
            ymd_hms(x),
            x
          )
        }
      )
      start_date <- as.Date(start)
      end_date <- as.Date(end)
      months <- as.numeric(end_date - start_date) / 30

      data.frame(start, end, title, start_date, end_date, months, issue_number)
    }
  })


  vals <- reactiveValues(
    p1 = NULL,
    p2 = NULL,
    p3 = NULL,
    p4 = NULL,
    p5 = NULL,
    p6 = NULL,
    p7 = NULL
  )



  output$plot1 <- renderPlotly({
    my_table <- fetchGithubTable()
    vals$p1 <- ggplot(my_table) +
      geom_linerange(
        aes(
          ymin = end_date,
          ymax = start_date,
          x = months,
          color = issue_number,
          size = I(1),
          text = title
        )
      ) +
      scale_color_gradientn(colours = rainbow(5)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      xlab("Months taken") +
      ylab("Time span that issue covered") +
      ggtitle("Months to complete vs Time span that issue covered") +
      theme_classic(base_size = global_size)
    plotly::ggplotly(vals$p1, width = 1000, height = 600)
  })

  output$plot2 <- renderPlotly({
    my_table <- fetchGithubTable()
    vals$p2 <- ggplot(my_table) +
      geom_linerange(
        aes(
          ymin = end_date,
          ymax = start_date,
          x = issue_number,
          color = months,
          text = title, size = I(1)
        )
      ) +
      scale_color_gradientn(colours = rainbow(5)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      xlab("Issue number") +
      ylab("Time taken to close issue") +
      ggtitle("Issues completed over time") +
      theme_classic(base_size = global_size)
    plotly::ggplotly(vals$p2, width = 1000, height = 600)
  })


  output$plot3 <- renderPlotly({
    my_table <- fetchGithubTable()
    vals$p3 <- ggplot(my_table) +
      geom_point(
        aes(
          y = months, x = end_date, color = issue_number, text = title,
          size = I(1)
        )
      ) +
      scale_color_gradientn(colours = rainbow(5)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      xlab("Issue number") +
      ylab("Months taken to finish") +
      ggtitle("Issue number vs Time taken to complete") +
      theme_classic(base_size = global_size)
    plotly::ggplotly(vals$p3, width = 1000, height = 600)
  })



  output$plot4 <- renderPlotly({
    my_table <- fetchGithubTable()
    vals$p4 <- ggplot(my_table) +
      geom_point(
        aes(
          y = issue_number, x = end_date, color = months, text = title,
          size = I(1)
        )
      ) +
      scale_color_gradientn(colours = rainbow(5)) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      xlab("Close date") +
      ylab("Issue number") +
      ggtitle("Issue close date vs Issue number") +
      theme_classic(base_size = global_size)
    plotly::ggplotly(vals$p4, width = 1000, height = 600)
  })

  output$plot5 <- renderPlotly({
    my_table <- fetchGithubTable()
    vals$p5 <- qplot(months, data = my_table) +
      scale_y_log10() +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      xlab("Months taken to complete") +
      ylab("log10(#issues in bin)") +
      ggtitle("Binned distribution of time taken to complete issues") +
      theme_classic(base_size = global_size)
    plotly::ggplotly(vals$p5, width = 1000, height = 600)
  })


  output$plot6 <- renderPlotly({
    my_table <- fetchGithubTable()
    my_table$start <- as.POSIXct(my_table$start)
    my_table$end <- as.POSIXct(my_table$end)
    my_table[is.na(my_table)] <- as.Date(Sys.time())
    days <- as.Date(
      seq(
        min(my_table$start),
        Sys.time(), 86400
      )
    )

    total <- rowSums(
      outer(days, my_table$start, ">") & outer(days, my_table$end, "<")
    )
    my_table2 <- data.frame(days, total)


    vals$p6 <- qplot(days, total, data = my_table2) +
      geom_line() +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      ylab("Issues open") +
      xlab("Time") +
      ggtitle("Total issues open at any given time") +
      theme_classic(base_size = global_size)
    plotly::ggplotly(vals$p6, width = 1000, height = 600)
  })

  output$plot7 <- renderPlotly({
    df <- fetchGithubTable()
    df$Day <- as.character(round_date(df$end_date, unit = "day"))
    stats <- ddply(df, .(Day), summarise, Count = length(Day)) %>% filter(!is.na(Day))


    vals$p7 <- ggplot(stats, aes(Count)) +
      geom_bar() +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      ggtitle("Issues closed per day") +
      theme_classic(base_size = global_size)
    plotly::ggplotly(vals$p7, width = 1000, height = 600)
  })



  output$table <- renderDataTable({
    my_table <- fetchGithubTable()
    format(
      head(
        my_table[
          order(-my_table$months),
          c("issue_number", "title", "months")
        ],
        n = 100
      ),
      digits = 2
    )
  })




  output$exportpdf1 <- downloadHandler(
    filename = function() {
      "plot.pdf"
    }, content = function(file) {
      ggsave(file, plot = vals$p1, device = "pdf", width = 8, height = 4, scale = 2)
    }
  )

  output$exportpng1 <- downloadHandler(
    filename = function() {
      "plot.png"
    }, content = function(file) {
      ggsave(file, plot = vals$p1, device = "png", width = 8, height = 4, scale = 2)
    }
  )


  output$exportpdf2 <- downloadHandler(
    filename = function() {
      "plot.pdf"
    }, content = function(file) {
      ggsave(file, plot = vals$p2, device = "pdf", width = 8, height = 4, scale = 2)
    }
  )

  output$exportpng2 <- downloadHandler(
    filename = function() {
      "plot.png"
    }, content = function(file) {
      ggsave(file, plot = vals$p2, device = "png", width = 8, height = 4, scale = 2)
    }
  )


  output$exportpdf3 <- downloadHandler(
    filename = function() {
      "plot.pdf"
    }, content = function(file) {
      ggsave(file, plot = vals$p3, device = "pdf", width = 8, height = 4, scale = 2)
    }
  )

  output$exportpng3 <- downloadHandler(
    filename = function() {
      "plot.png"
    }, content = function(file) {
      ggsave(file, plot = vals$p3, device = "png", width = 8, height = 4, scale = 2)
    }
  )


  output$exportpdf4 <- downloadHandler(
    filename = function() {
      "plot.pdf"
    }, content = function(file) {
      ggsave(file, plot = vals$p4, device = "pdf", width = 8, height = 4, scale = 2)
    }
  )

  output$exportpng4 <- downloadHandler(
    filename = function() {
      "plot.png"
    }, content = function(file) {
      ggsave(file, plot = vals$p4, device = "png", width = 8, height = 4, scale = 2)
    }
  )


  output$exportpdf5 <- downloadHandler(
    filename = function() {
      "plot.pdf"
    }, content = function(file) {
      ggsave(file, plot = vals$p5, device = "pdf", width = 8, height = 4, scale = 2)
    }
  )

  output$exportpng5 <- downloadHandler(
    filename = function() {
      "plot.png"
    }, content = function(file) {
      ggsave(file, plot = vals$p5, device = "png", width = 8, height = 4, scale = 2)
    }
  )

  output$exportpdf6 <- downloadHandler(
    filename = function() {
      "plot.pdf"
    }, content = function(file) {
      ggsave(file, plot = vals$p6, device = "pdf", width = 8, height = 4, scale = 2)
    }
  )

  output$exportpng6 <- downloadHandler(
    filename = function() {
      "plot.png"
    }, content = function(file) {
      ggsave(file, plot = vals$p6, device = "png", width = 8, height = 4, scale = 2)
    }
  )


  output$exportpdf7 <- downloadHandler(
    filename = function() {
      "plot.pdf"
    }, content = function(file) {
      ggsave(file, plot = vals$p7, device = "pdf", width = 8, height = 4, scale = 2)
    }
  )

  output$exportpng7 <- downloadHandler(
    filename = function() {
      "plot.png"
    }, content = function(file) {
      ggsave(file, plot = vals$p7, device = "png", width = 8, height = 4, scale = 2)
    }
  )
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)
