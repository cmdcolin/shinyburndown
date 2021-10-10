
# step 1 run `npm install -g github-csv-tools`
# step 2 run `githubCsvTools -f file.csv`
library(ggplot2)
library(lubridate)
mytable <- read.csv("file.csv")
mytable$start <- ymd_hms(mytable$created_at)
mytable$end <- ymd_hms(mytable$closed_at)
mytable$start_date <- as.Date(mytable$start)
mytable$end_date <- as.Date(mytable$end)
mytable$months <- as.numeric(mytable$end_date - mytable$start_date) / 30

write.table(
  format(
    head(
      mytable[order(-mytable$months), c("number", "title", "months")], n = 100
    ),
    digits = 2
  ), "longest_open_issues.csv", row.names = F, quote = F, sep = "\t"
)



## Set factor level to order the activities on the plot

p<-ggplot(mytable) + geom_linerange(aes(
  ymin = end_date,
  ymax = start_date,
  x = months,
  color = number,
  size = I(1),
  text=title
)) +
  scale_color_gradientn(colours = rainbow(5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("Months taken") +
  ylab("Time taken to close issue") +
  ggtitle("completed issues gantt chart")


l<-plotly::ggplotly(p)
ggsave("issues.png",p)
htmlwidgets::saveWidget(l, "index.html")




p<-ggplot(mytable) + geom_linerange(aes(
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


l<-plotly::ggplotly(p)
ggsave("issues2.png",p)
htmlwidgets::saveWidget(l, "index2.html")



p <- ggplot(mytable)+geom_point(aes(
  y = months,
  x = end_date,
  color = number,
  text=title,
  size = I(1)
)) +
  scale_color_gradientn(colours = rainbow(5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("Issue number") +
  ylab("Months taken to finish") +
  ggtitle("Issues completed over time")


l<-plotly::ggplotly(p)
ggsave("issues3.png",p)
htmlwidgets::saveWidget(l, "index3.html")



p <- ggplot(mytable)+geom_point(aes(
  y = number,
  x = end_date,
  color = months,
  text=title,
  size = I(1)
)) +
  scale_color_gradientn(colours = rainbow(5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  xlab("End date") +
  ylab("Issue number") +
  ggtitle("Issues completed over time")


l<-plotly::ggplotly(p)
ggsave("issues4.png")
htmlwidgets::saveWidget(l, "index4.html")


p<-qplot(months, data = mytable) +
  scale_y_log10() +
  theme_bw() +
  theme(panel.grid = element_blank())

l<-plotly::ggplotly(p)
ggsave("distribution.png",p)
htmlwidgets::saveWidget(l, "distibution.html")


mytable[is.na(mytable)] <- Sys.time()
days <- as.Date(seq(min(mytable$start), Sys.time(), 86400))

total <- rowSums(outer(days, mytable$start, ">") & outer(days, mytable$end, "<"))
mytable2 <- data.frame(days, total)


p<-qplot(days, total, data = mytable2) +
  geom_line() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab("Issues open") +
  ggtitle("total issues open over time")

l<-plotly::ggplotly(p)
ggsave("burndown.png",p)
htmlwidgets::saveWidget(l, "burndown.html")
