library(ggplot2)
library(lubridate)
library(gh)

res = c()
data=NULL
len=1
page=1
while(len!=0) {
  result = gh_rate_limit()
  if(result$remaining==0) {
    time = as.numeric(difftime(Sys.time(),result$reset,units="secs"))
    print(-time)
    Sys.sleep(-time)
  }
  data = gh("GET /repos/GMOD/jbrowse-components/issues?state=all",page=page)
  page=page+1
  res=c(res,data)
  len=length(data)
  print(page)
}


title = sapply(res, function(row) { row[['title']] })
closed_at = sapply(res, function(row) { row[['closed_at']] })
created_at = sapply(res, function(row) { row[['created_at']] })
number = sapply(res, function(row) { row[['number']] })

start <- ymd_hms(created_at)
end <- sapply(closed_at,function(x) { ifelse(is.null(x),ymd_hms(x),x) })
start_date <- as.Date(start)
end_date <- as.Date(end)
months <- as.numeric(end_date - start_date) / 30

my_table=data.frame(start,end,title,start_date,end_date,months,number)

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
  ggtitle("Issues completed over time")


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
  ggtitle("Issues completed over time")



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
  ggtitle("Total issues open at any given time")

l<-plotly::ggplotly(p)
ggsave("burndown.png",p)
htmlwidgets::saveWidget(l, "burndown.html")
