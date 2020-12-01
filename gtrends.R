library(dplyr)
library(ggplot2)
#install.packages("anomalize")
library(anomalize)
#install.packages("gtrendsR")
library(gtrendsR)
#install.packages("prophet")
library(prophet)
#install.packages("plotly")
library(plotly)

gtrends_df = gtrends(c("Vaccine"), #keywords - start with one
                     gprop = "web", #choose: web, news, images, froogle, youtube
                     geo = c("US"), #only pull results for US (Country code)
                     time = "2010-01-01 2020-11-27")[[1]] #timeframe


#visualize with ggplot (optional but useful if you're choosing between keywords)

ggplot(data=gtrends_df, 
       aes(x=date, y=hits, group=keyword, col=keyword)) +
  geom_line() + 
  theme_bw() +
  labs(title = "Google Trends Data", 
       subtitle="Search volume", 
       x="Time", y="Relative Interest")

# Prepare data

gtrends_df_tbl = gtrends_df %>%
  mutate(date = lubridate::ymd(date)) %>%
  tibble::as.tibble()

# Anomalize

gtrends <- gtrends_df_tbl %>%
  time_decompose(hits, method = "twitter", trend = "1 year") %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = T) +
  labs(title = "google trends (Vaccine) - twitter + gesd method", x = "Time",t="Relative Interest", subtitle = "NL search volume for vote for the word Vaccine")

ggplotly(gtrends)
# Visualize inner workings off how algorithm detects anomalies in the remainder.

gtrends_decomposed <- gtrends_df_tbl %>%
  time_decompose(hits, method = "twitter", frequency = "1 year", trend = "1 year") %>%
  anomalize(remainder, metho = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()

ggplotly(gtrends_decomposed)

# EXTRA -> Forecasting

gtrends_df_for <- gtrends_df %>%
  mutate(date=lubridate::ymd(date)) %>% #parse date
  tbl_df() %>%
  mutate(ds=date,y=hits) %>%
  select(ds,y) #format for prophet

gtrends_df_for$cap <- 100 #google trends hits is a normalized value
gtrends_df_for$floor <- 0 #ranges from 0-100
m <- prophet(gtrends_df_for,growth = "logistic")

future <- make_future_dataframe(m, periods = 52, freq = "week")
future$cap <- 100
future$floor <- 0

forecast <- predict(m,future)
forecast %>%
  select(ds,yhat,yhat_lower,yhat_upper) %>%
  rename("week"="ds",
         "forecast" = "yhat",
         "forecast_lower" = "yhat_lower",
         "forecast_upper" = "yhat_upper") %>%
  filter(week>="2010-01-01")

gtrends_forecast <- plot(m, forecast)
ggplotly(gtrends_forecast+ggtitle("Google trends forecast - Logistic trend")+ylab("Relative Interest")+xlab("Time"))
