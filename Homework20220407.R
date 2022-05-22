#####Problem 1#####
# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns.
Stock_info = tidyquant::tq_get(c("AMZN", "FB", "NFLX"),
                       from = "2019-01-01",
                       to = "2021-04-01") %>%
  dplyr::select(symbol,date,adjusted)
# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation.
Dates = data.frame(Date = rep(seq.Date(from = lubridate::ymd('2019-01-01'), 
                                       to =lubridate::ymd('2021-04-01'), 
                                       by = 'days'), 3),
                   Symbol = c(rep("AMZN", 822),
                              rep("FB", 822),
                              rep("NFLX", 822)))

Combined = Dates %>%
  dplyr::left_join(Stock_info, by = c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol)%>%
  fill(adjusted, .direction = "downup")

rlang::last_error()

# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order.
newDF = Combined %>%
  filter(Symbol == "AMZN" || Symbol == "FB",
  between(Date, lubridate::ymd("2019-01-01"),lubridate::ymd("2019-07-01"))|
    between(Date, lubridate::ymd("2020-04-01"), lubridate::ymd("2020-07-01")))%>%
    dplyr::arrange(Symbol,desc(Date))

# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB.
  just_AMZN = newDF %>%
    filter(Symbol == "AMZN")
    firstAMZN = head(just_AMZN,1)
    lastAMZN = tail(just_AMZN,1)
    just_FB = newDF %>%
      filter(Symbol == "FB")
    firstFB = head(just_FB,1)
    lastFB = tail(just_FB,1)
    first_last=combine(firstAMZN, lastAMZN, firstFB, lastFB)
# 5.Select the last observation for each stock, for each month. 
    by_month = Combined %>%
    mutate(year = substr(Date, 1, 4),
           month = substr(Date, 6, 7),
           day = substr(Date, 9, 10)) %>%
      group_by(year, month) %>%
      filter(day == max(day)) %>%
      summarise(Date, Symbol, adjusted)%>%
      arrange(year, month)
    # How can I remove the year and month that were used for grouping?
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.
#####Problem 1#####

#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 
    SMA_table = Combined %>%
      group_by(Symbol)%>%
      mutate(SMA10 = SMA(adjusted,10),
             SMA26 = SMA(adjusted, 26),
             LagSMA10 = lag(SMA10),
             LagSMA26 = lag(SMA26)) %>%
      filter(!is.na(SMA26))%>%
# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
      mutate(Crossing = case_when(LagSMA10 > LagSMA26 & SMA10 < SMA26 ~ "below",
                                  LagSMA26 > LagSMA10 & SMA26 < SMA10 ~ "above",
                                  TRUE ~ "no crossing")) %>%
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.
      mutate(Signal = case_when(Crossing == "below" ~ "sell",
                                Crossing == "above" ~ "buy",
                                TRUE ~ "hold"))
    sum(SMA_table$Crossing == "below")
    sum(SMA_table$Crossing == "above")
    
#####Problem 2#####
