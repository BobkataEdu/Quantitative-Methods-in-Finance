#####Problem 1#####
# Write the following functions:
# 1.1. A function which replicates the SMA function. It should calculate the SMA
# for a vector to the left:
# 1.2. A function, which calculates the correlation coefficient between two vectors.
# It should replicate the cor function from the base package.
#####Problem 1#####
sample_vector = sample(100:300, 100)
SMA = c()
for (i in sample_vector){
  CumSum = mean(i:(i+9))
  SMA = c(SMA, CumSum)
  print(SMA(i:(i+9)))
}
#####Problem 2#####
# Find all prime numbers less than 100, using for/while loops.
numbersTo100 = c(1:100)
for(i in numbersTo100){
  divisor = 2
  isPrime = TRUE
  while(divisor < i && isPrime == TRUE){
    if (i%%divisor == 0){
      isPrime = FALSE
    }
    divisor = divisor + 1
  }
  if(isPrime == TRUE){
    print(i)
  }
}
#####Problem 2#####

#####Problem 3#####
# Read the wikipedia article and investopedia article on MACD:
# https://en.wikipedia.org/wiki/MACD
# https://www.investopedia.com/terms/m/macd.asp

# Download data for a stock of your choice and do the following:
Google_Stock = tq_get("GOOG",
       from ="2019-01-01",
       to = "2021-01-01")%>%
  dplyr::select(date, symbol, adjusted)
Dates = data.frame(Date = rep(seq.Date(from = lubridate::ymd('2019-01-01'), 
                                       to =lubridate::ymd('2021-01-01'), 
                                       by = 'days')),
                   Symbol = c(rep("GOOG", 732)))
Combined = Dates %>%
  dplyr::left_join(Google_Stock, by = c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol)%>%
  fill(adjusted, .direction = "downup")
# 1.Calculate the 26-period EMA(use the EMA function from tidyquant)
# 2.Calculate the 12-period EMA.
# 3.Calculate the MACD line(12-period EMA minus 26-period EMA)
# 4.Calculate the signal line - this is the 9-period EMA of the MACD.
With_EMA = Combined %>%
  mutate(EMA26 = EMA(adjusted, n = 26),
         EMA12 = EMA(adjusted, n = 12),
         MACD = EMA12 - EMA26,
         Signal = EMA(MACD, 9)) %>%
  filter(!is.na(Signal))

# 5.Calculate the buy/sell signals. This means create a new column which tell
# us if we should buy or sell. When the MACD line crosses the signal line
# from above(MACD is above signal then MACD is below signal) this is a sell signal. 
# If it crosses from below (MACD is below signal then MACD is above signal) this is a buy signal.
final_table = With_EMA %>%
  mutate(buy_sell = case_when(lag(MACD) > lag(Signal) & MACD < Signal ~ "above",
                           lag(MACD) < lag(Signal) & MACD > Signal ~ "below",
                           TRUE ~ "no crossing"),
         buy_sell_signal = case_when(buy_sell== "above" ~ "sell",
                            buy_sell == "below" ~ "buy",
                            TRUE ~ "hold"))
# 6. Simulate how the strategy preforms and compare it to a benchmark strategy
# of just buying and holding the stock.
# In order to do this start with a portfolio of 100$ invested in the stock on the first day
# and see how it performs. Example:
# I start with 100$ and a stock which costs 100$ at the beginning of my time period.
# I get a buy signal when the stock price is 90. I buy the stock.
# I get a sell signal to sell the stock when the price is 110. I sell it and 
# and don't get any more signals.I end up with 100 * 110 / 90 = 122.22 
# The benchmark portfolio is I buy the stock at 100 at the beginning and at
# the end of the period the stock price is 120. I end up with 120.
# 122.22 > 120. so the MACD strategy was beating the market.
startingmoney = 100
MarketVsMACD = final_table %>%
  mutate(hold_ratio = adjusted/lag(adjusted),
         strategy_ratio = case_when(buy_sell == "above" ~ 1,
                                    buy_sell == "below" ~ hold_ratio,
                                    TRUE ~ lag(1)))%>%
  filter(!is.na(hold_ratio))

hold_result = tail(cumprod(MarketVsMACD$hold_ratio)*100, 1) # this observes from 02-04-2019, instead of 01-01-2019
strategy_result = tail(cumprod(MarketVsMACD$strategy_ratio)*100, 1)
ifelse(strategy_result > hold_result, print("The strategy beat the market"), print("The strategy did not beat the market"))
#####Problem 3#####
