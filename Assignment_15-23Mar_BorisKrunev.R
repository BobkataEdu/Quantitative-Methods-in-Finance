library(dplyr)
#Problem 1
Balance <- 100
counter <- 0
Bet <- 1
counter <- 0
while(Balance > 0 & counter <= 1000){
  outcome <- sample(c("win", "loss"), 1, prob = c(0.486, 0.514))
  if (outcome == "win"){
    Balance <- Balance + Bet
    Bet <- 1
  }
  
  else{
    if(Bet > Balance){
      Balance <- Bet
    }
    Balance <- Balance - Bet
    Bet <- Bet * 2
  }
  counter <- counter + 1
  print(Balance)
}
#Problem 2
#5.2.4
library(nycflights13)
library(tidyverse)
#1 Had an arrival delay of two or more hours
filter(flights, dep_delay >= 120)
#2 Flew to Houston (IAH or HOU)
destQ2 <- filter(flights, dest %in% c('IAH', 'HOU'))
#3 Were operated by United, American, or Delta
filter(flights, carrier %in% c('UA', 'DL'))
#4  Departed in summer (July, August, and September)
filter (flights, month %in% c(7,8,9))
#5 Arrived more than two hours late, but didn't leave late
filter(flights, arr_delay > 120 & dep_delay <= 0)
#6 Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60 & dep_delay - arr_delay >= 30)
#7 Departed between midnight and 6am (inclusive)
filter(flights, dep_time <= 600)
# Q2 Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?
# It can be used to select limits to variables, we can use it to simplify #4
filter(flights, between(month, 7, 9))
# Q3 How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
view(filter(flights, is.na(dep_time)))
#All rows related to the liftoff of the filight are NA. This means that the flight was cancelled
# Q4 Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)
# Whatever value NA was before being put to the power of 0, it is transformed to 1, as every number, except 0, to the 0th is 1
#NA | TRUE equals true, because if NA is true, TRUE | TRUE = TRUE and if it is FALSE, FALSE | TRUE = TRUE
# FALSE & NA equals FALSE because FALSE & TRUE = FALSE and FALSE & FALSE = FALSE as well
#5.3.1 Exercises
#1 How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(flights, desc(is.na(dep_time)))
#2 Sort flights to find the most delayed flights. Find the flights that left earliest.
arrange(flights, desc(dep_delay), dep_time)
#3 Sort flights to find the fastest (highest speed) flights.
view(arrange(flights, desc(distance / air_time)))
#4 Which flights travelled the farthest? Which travelled the shortest?
view(arrange(flights,  desc(distance))) #flight 51 with distance 4983 km
view(arrange(flights, distance))# flight 	1632 with distance 17km
#5.4.1 Exercises
#1 Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.
#that would be 4! different ways, according to their arrangement 
#2 What happens if you include the name of a variable multiple times in a select() call?
#The call would ignore the duplicates
select(flights, dep_delay, dep_delay)#Would print dep_delay once
#3 What does the any_of() function do? Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))
#it would show all values, regardless if they are missing or not
#4 Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?
select(flights, contains("TIME"))
#Selects all columns where the string 'time' is present. By default it is not case sensitive. To change it into case sensitive, we should use ignore.case = FALSE 
select(flights, contains("TIME", ignore.case = FALSE)) #prints nothing
select(flights, contains("time", ignore.case = FALSE)) #works as intended
#5.5.2 Exercises
#1 Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they're not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.
transmute(flights,
          totalMinutesDep_Time = ((dep_time %/% 100) * 60) + dep_time %% 100,
          totalMinutesSched_Dep_Time = ((sched_dep_time %/% 100) * 60) + sched_dep_time %% 100
)
#2 Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?
view(flight_air_time <-mutate(
  flights,
  dep_time = dep_time %/% 100 * 60 + dep_time %% 100,
  arr_time = arr_time %/% 100 * 60 + arr_time %% 100,
  air_time_diff = air_time - arr_time + dep_time 
)) #Most if not all have arr_time_diff != 0. Probably because flights pass through different timezones.
#3 Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?
testTransmute = transmute(
  flights,
  dep_time = dep_time %/% 100 * 60 + dep_time %% 100,
  sched_dep_time = sched_dep_time %/% 100 * 60 + sched_dep_time %% 100,
  check = dep_delay - dep_time + sched_dep_time
)
nrow(filter(testTransmute, check == 0)) # ~ 9000 entries show that there is not a full corelation between the three variables
#4 Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().
topDelays = top_n(flights, 10, dep_delay)
arrange(topDelays, desc(dep_delay))
#5What does 1:3 + 1:10 return? Why?
vect1 = (1:3)
vect2 = (1:10)
sum = vect1 + vect2
#it adds 1 to every third element starting with 1, 2 to every third element starting with 2, and 3 to every third element strating with 3
# that is why it ends with 10 12 11 (10 = 8+2; 12 = 9 + 3 and 11 = 10 + 1)
#6What trigonometric functions does R provide?
# sin(), cos(), tan() and their inverses