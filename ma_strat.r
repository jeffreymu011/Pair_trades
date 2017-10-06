long <- function (i, spTemp){
  totalP <<- totalP + tradeQty
  tradeslist[i] <<- totalP
  action <<- totalP
  pricesB[i] <<- spTemp
  position <<- "LONG"
  tradeon <<- TRUE
  tradeprice <<- spTemp
}

short <- function(i, spTemp){
  totalP <<- totalP - tradeQty
  tradeslist[i] <<- totalP
  action <<- totalP
  pricesS[i] <<- spTemp
  position <<- "SHORT"
  tradeon <<- TRUE
  tradeprice <<- spTemp
}

close <- function(i, spTemp){
  if (position == "LONG"){
    totalP <<- totalP - tradeQty
    tradeslist[i] <<- totalP
    pricesS[i] <<- spTemp
  }else if (position == "SHORT"){
    totalP <<- totalP + tradeQty
    tradeslist[i] <<- totalP
    pricesB[i] <<- spTemp
  }
  action <<- totalP
  position <<- "NONE"
  tradeon <<- FALSE
  tradeprice <<- 0
  cum_return <<- cum_return + current_return
  current_return <<- 0 
}

crossover <- function(short1, short2, long1, long2){
  short1 <- as.numeric(short1)
  short2 <- as.numeric(short2)
  long1 <- as.numeric(long1)
  long2 <- as.numeric(long2)
  if (short1 >= long1 & short2 < long2){
    return ("SHORT")
  } else if (short1 <= long1 & short2 > long2){
    return ("LONG")
  } else {
    return ("NONE")
  }
}

# mean reversion strategy (moving average)
mean_trade <- function (stock1, stock2, hr) {
  
  short_rolling_period <- 30
  long_rolling_period <- 90
  trend_period <- 3
  take_profit <- 0.1 # take profit at % 
  stop_loss <- -0.1 
  
  tickers <- c(stock1, stock2)
  prices <<- lapply(tickers, 
                    function(x) try(getSymbols(x, env = NULL, src = "yahoo", 
                                               from = startDate, warnings=FALSE)))
  pair_prices <- create_pair_data(stock1, stock2, tickers)
  price1 <- pair_prices[,1]
  price2 <- pair_prices[,2]
  
  #spread price (in-sample)
  spreadT <- price1/(hr * price2)
  
  #compute statistics of the spread
  ma_short <- SMA(spreadT, n=short_rolling_period)
  ma_long <- SMA(spreadT, n=long_rolling_period)
  
  # generating signals for trades
  spreadL  <- length(spreadT)
  sp       <- as.numeric(spreadT)
  pricesB  <<- c(rep(NA,spreadL))
  pricesS  <<- c(rep(NA,spreadL))
  tradeon <<- FALSE
  position <<- "NONE"
  tradeQty <<- 100
  totalP   <<- 0
  tradeprice <<- 0 
  tradeslist <<- c(rep(NA,spreadL))
  current_return <<- 0
  cum_return <<- 0 
  returns  <- c(rep(NA,spreadL))
  test_list <- c(rep(NA,spreadL))
  
  # generating signals by iterating through spread series
  for(i in 1:spreadL) {
    if (i > long_rolling_period){ #starting at first instance of long MA
      spTemp <- sp[i]
      short_trend_up <-  all(diff(sp[(i-3):i]) >= 0) # check if short term trend is increasing
      #long_trend_up <- all(diff(sp[c(5,10,15)]) >= 0)  #check if long term trend is increasing 
      signal <- crossover(ma_short[i-1], ma_short[i], ma_long[i-1], ma_long[i])
      
      if (current_return >= take_profit){#take profit
        close(i, spTemp)
      } else if (current_return <= stop_loss){#stop loss
        close(i, spTemp)
      } else { # determining entries
        if(signal == "LONG" & short_trend_up & position != "LONG") { #long
          if (tradeon == FALSE){ #enter position
            long(i, spTemp)
          }else if (tradeon == TRUE){
            close(i, spTemp)
          }
        } else if(signal == "SHORT" & !short_trend_up & position != "SHORT") { #short
          if (tradeon == FALSE){
            short(i, spTemp)
          }else if (tradeon == TRUE){
            close(i, spTemp)
          }
        }
      }
      
      if(tradeon == TRUE){
        current_return <<- action*((spTemp - tradeprice)/abs(action*tradeprice))
        ret <- cum_return + current_return
      } else if(tradeon == FALSE){
        ret <- cum_return
      }
      # recording returns
      returns[i] <- ret
      test_list[i] <- current_return
    }
  }
  
  plot3 <<- plot(cbind(spreadT, ma_short, ma_long), type="l",
                 col=c("black", "green", "red"),
                 main = sprintf("%s vs. %s spread at (%s)", stock1, stock2, hr), lwd  = 1)
  points(xts(pricesB,index(spreadT)), col="green", cex=1.9, pch=19)
  points(xts(pricesS,index(spreadT)), col="red", cex=1.9, pch=19)
  
  # appending returns to time series object
  spreadT$returns <- returns
  plot4 <<- plot(spreadT$returns, multi.panel=TRUE)
  
  draw_trade()
}

draw_trade <- function(){
  #setting up frame
  par(mfrow = c(2,1))
  plot3
}

