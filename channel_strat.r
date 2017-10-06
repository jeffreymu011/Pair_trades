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

trigger <- function(price, channel_max, channel_min){
  tolerance <- 0.02
  if (price < channel_max*(1+tolerance) & price > channel_max*(1-tolerance)){
    return ("SHORT")
  } else if (price < channel_min*(1+tolerance) & price > channel_min*(1-tolerance)){
    return ("LONG")
  } else {
    return ("NONE")
  }
}


#static channel strategy
channel_trade <- function (stock1, stock2, hr) {
  
  channel_period <- 255 # 1 year
  take_profit <- 10 # take profit at % 
  stop_loss <- -5 
  
  tickers <- c(stock1, stock2)
  prices <<- lapply(tickers, 
                    function(x) try(getSymbols(x, env = NULL, src = "yahoo", 
                                               from = startDate, warnings=FALSE)))
  pair_prices <- create_pair_data(stock1, stock2, tickers)
  price1 <- pair_prices[,1]
  price2 <- pair_prices[,2]
  1
  #spread price (in-sample)
  spreadT <- price1/(hr * price2)
  
  #compute statistics of the spread
  roll_max <- runMax(spreadT, channel_period)
  roll_min <- runMin(spreadT, channel_period)

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
  cum_return <<- 100 #100 % 
  returns  <- c(rep(NA,spreadL))
  test_list <- c(rep(NA,spreadL))
  
  # generating signals by iterating through spread series
  for(i in 1:spreadL) {
    if (i > channel_period){ #starting at first instance of long MA
      spTemp <- sp[i]
      
      #long_trend_up <- all(diff(sp[c(5,10,15)]) >= 0)  #check if long term trend is increasing 
      signal <- trigger(spTemp, roll_max[i], roll_min[i])
      
      if (current_return >= take_profit){#take profit
        close(i, spTemp)
      } else if (current_return <= stop_loss){#stop loss
        close(i, spTemp)
      } else { # determining entries
        if(signal == "LONG" & position != "LONG") { #long
          if (tradeon == FALSE){ #enter position
            long(i, spTemp)
          }else if (tradeon == TRUE){
            close(i, spTemp)
          }
        } else if(signal == "SHORT" & position != "SHORT") { #short
          if (tradeon == FALSE){
            short(i, spTemp)
          }else if (tradeon == TRUE){
            close(i, spTemp)
          }
        }
      }
      
      if(tradeon == TRUE){
        current_return <<- 100*(action*((spTemp - tradeprice)/abs(action*tradeprice)))
        ret <- cum_return + current_return
      } else if(tradeon == FALSE){
        ret <- cum_return
      }
      # recording returns
      returns[i] <- ret
      test_list[i] <- current_return
    }
  }
  
  plot3 <<- plot(cbind(spreadT, roll_max, roll_min), type="l",
                 col=c("black", "red", "red"),
                 main = sprintf("%s vs. %s spread at (%s)", stock1, stock2, hr), lwd  = 1)
  points(xts(pricesB,index(spreadT)), col="green", cex=1.9, pch=19)
  points(xts(pricesS,index(spreadT)), col="red", cex=1.9, pch=19)
  
  # appending returns to time series object
  spreadT$returns <- returns
  plot4 <<- plot(spreadT$returns)
  
  # calculating drawdowns
  drawdowns <- c(rep(NA,spreadL))
  peak <- 100
  current_drawdown <- 0
  
  for(i in 1:spreadL) {
    rTemp <- returns[i]
    if (!is.na(returns[i])) {
      if (rTemp > peak){
        peak <- rTemp
      }
    }
    current_drawdown <- (rTemp - peak)/peak
    drawdowns[i] <- current_drawdown

  }
  
  spreadT$drawdowns <- drawdowns
  plot5 <<- plot(spreadT$drawdowns)
  
  draw_trade()
}

draw_trade <- function(){
  #setting up frame
  par(mfrow = c(3,1))
  print(plot3)
  print(plot4)
  print(plot5)
}

