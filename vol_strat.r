# volatility strategy
# trade function <- tests trade and generate signals
vol_trade <- function (stock1, stock2, hr) {
  
  rolling_period = 126
  
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
  meanT    <- SMA(spreadT, n=rolling_period)
  sdT      <- runSD(spreadT, rolling_period) # this is a vector
  
  sprintf("%s and %s", stock1, stock2)
  
  #distribution of the spread
  plot1 <<- hist(spreadT, col = "blue", breaks = 100, main = sprintf("Spread Histogram (%s vs. %s)", stock1, stock2), plot=FALSE)
  #abline(v=meanT,col="red")
  
  #plotting price
  plot2 <<- plot.xts(pair_prices, screens = factor(1, 1), col=c('red', 'blue'))
  addLegend(legend.loc = "topright", col=c('red', 'blue'), pch=15)
  
  # generating signals for trades
  spreadL  <- length(spreadT)
  pricesB  <- c(rep(NA,spreadL))
  pricesS  <- c(rep(NA,spreadL))
  sp       <- as.numeric(spreadT)
  tradeQty <- 100
  totalP   <- 0
  tradeslist <- c(rep(NA,spreadL))
  
  
  # generating signals and entries
  for(i in 1:spreadL) {
    upperThr <- meanT[i] + 1 * sdT[i]
    lowerThr <- meanT[i] - 1 * sdT[i]
    if (!(is.na(upperThr) & is.na(lowerThr))){ #making sure we have upper and lower band values before generating signals
      spTemp <- sp[i]
      if(spTemp < lowerThr) { #BUY
        if(totalP == 0){ #first signal
          totalP     <- totalP + tradeQty
          tradeslist[i] <- totalP
          pricesB[i] <- spTemp
        } else if(totalP < 0){ #current position is a short (sold)
          totalP     <- totalP + 2*tradeQty #reverse short position and immediately into long
          tradeslist[i] <- totalP
          pricesB[i] <- spTemp
        }
      } else if(spTemp > upperThr) { #SELL
        if(totalP == 0){ #first signals
          totalP <- totalP - tradeQty
          tradeslist[i] <- totalP
          pricesS[i] <- spTemp
        } else if(totalP > 0){ #current position is a long (bought)
          totalP <- totalP - 2*tradeQty #reverse long position and immediately into short
          tradeslist[i] <- totalP
          pricesS[i] <- spTemp
        }
      }
    }
  }
  
  plot3 <<- plot(spreadT, main = sprintf("%s vs. %s spread at (%s)", stock1, stock2, hr), multi.panel=TRUE)
  points(xts(pricesB,index(spreadT)), col="green", cex=1.9, pch=19)
  points(xts(pricesS,index(spreadT)), col="red", cex=1.9, pch=19)
  
  # calculating returns
  returns  <- c(rep(NA,spreadL))
  cum_returns <- c(0)
  test_list <-c(rep(NA,spreadL))
  firsttrade <- TRUE
  tradeon <- FALSE
  
  for(i in 1:spreadL){
    signal <- !(is.na(tradeslist[i]))
    if (signal == TRUE & firsttrade == TRUE){ # first trade
      # start recording
      tradeprice <- sp[i]
      action <- tradeslist[i]
      firsttrade <- FALSE
      tradeon <- TRUE
    } else if (signal == TRUE & firsttrade == FALSE) {
      tradeprice <- sp[i]
      action <- tradeslist[i]
      cum_returns[1] <- ret # last entry
    }
    if(tradeon == TRUE){
      ret <- cum_returns[1] + action*((sp[i]-tradeprice)/abs(action*tradeprice))
    } else if(tradeon == FALSE){
      ret <- cum_returns[1]
    }
    # recording returns
    returns[i] <- ret
    test_list[i] <- cum_returns
  }
  
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