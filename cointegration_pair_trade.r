library("tseries")
library(TTR)
library(egcm)
library(quantmod)
library(stringr)
attach(mtcars)
library(ggplot2)
library(grid)
library(readxl)
library(xlsx)
library(PerformanceAnalytics)

TSX_tickers <- read_excel("//bloomberg-pc/Users/BBerg/Desktop/g3-2/COOP DOCS/Jeffrey/pair_trading/TSX_tickers.xlsx", 
                          sheet = "Sheet3")
SPX_tickers <- read_excel("//bloomberg-pc/Users/BBerg/Desktop/g3-2/COOP DOCS/Jeffrey/pair_trading/US_Tickers.xlsx", 
                          sheet = "SPX")
DOW_tickers <- read_excel("//bloomberg-pc/Users/BBerg/Desktop/g3-2/COOP DOCS/Jeffrey/pair_trading/US_Tickers.xlsx", 
                          sheet = "DOW")
exchange_name <- "DOW"
stocktickers <- DOW_tickers #pick depending on which exchange

#setup
stockData <- new.env() #Make a new environment for quantmod to store data in
startDate <- as.Date("2007-09-11") #Specify period of time we are interested in
#endDate <- as.Date("2017-09-11")
filename <- paste("Cointegrated_pairs_", exchange_name, "-", Sys.Date(), ".xlsx", sep="")
filedir <- paste("//bloomberg-pc/Users/BBerg/Desktop/g3-2/COOP DOCS/Jeffrey/pair_trading/", filename, sep="")
if (file.exists(filedir)){
  file.remove(filedir)
} 

create_pair_data <- function(x, y, tickers){
  x_index <- which(tickers == x)
  y_index <- which(tickers == y)
  x_quote <- prices[[x_index]]  
  y_quote <- prices[[y_index]]
  if("try-error" %in% class(x_quote) || "try-error" %in% class(y_quote)){
    x_data <- NA
    y_data <- NA
  }else {
    x_data <- Cl(x_quote) #closing prices is retrieved from Cl() function
    y_data <- Cl(y_quote)
  }
  ts <- cbind(x_data, y_data)
  ts <- na.omit(ts)
  return (ts)
}

classlist <- function(x) {
  sapply(x, class)
}

prices <<- NA

sectors <- unique(stocktickers$Sector)
print(sectors)

for (industry in sectors){
  print(paste(industry, Sys.time()))
  subsecs <- subset(stocktickers,
                    stocktickers$Sector==industry, 
                    sort.by=c("MarketCap"))
  
  what_metrics <- yahooQF(c("Previous Close",
                            "Symbol",
                            "Market Capitalization"))
  
  metrics <- getQuote(paste(subsecs$Ticker, sep="", collapse=";"), what=what_metrics)
  #metrics <- metrics[metrics$`Market Capitalization` > 500000000,] # only care about 500M above
  metrics <- na.omit(metrics)
  relevant_tickers = metrics$Symbol
  
  #fetching data from yahoo
  prices <<- lapply(relevant_tickers, 
                   function(x) try(getSymbols(x, env = NULL, src = "yahoo", 
                                              from = startDate, warnings=FALSE)))
  # creating pairs of securities
  
  if (length(relevant_tickers) < 2){
    print("skipped")
    next
  }
  pairs_list <- t(combn(relevant_tickers,2))
  pairs_count <- dim(pairs_list)[1]
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # correlation testing
  r2_list <- rep(0, pairs_count)
  for (i in 1:pairs_count){
    name1 <- pairs_list[i,1]
    name2 <- pairs_list[i,2]
    pair_data <- create_pair_data(name1, name2, relevant_tickers)
    pair_R2 <- cor(pair_data[, 1], pair_data[, 2])
    r2_list[i] <- pair_R2
  }
  
  pairs_df <- data.frame(pairs_list, r2_list)
  filteredpairs1 <- pairs_df[pairs_df$r2_list >= 0.8,] #filter for correlation >= 0.8
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #conintegration tests
  
  coit_list <- rep(NA, pairs_count) #boolean whether pair is cointegrated list
  station_list <- rep(NA, pairs_count) #boolean whether pair is stationary list
  pval_list <- rep(NA, pairs_count)
  alpha_list <- rep(0, pairs_count)
  beta_list <- rep(0, pairs_count)
  
  for (i in 1:pairs_count){
    name1 <- pairs_list[i,1]
    name2 <- pairs_list[i,2]
    t1 <- try(pair_data <- create_pair_data(name1, name2, relevant_tickers)) #t1 just named arbitrarily 
    if("try-error" %in% class(t1)){
      print(paste("Error in:", industry, name1, name2))
      print(t1)
    } else { # no error t1
      price1 <- pair_data[, 1]
      price2 <- pair_data[, 2]
      t2 <- try(E <- egcm(price1, price2))
      if("try-error" %in% class(t2)){
        print(paste("Error in:", industry, name1, name2))
        coit_list[i] <- FALSE
        alpha_list[i] <- NA
        beta_list[i] <- NA
      }else { # no error in t2
        status <- is.cointegrated(E)
        alpha <- E$alpha
        beta <- E$beta
        coit_list[i] <- status
        alpha_list[i] <- alpha
        beta_list[i] <- beta
        if (status == TRUE){ # pairs are conintegrated
          #checking for stationary with adf test
          spread <- price1 - beta * price2
          adf_result <- adf.test(spread, alternative = "stationary")
          if (adf_result$p.value < 0.05){ #0.25 tolerance
            station_list[i] = TRUE
            pval_list[i] = adf_result$p.value
          } else {
            station_list[i] = FALSE
          }
        }else {
          station_list[i] = FALSE
        }
      } 
    }
  }

  pairs2_df <- cbind(pairs_list, coit_list, alpha_list, beta_list, station_list, pval_list)
  pairs2_df <- data.frame(na.omit(pairs2_df))
  filteredpairs2 <- pairs2_df[pairs2_df$coit_list == TRUE & 
                                pairs2_df$station_list == TRUE &
                                pairs2_df$alpha_list != 0 & 
                                pairs2_df$beta_list != 0,] #filter for cointegrated and stationary
  
  
  
  filteredpairs2$alpha_list <- as.numeric(as.character(filteredpairs2$alpha_list)) #converting factors to numeric
  filteredpairs2$beta_list <- as.numeric(as.character(filteredpairs2$beta_list)) #converting factors to numeric
  
  filteredpairs2 <- filteredpairs2[filteredpairs2$beta_list >= 1,] #eliminating betas below 1
  
  
  # merging the correlation and cointegration dataframes
  
  finalpairs <- merge(filteredpairs1, filteredpairs2, by.x=c("X1","X2"), by.y=c("V1", "V2"))
  if (is.data.frame(finalpairs) && nrow(finalpairs)!=0){ # if dataframe is not empty create pair
    finalpairs$pair_string <- paste("vol_trade('",finalpairs$X1,"','",finalpairs$X2,"',",finalpairs$beta_list,")", sep="")
    finalpairs$pair_string2 <- paste("mean_trade('",finalpairs$X1,"','",finalpairs$X2,"',",finalpairs$beta_list,")", sep="")
    finalpairs$pair_string3 <- paste("channel_trade('",finalpairs$X1,"','",finalpairs$X2,"',",finalpairs$beta_list,")", sep="")
  }
  View(finalpairs)

  write.xlsx2(finalpairs, filedir, sheetName = industry,
              col.names = TRUE, append = TRUE)
}