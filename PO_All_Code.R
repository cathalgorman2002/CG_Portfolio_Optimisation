library(fPortfolio)
library(timeSeries)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)

# Set locations to where you want the csv files containing weights to go to
in_path <- "C:\\Users\\catha\\Documents\\3rd Year\\Investment_and_Trading\\PO_Run\\"
out_path <- "C:\\Users\\catha\\Documents\\3rd Year\\Investment_and_Trading\\PO_Run\\Output\\"

# 0: Create Functions -----------------------------------

fn_create_time_series <- function(tickers, return_rate="daily", start_date="2019-01-01"){
  
  # Calculate returns: Daily
  portfolioPrices <- NULL
  for(Ticker in tickers){
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols.yahoo(Ticker, from=start_date, auto.assign=FALSE)[,4])
  }
  
  #Delete all dates with no prices
  portfolioPrices <- portfolioPrices[apply(portfolioPrices, 1, function(x) all(!is.na(x))),]
  #Rename columns
  colnames(portfolioPrices) <- tickers
  
  #Calculate Returns: Daily ROC
  portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
  portfolioReturns <- as.timeSeries(portfolioReturns)
  
  if(return_rate=="Monthly"){
    #Calculate Monthly or weekly returns
    Stock_Data <- tickers %>% lapply(function(x) getSymbols.yahoo(x, from="2016-01-01", auto.assign = FALSE)[,4]) %>%
      lapply(function(x) monthlyReturn(x))
    
    portfolioReturns <- do.call(merge, Stock_Data)
  }
  
  #Keep only the dates that have closing prices for all tickers
  portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
  colnames(portfolioReturns) <- tickers
  portfolioReturns <- as.timeSeries(portfolioReturns)
  
  return(portfolioReturns)
  
}

fn_eff_frontier <- function(portfolioReturns, frontier_plot_type=c(1,2,3,4)){
  # Calculate the efficient frontier
  portfolioReturns <- as.timeSeries(portfolioReturns)
  effFrontier <- portfolioFrontier(portfolioReturns, constraints = "LongOnly") # Longonly => No short investments
  
  #Plot Frontier
  #Options:
  #1: Plot efficient Frontier
  #2: Plot Minimum Variance Portfolio
  #3: PLot Tangency Portfolio
  #4: Plot Risk Returns of Each Asset
  #5: Plot Equal Weights Portfolio
  #6: Plot Two Asset Frontiers (Long)
  #7: Plot Monte Carlo Portfolios
  #8: Plot Sharpe Ratio
  eff_frontier_plot <- plot(effFrontier, frontier_plot_type)
  
  frontierWeights <- getWeights(effFrontier)
  colnames(frontierWeights) <- colnames(portfolioReturns)
  risk_return <- frontierPoints(effFrontier)
  
  
  eff_frontier_list <- list(effFrontier, frontierWeights, risk_return, eff_frontier_plot)
  names(eff_frontier_list) <- c("effFrontier", "frontierWeights", "risk_return", "eff_frontier_plot")
  
  return(eff_frontier_list)
}

fn_min_var_port <- function(portfolioReturns){
  mvp <- minvariancePortfolio(portfolioReturns, spec = portfolioSpec(), constraints = "LongOnly")
  mvpWeights <- getWeights(mvp)
  
  #Extract value at risk
  mvp_cov_risk <- covRisk(portfolioReturns, mvpWeights)
  mvp_var_risk <- varRisk(portfolioReturns, mvpWeights, alpha = 0.05)
  mvp_cvar_risk <- cvarRisk(portfolioReturns, mvpWeights, alpha = 0.05) #cvar = conditional value at risk
  
  #ggplot MVP weights
  df1 <- data.frame(mvpWeights)
  assets <- colnames(portfolioReturns)
  min_var_weights_plot <- ggplot(data=df1, aes(x=assets, y=mvpWeights, fill=assets)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black") +
    geom_text(aes(label = sprintf("%.02f %%", mvpWeights*100)),
              position=position_dodge(width = 0.9), vjust=-0.25, check_overlap = T) +
    ggtitle("Minimum Variance Portfolio Optimal Weights") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Assets", y="Weight (%)")
  
  #create list
  mvp_list <- list(mvp, mvpWeights, mvp_cov_risk, mvp_var_risk, mvp_cvar_risk, min_var_weights_plot)
  names(mvp_list) <- c("mvp", "mvpWeights", "mvp_cov_risk", "mvp_var_risk", "mvp_cvar_risk", "min_var_weights_plot")
  
  return(mvp_list)
}

fn_tangency_port <- function(portfolioReturns){
  tangencyPort <- tangencyPortfolio(portfolioReturns, spec = portfolioSpec(), constraints = "LongOnly")
  tangency_weights <- getWeights(tangencyPort)
  
  #Extract value at risk
  tangency_cov_risk <- covRisk(portfolioReturns, tangency_weights)
  tangency_var_risk <- varRisk(portfolioReturns, tangency_weights, alpha = 0.05)
  tangency_cvar_risk <- cvarRisk(portfolioReturns, tangency_weights, alpha = 0.05) #cvar = conditional value at risk
  
  #ggplot MVP weights
  df1 <- data.frame(tangency_weights)
  assets <- colnames(portfolioReturns)
  tangency_weights_plot <- ggplot(data=df1, aes(x=assets, y=tangency_weights, fill=assets)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black") +
    geom_text(aes(label = sprintf("%.02f %%", tangency_weights*100)),
              position=position_dodge(width = 0.9), vjust=-0.25, check_overlap = T) +
    ggtitle("Tangency Portfolio Optimal Weights") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="Assets", y="Weight (%)")
  
  #create list
  tangency_list <- list(tangencyPort, tangency_weights, tangency_cov_risk, 
                        tangency_var_risk, tangency_cvar_risk, tangency_weights_plot)
  names(tangency_list) <- c("tangencyPort", "tangency_weights", "tangency_cov_risk", 
                            "tangency_var_risk", "tangency_cvar_risk", "tangency_weights_plot")
  
  return(tangency_list)
}

# 1: Set parameters ---------------------------------

tickers <- c("TSLA", "CHPT", "ALLG", "VWS.CO", "FSLR", "SPWR", "ED", "TTE", "RES", "ENR", "CPFE3.SA", "ELET6.SA")
long_tickers <- c("TSLA", "ALLG", "ED", "TTE", "ENR.DE", "CPFE3.SA", "ELET6.SA", "ORSTED.CO", "RWE.DE")
short_tickers <- c("TSLA", "TTE", "CPFE3.SA", "ELET6.SA") 

# 2: Run Functions ---------------------------------

df_long <- fn_create_time_series(long_tickers, start_date = "2022-10-01", return_rate = "Daily")
eff_frontier_long <- fn_eff_frontier(df_long, frontier_plot_type = c(1,2,3,4,8))
min_var_port_long <- fn_min_var_port(df_long)
# tangency_port_long <- fn_tangency_port(df_long)

df_short <- fn_create_time_series(short_tickers, start_date = "2023-05-01", return_rate = "Daily")
eff_frontier_short <- fn_eff_frontier(df_short)
# min_var_port_short <- fn_min_var_port(df_short)
tangency_port_short <- fn_tangency_port(df_short)

# 3: Output Resulting Weights in csv format (Optional) -------------------------

# write.csv(tangency_port_short$tangency_weights, paste0(out_path, "short_weights.csv"))
# write.csv(min_var_port_long$mvpWeights, paste0(out_path, "long_weights.csv"))
