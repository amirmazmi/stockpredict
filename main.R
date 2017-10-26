#---------------------------------------------------------
#
# Script to analyse stock data
# 
# rm(list=ls())
#  
#
############################################################
## Initialize work environment
############################################################
getwd()

folderpath <- switch( Sys.info()[1], Windows="C:analysis script/", "/media/analysis script" )
setwd(folderpath)
getwd()

# load library
relib <- function(){
    source("./library/stock_analysis_func2.5.R")
    source("./library/MVO.R")
    return(TRUE)
}
relib()


############################################################
## Static variables
############################################################

# data start date
startDate <- today()-years(3) #-months(4)

# manual list or whole universe
manual_tick_option <- TRUE


ifiletickername <- "FILENAME HERE"
# ifile test <- "FILENAME HERE"
# ofilelastprice <- paste("FILENAME HERE",today, ".csv", sep="")
ofilerawforecast <- paste("FILENAME HERE",today()," raw forecast.csv", sep="")
ofilefforecast <- paste("FILENAME HERE",today()," final forecast.csv", sep="")



#---------------------------------------------------------
# List of tickers

### INDEXES ###
# # INDEXFTSE:FBMSCAP, INDEXFTSE:WIMAL
# idxtick <- setNames(do.call("rbind.data.frame", c(list(
#       c("KLCI ETF","KLSE:0820EA"),
#       stringsAsFactors=F))), c("companyname","ticker") )

# KLSE Main Market
tick_universe <- read.csv( ifiletickername, stringsAsFactors=F, 
                           comment.char="#")

### OR manual list tickers
tick_manual <- setNames(do.call("rbind.data.frame", c(list(
    # c(  "KLCI ETF" , "KLSE:0820EA"  ),
    c(  "MAYBANK"  , "KLSE:MAYBANK" ),
    c(  "FGV"     ,  "KLSE:FGV"      ),
    c( "PERMAJU INDUSTRIES BERHAD", "KLSE:PERMAJU"),
    # c( "PERWAJA HOLDINGS BERHAD", "skipped"),
    # c( "PESONA METRO HOLDINGS BERHAD", "KLSE:PESONA"),
    stringsAsFactors=F))), c("companyname","ticker") )



############################################################
## INITIALIZE
#   1. Get last 5 trading days average close and volume 
#      for all tickers
#   2. Apply some filtering for volatility and affordability
############################################################

# get average price and volume
tickers <- initSummary( manual_tick_option, tick_manual, tick_universe)
save(tickers, file="./tmp/tickers.RData")

# write out summary of price and volm for future reference
# write.csv( tickers[which(tickers$wkavgprice != -1),1:2],
#            "./data/ticker_names_avail.csv", quote=F, row.names=F)  


#------------------------------------------------------------
# review wkavgvolm - NEEDS FUNCTION!!!
summary(tickers$wkavgvolm)
hist(tickers$wkavgvolm, breaks=50)
for(i in seq_along(summary(tickers$wkavgvolm))){
    fwcat( names(summary(tickers$wkavgvolm))[i], " ", 12)
    cat( length(which(tickers$wkavgvolm > summary(tickers$wkavgvolm)[[i]])),
         "\n" )
    abline(v=summary(tickers$wkavgvolm)[[i]], col=i) }

#------------------------------------------------------------

# filter desired tickers based on weekly average
#   maxprice -> affordability
#   minvol -> liquidity
tickers1 <- tickFilter(tickers, maxprice=10, minvol=0.7*10^6, minprice=0.1)
save(tickers1, file="./tmp/tickers1.RData")


#combine with index
# tickers1 <- rbind( idxtick, tickers1)




############################################################
## Get OHLC data - assign to variable for easy manipulation
############################################################
tickcol <- grep("ticker", names(tickers1)) 
xtsraw <- lapply( tickers1[, tickcol], getOHLC, startDate )
save(xtsraw,file="./tmp/xtsraw.Rdata")


############################################################
## Summarize properties and apply indicators
############################################################

# set names to xts list
xtsnamed <- setNames( xtsraw, tickers1[, tickcol]) 

# properties reference table
tickprop <- xtsnamed %>% propDF( tickers1) %>%
                             missVal()

# filter low data and NAs
tickprop1 <- tickprop[ cleaner(tickprop),]
xtsclean <- xtsnamed[ cleaner(tickprop)]

# sanity check - show mismatch if any
which( (names(xtsclean) == tickprop1$ticker)== FALSE)


# setup data - add indicators and convert to data frame 
tic <- now()
dfcomp <- xtsclean %>% lapply(allTA)    %>%
                        lapply( convDF) %>%
                        setNames( names(xtsclean))  %>%
                        lapply( normprice)  %>%
                        lapply( retDaily)   %>%
                        lapply( cumRet)     # %>%     
                      ## exclude timekit for now since too much bias
                      #    lapply(tk_augment_timeseries_signature)
cat("\n[+] Indicators took", round(difftime( now(), tic, units="secs"),4), "seconds\n\n")



############################################################
## Run predictions
############################################################

# 1. alpha-beta calc
# 2. predict for the target ranges
tickprop2 <- tickprop1 %>% # alphabeta(dfcomp) %>%
                          ypredict( dfcomp,c("close"), 0.7)
names(tickprop2)
dim(tickprop2)                           
                         

############################################################
## Results
############################################################

# write out raw predictions
cond_NA <- which( is.finite(tickprop2$ypredict1day))
write.csv( tickprop2[cond_NA,], ofilerawforecast, quote=F, row.names=F)  


#-----------------------------------------------------------
# Filter forecasts

# prediction horizon
day1 <- c(2,4,10:20)

# filter final predictions - STILL TESTING
cond_RMSE <- tickprop2$RMSE1day <= 0.5 #for accuracy <0.007
cond_Rsq <- tickprop2$Rsquared1day >= 0.7
# cond_error <- tickprop2$ErrorCount1day <= 500
cond_percgain <- tickprop2$percgain1day >= 0.00
cond_lowgain <- tickprop2$lowpercgain1day >= -0.002
cond_NA <- is.finite(tickprop2$ypredict1day)
cond <- cond_percgain & cond_lowgain & cond_RMSE & cond_Rsq & cond_NA  #& cond_error 
names(tickprop2)

# final forecasts
dfforecast <- tickprop2[ which(cond), day1]
dim(dfforecast)
names(dfforecast)

# write out final predictions
write.csv( dfforecast, ofilefforecast, quote=F, row.names=F)  


############################################################
## Optimal Portfolio
############################################################

# get returns data
dfreturns <- getRetDaily( dfforecast$ticker, dfcomp2, "dailyret")
dfexpected <- dfforecast[,"percgain1day"]

# calc & plot eff frontier - historical 
eff <- eff.frontier( dfreturns)

# calc & plot eff frontier - expected
eff2 <- eff.frontier(dfreturns, expected.return=dfexpected )

# Final output 
MVO <- setNames( cbind.data.frame(eff$final, eff2$final), c("historical", "tomorrow") )
MVO

ofileMVO <- paste("FILEPATH HERE", today(), " optimal allocation.csv", sep="")
write.csv( MVO, ofileMVO, quote=F, row.names=T)  


