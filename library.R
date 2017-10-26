#---------------------------------------------------------
#
# Functions to analyse stock data
# 
# rm(list=ls())
# 
#    to do:
#           1. include open high low
#           2. stochastic oscillator
#
############################################################
##  Load libraries
############################################################
library(pacman)
p_load(quantmod)
p_load(ggplot2)
p_load(dplyr)
p_load(RcppRoll)
p_load(gridExtra)
p_load(cowplot)
p_load(scales)
p_load(plotly)
p_load(lubridate)
p_load(caret)
# p_load(timekit) - trend is biased

############################################################
## Define functions
############################################################

# date today
today <- date(now())

# print formatting
fwcat <- function(msg1, sep=".", totlen){
    lenmsg1 <- nchar(msg1)
    if(totlen <= lenmsg1){
        cat(" [+] print format error")
    }
    
    if(totlen > lenmsg1){
        seplen <- totlen - lenmsg1
        sepmsg <- paste0(rep(sep, seplen), collapse="")
        cat( paste0(msg1, sepmsg, collapse=""))
    }
    else{
        cat("[+] Uncaught exception: print formatting")
    }
}


# get prices
options("getSymbols.warning4.0"=FALSE)
getOHLC <- function( ticker, startDate, endDate=today, column=c(1:5), 
                     verbose=F, src="google" ){
    li <- as.data.frame( matrix(data=NA,1,5))
    fwcat(ticker, ".", 15)
    
    if( ticker == "skipped"){
        li[1,]<-rep( "skipped",5)
        cat( " - skipped    [+]\n")
        return( li[,column])
    }else{
        li <- tryCatch({
            getSymbols( ticker, src=src, verbose=verbose, auto.assign=F,
                        from=startDate, to=endDate, warnings=F)
        }, error=function(e) {
            # print(c("Error:",e$message))
            return( as.data.frame( matrix( data=rep("error",5),1,5), stringsAsFactors=F) )
        })
        
        # mean 1.2sec for sleep
        Sys.sleep( sample( round(rnorm(100, 1.1,0.14),2), 1))
        cat( "Done" )
        
        # error message for missing data
        if( colSums(is.na(li))[4] > 0 & colSums(is.na(li))[5] > 0 ){
            cat("  [-] NA ->")      
            cat( " Close:",colSums(is.na(li))[4], " Volm:", colSums(is.na(li))[5] )
            li <- as.data.frame( matrix( data=rep("error",5),1,5), stringsAsFactors=F)
        }
        cat("\n") 
        return (li[,column])
    }}

# get week summary of close price and volume
initSummary <- function(manual, dfticker1, dfticker2, tickcol=2){
    tic <- now()
    # select which list to use
    if(manual){ 
        dfticker <- dfticker1
    }else{
        dfticker <- dfticker2
    }
    
    # helper function to get weekly data and summarize
    initOHLC <- function( ticker){
        initlist <- data.frame(getOHLC(ticker, today-7, column=c(4:5) ))
        if(initlist[1,2] %in% c("error","skipped",NA) ){
            # summarize( initlist, mean(initlist[,1]), mean(initlist[,2]) )
            # pass extreme values
            return( c(-1,-1) )
        }else{
            return(colMeans(initlist))
        }
    }
    
    # apply to list and combine
    summry <- setNames( do.call( 
                    rbind.data.frame, lapply(dfticker[,tickcol], initOHLC) ),
                         c("wkavgprice", "wkavgvolm") ) 
    #timer
    cat("\n[+] Get summary took", 
            round(difftime( now(), tic, units="mins"),4), "minutes\n\n")
    return( cbind( dfticker, summry) )
}

# filter based on weekly average price and volume
# exclude missing data or errors
tickFilter <- function( dftickers, maxprice=1.5, minvol=2*10^5, minprice=0.1){
    # remove error in data
    initlen <- nrow(dftickers)
    cond_error <-  dftickers[,"wkavgprice"] != "error" & dftickers[,"wkavgprice"] != "skipped"
    cond_maxprice <- dftickers[,"wkavgprice"] <= maxprice
    cond_minprice <- dftickers[,"wkavgprice"] >= minprice
    cond_volm <- dftickers[,"wkavgvolm"] >= minvol
    cond <- cond_error & cond_maxprice & cond_minprice & cond_volm 
    dftickers <- dftickers[ which(cond),]
    cat( paste("\n >>> Filter results in", dim(dftickers)[1],"suitable tickers", sep=" ") )
    cat( paste("\n >>>  out of", initlen," tickers", sep=" ") )
    cat( "\n         maxprice:", format(maxprice, nsmall=2) )
    cat( "\n         minprice:", format(minprice, nsmall=2) ) 
    cat( "\n            minvol:", format(minvol,scientific=F, big.mark=","), "\n\n")
    return( dftickers)
}

# convert to data frame - use all columns
convDF <- function( extes ){
    df <- data.frame( date=index(extes), coredata(extes)) # %>%
                    # dont filter complete cases since already filtered    
                    #filter(complete.cases(.))
    
    
    names(df)[1:6] <- c("date", "open", "high", "low", "close","volume")
    return (df)
}


# generate properties data frame
propDF <- function(dfdata, dftickers){
    naVals <- function(dfdata1){
        vals <- colSums(is.na(dfdata1))
        return( c(vals[2], vals[3]) )
    }
    # missing values
    noVals <- setNames( do.call( rbind.data.frame, lapply( dfdata, naVals)),
                        c("CloseNA","VolmNA") )

    # rows and columns
    dimen <- setNames( do.call( rbind.data.frame, lapply( dfdata, dim)), 
                       c("rows","columns") )
    
    properties <- cbind(dftickers,dimen, noVals)
    return(properties)
}

# explicit print for NAs
missVal <- function(dfprop){
    # check for missing values
    if( length( which( dfprop$VolmNA >0 )) | 
        length( which( dfprop$CloseNA >0)) ) {
        cat("\n[+] Missing close data:\n\t", dfprop$ticker[which( dfprop$CloseNA>0)], 
            "\n",
            "\n[+] Missing volume data:\n\t", dfprop$ticker[which( dfprop$VolmNA>0)],
            "\n\n")
    }else{
        cat("\n[+] No missing values in close and volume data\n\n")
    }
    return(dfprop)
}

# filter condition for low data and NAs
cleaner <- function(tickerproperty){
    full_data <- which(tickprop$rows == max(tickprop$rows))
    closeNA <- !tickprop$CloseNA >0
    volNA <- !tickprop$VolmNA >0
    cond_clean <- full_data & closeNA & volNA 
    return(cond_clean)
}

# normalize prices
normprice <- function(df){
    df1 <- df %>% mutate( normd = df[,"close"]/df[1,"close"])
    return(df1)
}

# calculate daily returns
retDaily <- function(df){
    df <- df %>% mutate(dailyret=(shift1/close)-1)
    df[1,"dailyret"] <- 0
    # !!!PROBLEM HERE!!!
    df[ which( !is.finite(df$dailyret) ),"dailyret"] <- 0
    return(df)
}

# cumulative returns
cumRet <- function(df){
    df[,"cumreturn"] <- (df[,"close"]/df[1,"close"])-1
    return(df)
}




#---------------------------------------------------------
# Prediction functions 
#---------------------------------------------------------

# model alpha and beta
alphabeta <- function(df){
    dfcoef <- data.frame(alpha=double(), beta=double(), 
                         R2=double(), adjR2=double())
    for( i in seq_along(df) ){
        linmodel <- lm( df[[i]][,"dailyret"] ~ dfcomp[[1]][,"dailyret"])
        dfcoef[i,] <- c(coef(linmodel)[[1]], coef(linmodel)[[2]], 
                        summary(linmodel)$r.squared, 
                        summary(linmodel)$adj.r.squared )
    }
    dffinal <- cbind.data.frame(df, dfcoef)
    return( dffinal )
}

# SOME MISSING HERE





#---------------------------------------------------------
# Quantmod indicators
#---------------------------------------------------------

# functions built using quantmod library

# helper function to choose column data
coldata <- function( xts, coln, funcname="FUNC"){
    if( coln=="close" ) { column <- Cl(xts) }
    else{
        if( coln=="vol") { column <- Vo(xts) }
        else { cat( "\n [-] ", funcname, ": Option not available\n\n")}
    }
    return(column)
}

# Average directional index 
adx <- function( ticker, wind=14){
    result <- ADX(ticker, n=wind)
    final <- cbind(ticker, result)
    
    return( final)
}

# Average true range
atr <- function( ticker, wind=14){
    result <- ATR( ticker, n=wind)
    final <- cbind( ticker, result)
    
    return( final)
}

# PArabolic SAR
sar <- function( ticker, accel = c(0.2,0.2)){
    result <- setNames( SAR(ticker, accel=accel), "SAR")
    final <- cbind( ticker, result)
    
    return( final)
}

# Moving Average Convergence Divergence
macd <- function( ticker, nfast=12, nslow=26, nsig=9){
    result <- MACD( Cl( ticker), nFast=nfast, nSlow=nslow,
                    nSig=nsig)
    result[(1:nslow-1),"macd"] <- 0
    result[(1:(nslow+nsig-2)),"signal"] <- 0
    result$"MACDdiff" <- result[,"macd"] - result[,"signal"]
    final <- cbind( ticker,result)
    
    return( final )
}

# Exponential Moving Averages
ema <- function( ticker, coln="close", wind=c(3,5,7,9,11)){
    
    column <- coldata( ticker, coln, funcname="EMA")
    
    for( w in wind ){
        name <- paste( "EMA", as.character(w), sep="")
        errname <- paste( name, "err", sep="")
        
        result <- EMA( column, n=w)
        result$errname <- result - column
        names(result) <- c(name, errname)
        
        ticker <- cbind( ticker, result)
    }   
    # print( names( ticker))
    return(ticker)
}

# Simple Moving Averages
sma <- function( ticker, coln="close", wind=c(3,5,7,9,11)){
    
    column <- coldata( ticker, coln, funcname="SMA")
    
    for( w in wind ){
        name <- paste( "SMA", as.character(w), sep="")
        errname <- paste( name, "err", sep="")
        
        result <- SMA( column, n=w)
        result$errname <- result - column
        names(result) <- c(name, errname)
        
        ticker <- cbind( ticker, result)
    }   
    # print( names( ticker))
    return(ticker)
}

# Bollinger Bands
bbands <- function( ticker, coln="close", wind=11){
    column <- coldata( ticker, coln, funcname="BBands")
    result <- BBands(column)
    coln_name <- colnames(result)
    result$err <- result$up - result$dn
    names(result) <- c( coln_name, "widthbband")
    final <- cbind( ticker, result)
    
    return(final)
}

# Commodity Channel Index
cci <- function( ticker, coln="close"){
    column <- coldata( ticker, coln, funcname="CCI")
    result <- CCI(column)
    final <- cbind( ticker, result)
    
    return(final)
}

# Relative Strength Index
rsi <- function( ticker, coln="close"){
    column <- coldata( ticker, coln, funcname="RSI")
    result <- setNames( RSI(column), "rsi")
    final <- cbind( ticker, result)
    
    return(final)
}

# Williams' Percent Range
wpr <- function( ticker, coln="close"){
    column <- coldata( ticker, coln, funcname="WPR")
    result <- setNames( WPR(column), "wpr")
    final <- cbind( ticker, result)
    
    return(final)
}

# Triple Exponential Average
trix <- function( ticker, coln="close", wind=20, nsig=9){
    column <- coldata( ticker, coln, funcname="TRIX")
    result <- TRIX(column, n=wind, nSig=nsig)
    coln_name <- names(result)
    
    result$err <- result$signal - result$TRIX
    names(result) <- c(coln_name, "TRIXdiff")
    
    final <- cbind( ticker, result)
    
    return(final)
}

# Chande Momentum Oscillator
cmo <- function( ticker, coln="close", wind=14){
    column <- coldata( ticker, coln, funcname="CMO")
    result <- setNames( CMO(column, n=wind), "cmo")
    final <- cbind( ticker, result)
    
    return(final)
}

# Double Exponential Moving Average
dema <- function( ticker, coln="close", wind=10 ){
    column <- coldata( ticker, coln, funcname="DEMA")
    result <- DEMA(column, n=wind)
    final <- cbind( ticker, result)
    
    return(final)
}

# !! NOT IN USE - De-Trended Price Oscillator
# dpo <- function( ticker, coln="close", wind=10){
#     column <- coldata( ticker, coln, funcname="DPO")
#     result <- setNames( DPO(column, n=wind), "dpo")
#     final <- cbind( ticker, result)
#     
#     return(final)
# }

# Rate of Change/Momentum
roc <- function( ticker, coln="close", wind=10){
    column <- coldata( ticker, coln, funcname="ROC")
    result <- setNames( ROC(column, n=wind), "roc")
    final <- cbind( ticker, result)
    
    return(final)
}

# Stochastic Oscillator/Stochastic Momentum Index
smi <- function( ticker, coln="close", nfastK=14, nfastD=3, nslowD=3){
    column <- coldata( ticker, coln, funcname="SMI")
    result <- SMI(column, nFastK=nfastK, nFastD=nfastD, nSlowD=nslowD)
    coln_name <- names(result)
    result$diff <- result$signal - result$SMI
    names(result) <- c( coln_name, "SMIdiff")
    
    final <- cbind( ticker, result)
    
    return(final)
}

# helper function to apply all indicators
allTA <- function(ticker){
    result <- ticker %>% adx %>% 
        atr %>%
        sar %>%
        macd %>%
        ema %>%
        sma %>%
        bbands %>%
        cci %>%
        rsi %>%
        wpr %>%
        trix %>%
        cmo %>%
        dema %>%
        # dpo %>%
        roc %>%
        smi
    
    return(result)
}


#---------------------------------------------------------
# Self created indicators 
#---------------------------------------------------------

# calculate True Range and Average True Range
trueRange <- function(df, period=14, ATR=FALSE){
    df <- df %>% mutate( dayrange = high-low)
    if( ATR == TRUE){
        df1 <- df %>% mutate( hclose = abs(high-lag(close)),
                              lclose = abs(low-lag(close)),
                              TR = pmax( dayrange, hclose, lclose),
                              ATR = roll_mean( TR, period, align="right", fill=0) )
        # %>% select( -dayrange, -hclose, -lclose)
        # not really necessary to remove columns
        # but all columns have similar correlation
    }
    
    if(ATR == FALSE){
        df1 <- df
    }
    return(df1)
}

# rolling mean
simpMA <- function(df, columns, window=c(3,5,7,9,11)){
    # loop allows for multiple windows
    smalist <- c()
    for(w in seq_along(window)){
        # generate appropriate names
        name <- paste("sma", as.character(window[w]), columns, sep="")
        smalist[w] <- name
        errname <- paste("err", name, sep="")
        # get column index
        colm <- grep( paste("^",columns,sep=""), names(df))
        colm_names <- names(df)
        # calc SMA
        df <- df %>% mutate( newcolm = roll_mean( df[,colm], window[w],
                                                  align="right", fill=0 ) ) %>%
            # cannot be CLOSE; should be whatever column being used
            mutate(err = newcolm - df[,colm])
        
        ## first value at window
        # df[1:(window[w]-1),"err"] <- rep(0,(window[w]-1))
        
        # assign column names
        names(df) <- c(colm_names, name, errname)
    }
    # loop to find delta between SMAs
    for(r in seq_along(smalist)[1:length(smalist)-1]){
        k <- r
        while(k != length(smalist)){
            labelname <- paste("errsma",window[r],"_",window[k+1], "close", sep="")
            df[,labelname] <- df[,smalist[r]] - df[,smalist[k+1] ]
            k <- k+1
        }
    }   
    
    return(df)
}

# stochastic oscillator
stoch <- function(df, column, tperiod=14, fast=3, slow=3 ){
    colm <- grep( paste("^",column,sep=""), names(df))
    
    # FORMULA: %K = 100(C - L14)/(H14 - L14)
    
    #!!! need to manage NANs from Kperc division    
    stoch <- df %>% mutate( highesthigh = roll_max( high, tperiod, align="right", fill=0 ),
                            lowestlow = roll_min( low, tperiod, align="right", fill=0 ),
                            Kperc = ifelse( is.finite(100*((close - lowestlow)/(highesthigh - lowestlow))), 
                                            100*((close - lowestlow)/(highesthigh - lowestlow)), 0)
    ) %>%
        select( -highesthigh, -lowestlow)
    
    stoch[1:(tperiod-1),"Kperc"] <- rep(50,(tperiod-1))
    stoch <- stoch %>% mutate( Dpercfast = roll_mean( Kperc, fast, align="right", fill=50),
                               Dpercslow = roll_mean( Dpercfast, fast, align="right", fill=50),
                               errstochfast = Kperc - Dpercfast,
                               errstochslow = Dpercfast - Dpercslow )
    
    return(stoch)
}


# Bollinger band
SD <- function(df, columns, window=11){
    # get column index
    colm <- grep( paste("^",columns,sep=""), names(df))
    
    ## first value at window
    # df[1:window-1,"sdupper"] <- rep(0,window-1)
    # df[1:window-1,"sdlower"] <- rep(0,window-1)
    
    # df[window:nrow(df), "sdupper"] <- df[window:nrow(df),colm] + 
    #     2*roll_sd( df[,colm], window, align="right", fill=0 )
    # df[window:nrow(df), "sdlower"] <- df[window:nrow(df),colm] - 
    #     2*roll_sd( df[,colm], window, align="right", fill=0 )
    
    # rolling std dev
    df[, "sdupper"] <- df[,colm] + 
        2*roll_sd( df[,colm], window, align="right", fill=0 )
    df[, "sdlower"] <- df[,colm] - 
        2*roll_sd( df[,colm], window, align="right", fill=0 )
    
    # calc error of close and standard deviation
    df <- df %>% 
        mutate( errsdupper = sdupper - close ) %>%
        mutate( errsdlower = close - sdlower)
    
    # first value at window
    # df[1:window-1,"errsdupper"] <- rep(0,window-1)
    # df[1:window-1,"errsdlower"] <- rep(0,window-1)
    
    return(df)
} 




