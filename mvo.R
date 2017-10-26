#---------------------------------------------------------
#
# Mean Variance Optimization for optimal portfolio allocation
#
# Based on:
# http://economistatlarge.com/portfolio-theory/r-optimized-portfolio
# 
#
#   rm(list=ls())  
#
#---------------------------------------------------------

library(pacman)
p_load(quadprog)


getRetDaily <- function( list, dfdata, colm="dailyret"){
    extractRet <- function( df, col){
        descol <- grep( col, names(df), ignore.case=T)
        dfret <- data.frame(df[,descol])
        return( dfret)
    }
    df <- dfdata[list]
    retdf <- lapply( df, extractRet, colm)
    retFinal <- setNames( do.call( cbind.data.frame, retdf),
                          names(df))
    return(retFinal)
}

# Visualize eff frontier
plot.eff <- function( dflist){
    eff <- dflist[[1]]
    eff.optimal.point <- dflist[[2]]
    
    # Color Scheme
    ealred  <- "#7D110C"
    ealtan  <- "#CDC4B6"
    eallighttan <- "#F7F6F0"
    ealdark  <- "#423C30"
    canvas <- ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + 
                # coord_cartesian( xlim=c(0.01,max(eff$Std.Dev)*1.1),
                #                ylim= c(0.00015,max(eff$Exp.Return)*1.1)) +
                geom_point(alpha=.7, color="lightgreen") +
                geom_point(data=eff.optimal.point, 
                   aes(x=Std.Dev, y=Exp.Return),
                   color=ealred, size=5) +
                annotate(geom="text", x=eff.optimal.point$Std.Dev*0.75, 
                     y=eff.optimal.point$Exp.Return*1,
                     label=paste("Risk: ", 
                     round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
                     round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                     round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
                     hjust=0, vjust=1.5) +
                ggtitle("Efficient Frontier\nand Optimal Portfolio") +
                labs(x="Risk (std dev of portfolio variance)",y="Return") +
                theme(panel.background=element_rect(fill=eallighttan), 
                    text=element_text(color=ealdark),
                    plot.title=element_text(size=18, color=ealred))
    print(canvas)
    return(canvas)
}

eff.frontier <- function (dfreturns, expected.return=NULL, short=FALSE, 
                          max.allocation=0.40, risk.premium.up=.5, risk.increment=.001){
    # return argument should be a m x n matrix with one column per security
    # short argument is whether short-selling is allowed; default is no (short selling prohibited)
    # max.allocation is the maximum % allowed for any one security (reduces concentration)
    # risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
    # risk.increment is the increment (by) value used in the for loop
    
    covariance <- cov(dfreturns)
    # print(covariance)
    n <- ncol(covariance)
    
    # historical or predicted means
    if( is.null(expected.return) ){
        expreturn <- colMeans(dfreturns)
    }else{
        expreturn <- expected.return
    }
    
    # Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
    Amat <- matrix (1, nrow=n)
    bvec <- 1
    meq <- 1
    
    # Then modify the Amat and bvec if short-selling is prohibited
    if(short==FALSE){
        Amat <- cbind(1, diag(n))
        bvec <- c(bvec, rep(0, n))
        cat("[+] Long only positions\n")
    }
    
    # And modify Amat and bvec if a max allocation (concentration) is specified
    if(!is.null(max.allocation)){
        if(max.allocation > 1 | max.allocation <0){
            stop("max.allocation must be greater than 0 and less than 1")
        }
        if(max.allocation * n < 1){
            stop("Need to set max.allocation higher; not enough assets to add to 1")
        }
        Amat <- cbind(Amat, -diag(n))
        bvec <- c(bvec, rep(-max.allocation, n))
    }
    # Calculate the number of loops based on how high to vary the risk premium and by what increment
    loops <- risk.premium.up / risk.increment + 1
    loop <- 1
    
    # Initialize a matrix to contain allocation and statistics
    # This is not necessary, but speeds up processing and uses less memory
    eff <- matrix(nrow=loops, ncol=n+3)
    # Now I need to give the matrix column names
    colnames(eff) <- c(colnames(dfreturns), "Std.Dev", "Exp.Return", "sharpe")
    
    # Loop through the quadratic program solver
    for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
        dvec <- expreturn * i # This moves the solution up along the efficient frontier
        sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
        eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution *colSums((covariance * sol$solution))))
        eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% expreturn)
        eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
        eff[loop,1:n] <- sol$solution
        loop <- loop+1
    }
    f.frontier <- list()
    f.frontier[[1]] <- as.data.frame(eff)
    f.frontier[[2]] <- f.frontier[[1]][f.frontier[[1]]$sharpe==max(f.frontier[[1]]$sharpe),]
    if( nrow( f.frontier[[2]]) > 1){
        f.frontier[[2]] <- f.frontier[[2]][1,]
    }
    f.frontier[[3]] <- round( t( f.frontier[[2]]), 4)
    
    colnames(f.frontier[[3]]) <- c("Percent Allocation")
    
    
    f.frontier <- setNames( f.frontier, c("data","optimal","final"))
    print(f.frontier[[3]])
    plot.eff(f.frontier)
    return(f.frontier)
}









