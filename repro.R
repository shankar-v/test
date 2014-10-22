library ("gclus")
library ("forecast")
library ("MASS")
library ("tseries")
library ("fGarch")

wenhan.arima <- function(ticker = "DOV"){

    pdf(paste(ticker, ".forecast.processed.pdf", sep=""))
    
    df <- read.csv(paste(ticker, ".pca.multiplied.processed.csv", sep=""))
    
    seasonalCycle <- 4L
    
    for (i in 2:ncol(df)) {
        #vecPC <- rev(df$PC1)
        vecPC <- rev(df[, i]) # reverse so that the later time is behind
        
        ## Decompose TS
        pcTS <- ts(vecPC, frequency = seasonalCycle)
        pcTSComponents <- decompose(pcTS)
        plot(pcTSComponents)
        
        ## use simple exponential fit through HoltWinters
        fit <- HoltWinters(pcTS, beta=FALSE, gamma=FALSE)
        
        accuracy(fit)
        plot(forecast(fit, 3), main = paste("HoltWinters forecast of PC", i-1, sep=""))
        
        afit <- NULL
        
        tryCatch( {
            ## now try auto Arima model
            afit <- auto.arima(pcTS)
            ## afit <- custom.arima(pcTS)
            
            ## g<-garch(pcTS)
            ## afit <- g@fit
            
            ## afit <- garchFit( ~ garch(1, 1), data = pcTS)
            
        },
        warning = function(warnCond) {},
        error = function(errCond) {
            message(ticker, ":", errCond)
            message("\n")
            afit <- NULL
        },
        finally = {})
        
        ## Arima model
        if (!is.null(afit)) {
            plot(forecast(afit, 3), main = paste("ARIMA forecast of PC", i-1, sep=""))
            ## plot(predict(afit, 3))        
        }
        
    }
    
    dev.off()
}
