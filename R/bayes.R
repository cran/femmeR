###---c:/home/henrika/R/femmeR/R/bayes.R---
### Author: Henrik Andersson <h.andersson@nioo.knaw.nl>,  Andreas Hofmann <a.hofmann@nioo.knaw.nl>
### Creation Date: 2005/04/22 13:32:55
### Title: Bayesian FEMME
### Provides:
###  read.bay
###-----------------------------


read.bay <- function(bayfile){
  require(coda)
  lines <- readLines(bayfile)
  hashline <- grep("##",lines)
  baynames <- toupper(scan(file=bayfile,skip=hashline-3,quiet=TRUE,what="char",nlines=1))
  baynames <- baynames[2:(length(baynames)-2)]
  baynames <- sub("PAR_","",baynames)
  spacelines <- grep("^ $",lines)
  units <- scan(file=bayfile,skip=hashline-2,quiet=TRUE,what="char",nlines=1)
  data <- read.table(bayfile,skip=hashline,nrows=spacelines[spacelines>hashline][1]-(hashline+1))
  data <- data[,2:(ncol(data)-2)]
  names(data) <- baynames
  filename <- basename(bayfile)
  coda.data <- mcmc(data)
  res <- list(pars=baynames,filename=filename,data=coda.data)
  class(res) <- "bay"
return(res)
}

print.bay <- function(x,...){
  cat("femmeR object of class bay\n")
  cat("Filename:",x$filename,"\n")
  cat("\n")
  cat("Parameters\n")
  cat("--------------------\n")
  for(i in seq(along=colnames(x$data))){
    cat(colnames(x$data)[i],"\n")
  }
}


plot.bay <- function(x,...){
  plot(x$data,...)
}

pairs.bay <- function(x,start=0,end=0,...)
     {
	panel.hist <- function(x, ...)				
     		{
        		 usr <- par("usr"); on.exit(par(usr))
         	         par(usr = c(usr[1:2], 0, 1.5) )
                         h <- hist(x, plot = FALSE)
                         breaks <- h$breaks; nB <- length(breaks)
                         y <- h$counts; y <- y/max(y)
                         rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
                 }
	Rdata <- data.frame(x$data)
        num_rows <- dim(Rdata[1])[1]
	if ((start > 0) && (end > 0) && (start <= num_rows) && (end <= num_rows))
		{
			Rdata <- Rdata[(start:end),]
		} 
	pairs(Rdata, pch='.',diag.panel=panel.hist,labels=NULL,gap=0,xaxt="n",yaxt="n",upper.panel=NULL,...)
     }
