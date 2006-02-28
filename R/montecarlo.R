###---femmeR/R/montecarlo.R---
### Authors: Henrik Andersson <h.andersson@nioo.knaw.nl>
###          Karline Soetaert <k.soetaert@nioo.knaw.nl>
### Creation Date: 2005/08/22 14:16:05
### Title: Monte Carlo
### Provides: read.crl
###           plot.crl
###-----------------------------
 
read.crl <- function(crlfile){
  lines <- readLines(crlfile)
  hashline <- grep("##",lines)
  crlnames <- toupper(scan(file=crlfile,skip=hashline-3,quiet=TRUE,what="char",nlines=1))
  spacelines <- grep("^ $",lines)
  spaceline <- spacelines[spacelines>hashline][1]
  units      <- scan(file=crlfile,skip=hashline-2,quiet=TRUE,what="char",nlines=1)
  data       <- read.table(crlfile,skip=hashline,nrows=spacelines[spacelines>hashline][1]-(hashline+1))
  names(data) <- crlnames
  filename <- basename(crlfile)
  res <- list(vars=crlnames,units=units,data=data,filename=filename)
  class(res) <- "crl"
  return(res)
}

plot.crl <- function(x,xvari=2,yvari=3,type='p',
                     pch=".",main=x$filename,xlab=NULL,ylab=NULL,
                     mar=NULL,oma=NULL,size=1,...){
  ##=================================================================
  ## Error checking
  if(length(yvari)>length(x$vars)) yvari <- 1:length(x$vars)
  ##=================================================================
  ## Several Y variables -> Multiplot

  
  numpar <- length(xvari)
  numvar <- length(yvari)
    par(mfrow=c(numvar,numpar))
  if(is.null(mar))  par(mar=c(2.5, 2.5,0, 0))     # margin size
  else par(mar=mar)
  
  if(is.null(oma))  par(oma=c(2,3,2,0))                # outer margin
  else par(oma=oma)

  for(j in yvari){
    for(i in xvari){
      
      ##-------------------------------------------------------------
      ## Deconstructing the data object
      ##-------------------------------------------------------------
      xdata <- x$data
      vars <- x$vars
      units <- x$units
      ##-------------------------------------------------------------
      
      ##-------------------------------------------------------------
      ## If x or y is a character select appropriate variables
      ##-------------------------------------------------------------
      if(is.character(i)){
        if(i %in% x$vars==FALSE) stop("This X variable does not exists")
        i <- which(names(xdata)==i)
      }
      
      if(is.character(j)){
        if(yvari %in% x$vars==FALSE) stop("This Y variable does not exists")
        j<- which(names(xdata)==j)
      }
      
      xs <- xdata[,i]
      ys <- xdata[,j]
      ##-------------------------------------------------------------
      
      ##-------------------------------------------------------------
      ## Main plot
      ##-------------------------------------------------------------
      plot(xs,ys,
           type=type,
           pch=pch,
           ...)
      

    }
  }

  ##-----------------------------------------------------------------
  ## Labels
  ##-----------------------------------------------------------------
  
  ## variable names
  xpos      <-seq(1/numpar,1,length.out=numpar)
  if (is.null(xlab)) {
    parnames <- x$vars[xvari]
    parnames <- sub("PAR_", "", parnames )
  }
  else parnames <- xlab
  
  parunits    <- x$units[xvari]
  for (i in 1:numpar)  {  
    mtext(side=1,parnames[i],adj=1,line=0,at= xpos[i],outer=TRUE,cex=size)
    mtext(side=1,parunits[i],adj=1,line=1,at= xpos[i],outer=TRUE,cex=size)
  }
  
  ## parameter names - sequence is reversed (plotted from top-bottom)
  ypos      <- rev(seq(1/numvar,1,length.out=numvar))
  varunits    <- x$units[yvari]
  if (is.null(ylab)) {
    varnames <- x$vars[yvari]
    varnames <- sub("PAR_", "", varnames )
  }
  else varnames <- ylab
  
  
  for (i in 1:numvar)  {  
    mtext(side=2,varnames[i],adj=1,line=1,at= ypos[i],outer=TRUE,cex=size)
    mtext(side=2,varunits[i],adj=1,line=0,at= ypos[i],outer=TRUE,cex=size)
  }
  
  mtext(main,side=3,adj=0.5,line=1,outer=TRUE,cex=1)

  ##=================================================================
  ## END OF plot.crl
  ##=================================================================
}

print.crl <- function(x,...){
  cat("femmeR object of class crl\n")
  cat("Filename:",x$filename,"\n")
  cat("\n")
  cat("Variables\n")
  cat("--------------------\n")
  for(i in 1:length(x$vars)){
    cat(i,x$vars[i],"\n")
  }
}
