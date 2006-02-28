## Henrik Andersson, Karline Soetaert
## Module for sensitivity - identifiability functions
## Applicable to FEMME .PCV files
##===================================================================
## Provides:
##-------------------------------------------------------------------
## read.pcv Reads sensitivity values file
##
## dmsqr based on Brun et al, 2001, Wat. Resources Res. 37(4):1015-1030
## dmabs
##
## sens.rank Sorts sensitivity measures according to dmsqr
## summary.pcv Alias to sens.rank
##
## sens.plot Plots sensitivity functions of all observed variables in the simplest way
## plot.pcv Alias to sens.plot
##
## collin.format.table Formats the collinearity with parameters names instead of ones and zeros
##
## collin.remove Remove unwanted parameter combinations
##
## read.sns Read sensivity files
## plot.sns Plots sensivity files (not parcovariance, normal sensitvity)
##-------------------------------------------------------------------

##===================================================================
##
## (1) Read values from .pcv file
##
##===================================================================

read.pcv <- function(pcvfile){
  
  filename <- basename(pcvfile)
  ##------------------------------------
  ## Input: FEMME .pcv file
  ## Output: pcv object (list with sensitivity + collinearity)
  ##------------------------------------
  
  sens.read <- function(pcvfile) {
    
    lines <- readLines(pcvfile)
    g <- grep( "@@START DATAPOINT SENSITIVITY@@|@@END DATAPOINT SENSITIVITY@@",lines )
    lines <- lines[ seq( g[1]+1,g[2]-1) ]
    textlines <- textConnection(lines)
    mydata <- read.table(textlines,header=TRUE)
    close(textlines)
    return(mydata)
  }
  
  collin.read <- function(pcvfile) {
    
    lines <- readLines(pcvfile)
    g <- grep( "@@START PARAMETER COLLINEARITY@@|@@END PARAMETER COLLINEARITY@@",lines )
    lines <- lines[ seq( g[1]+1,g[2]-1) ]
    textlines <- textConnection(lines)
    mydata <- read.table(textlines,header=FALSE)
    close(textlines)
    return(mydata)
    
  }
  
  resultat <- list(sens=sens.read(pcvfile),collin=collin.read(pcvfile),filename=filename)
  class(resultat) <- "pcv"
  return(resultat)
}

##===================================================================
##
## (2) Calculate and plot sensitivity measures
##
##===================================================================

dmsqr <- function(s) sqrt(1/length(s)*sum(s^2)) #squared
##-------------------------------------------------------------------

dmabs <- function(s) 1/length(s)*sum(abs(s)) #absolute

##-------------------------------------------------------------------

summary.pcv <- function(object,parnames=NULL,order=TRUE,...){  
  ##-----------------------------
  ## Input: pcv object (opt. parameter names)
  ## Output: sorted sensitivity ranking
  ##------------------------------

  sens.data <- object$sens
  
  s <- matrix(0,nrow=length(sens.data)-4,ncol=5) #Create empty matrix of correct length
  for(i in 1:(length(sens.data)-4))
    {
      s[i,1] <- dmsqr(sens.data[,i+4])
      s[i,2] <- dmabs(sens.data[,i+4])
      s[i,3] <- mean(sens.data[,i+4])
      s[i,4] <- max(sens.data[,i+4])
      s[i,5] <- min(sens.data[,i+4])
    }
  if(is.null(parnames))  {rownames(s) <-  names(sens.data)[5:length(sens.data)]}
  else {rownames(s) <- parnames}
  colnames(s) <- c("dmsqr","dmabs","mean","max","min")
  if(order){
    s.order <- order(s[,1],decreasing=TRUE)
    s.sort <- s[s.order,]
    return(s.sort)} else {
      return(s)
    }
}


##-------------------------------------------------------------------

plot.pcv <- function(x,xvar=seq(along=sens.data[,1]),ylim=NULL,pari=seq(1,length(x$sens)-4),parnames=NULL,type='b',scale=FALSE,...){  
  ##------------------------------------------------
  ## Input: pcv object (output from pcv.read)
  ## Output: sensitivity plots
  ##-------------------------------------------------

  sens.data <- x$sens

  s.names <- names(sens.data)[5:length(sens.data)]
  if(is.null(parnames))           parnames=s.names
  
  ## 1: Plots only the chosen parameter
  if(length(pari)==1)
    {
      ## Ensure that 0 is part of the y-range
      if(is.null(ylim)){
        lims <- range(sens.data[,pari+4])
        if(all(lims>0)) lims[1] <- 0
        if(all(lims<0)) lims[2] <- 0
      }else{
        lims <- ylim
      }
      
      plot(xvar,sens.data[,pari+4],
           main=parnames[pari],
           ylab="Relative sensitivity",
           type=type,
           ylim=lims,
           ...
           )
      abline(h=0,lty=2)
    }
  
  ## 2: Plots all parameters
  else {
    par(mfrow=c(2,ceiling((length(pari))/2)))
    par(oma=c(0,2,3.1,0))
    par(mar=c(5.1,4.1,4.1,1.1))

        
    for (i in pari)
      {
        
        if(scale) {
          lims <- range(sens.data[,5:length(sens.data)])
          if(all(lims>0)) lims[1] <- 0
          if(all(lims<0)) lims[2] <- 0          
        }
        else {
          ## Ensure that 0 is part of the y-range
          lims <- range(sens.data[,i+4])
          if(all(lims>0)) lims[1] <- 0
          if(all(lims<0)) lims[2] <- 0          
        }

        
        plot(xvar,sens.data[,i+4],
             main=parnames[i],
             type=type,
             ylab="",
             ylim=lims,
             ...
             )
        abline(h=0,lty=2)
        
      }
    mtext(side=2,"Relative sensitivity",outer=TRUE)
  }
}

##-------------------------------------------------------------------

points.pcv <- function(x,xvar=seq(along=sens.data[,1]),
                       pari=1,type='b',...){  
  ##------------------------------------------------
  ## Input: pcv object (output from pcv.read)
  ## Output: add points or lines to a sensitivity plot
  ##-------------------------------------------------

  sens.data <- x$sens

      points(xvar,sens.data[,pari+4],
             type=type,
             ...
             )
  
}



##-------------------------------------------------------------------

collin.format.table <- function(x,parnames=NULL){
  ##------------------------------------------------------
  ## Input: pcv object  (opt. parameter names)
  ## Output: formatted table
  ##------------------------------------------------------

  collin.table <- x$collin
  
  table <- collin.table[,-c(1,2)]       #Skip first two columns
  ##If no parameter names are given numbers are given instead
  if(is.null(parnames)){
    parameters <- apply(table > 0, 1, function(idx)
                        paste(seq(1,length(table))[idx], collapse=" "))
  }
  ##Use given parameter names
  else {
    parameters <- apply(table > 0, 1, function(idx)
                        paste(parnames[idx], collapse=" "))
  }
  a <- data.frame(collin.table[,1],parameters,collin.table[,2])
  names(a) <- c("Combinations","Parameters","Value")
  return(a)
}
##-------------------------------------------------------------------

collin.remove <- function(x,pari){
  ##-----------------------------------------------------------------
  ## Input: collinearity table + parameters to remove
  ## Output: Shortened collinearity table
  ##-----------------------------------------------------------------

  collin.table <- x$collin
  
  idx <- unique(which(collin.table[,(pari+2)]==1,arr.ind=TRUE)[,1])
  resultat <- list(sens=x$sens,collin=collin.table[-idx,])
  class(resultat) <- "pcv"
  return(resultat)
  
}

print.pcv <- function(x,...){
  cat("femmeR object of class pcv\n")
  cat("Filename:",x$filename,"\n")
  cat("\n")
  cat("Parameters\n")
  cat("--------------------\n")
  for(i in 5:length(names(x$sens))){
    cat(names(x$sens)[i],"\n")
  }
}




### Sensitivity

###-----------------------------
### Title: Sensitivity FEMME
###  read.sns
###  plot.sns
###-----------------------------


read.sns <- function(snsfile){
  lines      <- readLines(snsfile)
  hashline   <- grep("##",lines)
  snsnames   <- scan(file=snsfile,skip=hashline-3,quiet=TRUE,what="char",nlines=1)
  spacelines <- grep("^ $",lines)
  units      <- scan(file=snsfile,skip=hashline-2,quiet=TRUE,what="char",nlines=1)
  data       <- read.table(snsfile,skip=hashline,nrows=spacelines[spacelines>hashline][1]-(hashline+1))
  names(data)<- snsnames
  filename   <- basename(snsfile)
  
  ## sensitivity variables are grouped by 4: MIN, MEAN, MAX, STD; here we create a sequence to each

  seq        <- seq(-1,length(data),by=4)  
  seq[1]     <- 1                               ## First one is time, sequence is 1,3,7,11,...
  
  ## and the names of each variable , a substring, starting from 5th character (but not for first)
  seqnames   <- substr(snsnames[seq],5,50)
  seqnames[1]<- snsnames[1]

  res        <- list(names=seqnames, seq=seq,vars=snsnames,units=units,data=data,filename=filename)
  class(res) <- "sns"
  return(res)
}

print.sns <- function(x,...){
  cat("femmeR object of class sns\n")
  cat("Filename:",x$filename,"\n")
  cat("\n")
  cat("Variables\n")
  cat("--------------------\n")
  for(i in 1:length(x$vars)){
    cat(i,x$vars[i],"\n")
  }
}



plot.sns <- function(x,xvari=1,yvari=2,rev="",type='l',
                     pch=16,main=x$filename,xlab=NULL,ylab=NULL,
                     xlim=NULL,ylim=NULL,...){
  ##=================================================================
  ## Deconstructing the data object
  xdata <- x$data
  vars  <- x$vars
  units <- x$units
  seq   <- x$seq
  names <- x$names
  ##=================================================================

  
  ##=================================================================
  ## If x or y is a character select appropiate variables in sequence
  ##-----------------------------------------------------------------
  if(is.character(xvari)){
    xvari <- which(x$names==xvari)
  }
  
  if(is.character(yvari)){
    yvari <- which(x$names(xdata)==yvari)
  }
  
  ## sequence variables give exact position in data 
  xi<-xvari           ## position in sequence, and in names
  yi<-yvari

  xvari<-seq[xvari]   ## position in data, in vars, in units
  yvari<-seq[yvari]

  xs <- xdata[,xvari]
  ys <- xdata[,yvari]    ## Mean of sensitivity variable
  y1 <- xdata[,yvari-1]  ## Min of sensitivity variable
  y2 <- xdata[,yvari+1]  ## Max of sensitivity variable

  xx <- c(xs, rev(xs))   ## for polygon xx: we have to end where we started
  yy <- c(y1, rev(y2))   ## rev is the reverse of the series

  ##=================================================================

  ##=================================================================
  ## Construct default axis labels
  ##-----------------------------------------------------------------
  if(is.null(xlab)){
    xlab=paste(names[xi]," (",units[xvari],")",sep="")
  }
  
  if(is.null(ylab)){
    ylab=paste(names[yi]," (",units[yvari],")",sep="")
  }
  ##=================================================================

  ##=================================================================
  ## Axes limits
  ##-----------------------------------------------------------------
  if(is.null(xlim)) {

    if(rev=="y"){
      xlim <- range(xs)
    }
    
    if(rev=="x"){
      xlim <- rev(range(xs))
    }
    
    if(rev%in%c("xy","yx")){
      xlim <- rev(range(xs))
    }
    
    if(rev==""){
      xlim <- range(xs)
    }
  }

  if(is.null(ylim)) {
    
    if(rev=="y"){
      ylim <- rev(range(yy))
    }

    if(rev=="x"){
      ylim <- range(yy)
    }
    
    if(rev%in%c("xy","yx")){
      ylim <- rev(range(yy))
    }
    
    if(rev==""){
      ylim <- range(yy)
      xlim <- range(xs)
    }
  }
  ##=================================================================

  
  ##=================================================================
  ## Main plot: first an empty plot, to allow polygon 
  ##-----------------------------------------------------------------
    par(bg = "white")                  ## Background color
    
	plot(xx,yy,
       xlab=xlab,
       ylab=ylab,
       ylim=ylim,
       xlim=xlim,
       type="n",
       main=main,
       ...)
    polygon(xx, yy, col = "gray")      ## Adds a polygon  

    xy <- xy.coords(xs,ys, 
	                xlab = NULL, 
					ylab = NULL, 
					log = NULL, 
					recycle = FALSE)
    plot.xy(xy,"l")                    ## plot average as a line

  }
  ##=================================================================

  ##=================================================================
  ## END OF plot.sns
  ##=================================================================
