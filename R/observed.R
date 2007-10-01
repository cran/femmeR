###=============================
### Title: Observed data files
### Provides: read.obs
###           plot.obs
###-----------------------------
read.obs  <- function (obsfile) 
{
  lines         <- toupper(readLines(obsfile))
  hashline      <- grep("##",lines)                      ## index to lines that have "##"
  lines         <- lines[-(1:hashline)]                  ## remove

  commentline   <- grep("!",lines)                       ## index to lines that have "!"
  emptyline     <- (1:length(lines))[nchar(lines)==0]    ## index to lines that are empty
  
  if (length(commentline) +length(emptyline) >0) {
    lines         <- lines[-union(commentline,emptyline)]  } 

  len           <- length(lines)

  XCTMP         <- textConnection(lines)                 ## paste all lines (internal file)  
  obs           <- read.table(XCTMP,header=T)            ## and read the data...

  ## find columns with names, time, values, with absolute and relative error (use substr - substring)

  iname         <- which (substr(names(obs),1,5) %in% c("X.VAR","X.OBS","X.NAM","X.FOR","X.PAR"))
  itime         <- which (substr(names(obs),1,5) %in% c("X.TIM","X.HOU","X.DAY","X.MIN","X.SEC","X.YEA","X.MIL","X.KYE"))
  ivalue        <- which (substr(names(obs),1,5) ==     "X.VAL"                                 )
  iabserr       <- which (substr(names(obs),1,5) ==     "X.ABS"              )
  irelerr       <- which (substr(names(obs),1,5) ==     "X.REL"              )

  ## if multiple value fields
  if (length(ivalue)>1)
    {                           ## calculate mean and standard deviation and replace columns
      len<-length(ivalue)
      meanvalue     <- obs[ivalue[1]]
      for (i in 2:len) { meanvalue<-meanvalue+obs[ivalue[i]]}
      meanvalue<-meanvalue/len
      stdvalue <-(obs[ivalue[1]]- meanvalue)^2
      for (i in 2:len) { stdvalue<-stdvalue+(obs[ivalue[i]]-meanvalue)^2 }
      stdvalue <-sqrt(stdvalue/(len-1))
      if (length(iabserr)>0) {obs[ivalue[2]] <- obs[iabserr[1]]}
      else                   {obs[ivalue[2]] <- 0}
      obs[ivalue[2]] <- obs[ivalue[2]] + stdvalue
      obs[ivalue[1]] <- meanvalue
      obs            <- obs[-ivalue[3:len]]
      iabserr        <- ivalue[2]
      ivalue         <- ivalue[1]
    }


  ## and give them nominal names
  names(obs)[iname]  <- "name"
  names(obs)[itime]  <- "time"
  names(obs)[ivalue] <- "value"
  names(obs)[iabserr]<- "abserr"
  names(obs)[irelerr]<- "relerr"
  ifirst                 <- 1   ## first numerical column
  if (iname == 1) ifirst <- 2

  ## do something with descriptor names... first find them

  descriptors        <- (1:length(names(obs)))[-c(iname,itime,ivalue,iabserr,irelerr)]
  numdescriptors     <- length(descriptors)

  ## make the names of the observed data factors, so that they can be selected
  obs$name           <- factor(obs$name)
  vars               <- levels(obs$name)
  filename           <- basename (obsfile)
  result             <- list(data=obs,filename=filename,descriptors=descriptors,vars=vars,
                             ifirst=ifirst,ivalue=ivalue,iabserr=iabserr,irelerr=irelerr)  ## keep position of values
  class(result)       <- "obs"
  return(result)

}  ## end read.obs


 plot.obs  <- function (x ,xvari="time",yvari=NULL,
                       rev="",type="b",pch=16,
					   time=NULL,
					   main=x$filename,err="l",errpch=1,
                       xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,...)
  {
    obsname     <- x$vars
    numlevels   <- length(obsname)

    ##-------------------------------------------------------------------------------------
    ## x-variable: find its column position ; i fnot found, take first numerical column
    ##-------------------------------------------------------------------------------------

    if (is.character(xvari)) {xvari  <-which(toupper(names(x$data))==toupper(xvari))
                              if (length(xvari) == 0) xvari <- x$ifirst }
    
    xvar <- names(x$data)[xvari] 

    ##-------------------------------------------------------------------------------------
    ## y-variable: find which observed value it is... 
    ##-------------------------------------------------------------------------------------

    if (is.null     (yvari)) {           ## All variables plotted 
      plotlist <- 1:numlevels
    }
    else if (is.character(yvari)) {      ## Selection of names
      plotlist <- NULL
      for (i in 1:length(yvari)) plotlist <-c(plotlist,which(obsname==toupper(yvari[i])))
    }
    else   plotlist <- yvari             ## Selection of positions

    yvari       <- x$ivalue              ## column with y-s

    setxlim     <- is.null(xlim)  
    setmain     <- (main == x$filename)
    setylim     <- is.null(ylim)
    setylab     <- is.null(ylab)
    setxlab     <- is.null(xlab)
    reverseaxes <- length(grep("v",rev))>0   ## HENRIK: IF rev = "v" then x and y-axis are reversed !
    reverseXax  <- length(grep("x",rev))>0
    reverseYax  <- length(grep("y",rev))>0

    if (reverseaxes) {tt<-xvari; xvari<-yvari; yvari<-tt}

    if (length(plotlist)>1)                         ## if multiple graphs on one page...
      {
        numrows     <- sqrt(length(plotlist)-0.01) + 1  ## number of rows and columns
        par(mfrow = c(numrows, numrows) )
      }

    for (i in plotlist) 
      {

        ## selection of the data 
        if (is.null(time)){selection <-           which (x$data$name == obsname[i])  }
        else              {selection <- intersect(which (x$data$time == time      ),
                                                  which (x$data$name == obsname[i]) )}
        xx    <- x$data[,xvari]  [selection]
        yy    <- x$data[,yvari]  [selection]  
        
        if (setxlim) { xlim_  <- range(xx)   }
        else         { xlim_  <- xlim        }

        if (setmain) { main_  <- obsname[i] }
        else         { main_  <- main[i]    }
        
        if (setylim) { ylim_  <- range(yy) }
        else         { ylim_  <- ylim   }

        if (setylab) { ylab_  <- '-'       }
        else         { ylab_  <- ylab[i]   }

        if (setxlab) { xlab_  <- xvar     }
        else         { xlab_  <- xlab[i]   }

        if (reverseaxes) {tt<-xlab_; xlab_<-ylab_; ylab_<-tt}

        ## check if error bars are needed 

        abserr <- FALSE
        relerr <- FALSE
        
        if (err != "") 
          {
            error   <- 0
            abserr  <- length(x$iabserr > 0)
            relerr  <- length(x$irelerr > 0)
            if (abserr )       error <-            x$data$abserr  [selection]
            if (relerr )    { 

              if (reverseaxes){ error <- error + xx*x$data$relerr  [selection]}
              else            { error <- error + yy*x$data$relerr  [selection]}
            }

            if (abserr + relerr) {
              
              if (reverseaxes) 
                {
                  xxmin  <- xx-error
                  xxmax  <- xx+error
                  if (setxlim) xlim_  <- range(c(xxmin,xxmax))
                }

              else 
                {
                  yymin  <- yy-error
                  yymax  <- yy+error
                  if (setylim)  ylim_  <- range(c(yymin,yymax))
                }
            }
          } 

        ## if x- and y-axis must be descending...

        if (reverseXax) xlim_ <- rev(xlim_)
        if (reverseYax) ylim_ <- rev(ylim_)

        ## plot first the axes
        plot(xx,yy,xlab=xlab_,ylab=ylab_,ylim=ylim_,xlim=xlim_,type="n",main=main_,...)
        
        ## now add symbols
        xy<- xy.coords(xx,yy,xlab=NULL,ylab=NULL,log=NULL)
        plot.xy(xy,type=type,...)

        ## and add errors:
        if (abserr + relerr > 0) 

          {
            numbars <- length(yy)
            for (j in 1:numbars) 
              {  
                if (reverseaxes)
                  {
                    xy<- xy.coords(c(xxmin[j],xxmax[j]),c(yy[j],yy[j]),xlab=NULL,ylab=NULL,log=NULL)
                  }
                else
                  {
                    xy<- xy.coords(c(xx[j],xx[j]),c(yymin[j],yymax[j]),xlab=NULL,ylab=NULL,log=NULL)
                  }
                plot.xy(xy,type=err,lty=errpch,pch=errpch,...)
              }
          }


        ## now for the error bars
        
      }   ## plotlist
    
}


points.obs  <- function (x ,xvari="time",yvari=1,rev="",...)
  {
    ##-------------------------------------------------------------------------------------
    ## x-variable: find its column position ; i fnot found, take first numerical column
    ##-------------------------------------------------------------------------------------

    if (is.character(xvari)) {xvari  <-which(toupper(names(x$data))==toupper(xvari))
                              if (length(xvari) == 0) xvari <- x$ifirst }
    
    xvar <- names(x$data)[xvari] 

    ##-------------------------------------------------------------------------------------
    ## y-variable: find which observed value it is... 
    ##-------------------------------------------------------------------------------------

    if(is.numeric(yvari)) yvari <- x$vars[yvari]

    if(!yvari%in%x$vars) warning("No such Y variable")
    selection <-           which(x$data$name == yvari)
        
    xx    <- x$data[,xvari]  [selection]
    yy    <- x$data[,x$ivalue]  [selection]  
    
    ##---------------------------------------------------------------
    ## Add the points
    ##---------------------------------------------------------------

    if(rev=="v"){ tt <- xx;xx <- yy; yy <- tt}
    points(xx,yy,...)
    
}
