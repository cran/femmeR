### Author: Henrik Andersson <h.andersson@nioo.knaw.nl>
### Created 2005/04/15
### Modified: 
### Title: Femme general
### Provides:
### read.st1
### read.obs
### plot.st1
###-----------------------------


read.st1 <- function(st1file){
  lines <- readLines(st1file)
  hashline <- grep("##",lines)
  st1names <- scan(file=st1file,skip=hashline-3,quiet=TRUE,what="char",nlines=1)
##  spacelines <- grep("^ $",lines)
  units <- scan(file=st1file,skip=hashline-2,quiet=TRUE,what="char",nlines=1)
  data <- read.table(st1file,skip=hashline)
  names(data) <- st1names
  filename <- basename(st1file)
  res <- list(vars=st1names,units=units,data=data,filename=filename)
class(res) <- "st1"
return(res)
}



read.o1 <- function(o1file){
  lines <- readLines(o1file)
  funnyline <- grep("\\{",lines)
  skip <- ifelse(length(funnyline)>0,2,0)  
  hashline <- grep("##",lines)
  o1names <- toupper(scan(file=o1file,skip=skip,quiet=TRUE,what="char",nlines=1))
  spacelines <- grep("^ $",lines)
  spaceline <- spacelines[spacelines>hashline][1]
  units <- scan(file=o1file,skip=skip+1,quiet=TRUE,what="char",nlines=1)
  data <- read.table(o1file,skip=hashline,nrows=spaceline-(hashline+1))
  names(data) <- o1names
  filename <- basename(o1file)
  res <- list(vars=o1names,units=units,data=data,filename=filename)
  class(res) <- "o1"
  return(res)
}


read.o2 <- function(o2file){
  ##  lines <- readLines(o2file)
  ##   funnyline <- grep("\\{",lines)
  ##   skip <- ifelse(length(funnyline)>0,2,0)  
  ##   hashline <- grep("##",lines)
  ##   o2names <- toupper(scan(file=o1file,skip=skip,quiet=TRUE,what="char",nlines=1))
  ##   spacelines <- grep("^ $",lines)
  ##   spaceline <- spacelines[spacelines>hashline][1]
  ##   units <- scan(file=o1file,skip=skip+1,quiet=TRUE,what="char",nlines=1)
  ##   data <- read.table(o1file,skip=hashline,nrows=spaceline-(hashline+1))
  ##   names(data) <- o2names
  ##   filename <- basename(o2file)
  ##   res <- list(vars=o1names,units=units,data=data,filename=filename)
  ##   class(res) <- "o1"
  ##   return(res)
  
  ##-----------------------------------------------------------------
  ## Input: FEMME output file with sediment data in array format
  ## Output: List with data in a nested list, depth info and variables
  ##-----------------------------------------------------------------
  test <- scan(o2file,nlines=1,what="character")
  test2 <- factor(sub('\\(.*\\)',"",test))
  vars <- levels(test2)
  endline <- which(count.fields(o2file)==1)         #Finds the empty line
  depth <- as.numeric(unique(scan(o2file,nlines=1,skip=2,what="character"))[-1])  #Extracts Sediment Depth
  data <- read.table(o2file,skip=4,nrows=endline-4) 
  names(data) <- test

  time <- as.vector(data[,test2=="Time"])
  timeidx <- which(vars=="Time")
  vars <- vars[-timeidx]
  datalist <- list(NA) #Initalizes the data list
  for(i in seq(along=vars)){
    datalist[[i]] <- as.matrix(data[,test2==vars[i]])
  }
  names(datalist) <- vars
  resultat <- list(data=datalist,vars=vars,depth=depth,time=time)
  class(resultat) <- "o2"
  return(resultat)
}





plot.st1 <- function(x,xvari=1,yvari=2,rev="",type='l',obs=NULL,
                     pch=16,main=x$filename,xlab=NULL,ylab=NULL,
                     xlim=NULL,ylim=NULL,mar=NULL,oma=NULL,obsname=obs$filename,...){
  ##=================================================================
  ## Error checking
  if(length(xvari)>1) stop("Sorry, but multiple x variables are not supported yet!")

  if(length(yvari)>length(x$vars)) yvari <- 1:length(x$vars)
  ##=================================================================
  ## Several Y variables -> Multiplot
    size <- ceiling(sqrt(length(yvari)))
  if(length(yvari)>1) {
    par(mfrow=c(size,size))
    
    if(is.null(oma))  par(oma=c(0,0,2,0))                 # outer margin
    else par(oma=oma)
                     }
  
  if(is.null(mar))  par(mar=c(4.1, 4.1, 1.1, 1.1))     # margin size
  else par(mar=mar)
  

  
  for(yvari_j in yvari){
    
    ##=================================================================
    ## Deconstructing the data object
    xdata <- x$data
    vars <- x$vars
    units <- x$units
    ##=================================================================
    
    ##=================================================================
    ## If x or y is a character select appropriate variables
    ##-----------------------------------------------------------------
    if(is.character(xvari)){
      if(xvari %in% x$vars==FALSE) stop("This X variable does not exists")
      xvari <- which(names(xdata)==xvari)
    }
    
    xvarname <- x$vars[xvari]
    
    if(is.character(yvari_j)){
      if(yvari %in% x$vars==FALSE) stop("This Y variable does not exists")
      yvari_j<- which(names(xdata)==yvari_j)
    }

    yvarname <- x$vars[yvari_j]
    
    xs <- xdata[,xvari]
    ys <- xdata[,yvari_j]
    ##=================================================================

    ##=================================================================
    ## Construct default axis labels
    ##-----------------------------------------------------------------
    if(is.null(xlab)){
      xlab=paste(vars[xvari]," (",units[xvari],")",sep="")
    }

    
    if(is.null(ylab)){
      ylab_j=paste(vars[yvari_j]," (",units[yvari_j],")",sep="")
    }
    else{ylab_j <- ylab}
    ##=================================================================

    ##=================================================================
    ## Axes limits
    ##-----------------------------------------------------------------
    if(is.null(xlim)) {
      xlim <- range(xs)
      
      reverseXaxis <- length(grep("x",rev))>0
      
      if(reverseXaxis){
        xlim <- rev(range(xs))
      }
      
    }

    if(is.null(ylim)) {
      ylim_j <- range(ys)

      reverseYaxis <- length(grep("y",rev))>0

      if(reverseYaxis){
        ylim_j<- rev(range(ys))
      }
  
     
    }
    else{
      ylim_j=ylim
    }
    ##=================================================================

    ## Reversion of y and x data
    reverseaxes <- length(grep("v",rev))>0

    if(reverseaxes){tt <- xs; xs <- ys; ys <- tt
                     tlab <- xlab; xlab <- ylab_j; ylab_j<- tlab
                     tlim <- xlim; xlim <- ylim_j ; ylim_j <- tlim}
                     
    ##=================================================================
    ## Main plot
    ##-----------------------------------------------------------------



    if(!is.null(obs)){

      ##-------------------------------------------------------------------------------------
      ## x-variable: find its column position ; i fnot found, take first numerical column
      ##-------------------------------------------------------------------------------------

      if (is.character(xvarname)) {xvarname  <-which(toupper(names(obs$data))==toupper(xvarname))
                                   if (length(xvarname) == 0) xvarname <- obs$ifirst }
    
      xvar <- names(obs$data)[xvarname] 

      ##-------------------------------------------------------------------------------------
      ## y-variable: find which observed value it is... 
      ##-------------------------------------------------------------------------------------

      if(is.numeric(yvarname)) yvarname <- obs$vars[yvarname]

      if(!yvarname%in%obs$vars) warning("No such Y variable")
      selection <-           which(obs$data$name == yvarname)
      
      xx    <- obs$data[,xvarname]  [selection]
      yy    <- obs$data[,obs$ivalue]  [selection]  
      

      xlim <- range(xlim,xx)

      ylim_j <- range(ylim_j,yy)

        
      plot(xs,ys,
           xlab=xlab,
           ylab=ylab_j,
           ylim=ylim_j,
           xlim=xlim,
           type=type,
           ...)
   
      if(reverseaxes) {
        points.obs(obs,xvarname,yvarname,rev="v",...)}
      else{
        points.obs(obs,xvarname,yvarname,...)
      }
      mtext(side=3,obsname)
      }

    else{
      plot(xs,ys,
           xlab=xlab,
           ylab=ylab_j,
           ylim=ylim_j,
           xlim=xlim,
           type=type,
           ...)
    }
  }
  
  if(length(yvari)>1) {
    title(main,outer=TRUE) }
  else{
    title(main) }
  ##=================================================================
  ## END OF plot.st1
  ##=================================================================
}


##===================================================================
## plot.st1 is using the same function as plot.o1
##-------------------------------------------------------------------

plot.o1 <- plot.st1

##===================================================================


plot.o2 <- function(x,zvari=1,col=topo.colors(100),nlevels=10,
xlab="Time",ylab="Depth",rev="",xlim=NULL,ylim=NULL,labcex=1.5,
linecol=1,contour=TRUE,...){
xs <- x$time
ys <- x$depth

if(is.character(zvari)){
  zvari <- which(names(x$data)==zvari)
}

z <- x$data[[zvari]]

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
      ylim <- rev(range(ys))
    }

    if(rev=="x"){
      ylim <- range(ys)
    }
    
    if(rev%in%c("xy","yx")){
      ylim <- rev(range(ys))
    }
    
    if(rev==""){
      ylim <- range(ys)
      xlim <- range(xs)
    }
  }
  ##=================================================================


image(xs,ys,z,col=col,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,...)
if(contour){
  contour(xs,ys,z,nlevels=nlevels,xlim=xlim,ylim=ylim,add=T,labcex=labcex,col=linecol,...)
}
}

## Print methods

print.o1 <- function(x,...){
  cat("femmeR object of class o1\n")
  cat("Filename:",x$filename,"\n")
  cat("\n")
  cat("Variables\n")
  cat("--------------------\n")
  for(i in seq(along=names(x$data))){
    cat(i,names(x$data)[i],"\n")
  }
}

print.st1 <- function(x,...){
  cat("femmeR object of class st1\n")
  cat("Filename:",x$filename,"\n")
  cat("\n")
  cat("Variables\n")
  cat("--------------------\n")
  for(i in seq(along=names(x$data))){
    cat(i,names(x$data)[i],"\n")
  }
}

print.o2 <- function(x,...){
  cat("femmeR object of class o2\n")
  cat("Filename:",x$filename,"\n")
  cat("\n")
  cat("Variables\n")
  cat("--------------------\n")
  for(i in seq(along=names(x$data))){
    cat(i,names(x$data)[i],"\n")
  }
}

## END OF FILE


