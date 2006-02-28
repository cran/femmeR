###-----------------------------
### Title: readweb FEMME
### reading foodweb file 
### readweb
###-----------------------------



read.web  <- function (webfile) {
  ## initialise
  flows         <- NULL      ## food web flows
  variables     <- NULL      ## food web variables 
  flowranges    <- NULL      ## ranges of flows
  varranges     <- NULL      ## ranges of variables

  ## find start position of each section
  lines         <- toupper(readLines(webfile))
  flowline      <- grep("=         NETWORK  FLOWS          =",lines)   ## index to flows
  varline       <- grep("=         VARIABLES               =",lines)   ## index to variables
  rangeline     <- grep("=         NETWORK  RANGES         =",lines)   ## index to flow ranges
  vrangeline    <- grep("=      VARIABLES AND RANGE        =",lines)   ## index to variable ranges

  emptyline     <- grep("^ $",lines)                                   ## index to lines that are empty
  emptyline     <-c(emptyline,length(lines)+1)                         ## if no empty line at end of file

  ## read flow section
  if (length(flowline)  > 0) { flows     <- read.table(webfile,header=TRUE,skip=flowline+2,
                                                       nrows=emptyline[emptyline>flowline +2][1]-(flowline +4)) }
  numflows           <- dim(flows)[1]                              ## number of flows
  names(flows)[1]    <- "name"                                     ## new heading

  ## read variables section
  if (length(varline)   > 0) { variables <- read.table(webfile,header=TRUE,skip=varline+2 ,
                                                       nrows=emptyline[emptyline>varline  +2][1]-(varline  +4)) }
  numvariables       <- dim(variables)[1]
  names(variables)[1]<- "name"

  ## read range sections
  if (length(rangeline) > 0) { flowranges<- read.table(webfile,header=TRUE,skip=rangeline+2 ,
                                                       nrows=emptyline[emptyline>rangeline+2][1]-(rangeline+4)) }
  else                       { flowranges$name<- flows$name;flowranges$Value <- flows$Value}

  if (length(vrangeline)> 0) { varranges <- read.table(webfile,header=TRUE,skip=vrangeline+2 ,
                                                       nrows=emptyline[emptyline>vrangeline+2][1]-(vrangeline+4)) }

  ## make the names of the components and create the flow matrix
  fromto             <- strsplit(as.character(flows$name),"->")    ## split flows into their components
  components         <- fromto[[1]]                                ## and put in one array
  for (i in 2:numflows) {components  <- c(components,fromto[[i]]) }
  components         <- levels(factor(components))                 ## keep unique only 
  numcomp            <- length(components)                         ## flow matrix
  flowmatrix         <- matrix(0,nrow=numcomp,ncol=numcomp)
  for (i in 1:numflows) {from  <- fromto[[i]][1] ; to <- fromto[[i]][2] ; value <- flows$Value[i]
                         x1    <- which (components == from)
                         y1    <- which (components == to)
                         flowmatrix[x1,y1] <- value                      
                       }

  ## save the results
  flownames          <- as.character(flows$name)
  varnames           <- variables$name

  filename           <- basename (webfile)
  result             <- list(flows=flows,flownames=flownames,flowmatrix=flowmatrix,
                             components=components,
                             variables=variables,   varnames=varnames, 
                             flowranges=flowranges, varranges=varranges,
                             filename=filename)  
  class(result)       <- "web"
  return(result)

}

plot.web   <- function (x, sizelab = 1.5, sizefig = 1.3, main="",sub="", log=FALSE, legend=TRUE,...)  {
  
  ## sizelab: relative size of labels
  ## sizefig: relative size of figures; relative to 1 (unit circle)
  ## main: main title
  ## log: log scale or not
  ## legend: include legend or

  ##-----------------------------------------------------------------
  ## Deconstructing the data object
  ##-----------------------------------------------------------------
    components <- x$components      ## food web components
    numcomp    <- length(components)  
    if (!as.logical(log)) { flowmatrix <-       x$flowmatrix ;       zero = 0    }
	else       { flowmatrix <- log10(x$flowmatrix+1e-20); zero = -20  }
  ##-----------------------------------------------------------------
  
  maxflow    <- max(flowmatrix)
  minflow    <- min(flowmatrix[flowmatrix != zero])
  figlim     <- c(-sizefig,sizefig)   
  usr        <- par("usr"); on.exit(par(usr))
  par(mar=c(1,1,1,1))

  ##-----------------------------------------------------------------
  ## empty plot, unit circle
  ##-----------------------------------------------------------------
  plot(c(0, 0), type = "n", ylab = "", asp = 1, xaxt = "n", 
       yaxt = "n", frame.plot = FALSE, xlim = figlim,
       ylim = figlim,main=main,sub=sub,xlab="")

  ## circle - not drawn
  #symbols(0, 0, cir = 1.0, inc = FALSE, add = TRUE)

  ##-----------------------------------------------------------------
  ## labels of components
  ##-----------------------------------------------------------------
  
  alpha0  <- pi/2
  alpha   <- alpha0 - (1:numcomp) * 2 * pi/numcomp
  offset  <- 1.0
  xl      <- cos(alpha)
  yl      <- sin(alpha)

  for (i in 1:numcomp) {                           ## write labels
		    if (    xl[i]  > 0     ) adjust = 0
		    if (    xl[i]  < 0     ) adjust = 1
		    if (abs(xl[i]) < 0.0001) adjust = 0.5

            text(offset * xl[i], offset * yl[i], components[i], 
             adj =adjust, cex = par("cex") * sizelab)
 
                        }
  ##-----------------------------------------------------------------
  ## arrows representing the flows
  ##-----------------------------------------------------------------
  
  par(lend=1)
  maxarrow <- 10 / maxflow
  minarrow <- 1

  xi       <- xl-0.05* cos(alpha)
  yi       <- yl-0.05* sin(alpha)

  xe       <- xl-0.05* cos(alpha)
  ye       <- yl-0.05* sin(alpha)


  for (i in 1:numcomp) {
		  x2 <- xi[i] ; y2 <- yi[i]
          for (j in 1:numcomp)  {
		       if (flowmatrix[i,j] >zero ) 
			   {  x1 <- xe[j] ; y1 <- ye[j] ; size = minarrow + maxarrow * flowmatrix[i,j]
			    arrows (x1,y1,x2,y2,length=0.1,code=1,lwd=size)}
		                        }    ## end j
                       
					   }  ## end i
  ##-----------------------------------------------------------------
  ## legend
  ##-----------------------------------------------------------------

  if (as.logical(legend))   {
  sizeleg = par("cex") * sizelab
  ## size of largest and smallest arrows

  if (!as.logical(log)) {tmax <-    maxflow; tmin <-    minflow}
  else      {tmax <- 10^maxflow; tmin <- 10^minflow}

  legend("bottomright",legend=c(tmax,tmin),cex=c(sizeleg,sizeleg),lwd=c(10,1))
                }
}

###-----------------------------
###  end plotweb
###-----------------------------


dotchart.web   <- function (x, log="", pch = 16, bg = par("bg"), color = par("fg"), 
                            lcolor = "gray", xlim = NULL, main = NULL, 
                            xlab = NULL, ylab = NULL,sizelab = 1.0, ...)
{
    
    ## sizelab: relative size of labels
    ## main: main title
    ## log: log scale or not  TO BE IMPLEMENTED

    ##-----------------------------------------------------------------
    ## Deconstructing the data object
    ##-----------------------------------------------------------------
    
    labels   <- x$flownames 
    ranges   <- x$flowranges
    num      <- 4
    if (length(ranges) < 4) { num <- 2; ranges <- x$flows }
    ## karline: check for NANs ("unbounded")


    if (log =="x") {

      minflow<-min(ranges[2:num][ranges[2:num]!=0])   ## minimum, but larger than 0
      ranges[2:num][ranges[2:num]==0]<-minflow        ## replace 0 with minimum
      
    }
    numflows <- length(labels)

    ##-----------------------------------------------------------------
    labelwidth   <- max(strwidth(labels, "inch")*sizelab, na.rm = TRUE)
    labelheight  <- strheight("M", "inch")
    plot.new()

    ##-----------------------------------------------------------------
    ## new margins
    ##-----------------------------------------------------------------

    nmar         <- par("mar")
    nmar[2]      <- nmar[4] + (labelwidth + 0.1)/labelheight
    par(mar = nmar)

    y            <- 1:numflows
    if (is.null (xlim)) xlim <- range(c(ranges$Minimum,ranges$maximum,ranges$Value))
    ylim <- c(0, numflows + 1)

    plot.window(xlim = xlim, ylim = ylim, log = log)

    loffset <- (labelwidth + 0.1)/labelheight
    labs    <- labels[y]
    mtext(labels, side = 2, line = loffset, at = y, adj = 0, 
          col = color, las = 2, cex = par("cex") * sizelab, ...)
    
    abline  (h = y, lty = "dotted", col = lcolor)
    points  (ranges$Value, y, pch = pch, col = color, bg = bg)
    if (num == 4) segments(ranges$Minimum,y,ranges$Maximum,y,col=color,lty=1)
    axis(1)
    box()
    title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
  }






