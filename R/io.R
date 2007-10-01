###---c:/home/henrika/R/femmeR/R/io.R---
###------------------------------------------------------------------
### Author: Henrik Andersson <h.andersson@nioo.knaw.nl>
### Creation Date: 2005/06/06 11:23:30
### Title: Basic Input/Output FEMME
###------------------------------------------------------------------
### Provides: write.par
###           read.par
###           print.parameters
###           read.clb
###           print.calibration
###           read.avg
###
###------------------------------------------------------------------


###==================================================================
### Parameter files *.par: Do Not Write to parameters.dec and the like
###==================================================================

 write.par <- function(parameters,file="",header="Parameter values",ask=TRUE){
   if(ask&file!=""&file.exists(file)){
     cat("Are you sure you want to overwrite this file? (y/N) \n")
     answer <- readline()
     if(!answer%in%c("y","Y")) {cat("File exists, and you wish to keep it...\n")
                           return(invisible())}
     
   }
     
   if(is.null(attr(parameters,"header"))){
     cat(file=file,header,"\n")
   }
   else{
     cat(file=file,attr(parameters,"header"),sep="\n")
   }

  cat(file=file,"##############\n",append=TRUE)
  for(i in seq(along=parameters)){
    cat(file=file,
        paste(names(parameters)[i]," = ",parameters[i],"\n"),
        append=TRUE)
  }
}


read.par <- function(parfile){
  ## Read all lines
  lines <- readLines(parfile)

  ## Find #### line
  hashline <- grep("##",lines)

  ## Find empty lines
  spacelines <- grep("^ *$",lines)

  ## Remove empty lines
  parlines <- lines[-union(spacelines,1:hashline)]

  ## Remove spaces in start of lines
  parlines2 <- lapply(parlines,function(x) gsub("^ +","",x))
  
  ## Initialise result list
  parameters <- list(length(parlines))

  
    for(i in seq(along=parlines)){
      
      ## Split at multiple spaces
      temp <- strsplit(parlines2[[i]]," +")

      name <- temp[[1]][1]
      value <- temp[[1]][3]
      parameters[[i]] <- value
      names(parameters)[[i]] <- name
  }

  fullheader <- lines[1:(hashline-1)]
  spaceheader <- grep("^ *$",fullheader)
  header <- fullheader[-spaceheader]
  attr(parameters,"header") <- header
  class(parameters) <- "parameters"
return(parameters)
}


print.parameters <- function(x,...){
  cat(file="",attr(x,"header"),sep="\n",...)
  cat(file="","##############\n",...)
  for(i in seq(along=x)){
    cat(file="",
        paste(names(x)[i]," = ",x[i]),sep="\n",...)
          }
}

read.clb <- function(clbfile){
  ## Read all lines
  lines <- readLines(clbfile)

  ## Find best values line
  bestline <- grep("Best value",lines)

  ## Find empty line after bestline
  spacelines <- grep("^ *$",lines)
  endline <- spacelines[spacelines>bestline][1]

  parlines <- lines[(bestline+1):(endline-1)]
   ## Initialise result list
  parameters <- list()

  
    for(i in seq(along=parlines)){
      
      ## Split at multiple spaces
      temp <- strsplit(parlines[[i]]," +")

      name <- temp[[1]][1]
      value <- as.numeric(temp[[1]][2])
      parameters[[i]] <- value
      names(parameters)[[i]] <- name
  }


  class(parameters) <- "calibration"
  return(parameters)
}

print.calibration <- function(x,...){
  cat(file="","Best values\n",...)
  for(i in seq(along=x)){
    cat(file="",
        paste(names(x)[i],":",x[i]),sep="\n",...)
          }
}

read.avg <- function(avgfile)

{

  lines <- readLines(avgfile)

  hashlines <- grep("##", lines)

  spacelines <- grep("^ $",lines)

  avgnames <- c()

  units <- c()

  data <- c()

  for (i in 1:(length(hashlines)))

    {

      avgnames <- c(avgnames, scan(file = avgfile, skip = hashlines[[i]] - 3, quiet = TRUE, what = "char", nlines = 1))

      units    <- c(units, scan(file = avgfile, skip = hashlines[[i]] - 2, quiet = TRUE, what = "char", nlines = 1))

      data   <- c(data, read.table(avgfile, skip = hashlines[[i]], nrows = (spacelines[spacelines > hashlines[[i]]][1] - (hashlines[[i]] + 1))))

    }

  names(data) <- avgnames

  filename <- basename(avgfile)

  seq <- seq(-1, length(data), by = 4)

  seq[1] <- 1

  seqnames <- substr(avgnames[seq], 5, 50)

  seqnames[1] <- avgnames[1]

  res <- list(names = seqnames, seq = seq, vars = avgnames, units = units, data = data, filename = filename)

  class(res) <- "avg"

  return(res)

}

