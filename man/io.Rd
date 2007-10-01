\name{Input/Output}
\alias{write.par}
\alias{read.par}
\alias{read.clb}
\alias{print.parameters}
\alias{print.calibration}
\alias{read.avg}


\title{File handling in femmeR}
\description{
Running FEMME application from inside the R environment is facilitated
with these functions
}
\usage{
## Do not overwrite the model-default parameters.dec file, only files
## specific to the particular run

write.par(parameters,file="",header="Parameter values",ask=TRUE)

read.par(parfile)

read.clb(clbfile)

read.avg(avgfile)


}
\arguments{
  \item{parameters}{Names list of parameters with parameter values}
  \item{file}{Filename to be write to}
  \item{parfile}{Filename to read parameters from}
  \item{clbfile}{Filename to read calibration from}
  \item{avgfile}{Filename to read averages from}
  \item{header}{Description of the parameter file}
  \item{ask}{Logical indicating whether to ask user before overwriting
    an existing file}
}
\references{Soetaert et al, 2002}
\author{Henrik Andersson <h.andersson@nioo.knaw.nl>, Andreas Hofmann <a.hofmann@nioo.knaw.nl>}
\note{Use at your own risk}
\examples{
## For examples see: vignette("femmeR")
}
\keyword{file}
\keyword{misc}


