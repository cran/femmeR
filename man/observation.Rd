\name{observed}
\alias{read.obs}
\alias{plot.obs}
\alias{points.obs}
\title{Observed data files}
\description{
}
\usage{
read.obs(obsfile)

plot.obs(x ,xvari="time",yvari=NULL,rev="",type="b",pch=16,time=NULL,main=x$filename,err="l",errpch=1,
                       xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,...)

points.obs(x ,xvari="time",yvari=1,rev="",...)
}
\arguments{
  \item{obsfile}{File ending in .obs}
  \item{x}{object of class obs created by \code{read.obs}}
  \item{xvari}{Index of X Variable}
  \item{yvari}{Index of Y Variable}
  \item{rev}{Reversion of axis}
  \item{main}{Main title}
  \item{xlab}{X axis label}
  \item{ylab}{Y axis label}
  \item{type}{Plot type}
  \item{pch}{Plot symbol}
  \item{time}{????????????}
  \item{err}{?????????????}
  \item{xlim}{Limits of the X axis}
  \item{ylim}{Limits of the Y axis}
  \item{errpch}{Plotting character}
  \item{...}{Additional plot parameters}
}
\value{
}
\details{
}
\references{
}
\seealso{
}
\examples{

}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}
\keyword{hplot}