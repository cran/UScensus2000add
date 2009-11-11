\name{demToNum}
\alias{demToNum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts Demographics to Numeric
}
\description{
Converts the demographic variables in the \code{data.frame} object in the \pkg{UScensus2000cdp},
\pkg{UScensus2000tract}, \pkg{UScensus2000blkgrp}, and \pkg{UScensus2000blk} packages to numeric  values.
}
\usage{
demToNum(sp.object = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp.object}{ \code{SpatialPolygonsDataFrame}, must be from the \pkg{UScensus2000}-suite; however this will also convert the demographics added by the \code{demographics.add} function in the \pkg{UScensus2000add} package.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Zack W. Almquist
}
%\note{
%%  ~~further notes~~
%}


\seealso{
\link[UScensus2000add:demographics.add]{demographics.add}
}
\examples{
data(alaska.tract)

###Convert demographics to numeric
alaska.tract<-demToNum(alaska.tract)

spplot(alaska.tract[,'pop2000'],ylim=c(51.78495, 71.33953),xlim=c(-176.81043, -130.0427))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


