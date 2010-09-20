\name{demographics.add}
\alias{demographics.add}
\title{
Add Census SF1 Demographic Variables to State sp Objects
}
\description{
Helper function to the census \pkg{UScensus2000}-suite of R packages. This function will download the necessary census files to attach selected demographic variables to the selected level of \code{sp} object. warning if you have limited bandwidth this function may cause problems. 
}
\usage{
demographics.add(dem = NULL, state = NULL, statefips = FALSE, level = c("tract", "blk", "blkgrp", "cdp"), census = "sf1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dem}{
Takes in a vector of one or more census variables as defined in SF1tech report (\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}). 
}
  \item{state}{
This has to be a string and can either be the full name (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you  have to change \code{statefips} to \code{TRUE}.
}
  \item{statefips}{
logical: are you providing \code{state} with a FIPS code instead of a name. By default, \code{statefips}=\code{FALSE}.
}
  \item{level}{
Takes in one of three values: "tract", "blk",  or "blkgrp". This defines the geographic level of data for the county.
}
  \item{census}{
Right now data is only available from the SF1 files, hopefully in the near future compatibility with SF3 will be added.
}
}

%\details{}
\value{
An object of class \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}.
}
\references{
Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. \cr \cr

Census 2000 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2001. \cr
\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}
}
\author{Zack W. Almquist \email{almquist@uci.edu}}
%\note{%%  ~~further notes~~}

\section{Warning }{You must have the packages \code{UScensus2000blkgrp} and \code{UScensus2000blk} installed to use levels "blkgrp" and "blk" respectively.\cr
Depending on how many demographic variables and how big the state is, this function can download 100 megabytes or more of data to perform this task.
}


%%\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{

\dontrun{
##Add the SF1 variables "P001001", "P045003"
rhode_island<-demographics.add(dem=c("P001001","P045003"),state="ri",level="tract")
names(rhode_island)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{census}
\keyword{demography}% __ONLY ONE__ keyword per line
