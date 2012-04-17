%% File obs2swatcup.Rd
%% Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
%%                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{obs2swatcup}
\Rdversion{1.1}
\alias{obs2swatcup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
obs2swatcup
}
\description{
This function generates a file with observed values of streamflows for being used as input to SWAT-CUP (\url{http://www.eawag.ch/organisation/abteilungen/siam/software/swat/index_EN})
}
\usage{
obs2swatcup(x, tstep="daily", from, to, date.fmt="\%Y-\%m-\%d", fname="Observations", dates, version=2, var.id="q_1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo object with flow measurement(s) in 'm3/s'. It could be 'daily' or 'monthly'
}
  \item{tstep}{
time step in which are stored the values of 'x'. Valid values are: 'daily, 'monthly'
}
  \item{from}{
Character indicating the starting date for the values stored in all the files that will be read. It HAs to be in the format indicated by 'date.fmt'
}
  \item{to}{
Character indicating the starting date for the values stored in all the files that will be read. It HAs to be in the format indicated by 'date.fmt'
}
  \item{date.fmt}{
Character indicating the date format in which you provide 'from' and 'to', e.g. "%d-%m-%Y"
}
  \item{fname}{
Filename (without extension) of the output file. The number of observations will be added at the end of the filename
}
  \item{dates}{
OPTIONAL (not implmented yet). Vector with the dates to be used for converting a raw 'x' values into a 'zoo' object
}
  \item{version}{
integer indicating the version of SWAT-CUP for which the file has to be written
}
  \item{var.id}{
OPTIONAL. Only needed when 'version==3'. \cr
Character that have to be added as prefix before the year and day/month as second column in the observation file
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
\references{
\url{http://www.eawag.ch/organisation/abteilungen/siam/software/swat/downloads/Manual_SwatCup.pdf}
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
%%\examples{
%%
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
