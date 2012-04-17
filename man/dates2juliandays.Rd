%% File dates2juliandays.Rd
%% Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
%%                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{dates2juliandays}
\Rdversion{1.1}
\alias{dates2juliandays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dates2juliandays
}
\description{
This function converts a vector of days into a vector of Julian days, but in the format required for SWAT hydrological model, in the sense that each julian day goes only from 1 to 365/366, so, every year the origin for counting the julian day is set to the 31-Dec of the previous year, in a way that the 01-January takes 1 as its Julian day
}
\usage{
dates2juliandays(dates, verbose)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dates}{
vector containing the dates that will be converted into Julian days in SWAT format
}
  \item{verbose}{
logical; if TRUE, progress messages are printed
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
%%\references{
%% ~put references to the literature/web site here ~
%%}
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

%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }
