%% File read_autocal.Rd
%% Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
%%                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{read_autocal}
\Rdversion{1.1}
\alias{read_autocal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read_autocal
}
\description{
Function for reading the \sQuote{autocalX.out} resulting after the auto-calibration process in SWAT 2005, and giving as output a data.frame with the columns of the read file. \cr
}
\usage{
read_autocal(file, tstep, out.type="Q", outID=NA, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
the name of the file which the data are to be read from. If it does not contain an _absolute_ path, the file name is _relative_ to the current working directory, 'getwd()'. Tilde-expansion is performed where supported.
}
  \item{tstep}{
Time step used for the simulation that created the 'autocalX.out' file. \cr
It must be one of the following values: c("daily", "monthly", "annual"), 
}
  \item{out.type}{
Type of results that have to be read. It must be one of the following values: \cr
-) "Q"       : only results related to water quantity are read (first 3 columns): c("YEAR", "DAY", "FLOWcms") \cr
-) "Q+Sed"   : only results related to water quantity AND sediments are read (first 4 columns): The previously mentioned 3 along with c("SEDCONCmg/kg") \cr
-) "Q+Sed+WQ": all the columns of the 'autocalX.out' are read 
}
  \item{outID}{
Currently not used....
%%OPTIONAL. Integer with the number of the reach for wich the results will be provided. \cr
%%If this argument is not provided, the results will be given for all the reaches in 'autocalX.out'
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
\note{
\bold{This function has not been exhaustively tested}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
%%\examples{
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
