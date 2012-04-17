%% File read_rsv.Rd
%% Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
%%                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{read_rsv}
\Rdversion{1.1}
\alias{read_rsv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read_rsv
}
\description{
Function for reading the 'output.rsv' files of SWAT 2005 (regarding reservoirs), and giving as output a data.frame with the columns of the read file. \cr
}
\usage{
read_rsv(file="output.rsv", out.type="Q", rsvID=NA, col.names=NULL, tstep,                    
         Date.Ini, Date.Fin, date.fmt="\%Y-\%m-\%d", verbose=TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
the name of the file which the data are to be read from. If it does not contain an _absolute_ path, the file name is _relative_ to the current working directory, 'getwd()'. Tilde-expansion is performed where supported.
}
  \item{out.type}{
Type of results that have to be read. It must be one of the following values: \cr
-) "Q"       : only results related to water quantity are read (first 8 columns): c("RES", "MON", "VOLUMEm3", "FLOW_INcms", "FLOW_OUTcms", "PRECIPm3", "EVAPm3", "SEEPAGEm3") \cr
-) "Q+Sed"   : only results related to water quantity AND sediments are read (first 11 columns): The previously mentioned 8 along with c("SED_INtons", "SED_OUTtons", "SED_CONCppm") \cr
-) "Q+Sed+WQ": all the columns of the 'output.rsv' are read 
}
  \item{rsvID}{
OPTIONAL. Integer with the number of the reservoir for wich the results will be provided. \cr
If this argument is not provided, the results will be given for all the reaches in 'output.rsv'
}
  \item{col.names}{
OPTIONAL. character with the column name in \code{file} that stores the results that the user wants to convert into a zoo object
}
  \item{tstep}{
Time step used for the simulation that created the \sQuote{output.rsv} file. \cr
It must be one of the following values: c("daily", "monthly", "annual"), 
}
  \item{Date.Ini}{
OPTIONAL. character with the starting date for the results that are stored in \code{file}
}
  \item{Date.Fin}{
OPTIONAL. character with the ending date for the results that are stored in \code{file}
}
  \item{date.fmt}{
character, with the format used to define \code{Date.Ini}, \code{Date.Fin}. See 'format' in 'as.Date'.
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
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
