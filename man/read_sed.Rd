%% File read_sed.Rd
%% Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
%%                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{read_sed}
\Rdversion{1.1}
\alias{read_sed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read_sed
}
\description{
Function for reading the \sQuote{output.sed} files of SWAT 2009 and not in SWAT 2005  (regarding sediment loads), and giving as output a data.frame with the columns of the read file. \cr

The \sQuote{output.sed} file has 1 row representing the header, 4 columns with the text \sQuote{RCH}, \sQuote{GIS}, \sQuote{MON}, \sQuote{AREA}, and 19 additional colums with results regarding sediments loads on reaches.
}
\usage{
read_sed(file="output.sed", rchID=NA, col.names=NULL, tstep,                    
         Date.Ini, Date.Fin, date.fmt="\%Y-\%m-\%d", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
the name of the file which the data are to be read from. If it does not contain an _absolute_ path, the file name is _relative_ to the current working directory, 'getwd()'. Tilde-expansion is performed where supported.
}
  \item{rchID}{
OPTIONAL. Integer with the number of the reach for which the results will be provided. \cr
If this argument is not provided, the results will be given for all the reaches in \sQuote{output.sed}
}
  \item{col.names}{
OPTIONAL. character with the column name in \code{file} that stores the results that the user wants to convert into a zoo object
}
  \item{tstep}{
Time step used for the simulation that created the \sQuote{output.sed} file. \cr
It must be one of the following values: c("daily", "monthly", "annual"), 
}
  \item{Date.Ini}{
OPTIONAL, only used when \code{rchID} is provided AND it is a unique number. \cr
character with the starting date for the results that are stored in \code{file}, which is used to provide a zoo object as output.
}
  \item{Date.Fin}{
OPTIONAL, only used when \code{rchID} is provided AND it is a unique number. \cr
character with the ending date for the results that are stored in \code{file}, which is used to provide a zoo object as output.
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

\seealso{
\code{\link{read_rch}}, \code{\link{read_sub}}, \code{\link{read_hru}}, \code{\link{read_rsv}}
}
%%\examples{
%%
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
