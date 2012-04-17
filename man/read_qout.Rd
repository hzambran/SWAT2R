%% File read_qout.Rd
%% Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
%%                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{read_qout}
\Rdversion{1.1}
\alias{read_qout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read_qout
}
\description{
This function reads the file with the simulated streamflows for the parameter sets obtained with SWAT-CUP 
}
\usage{
read_qout(file, ua.algorithm, nsim,  nobs, verbose=TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character with the name of the file which the data are to be read from. Valid values are in \code{c('95ppu.sf2', '95ppu.out')}. If it does not contain an absolute path, the file name is relative to the current working directory, getwd()
}
  \item{ua.algorithm}{
OPTIONAL, only required when \code{file} is  missing. \cr
character, indicating the SWAT-CUP algorithm that created \code{file}. Valid values are: \cr
-) \kbd{GLUE} : \code{file} is set to \sQuote{modelres.out}, with the streamflows of ALL the parameter sets, stored by rows \cr
-) \kbd{SUFI2}: \code{file} is set to \sQuote{q\_1.out}, with the streamflows of ALL the parameter sets, stored by rows \cr
-) \kbd{PSO}  : \code{file} is set to \sQuote{q1\_beh.out}, with the streamflows corresponding to the BEHAVIOURAL parameter sets only, stored by rows  
}
  \item{nsim}{
OPTIONAL. Only required when \code{ua.algorithm} is in \code{'GLUE', 'SUFI2', 'PSO'} \cr
numeric, with the number of simulations specified in the file \sQuote{par_inf.sf2}, located in teh folder \sQuote{SUFI2.IN} (e.g.: Number_of_LH_sims=2000)
}
  \item{nobs}{
OPTIONAL. Only required when \code{ua.algorithm} is in \code{'SUFI2', 'PSO'} \cr
number of observation specified in the file \sQuote{observed.sf2}, located in the folder \sQuote{SUFI2.IN} (e.g., NO_Data= 6935)
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
\references{
SWAT-CUP at \url{http://www.eawag.ch/organisation/abteilungen/siam/software/swat/index_EN}
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read_sub}}, \code{\link{read_hru}}, \code{\link{read_rsv}}, \code{\link{read_95ppu}}
}
%%\examples{
%%
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
