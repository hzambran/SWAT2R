%% File read_95ppu.Rd
%% Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
%%                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{read_95ppu}
\Rdversion{1.1}
\alias{read_95ppu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read_95ppu
}
\description{
This function reads the SWAT-CUP file \sQuote{95ppu.sf2}, which contains the 95\% of predictive uncertainty corresponding to the behavioural parameter sets, and optionally, plots the corresponding uncertainty bounds. Fields in '95ppu.sf2' are : observed   L95PPU    U95PPU   Best_Sim   M95PPU
}
\usage{
read_95ppu(file, ua.algorithm, plot=TRUE, dates, date.fmt="\%Y-\%m-\%d", 
           gof.leg= TRUE, gof.digits=2, main="95PPU Plots", ylab="Q, [m3/s]", 
           verbose=TRUE,...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character with the name of the file which the data are to be read from. Valid values are in \code{c('95ppu.sf2', '95ppu.out')}. If it does not contain an absolute path, the file name is relative to the current working directory, getwd()
}
  \item{ua.algorithm}{
OPTIONAL, only required when \code{file} is  missing. \cr
character, indicating the SWAT-CUP algorithm that created \code{file}. Valid values are in \code{c('GLUE', 'SUFI2')}
}
  \item{plot}{
logical, indicating if the 95PPU and the observed values have to be plotted or not
}
  \item{dates}{
Dates for the correponding values in the 'sim' and 'obs' time series. \cr
If \code{dates} is a factor, it have to be converted into \sQuote{Date} class, using the date format  specified by \code{date.fmt}. \cr
If \code{dates} is already of Date class, the number of dates must be equal to the number of elements in the simulated and obsserved values
}
  \item{date.fmt}{
character, with the format used to define \code{Date.Ini}, \code{Date.Fin}. See 'format' in 'as.Date'. Default value is "\%Y-\%m-\%d"
}
  \item{gof.leg}{
logical, indicating if the p-factor and r-factor have to be computed and ploted as legends on the graph.
}
  \item{gof.digits}{
 OPTIONAL, numeric. Only used when \code{gof.leg=TRUE}. Decimal places used for rounding p-factor and r-factor
}
  \item{main}{
character with an overall title for the plot: see 'title'. 
}
  \item{ylab}{
character with a title for the y axis: see 'title'.
}
  \item{verbose}{
logical; if TRUE, progress messages are printed
}
  \item{\dots}{
additional argument to be passed to the \code{\link[hydroGOF]{plotbands}} function
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
\code{\link{read_sub}}, \code{\link{read_hru}}, \code{\link{read_rsv}}, \code{\link[hydroGOF]{plotbands}}
}
%%\examples{
%%
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
