# File dates2juliandays.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                         dates2juliandays                             #
########################################################################
# Purpose    : This function converts a vector of days into a vector of#
#              Julian days, but in the format required for SWAT        #
#              hydrological model, in the sense that each julian day   #
#              goes only from 1 to 365/366, so, every year the origin  #
#              for counting the julian day is set to the 31-Dec of the #
#              previous year, in a way that the 01-January takes 1 as  #
#              its Julian day                                          #
########################################################################
# Output     : numeric, data.frame or zoo object                       #
########################################################################
# Author     : Mauricio Zambrano-Bigiarini                             #
########################################################################
# Started    : 16-Feb-2009                                             #
# Updates    : 02-Oct-2009                                             #
#              16-Apr-2012 at JRC                                      #
########################################################################

# 

# 'dates' : vector containing the dates that will be converted into Julian days in SWAT format
dates2juliandays <- function(dates, verbose=FALSE) {
  
   # Checking that the user provied a valid argument for 'x'
  if ( is.na( match( class(dates), c("Date") ) ) ) 
      stop("Invalid argument: 'dates' must be of class 'Date'")
      
  jdays <- numeric(length(dates))

  if (verbose) pbar <- txtProgressBar(min = 0, max = length(dates), style = 3)

  jdays[1:length(dates)] <- sapply(1:length(dates), function(j,x) {
                             if (verbose) setTxtProgressBar(pbar, j)
                             lastyear <- as.numeric(format(x[j], "%Y")) - 1
                             jdays[j] <- julian( as.Date(x[j]), origin=as.Date( paste(lastyear, "-12-31", sep="") ) )
                             }, x = dates)
   # clossing the progress bar
  if (verbose) close(pbar)

  return(jdays)

} # 'dates2juliandays' END
