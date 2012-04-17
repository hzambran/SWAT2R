# File read_95ppu.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                           read_95ppu                                 #
########################################################################
# Purpose    : This function reads the SWAT-CUP file '95ppu.sf2', which#
#              contains the 95% of predictive uncertainty corresponding#
#              to the behavioural parameter sets, and optionally, plots#
#              the corresponding uncertainty bounds                    #
#              Fields in '95ppu.sf2' are :                             #
#                      observed   L95PPU    U95PPU   Best_Sim   M95PPU #
#              SWAT-CUP at http://www.eawag.ch/organisation/abteilungen/siam/software/swat/index_EN      #
########################################################################
# Output     : numeric, data.frame or zoo object                       #
########################################################################
# Author     : Mauricio Zambrano-Bigiarini                             #
########################################################################
# Started    : 28-Jun-2010                                             #
# Updates    : 16-Apr-2012 at JRC                                      #
########################################################################

# file        : character with the name of the file which the data are to be read from. \cr
#               Valid values are in c('95ppu.sf2', '95ppu.out')
#               If it does not contain an absolute path, the file name is relative 
#               to the current working directory, getwd(). Tilde-expansion is performed where supported. As from R  2.10.0 this can be a compressed file (see file). 
# ua.algorithm: OPTIONAL, only required when 'file' is  missing.
#               character, indicating  the SWAT-CUP algorithm that created 'file'.
#               Valid values are in c('GLUE', 'SUFI2')
# plot        : logical, indicating if the 95PPU and the observed vaues have to be plotted or not
# dates       : Dates for the correponding values in the 'sim' and 'obs' time series
#               If 'dates' is a factor, it have to be converted into 'Date' class, 
#               using the date format  specified by 'date.fmt'
#               If 'dates' is already of Date class, the number of dates
#               must be equal to the number of elements in 'sim' and 'obs'
# 'date.fmt'  : character indicating the format in which the dates entered are stored in  'dates' \cr
#               Default value is "\%Y-\%m-\%d"
# 'gof.leg'   : logical, indicating if the p-factor and r-factor have to be
#               computed and ploted as legends on the graph.
# 'gof.digits': OPTIONAL, numeric. Only used when 'gof.leg=TRUE'. 
#               Decimal places used for rounding p-factor and r-factor
# main        : character with an overall title for the plot: see 'title'. 
# ylab        : character with a title for the y axis: see 'title'.
# ...         : additional argument to be passed to the 'plotbands' function
                       
read_95ppu <- function(file, 
                       ua.algorithm, 
                       plot=TRUE, 
                       dates, 
                       date.fmt="%Y-%m-%d", 
                       gof.leg= TRUE, 
                       gof.digits=2, 
                       main="95PPU Plots", 
                       ylab="Q, [m3/s]", 
                       verbose=TRUE,
                       ... ) {

  if ( missing(file) ) {
    
    # Checking 'ua.algorithm'  
    if ( missing(ua.algorithm) ) {
      stop( "Missing argument: 'file'or 'ua.algorithm' must be provided" )
    } else  # Checking that the user provied a valid argument for 'ua.algorithm'       
        if (is.na(match(ua.algorithm, c("GLUE", "SUFI2") ) ) ) {
          stop("Invalid argument: 'ua.algorithm' must be in c('GLUE', 'SUFI2')") 
        } # IF end
        else ifelse ( ua.algorithm == "SUFI2", file<-"95ppu.sf2", file<-"95ppu.out")          
  } # IF end
  
  # Checking 'file'
  if ( is.na(match(file, c("95ppu.sf2", "95ppu.out") ) ) )
          stop("Invalid argument: 'file' must be in c('95ppu.sf2', '95ppu.out')") 
  
  # Checking that 'file' exists
  if ( !file.exists(file) )
    stop( paste("Invalid argument value: The file '", basename(file), "' doesn't exist", sep="" ) )
  
  # # Reading ALL the PARAMETER SETS got with SUFI-2
  if (verbose) print( paste("[Reading the file '", basename(file), "' ...]", sep="" ), quote=FALSE  )  
  q95 <- read.table(file, header=TRUE, skip=1, fill=TRUE, stringsAsFactors=FALSE)
  
  # The '95ppu.sf2' file at the end has a blank line followed byt he next four lines:
  
  # p-factor= X.XX
  # d_factor= X.XX
  # R2= X.XX
  # Nash_Sutclif= X.XX
  q95 <- q95[1:(nrow(q95)-4), ]
  
  # Converting the character vector into a numeric one
  q95$observed <- as.numeric(q95$observed)  
  
  # If the user wants to plot the results
  if (plot) {
      
      # plotting 95PPU
      hydroGOF::plotbands(x=q95$observed, lband=q95$L95PPU, uband=q95$U95PPU, 
                          dates=dates, date.fmt=date.fmt, gof.leg=gof.leg, 
                          gof.digits=gof.digits, main=main, ylab=ylab, ...)  
  
  } # IF end  
  
  return(q95)
  
}  # 'read_95ppu' END
