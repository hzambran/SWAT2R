# File rch2zoo.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                            rch2zoo                                   #
########################################################################
# Purpose    : Reading a 'output.rch' file with the outputs of the SWAT#
#              hydrological model, in order to extract the simulated   #
#              discharges for a given reach, and then transforming it  #
#              into a zoo object                                       #
########################################################################
# Output     : numeric, data.frame or zoo object                       #
########################################################################
# Author     : Mauricio Zambrano-Bigiarini                             #
########################################################################
# Started    : 13-Dec-2010 at JRC Ispra                                #
# Updates    : 16-Apr-2012 at JRC                                      #
########################################################################

rch2zoo <- function(file="output.rch",
                    out.type = "Q",
                    rchID=1,
                    col.name="FLOW_OUTcms",  # character with the column name in 'file' that stores the results that the user wants to convert into a zoo object
                    Date.Ini,                # character with the starting date for the results that are stored in 'file'
                    Date.Fin,                # character with the ending date for the results that are stored in 'file'
                    date.fmt="%Y-%m-%d",     # character, with the format used to define 'Date.Ini", "Date.Fin". See 'format' in 'as.Date'.
                    tstep="daily",           # character with the time step of the results stored in 'file'. Valid values are in ('daily', 'monthly', 'annual')
                    verbose = TRUE
                    ) {

  # Reading the RCH file
  rch <- read_rch(file=file, tstep=tstep, out.type = out.type, rchID=rchID, verbose = verbose)
  
  if ( !(col.name %in% colnames(rch) ) )
    stop( paste("Invalid argument '", col, "' is not a column name in '", file, "'", sep="") )
   
  # Getting only the colum(s) defined by the user
  rch <- rch[, col.name]
 
  # transforming from numeric to zoo
  if (!missing(Date.Ini) & !missing(Date.Fin)) {
    if (tstep=="daily") {
      dates <- hydroTSM::dip(Date.Ini, Date.Fin, date.fmt=date.fmt)
      } else if (tstep=="monthly") {
         dates <- hydroTSM::mip(Date.Ini, Date.Fin, date.fmt=date.fmt)
         } else if (tstep=="annual") dates <- hydroTSM::yip(Date.Ini, Date.Fin, date.fmt=date.fmt)
    rch <- hydroTSM::vector2zoo(x=rch, dates=dates)
  } # IF end

  return(rch)

} # 'rch2zoo' END
