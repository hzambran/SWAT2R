# File read_sed.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                               http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                             read_sed                                 #
########################################################################
# Purpose: Function for reading the 'output.sub' files of SWAT 2005    #
#          (subbasin outputs)                                          #
########################################################################
# Author : Mauricio Zambrano-Bigiarini, with collaboration of 'Mikhail'#
########################################################################
# Started: 24-Aug-2012                                                 #
# Updates:                                                             #
########################################################################


# Please note that I've never worked either with sediment nor water quality!!
read_sed <- function(file="output.sed", 
                     rchID=NA, 
                     col.names=NULL,          # character with the column name in 'file' that stores the results that the user wants to convert into a zoo object
                     tstep,                    
                     Date.Ini,                # character with the starting date for the results that are stored in 'file'
                     Date.Fin,                # character with the ending date for the results that are stored in 'file'
                     date.fmt="%Y-%m-%d",     # character, with the format used to define 'Date.Ini", "Date.Fin". See 'format' in 'as.Date'.
                     verbose=TRUE){

  # Checking that 'file' exists
  if ( !file.exists(file) )
    stop( paste("Invalid argument value: The file '", basename(file), "' does not exist", sep="" ) ) 

  # Checking that the user provided a valid value for 'tstep'
  if (missing(tstep)) {
        stop("Missing argument value: 'tstep' must be in c('daily','monthly','annual')") 
  } else  # Checking the validity of the 'unit' argument
         if ( is.na( match(tstep, c("daily", "monthly", "annual") ) ) ) {
            stop("Invalid argument value: 'tstep' must be in c('daily', 'monthly', 'annual')" ) }
                
 sed.names <- c( "RCH", "GIS",  "MON", "AREAkm2", 
                 "SED_INtons",  "SED_OUTtons", "SAND_INtons", "SAND_OUTtons", 
                 "SILT_INtons", "SILT_OUTtons", "CLAY_INtons", "CLAY_OUTtons",
                 "SMAG_INtons", "SMAG_OUTtons", "LAG_INtons", "LAG_OUTtons", 
                 "GRA_INtons",  "GRA_OUTtons", 
                 "CH_BNKtons",  "CH_BEDtons", "CH_DEPtons",
                 "FP_DEPtons",  "TSSmg/L")
    
  # Reading the file
  if (verbose) message("[ Reading the file '", basename(file), "' ... ]" )  
      
  # Reading ALL the outputs        
  sed <- read.fortran(file, header=FALSE, skip = 1, c("6X", "I4", "x", "I8", "x", "F4", "20F12")) 
         
  # Assigning the the complete list of names, (adding the following names to the previous list: c(""LATNO3kg/h", "GWNO3kg/ha", "CHOLAmic/L", "CBODU mg/L", "DOXQ mg/L")
  colnames(sed) <- sed.names
  
  # If the user provided a reach number, only those results will be returned to the user  
  if ( !missing(rchID) ) {
    non.int <- which(rchID != floor(rchID))
    if ( length(non.int) > 0 ) {      
      stop("Invalid argument: 'rchID' must be integer" ) 
    } else sub <- sub[sub$SUB == rchID, ]
    
    #############################
    # numeric -> zoo
    if ( (length(rchID)==1) & (!missing(Date.Ini) & !missing(Date.Fin)) ) {
      if (tstep=="daily") {
        dates <- hydroTSM::dip(Date.Ini, Date.Fin, date.fmt=date.fmt)
        } else if (tstep=="monthly") {
           dates <- hydroTSM::mip(Date.Ini, Date.Fin, date.fmt=date.fmt)
           } else if (tstep=="annual") dates <- hydroTSM::yip(Date.Ini, Date.Fin, date.fmt=date.fmt)
      sub <- zoo::zoo(sub, dates)
    } # IF end
    
  } # IF end
  
  #############################
  # Selecting only some columns
  if (!is.null(col.names)) {
    if ( any( !( col.names %in% colnames(sed) ) ) )
      stop( paste("Invalid argument '", col, "' is not a column name in '", file, "'", sep="") )
   
    # Getting only the column(s) defined by the user
    sed <- sed[, col.names]
  } # IF end
  
  return(sub)
  
} # 'read_sub' END
