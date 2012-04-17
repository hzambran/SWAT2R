# File read_rch.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                           read_rch                                   #
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
# Started    : 25-Feb-2009 at UT                                       #
# Updates    : 02-Oct-2009 at UT                                       #
#              22-Jan-2011 at JRC                                      #
#              16-Apr-2012 at JRC                                      #
########################################################################

# This file has 9 rows representing the header, and 1 colum with the 'REACH' text and
# 43 additional colums with results regarding water quantity, sediments, and water quality
# 

# tstep     : time step used for printing the results in the 'output.rch' file.
#			  Valid values are: "daily", "monthly", "annual"

# 'file'    : the name of the file which the data are to be read from.
#             If it does not contain an _absolute_ path, the file name is
#             _relative_ to the current working directory, 'getwd()'.
#             Tilde-expansion is performed where supported.
          
# 'tstep'   : Time step used for the simulation that created the 'output.rch' file.
#             Must be one of the following values: c("daily", "monthly", "annual"), 
#             stands for daily, monthly and annual times steps
# 'out.type': Type of results that have to be read
#             Must be one of the following values: 
#             -) "Q"       : only results related to water quantity are read (first 8 columns)
#                            c("RCH", "GIS", "MON", "DrAREAkm2", 
#                              "FLOW_INcms", "FLOW_OUTcms", "EVAPcms", "TLOSScms")
#             -) "Q+Sed"   : only results related to water quantity AND sediments are read (first 11 columns)
#                            c("RCH", "GIS", "MON", "DrAREAkm2", "FLOW_INcms", "FLOW_OUTcms", 
#                              "EVAPcms", "TLOSScms", "SED_INtons", "SED_OUTtons", "SEDCONCmg/kg")
#             -) "Q+Sed+WQ": all the columns of the 'output.rch' are read 
# 'rchID'   : OPTIONAL. Integer with the number of the reach for wich the results will be provided.
#             If this argument is not provided, the results will be given for all the reaches in 'output.rch'

# Please note that I never worked with sediment nor water quality!!
read_rch <- function(file="output.rch", 
                     out.type="Q", 
                     rchID=NA, 
                     col.names=NULL, # character with the column name in 'file' that stores the results that the user wants to convert into a zoo object
                     tstep,                    
                     Date.Ini,                # character with the starting date for the results that are stored in 'file'
                     Date.Fin,                # character with the ending date for the results that are stored in 'file'
                     date.fmt="%Y-%m-%d",     # character, with the format used to define 'Date.Ini", "Date.Fin". See 'format' in 'as.Date'.
                     verbose=TRUE){

  # Checking that 'file' exists
  if ( !file.exists(file) )
    stop( paste("Invalid argument value: The file '", basename(file), "' doesn't exist", sep="" ) )

  # Checking that the user provided a valid value for 'tstep'
  if (missing(tstep)) {
        stop("Missing argument value: 'tstep' must be in c('daily','monthly','annual')") 
  } else  # Checking the validity of the 'unit' argument
         if ( is.na( match(tstep, c("daily", "monthly", "annual") ) ) ) {
            stop("Invalid argument value: 'tstep' must be in c('daily', 'monthly', 'annual')" ) }
  
  # Checking that the user provided a valid value for 'out.type'   -- 
  if ( is.na( match(out.type, c("Q", "Q+Sed", "Q+Sed+WQ") ) ) ) {
        stop("Invalid argument value: 'out.type' must be in c('Q', 'Q+Sed', 'Q+Sed+WQ')" ) }
  
  rch.names <- c("RCH", "GIS", "MON", 
                 "DrAREAkm2",  "FLOW_INcms",  "FLOW_OUTcms", "EVAPcms", "TLOSScms", 
                 "SED_INtons", "SED_OUTtons", "SEDCONCmg/kg",
                 "ORGN_INkg", "ORGN_OUTkg", "ORGP_INkg", "ORGP_OUTkg", "NO3_INkg",
                 "NO3_OUTkg", "NH4_INkg", "NH4_OUTkg", "NO2_INkg", "NO2_OUTkg",
                 "MINP_INkg", "MINP_OUTkg", "Algae_INkg", "Algae_OUTkg", "CBOD_INkg",
                 "CBOD_OUTkg", "DISOX_INkg", "DISOX_OUTkg", "SOLPST_INmg",
                 "SOLPST_OUTmg", "SORPST_INmg", "SORPST_OUTmg", "REACTPSTmg",
                 "VOLPSTmg", "SETTLPSTmg", "RESUSP_PSTmg", "DIFFUSEPSTmg",
                 "REACBEDPSTmg", "BURYPSTmg", "BED_PSTmg", "BACTP_OUTct",
                 "BACTLP_OUTct", "CMETAL#1kg", "CMETAL#2kg", "CMETAL#3kg")
                   
  #rch.widths <- c(6,
  #                4,9,6,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
  #               12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
  #               12,12,12,12)
  
  # Reading the output file of the simulation
  if (verbose) 
    message( paste("[Reading the file '", basename(file), "' ...]", sep="" )  )  
    
  if (out.type=="Q") {
    # Reading only the 8 variables related to water quantity. 
    # 6x  : "REACH", 
    # I4  : RCH; 
    # I9  : GIS; 
    # F6  : MON (not always is an integer !!)
    # 5F12: "DrAREAkm2", "FLOW_INcms", "FLOW_OUTcms", "EVAPcms", "TLOSScms"
    rch <- read.fortran(file, header=FALSE, skip=9, c("6X","I4", "I9", "F6", "5F12", "38X"))    
    
    # Assigning the names c("RCH", "GIS", "MON", "DrAREAkm2", "FLOW_INcms", "FLOW_OUTcms", "EVAPcms", "TLOSScms")
    colnames(rch) <- rch.names[1:8]
    
  } else if (out.type=="Q+Sed") {
  
      # Reading only the 11 variables related to water quantity and sediments
      rch <- read.fortran(file, header=FALSE, skip=9, c("6X","I4", "I9", "F6", "8F12", "35X"))  
        
      # Assigning the names c("RCH", "GIS", "MON", "DrAREAkm2", "FLOW_INcms", "FLOW_OUTcms", "EVAPcms", "TLOSScms", "SED_INtons", "SED_OUTtons", "SEDCONCmg/kg")
      colnames(rch) <- rch.names[1:11]
      
    } else if (out.type=="Q+Sed+WQ") {
    
        # Reading ALL the outputs 
        rch <- read.fortran(file, header=FALSE, skip=9, c("6X","I4", "I9", "F6", "43F12"))    
        # Assigning the names c("RCH", "GIS", "MON", "DrAREAkm2", "FLOW_INcms", "FLOW_OUTcms", "EVAPcms", "TLOSScms", "SED_INtons", "SED_OUTtons", "SEDCONCmg/kg")
        colnames(rch) <- rch.names
        
  } # ELSE end
  
  # When a monthly time step was used for printing the results in 'output.rch',
  # after writing the 12 months values for all the subcatchments and for each year,
  # a mean annual value for each subcatchment is printed, using the year (e.g., 1961)
  # as the 'MON' field value (instead of the number corresponding to the month (1,2,...,12) ). 
  
  # Also, after writing the mean annual value for the last cahtchment, a mean value 
  # for the complete period is printed for each subcatchment, using the numer of simulated years (e.g. 11.0)
  # as the 'MON' field value (instead of the number corresponding to the month (1,2,...,12) ). 
  
  # The next line ONLY filters out the mean annual values for all the subcatchments
  if (tstep == "monthly") {
    rch <- rch[rch$MON %in% 1:12, ] }
    
  # When a yearly time step was used for printing the results in 'output.rch',
  # at the end of the file a mean value for all the period simulated and each 
  # subcatchment is printed, using the amount of years simulated (e.g., 25.0)
  # as the 'MON' field value (instead of the number corresponding to the year (1961, 1961...,1985) ). 
  # The next line is for filtering out these mean values for all the subcatchments 
  # (assuming that all the simulation will start after the year 1900 and that nobody sill simulate 1900 years !)
  if (tstep == "annual") {
    rch <- rch[rch$MON > 1900, ] }  
  
  # If the user provided a reach numer, only those results will be returned to the user  
  if ( !missing(rchID) ) {
    non.int <- which(rchID != floor(rchID))
    if ( length(non.int) > 0 ) {      
      stop("Invalid argument: 'rchID' must be integer" ) 
    } else rch <- rch[rch$RCH == rchID, ]
  } # IF end
  
  #############################
  # Selecting only some columns
  if (!is.null(col.names)) {
    if ( any( !( col.names %in% colnames(rch) ) ) )
      stop( paste("Invalid argument '", col, "' is not a column name in '", file, "'", sep="") )
   
    # Getting only the colum(s) defined by the user
    rch <- rch[, col.names]
  } # IF end
 
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
  
} # 'read_rch' END
