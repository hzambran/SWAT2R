# File read_rsv.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                             read_rsv                                 #
########################################################################
# Purpose: Function for reading the 'output.rsv' files of SWAT 2005    #
#          (reservoir outputs)                                         #
########################################################################
# Author : Mauricio Zambrano-Bigiarini                                 #
########################################################################
# Started: Apr-2009;                                                   #
# Updates: 02-Oct-2009 ; 22-Jan-2011                                   #
#          16-Apr-2012 ; 08-Aug-2012 ; 09-Aug-2012                     #
########################################################################

# 

# 'drty': Directory where is located the file that have to be read
# 'tstep'  : Time step used for the simulation that created the 'output.rsv' file.
#            Must be one of the following values: c("daily", "monthly", "annual"), 
#            stands for daily, monthly and annual times steps
# 'out.type': Type of results that have to be read
#                Must be one of the following values: 
#               -) "Q"       : only results related to water quantity are read (first 8 columns)
#                              c("RES", "MON", 
#                                "VOLUMEm3", "FLOW_INcms", "FLOW_OUTcms", "PRECIPm3", "EVAPm3",
#                                "SEEPAGEm3")
#               -) "Q+Sed"   : only results related to water quantity AND sediments are read (first 11 columns)
#                               c("RES", "MON", 
#                                "VOLUMEm3", "FLOW_INcms", "FLOW_OUTcms", "PRECIPm3", "EVAPm3",
#                                "SEEPAGEm3", 
#                                "SED_INtons", "SED_OUTtons", "SED_CONCppm" )
#               -) "Q+Sed+WQ": all the columns of the 'output.rsv' are read 
# 'rchID'      : OPTIONAL. Integer with the number of the reach for wich the results will be provided.
#                If this argument is not provided, the results will be given for all the reaches in 'output.rch'

read_rsv <- function(file="output.rsv", 
                     out.type="Q", 
                     rsvID=NA, 
                     col.names=NULL,          # character with the column name in 'file' that stores the results that the user wants to convert into a zoo object
                     tstep,                    
                     Date.Ini,                # character with the starting date for the results that are stored in 'file'
                     Date.Fin,                # character with the ending date for the results that are stored in 'file'
                     date.fmt="%Y-%m-%d",     # character, with the format used to define 'Date.Ini", "Date.Fin". See 'format' in 'as.Date'.
                     verbose=TRUE
                     ){
 
  # Checking that 'file' exists
  if ( !file.exists(file) )
    stop( paste("Invalid argument value: The file '", basename(file), "' doesn't exist", sep="" ) )
         
  # Checking that the user provided a valid value for 'tstep'
  if (missing(tstep)) {
        stop("Missing argument value: 'tstep' must be in c('daily','monthly','annual')") 
  } else  # Checking the validity of the 'unit' argument
         if ( is.na( match(tstep, c("daily", "monthly", "annual") ) ) ) {
            stop("Invalid argument value: 'tstep' must be in c('daily', 'monthly', 'annual')" ) }
            
  # Checking that the user provided a valid value for 'out.type'    
  if ( is.na( match(out.type, c("Q", "Q+Sed", "Q+Sed+WQ") ) ) ) {
        stop("Invalid argument value: 'out.type' must be in c('Q', 'Q+Sed', 'Q+Sed+WQ')" ) }
 
  rsv.names <- #c("TYPE", : It corresponds to the column without title
               c("RES", "MON", 
                 "VOLUMEm3", "FLOW_INcms", "FLOW_OUTcms", "PRECIPm3", "EVAPm3",
                 "SEEPAGEm3", 
                 
                 "SED_INtons", "SED_OUTtons", "SED_CONCppm", 
                 
                 "ORGN_INkg", "ORGN_OUTkg", "RES_ORGNppm", "ORGP_INkg", "ORGP_OUTkg", 
                 "RES_ORGPppm", "NO3_INkg", "NO3_OUTkg", "RES_NO3ppm", "NO2_INkg", 
                 "NO2_OUTkg", "RES_NO2ppm", "NH3_INkg", "NH3_OUTkg", "RES_NH3ppm", 
                 "MINP_INkg", "MINP_OUTkg", "RES_MINPppm", "CHLA_INkg", "CHLA_OUTkg", 
                 "SECCHIDEPTHm", "PEST_INmg", "REACTPSTmg", "VOLPSTmg", "SETTLPSTmg", 
                 "RESUSP_PSTmg", "DIFFUSEPSTmg", "REACBEDPSTmg", "BURYPSTmg", "PEST_OUTmg", 
                 "PSTCNCWmg/m3", "PSTCNCBmg/m3")             
                                         
  #rsv.widths <- c(6,
                  #8,5,
                  #12,12,12,12,12,
                  #12,
                  
                  #12,12,12,
                  
                  #12,12,12,12,12,
                  #12,12,12,12,12,
                  #12,12,12,12,12,
                  #12,12,12,12,12,
                  #12,12,12,12,12,
                  #12,12,12,12,12,
                  #12,12)
    
  
  # Reading the output file of the simulation
  #rsv <- read.fwf(fname, widths= rsv.widths, header=FALSE, skip=9, sep = "\t")  
  #colnames(rsv) <- rsv.names
  
  # Reading the output file of the simulation
  if (verbose) 
    print( paste("[Reading the file '", basename(file), "' ...]", sep="" ), quote=FALSE  ) 
  
  # Reading the output file of the simulation
  if (out.type=="Q") {
    # Reading only the 8 variables related to water quantity. 6x: ; I8: RES; F5: MON (not always is an integer !!); 6F12:  VOLUMEm3  FLOW_INcms FLOW_OUTcms    PRECIPm3      EVAPm3   SEEPAGEm3 
    rsv <- read.fortran(file, header=FALSE, skip=9, c("6X", "I8", "F5", "6F12"))    
    # Assigning the names 
    colnames(rsv) <- rsv.names[1:8]
  } else if (out.type=="Q+Sed") {
      # Reading only the 11 variables related to water quantity and sediments
      rsv <- read.fortran(file, header=FALSE, skip=9, c("6X", "I8", "F5", "6F12", "3F12"))    
      # Assigning the the complete list of names
      colnames(rsv) <- rsv.names[1:11]
    } else if (out.type=="Q+Sed+WQ") {
        # Reading ALL the outputs 
        rsv <- read.fortran(file, header=FALSE, skip=9, c("6X", "I8", "F5", "6F12", "3F12", "32F12"))    
        # Assigning the the complete list of names
        colnames(rsv) <- rsv.names
  } # ELSE end
  
  # When a monthly time step was used for printing the results in 'output.rsv',
  # after writing the 12 months values for all the reservoirs and for each year,
  # an annual mean value for each reservoir is printed, using the year (e.g., 1961)
  # as the 'MON' field value (instead of the number corresponding to the month (1,2,...,12) ). 
  # The next line is for filtering out these annual mean values for all the reservoirs
  if (tstep == "monthly") {
    rsv <- rsv[rsv$MON %in% 1:12, ] }
    
  # When a yearly time step was used for printing the results in 'output.rsv',
  # at the end of the file a mean value for all the period simulated and each 
  # reservoir is printed, using the amount of years simulated (e.g., 25.0)
  # as the 'MON' field value (instead of the number corresponding to the year (1961, 1961...,1985) ). 
  # The next line is for filtering out these mean values for all the reservoirs 
  # (assuming that all the simulation will start after the year 1900 and that nobody sill simulate 1900 years !)
  if (tstep == "annual") {
    rsv <- rsv[rsv$MON > 1900, ] }   
    
  # If the user provided a reservoir number, only those results will be returned to the user  
  if ( !missing(rsvID) ) {
    if ( rsvID != floor(rsvID) ) {      
      stop("Invalid argument: 'rsvID' must be integer" ) 
    } else rsv <- rsv[rsv$RES == rsvID, ]
    
    #############################
    # numeric -> zoo
    if ( (length(rsvID)==1) & (!missing(Date.Ini) & !missing(Date.Fin)) ) {
      if (tstep=="daily") {
        dates <- hydroTSM::dip(Date.Ini, Date.Fin, date.fmt=date.fmt)
        } else if (tstep=="monthly") {
           dates <- hydroTSM::mip(Date.Ini, Date.Fin, date.fmt=date.fmt)
           } else if (tstep=="annual") dates <- hydroTSM::yip(Date.Ini, Date.Fin, date.fmt=date.fmt)
      rsv <- zoo::zoo(rsv, dates)
    } # IF end
    
  } # IF end
  
  #############################
  # Selecting only some columns
  if (!is.null(col.names)) {
    if ( any( !( col.names %in% colnames(rsv) ) ) )
      stop( paste("Invalid argument '", col, "' is not a column name in '", file, "'", sep="") )
   
    # Getting only the colum(s) defined by the user
    rsv <- rsv[, col.names]
  } # IF end
  
  return(rsv)
  
} # 'read_rsv' END  
