# File read_autocal.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                         read_autocal                                 #
########################################################################
# Purpose    : Function for reading the 'autocalX.out' file resulting  #
#              after the autocalibration process in SWAT 2005          #
########################################################################
# Output     : numeric, data.frame or zoo object                       #
########################################################################
# Author     : Mauricio Zambrano-Bigiarini                             #
########################################################################
# Started    : 06-Mar-2009                                             #
# Updates    : 02-Oct-2009                                             #
#              16-Apr-2012 at JRC                                      #
########################################################################

# 'fname'   : Name with path AND/OR extension of the file that have to be read
# 'drty'    : Directory where is located the file that have to be read
# 'tstep'  : Time step used for the simulation that created the 'output.rch' file.
#                Must be one of the following values: c("Daily", "Monthly", "Annual"), 
#                stands for daily, monthly and annual times steps
# 'out.type': Type of results that have to be read
#                Must be one of the following values: 
#               -) "Q"       : only results related to water quantity are read (first 8 columns)
#                              c("RCH", "GIS", "MON", "DrAREAkm2", 
#                                "FLOW_INcms", "FLOW_OUTcms", "EVAPcms", "TLOSScms")
#               -) "Q+Sed"   : only results related to water quantity AND sediments are read (first 11 columns)
#                               c("RCH", "GIS", "MON", "DrAREAkm2", "FLOW_INcms", "FLOW_OUTcms", 
#                                 "EVAPcms", "TLOSScms", "SED_INtons", "SED_OUTtons", "SEDCONCmg/kg")
#               -) "Q+Sed+WQ": all the columns of the 'output.rch' are read 
# 'outID'      : OPTIONAL. Integer with the number of the reach for wich the results will be provided.
#                If this argument is not provided, the results will be given for all the reaches in 'autocalX.out'
read_autocal <- function(file, tstep, out.type="Q", outID=NA, verbose=TRUE ) {

  # Checking that the user provided a valid value for 'file'
  if (missing(file)) {
    stop("Missing argument: 'file' must be provided" )
  } else if ( !file.exists(file) ) {
      stop( paste("Invalid argument value: The file '", basename(file), "' doesn't exist", sep="" ) )
    } # ELSE end

  # Checking that the user provided a valid value for 'tstep'
  if (missing(tstep)) {
        stop("Missing argument value: 'tstep' must be in c('daily','monthly','annual')") 
  } else  # Checking the validity of the 'unit' argument
         if ( is.na( match(tstep, c("daily", "monthly", "annual") ) ) ) {
            stop("Invalid argument value: 'tstep' must be in c('daily', 'monthly', 'annual')" ) }
            
  # Checking that the user provided a valid value for 'out.type'    
  if ( is.na( match(out.type, c("Q", "Q+Sed", "Q+Sed+WQ") ) ) ) {
        stop("Invalid argument value: 'out.type' must be in c('Q', 'Q+Sed', 'Q+Sed+WQ')" ) }
        
 
  autocal.names <- c("YEAR", "DAY", 
                     "FLOWcms", 
                     
                     "SEDCONCmg/kg", 
                     
                     "ORGNkg", "ORGPkg", "NO3kg", "NH4kg", "NO2kg", 
                     "MINPkg", "CBODkg", "DISOXkg", "CHOLAmic/L", "SOLPSTmg", 
                     "SORPSTmg", "BACTPct",  "BACTLPct", "CMETAL#1kg", "CMETAL#2kg",
                     "CMETAL#3kg", "Temp", "KjeldahlN", "TotalN", "TotalP")
                   
  autocal.widths <- c(5,5, 
                     14,
                     11,
                     11,11,11,11,11,11,11,11,11,11,
                     11,11,11,11,11,11,11,11,11,11)
    
  
  # Reading the output file of the simulation
  #autocal <- read.fwf(fname, widths= autocal.widths, header=FALSE, skip=0, sep = "\t")
  
  #colnames(autocal) <- autocal.names
  
  # Reading the output file of the simulation
  if (verbose) 
    print( paste("[Reading the file '", basename(file), "' ...]", sep="" ), quote=FALSE  ) 
  
  # Reading the output file of the simulation
  if (out.type=="Q") {
  
    # Reading only the 8 variables related to water quantity. 6x: "BIGSUB", I4:SUB; I9:GIS; F5: MON (not always is an integer !!)
    autocal <- read.fortran(file, header=FALSE, skip=9, c("I5", "I5", "F14"))    
    # Assigning the names 
    colnames(autocal) <- autocal.names[1:3]
    
  } else if (out.type=="Q+Sed") {
  
      # Reading only the 11 variables related to water quantity and sediments
      autocal <- read.fortran(file, header=FALSE, skip=9, c("I5", "I5", "F14", "F11"))    
      # Assigning the the complete list of names
      colnames(autocal) <- autocal.names[1:4]
      
    } else if (out.type=="Q+Sed+WQ") {
    
        # Reading ALL the outputs 
        autocal <- read.fortran(file, header=FALSE, skip=9, c("I5", "I5", "F14", "F11", "20F11"))    
        # Assigning the the complete list of names
        colnames(autocal) <- autocal.names
        
  } # ELSE end
  
  return(autocal)
  
} # 'read_autocal' END
