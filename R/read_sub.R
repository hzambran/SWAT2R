# File read_sub.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                             read_sub                                 #
########################################################################
# Purpose: Function for reading the 'output.sub' files of SWAT 2005    #
#          (subbasin outputs)                                          #
########################################################################
# Author : Mauricio Zambrano-Bigiarini                                 #
########################################################################
# Started: 02-Mar-2009;                                                #
# Updates: 02-Oct-2009 ; 22-Jan-2011                                   #
#          17-Apr-2012                                                 #
########################################################################

# The last (additional) rows have the average value for each subbasin during all the time period
# 'drty' : Directory where is located the file that have to be read
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
# 'subID'      : OPTIONAL. Integer with the number of the reach for wich the results will be provided.
#                If this argument is not provided, the results will be given for all the reaches in 'output.sub'

# Please note that I never worked with sediment nor water quality!!
read_sub <- function(file="output.sub", 
                     out.type="Q", 
                     subID=NA, 
                     col.names=NULL,          # character with the column name in 'file' that stores the results that the user wants to convert into a zoo object
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
            
  # Checking that the user provided a valid value for 'out.type'    
  if ( is.na( match(out.type, c("Q", "Q+Sed", "Q+Sed+WQ") ) ) ) {
        stop("Invalid argument value: 'out.type' must be in c('Q', 'Q+Sed', 'Q+Sed+WQ')" ) }
 
  sub.names <- #c("TYPE", "SUB", "GIS", "MON", "AREAkm2", "PRECIPmm", "SNOMELTmm", 
               c("SUB", "GIS", "MON", 
                 "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", 
                 "SWmm", "PERCmm", "SURQmm", "GW_Qmm", "WYLDmm", 
                 
                 "SYLDt/ha", 
                 
                 "ORGNkg/ha", "ORGPkg/ha", "NSURQkg/ha", " SOLPkg/ha",
                 "SEDPkg/ha", "LAT Q(mm)", "LATNO3kg/h", "GWNO3kg/ha", 
                 "CHOLAmic/L", "CBODU mg/L", "DOXQ mg/L")
                   
  #sub.widths <- c(6,
                  #4,9,5,
                  #10,10,10,10,10,10,10,10,10,10, 
                  #10,10,10,10,10,10,10,
                  #10,10,10, 10,10)
    
  # Reading the output file of the simulation
  if (verbose) 
    print( paste("[Reading the file '", basename(file), "' ...]", sep="" ), quote=FALSE  )  
  
  if (out.type=="Q") {
  
    # Reading only the 13 variables related to water quantity. 
    # 6x   : "BIGSUB", 
    # I4   : SUB; 
    # I9   : GIS; 
    # F5   : MON (not always is an integer !!), 
    # 10F10: "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm", "PERCmm", "SURQmm", "GW_Qmm", "WYLDmm"
    sub <- read.fortran(file, header=FALSE, skip=9, c("6X","I4", "I9", "F5", "10F10"))    
    
    # Assigning the names c("SUB", "GIS", "MON", "AREAkm2", "PRECIPmm", "SNOMELTmm", "PETmm", "ETmm", "SWmm", "PERCmm", "SURQmm", "GW_Qmm", "WYLDmm")
    colnames(sub) <- sub.names[1:13]
    
  } else if (out.type=="Q+Sed") {
  
      # Reading only the 14 variables related to water quantity and sediments
      sub <- read.fortran(file, header=FALSE, skip=9, c("6X","I4", "I9", "F5", "10F10", "F10"))  
        
      # Assigning the the complete list of names, (adding the following names to the previous list: names c("SYLDt/ha", "ORGNkg/ha", "ORGPkg/ha", "NSURQkg/ha", " SOLPkg/ha", "SEDPkg/ha", "LAT Q(mm)")
      colnames(sub) <- sub.names[1:14]
      
    } else if (out.type=="Q+Sed+WQ") {
    
        # Reading ALL the outputs 
        sub <- read.fortran(file, header=FALSE, skip=9, c("6X","I4", "I9", "F5", "10F10", "F10", "11F10"))   
         
        # Assigning the the complete list of names, (adding the following names to the previous list: c(""LATNO3kg/h", "GWNO3kg/ha", "CHOLAmic/L", "CBODU mg/L", "DOXQ mg/L")
        colnames(sub) <- sub.names
        
  } # ELSE end
  
  # Reading the output file of the simulation
  #sub <- read.fwf(fname, widths= sub.widths, header=FALSE, skip=9, sep = "\t")
  #colnames(sub) <- sub.names
  
   # If the user provided a subbasin numer, only those results will be returned to the user  
  if ( !missing(subID) ) {
    non.int <- which(subID != floor(subID))
    if ( length(non.int) > 0 ) {      
      stop("Invalid argument: 'subID' must be integer" ) 
    } else sub <- sub[sub$SUB == subID, ]
  } # IF end
  
  #############################
  # Selecting only some columns
  if (!is.null(col.names)) {
    if ( any( !( col.names %in% colnames(sub) ) ) )
      stop( paste("Invalid argument '", col, "' is not a column name in '", file, "'", sep="") )
   
    # Getting only the colum(s) defined by the user
    sub <- sub[, col.names]
  } # IF end
 
  #############################
  # transforming from numeric to zoo
  if (!missing(Date.Ini) & !missing(Date.Fin)) {
    if (tstep=="daily") {
      dates <- hydroTSM::dip(Date.Ini, Date.Fin, date.fmt=date.fmt)
      } else if (tstep=="monthly") {
         dates <- hydroTSM::mip(Date.Ini, Date.Fin, date.fmt=date.fmt)
         } else if (tstep=="annual") dates <- hydroTSM::yip(Date.Ini, Date.Fin, date.fmt=date.fmt)
    sub <- hydroTSM::vector2zoo(x=sub, dates=dates)
  } # IF end
  
  return(sub)
  
} # 'read_sub' END
