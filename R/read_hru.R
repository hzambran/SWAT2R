# File read_hru.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                            read_hru                                  #
########################################################################
# Purpose    : Function for reading the 'output.hru' files of SWAT 2005#
#              (HRU outputs)                                           #
########################################################################
# Output     : numeric, data.frame or zoo object                       #
########################################################################
# Author     : Mauricio Zambrano-Bigiarini                             #
########################################################################
# Started    : 27-Feb-2009                                             #
# Updates    : 02-Oct-2009                                             #
#              22-Jan-2011 at JRC                                      #
#              17-Apr-2012 at JRC                                      #
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
# 'hruID'      : OPTIONAL. Integer with the number of the reach for wich the results will be provided.
#                If this argument is not provided, the results will be given for all the reaches in 'output.hru'
read_hru <- function(file="output.hru", 
                     out.type="Q", 
                     hruID=NA, 
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
 
  hru.names <- c("LULC", "HRU", "GIS", "SUB", "MGT", "MON", 
                 "AREAkm2", "PRECIPmm", "SNOFALLmm", "SNOMELTmm", "IRRmm", 
                 "PETmm", "ETmm", "SW_INITmm", "SW_ENDmm", "PERCmm", 
                 "GW_RCHGmm", "DA_RCHGmm", "REVAPmm", "SA_IRRmm", "DA_IRRmm", 
                 "SA_STmm", "DA_STmm", "SURQ_GENmm", "SURQ_CNTmm", "TLOSSmm", 
                 "LATQmm", "GW_Qmm", "WYLDmm", "DAILYCN", "TMP_AVdgC", 
                 "TMP_MXdgC", "TMP_MNdgC", "SOL_TMPdgC", "SOLARMJ/m2",
                 
                 "SYLDt/ha", "USLEt/ha", 
                 
                 "N_APPkg/ha", "P_APPkg/ha", "NAUTOkg/ha", "PAUTOkg/ha", "NGRZkg/ha", 
                 "PGRZkg/haN", "CFRTkg/haP", "CFRTkg/ha", "NRAINkg/ha", "NFIXkg/ha", 
                 "F-MNkg/ha", "A-MNkg/ha", "A-SNkg/ha", "F-MPkg/ha", "AO-LPkg/ha", 
                 "L-APkg/ha", "A-SPkg/ha", "DNITkg/ha", "NUPkg/ha", "PUPkg/ha", 
                 "ORGNkg/ha", "ORGPkg/ha", "SEDPkg/ha", "NSURQkg/ha", "NLATQkg/ha", 
                 "NO3Lkg/ha", "NO3GWkg/ha", "SOLPkg/ha", "P_GWkg/ha", "W_STRS", 
                 "TMP_STRS", "N_STRS", "P_STRS", "BIOMt/ha", "LAI", 
                 "YLDt/ha", "BACTPct", "BACTLPct", "WTAB", "CLIm", 
                 "WTAB", "SOLm")
                   
  #~ hru.widths <- c(4,5,9,5,5,5,
                  #~ 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10,
                  #~ 10,10,10,10,10,10,10,10,10,
                  
                  #~ 10,10,
                  
                  #~ 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10,
                  #~ 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10,
                  #~ 10,10
                  
                  #~ 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10) ?????
                  
 
    
  # Reading the output file of the simulation
  #hru <- read.fwf(fname, widths= hru.widths, header=FALSE, skip=9, sep = "\t")  
  #colnames(hru) <- hru.names
  
  # Reading the output file of the simulation
  if (verbose) 
    print( paste("[Reading the file '", basename(file), "' ...]", sep="" ), quote=FALSE  ) 
  
  # Reading the output file of the simulation
  if (out.type=="Q") {
  
    # Reading only the 35 variables related to water quantity. 
    # A4   : LULC, 
    # I5   : HRU; 
    # I9   : GIS; 
    # 3F5  : SUB, MGT, MON (not always is an integer !!)
    # 29F10:  "AREAkm2", "PRECIPmm", "SNOFALLmm", "SNOMELTmm", "IRRmm", 
    #          "PETmm", "ETmm", "SW_INITmm", "SW_ENDmm", "PERCmm", 
    #          "GW_RCHGmm", "DA_RCHGmm", "REVAPmm", "SA_IRRmm", "DA_IRRmm", 
    #          "SA_STmm", "DA_STmm", "SURQ_GENmm", "SURQ_CNTmm", "TLOSSmm", 
    #          "LATQmm", "GW_Qmm", "WYLDmm", "DAILYCN", "TMP_AVdgC", 
    #          "TMP_MXdgC", "TMP_MNdgC", "SOL_TMPdgC", "SOLARMJ/m2",
    hru <- read.fortran(file, header=FALSE, skip=9, c("A4", "I5", "I9", "3F5", "29F10"))    
    
    # Assigning the names
    colnames(hru) <- hru.names[1:35]
    
  } else if (out.type=="Q+Sed") {
  
      # Reading only the 11 variables related to water quantity and sediments
      hru <- read.fortran(file, header=FALSE, skip=9, c("A4", "I5", "I9", "3F5", "29F10", "2F10"))  
        
      # Assigning the the complete list of names
      colnames(hru) <- hru.names[1:37]
      
    } else if (out.type=="Q+Sed+WQ") {
    
        # Reading ALL the outputs 
        hru <- read.fortran(file, header=FALSE, skip=9, c("A4", "I5", "I9", "3F5", "29F10", "2F10", "42F10"))    
        # Assigning the the complete list of names
        colnames(hru) <- hru.names
        
  } # ELSE end
  
  # From character to factor
  hru$LULC <- as.factor(hru$LULC)
  
   # If the user provided a reach numer, only those results will be returned to the user  
  if ( !missing(hruID) ) {
    non.int <- which(hruID != floor(hruID))
    if ( length(non.int) > 0 ) {       
      stop("Invalid argument: 'hruID' must be integer" ) 
    } else hru <- hru[hru$HRU == hruID, ]
  } # IF end
  
  #############################
  # Selecting only some columns
  if (!is.null(col.names)) {
    if ( any( !( col.names %in% colnames(hru) ) ) )
      stop( paste("Invalid argument '", col, "' is not a column name in '", file, "'", sep="") )
   
    # Getting only the colum(s) defined by the user
    hru <- hru[, col.names]
  } # IF end
 
  #############################
  # transforming from numeric to zoo
  if (!missing(Date.Ini) & !missing(Date.Fin)) {
    if (tstep=="daily") {
      dates <- hydroTSM::dip(Date.Ini, Date.Fin, date.fmt=date.fmt)
      } else if (tstep=="monthly") {
         dates <- hydroTSM::mip(Date.Ini, Date.Fin, date.fmt=date.fmt)
         } else if (tstep=="annual") dates <- hydroTSM::yip(Date.Ini, Date.Fin, date.fmt=date.fmt)
    hru <- hydroTSM::vector2zoo(x=hru, dates=dates)
  } # IF end
  
  return(hru)
  
} # 'read_hru' END
