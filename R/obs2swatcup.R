# File obs2swatcup.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                         obs2swatcup                                  #
########################################################################
# Purpose    : This function generates a file with observed values of  #
#              streamflows for being used as input to SWAT-CUP         #
#              http://www.eawag.ch/organisation/abteilungen/siam/software/swat/index_EN)
########################################################################
# Output     : numeric, data.frame or zoo object                       #
########################################################################
# Author     : Mauricio Zambrano-Bigiarini                             #
########################################################################
# Started    : 28-Oct-2009                                             #
# Updates    : 02-Dec-2009                                             #
#              23-Nov-2010                                             #
#              16-Apr-2012 at JRC                                      #
########################################################################

# x        : zoo object with flow measurement(s) in 'm3/s'. It could be 'daily' or 'monthly'
# tstep    : time step in which are stored the values of 'x'. Valid values are: 'daily, 'monthly'
# from     : Character indicating the starting date for the values stored in all the files that 
#            will be read. It HAs to be in the format indicated by 'date.fmt'
# to       : Character indicating the starting date for the values stored in all the files that 
#            will be read. It HAs to be in the format indicated by 'date.fmt'
# date.fmt : Character indicating the date format in which you provide 'from' and 'to', e.g. "%d-%m-%Y"
# fname    : Filename (without extension) of the output file. The number of observations will be added at the end of the filename
# dates    : OPTIONAL (not implmented yet). Vector with the dates to be used for converting a raw 'x' values into a 'zoo' object

# version  : integer indicating the version of SWAT-CUP for which the file has to be written
# var.id   : OPTIONAL. Only needed when 'version==3'. 
#            Character that have to be added as prefix before the year and day/month
#            as second column in the observation file
obs2swatcup <- function(x, tstep="daily", from, to, date.fmt="%Y-%m-%d", fname="Observations", dates, version=2, var.id="q_1" ) {   

    if (class(x) != "zoo" ) stop("'x' must be of 'zoo' class")
    
    # Checking that the user provied a valid argument for 'var.type'       
    if (is.na(match(tstep, c("daily", "monthly") ) ) ) 
            stop("Invalid argument: 'tstep' must be in c('daily', 'monthly')")
            
    # Checking that the user provided 'from'
    if (missing(from))
     stop("Missing argument value: 'from' must contain the desired starting date for the observations") 
     
    # Checking that the user provided 'to'
    if (missing(to))
     stop("Missing argument value: 'to' must contain the desired ending date for the observations") 
     
    # Checking that the user provided 'fname'
    if (missing(fname)) {
     message("Missing argument value: 'fname'. Using 'Observations as 'fname'")  
    } # IF end
     
    from <- as.Date(from, format= date.fmt)
    to   <- as.Date(to, format= date.fmt) 

    obs <- x 
    
    if (tstep=="daily") {         
      Time.Window <- seq(from=from, to= to, by="days")
    } else if (tstep=="monthly") { 
        Time.Window <- seq(from=from, to= to, by="months")
      } # ELSE end
      
    obs <- window(obs, start=from, end=to)  
    
    # Total number of observations, including NA's
    nobs <- length(obs)

    # Finding the Days with missing values           
    # index of the values with NA
    na.index <- which(is.na(obs))
    message("NA indexes:")
    print(na.index, quote=FALSE)

    # Looking at the dates of the NA values    
    message(paste("NA values:", length(obs[na.index]) ) )
    print(obs[na.index], quote=FALSE)
      
    # Subsetting the Observed values               
    Year.Ini <- format(from, "%Y")
    Year.Fin <- format(to, "%Y")  

    if (version==2) { VarDate.ID=NA
    } else if ( version==3) {
       message("Computing julian dates...")
       VarDate.ID <- paste(var.id, "_", format(time(obs), "%Y"), ".", dates2juliandays(time(obs), verbose=TRUE), sep="" )
      } # ELSE end

    # Creating the structure of the output file
    obs.df  <- data.frame(ID=1:nobs, Date= time(obs), VarDateID= VarDate.ID, Value=as.numeric(obs) )

    # Identifying the valid values (not 'NA')
    valid.index <- which(!is.na(obs.df$Value))

    # Deleting the missing values (NA)
    obs.df <- obs.df[valid.index, ]
    
    # filename of the output file
    fname <- paste(fname, "-", nrow(obs.df), "obs.txt", sep="")

    # Creating the output variable, depending on the version of the selected version of SWAT-CUP
    if (version==2) {
      obs.df           <- obs.df[,c("ID","Value")]
      colnames(obs.df) <- c("number", "data")
    } else if (version==3) {
        obs.df           <- obs.df[,c("ID", "VarDateID", "Value")]
        colnames(obs.df) <- c("number", "Var_Date", "data")
      } # IF end

    # Write the output file
    write.table(obs.df, fname, row.names=FALSE, quote=FALSE)

    # total number of days within the period from 'from' to 'to'    
    nobs <- length(obs)
    message(paste("Number of days in the period            :", nobs) ) 

    # Shows the number of the observed data in 'x' that will be used for comparison with simulated values
    nrow(obs.df)
    message(paste("Number of written observations (not NAs):", nrow(obs.df) ) ) 

} # 'obs2swatcup' end
