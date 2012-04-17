# File read_qout.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                           read_qout                                  #
########################################################################
# Purpose    : This function reads the file with the simulated stream- #
#              flows for the parameter sets obtained with SWAT-CUP     #
#              SWAT-CUP at http://www.eawag.ch/organisation/abteilungen
#                          /siam/software/swat/index_EN                #
########################################################################
# Output     : numeric, data.frame or zoo object                       #
########################################################################
# Author     : Mauricio Zambrano-Bigiarini                             #
########################################################################
# Started    : 09-Jun-2010                                             #
# Updates    : 03-Dec-2010                                             #
#              14-Jan-2011 ; 05-May-2011                               #
#              16-Apr-2012 at JRC                                      #
########################################################################


# The default names of this files are:
# -) SWAT-CUP - GLUE : -) 'modelres.out', with the streamflows of ALL the 
#                                         parameter sets, stored by rows
#                      -) 'modelres.beh', with the streamflows corresponding to 
#                                         the BEHAVIOURAL parameter sets only, 
#                                         stored by rows
# -) SWAT-CUP - SUFI2: -) 'q1.out', with the streamflows corresponding to ALL 
#                                   the parameter sets run, stored by rows
# -) SWAT-CUP - PSO  : -) 'q1_beh.out' with the streamflows corresponding to 
#                                      the BEHAVIOURAL parameter sets only, 
#                                      stored by rows  


# file   : the name of the file which the data are to be read from. 
#          If it does not contain an absolute path, the file name is relative to the current working directory, getwd(). Tilde-expansion is performed where supported. As from R  2.10.0 this can be a compressed file (see file). 
# ua.algorithm: character, indicating  the SWAT-CUP algorithm that created 'file'.
#          Valid values are in c('GLUE', 'SUFI2')
# nobs   : ONLY required when 'ua.algorithm' is equal to 'SUFI2'.
#          number of observation specified in the file 'observed.sf2', located in teh folder 'SUFI2.IN' (e.g., NO_Data=	6935)
# nsim   : Number of simulations specified in the file 'par_inf.sf2', located in teh folder 'SUFI2.IN' (e.g.: Number_of_LH_sims=2000)

read_qout <- function(file, ua.algorithm, nsim,  nobs, verbose=TRUE) {
                         
  # Checking that the user provided a value for 'file'
  if ( missing(file) ) {
    stop( paste("Missing argument: 'file' must be provided", sep="" ) )
  } else  
    # Checking that 'file' exists
    if ( !file.exists(file) )
      stop( paste("Invalid argument value: The file '", basename(file), "' doesn't exist", sep="" ) )
    
  # Checking 'ua.algorithm'  
  if ( missing(ua.algorithm) ) {
    stop( "Missing argument: 'ua.algorithm'" )
  } else  # Checking that the user provided a valid argument for 'ua.algorithm'       
      if (is.na(match(ua.algorithm, c("GLUE", "SUFI2", "PSO") ) ) ) 
            stop("Invalid argument: 'ua.algorithm' must be in c('GLUE', 'SUFI2', 'PSO')") 
           
  # For SUFI2 and PSO  'nobs' is required
  if ( ua.algorithm %in% c("SUFI2", "PSO") ) {         
      # Checking that the user provided a value for 'nobs'
      if ( missing(nobs) )
        stop( "Missing argument: 'nobs' must be provided when 'ua.algorithm' is in c('SUFI2', 'PSO')" )
  } # IF end
  
  # Checking that the user provided a value for 'nsim'
  if ( ua.algorithm %in% c("GLUE", "SUFI2", "PSO") )
    if ( missing(nsim) ) stop( "Missing argument: 'nsim' must be provided" )

  if ( ua.algorithm %in% c("GLUE", "SUFI2", "PSO") ) lskip <- 0

  # Reading the SWAT-CUP file with the simulated streamflows for each 
  # parameter set (by default 'q1.out')  
  # This may be a very TIME-CONSUMING task !!
  if (verbose) message( paste("[Reading the file '", basename(file), "' ...]", sep="" ) )
  q.all <-  read.table(file, skip=lskip, fill=TRUE, row.names=NULL, header=FALSE)
  
  # The streamflows simulated with the GLUE algorithm in SWAT-CUP are 
  # stored by rows, i.e.:
  # nrow(file) = number of parameter sets
  # ncol(file) = number of observations; whereas
  
  # The streamflows simulated with the SUFI2 algorithm in SWAT-CUP are 
  # stored in a SINGLE COLUMN, which starts with a sequential number indicating the simulation (e.g., 1,2,...,nsim), followed by the simulated streamflows
  if ( ua.algorithm == "GLUE" ) {
  
      Q <- t(as.matrix(q.all))
  
  } else if ( ua.algorithm %in% c("SUFI2", "PSO") ) {
  
      # Creating the matrix that will store only the streamflows predicted by each parameter sets 
      Q <- matrix(NA, ncol=nsim, nrow=nobs)
      
      if (verbose) message( paste("[Processing ", nsim, "simulations ...]", sep= " ") )
      
      if (verbose) pbar <- txtProgressBar(min = 0, max = nsim, style = 3)
       
      for (i in 1:nsim) { 
        
        setTxtProgressBar(pbar, i)
        
        Q[, i] <- q.all[(nobs*(i-1)+(i+1)):(nobs*i+i), 2]      
        
      } # FOR end
      
      # clossing the progress bar
      if (verbose) close(pbar)
  
  } # ELSE end
    
  if (verbose) message( paste("[Number of simulations: ", ncol(Q), "]", sep="" ) )
  if (verbose) message( paste("[Number of tsteps     : ", nrow(Q), "]", sep="" ) )
  
  #saving memory, deleting  the variable with All the streamflows
  remove(q.all) 
  
  colnames(Q) <- paste("Sim", 1:nsim, sep="")
  rownames(Q) <- paste("t", 1:nrow(Q), sep="")
  
  return(Q)
  
}  # 'read_qout' END 
