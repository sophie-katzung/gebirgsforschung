# ---------------------------------------------------------------------------- #
#                                                                              #
#                 R-Version of the ESCIMO.spread v2 snow model                 #
#             which was originally developed by Marke et al. (2016)            #
#           building upon earlier work by Strasser and Marke (2010)            #
#                                                                              #
#                         Code implementation in R by:                         #
#               Thomas Marke, University of Innsbruck 2024/2025                #
#                                                                              #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#                                                                              #
# Reference:                                                                   #
#                                                                              #
# Marke, T., Mair, E., Förster, K., Hanzer, F., Garvelmann, J.,                #
# Pohl, S., Warscher, M. and Strasser, U. (2016): ESCIMO.spread (v2):          #
# parameterization of a spreadsheet-based energy balance snow model for        #
# inside-canopy conditions, Geosci. Model Dev., 9, 633-646,                    #
# https://doi.org/10.5194/gmd-9-633-2016.                                      #
#                                                                              #
# ---------------------------------------------------------------------------- #

# ************************************************************
# Setup screen
# ************************************************************
print('')
print('          +-+-+-+-+-+-+-+-+-+-+')
print('          |E|S|C|I|M|O|v|2|-|R|')
print('          +-+-+-+-+-+-+-+-+-+-+')
print('')

# ************************************************************
# Working environment
# ************************************************************

# Load libraries
library(this.path)      # Set working directory to the path of this script
library(lubridate)      # Support working with time information
library(RNetCDF)        # Read/write NetCDF data in R
library(fields)         # Create simple plots for grids
library(readxl)         # Read Excel data
library(writexl)        # Write Excel data

# On Linux: In case of errors make sure NetCDF directories are included entering 'echo $LD_LIBRARY_PATH' in console!
# If they are not, check your library and bin pathes by using 'nc-config --all' in console! 
# If necessary, add pathes e.g. by: export LD_LIBRARY_PATH=/usr/include:$LD_LIBRARY_PATH and 
# export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH!

# Clean up environment
rm(list=ls())

# Get script path and set working directory
scriptpath=this.path()
setwd(dirname(scriptpath))

# Source external code including several functions
source("ESCIMOv2-R_Functions.R")

# Set precision for outputs
outputprecision='NC_FLOAT'

# Record start time
starttime=Sys.time()

# ************************************************************
# Check if the model is called from command line
# ************************************************************

# Use command line arguments
args=commandArgs(trailingOnly=TRUE)

# Check if arguments are passed
if(length(args)>1) {
  commandline=1
} else {
  commandline=0
}

# ************************************************************
# Set input and output files
# ************************************************************-

# Use command line parameters (make sure to follow this order calling the model)
if (commandline==1) {
  
  # Read arguments
  meteoinput=args[1]              # Define meteorological input path with subfolders for each variable (e.g., './Input/MySite/')          
  staticinput=args[2]             # Define static (elevation, LAIeff) input file (e.g., './Input/MySite/MySite_static.nc')  
  escimooutput=args[3]            # Define output path and file (e.g., './Output/MySite/MySite_output.nc')
  outputpath=args[4]              # Define output path to write point outputs to (e.g., './Output/MySite/') 

# Use information defined below    
} else {
  
  # Define path for meteorological input (with subfolders for all variables) and static (elevation, LAIeff) input files
  meteoinput='../../R1-ModelSetupPointMode/Output/'
  staticinput='../../R1-ModelSetupPointMode/Output/Static_proviantdepot.nc'

  # Define output path and file
  escimooutput='GriddedOutput.nc'
  outputpath='../Output_Proviantdepot/'
}

# Print arguments
print(paste('Meteorological input path: ',meteoinput,sep=''))
print(paste('Static input file: ',staticinput,sep=''))
print(paste('Output file: ',escimooutput,sep=''))
print(paste('Output path: ',outputpath,sep=''))

# ************************************************************
# Model time
# ************************************************************

# Use command line parameters (make sure to follow this order calling the model)
if (commandline==1) {
  
  # Read arguments
  modelstart=args[5]              # Define time of model start (e.g., '1970-10-01')          
  modelend=args[6]                # Define time of model end (e.g., '1971-04-30') 
  
# Use information defined below    
} else {
  
  # Define start and end of model simulations
  modelstart='2019-10-04'
  modelend='2020-10-04'
}

# Print arguments
print(paste('Model start time: ',modelstart,sep=''))
print(paste('Model end time: ',modelend,sep=''))

# ************************************************************
# Output locations
# ************************************************************

# Use command line parameters (make sure to follow this order calling the model)
if (commandline==1) {
  
  # Read arguments
  outputlocations=as.character(args[7])               # File containing coordinates of output locations (use 'none' if not available)
  
  # Use information defined below  
} else {
  
  # Output locations
  #outputlocations='Output_Locations_Rofental.xlsx'   # File containing coordinates of output locations (use 'none' if not available)
  outputlocations='none'                              # File containing coordinates of output locations (use 'none' if not available)
}

# ************************************************************
# Optional parameters
# ************************************************************

# Use command line parameters (make sure to follow this order calling the model)
if (commandline==1) {
  
  # Read arguments
  lwinopt=as.numeric(args[8])         # Option for longwave radiation parameterisation (1-5)          
  snowdensopt=as.numeric(args[9])     # Option for snow density parameterisation (1-2)
  precipcorropt=as.numeric(args[10])   # Option for correcting solid precipitation (1-2)
  
  # Use information defined below  
} else {
  
  # Option for longwave radiation parameterization (use 1 as default)
  # lwinopt=1 # Swinbank (1963) with cloud modification by Jacobs (1978)
  # lwinopt=2 # Dilley and O’Brien (1998) with cloud modification by Jacobs (1978)
  # lwinopt=3 # Liston and Elder (2006) based on Iziomon et al. (2003)
  # lwinopt=4 # Prata (1996) with cloud modification by Sugita and Brutsaert (1993)
  # lwinopt=5 # Maykut and Church (1973)
  lwinopt=1
  
  # Option for snow density estimation (use 1 as default)
  # snowdensopt=1 # Dawson et al. (2017)
  # snowdensopt=2 # Essery et al. (2013)
  snowdensopt=1
  
  # Option for correcting solid precipitation for systematic undercatch (use 1 as default)
  # precipcorropt=1 # Goodison et al. (1998)
  # precipcorropt=2 # Kochendorfer et al. (2017)
  # precipcorropt=0 # no correction
  precipcorropt=1
}

# Print arguments
print(paste('Option for longwave radiation parameterisation: ',lwinopt,sep=''))
print(paste('Option for snow density parameterisation : ',snowdensopt,sep=''))
print(paste('Option for correcting solid precipitation : ',precipcorropt,sep=''))

# ************************************************************
# Define meteo-input related settings
# ************************************************************

# FOR GRIDS: Define the names of meteo input files with separate files for all variables!
temppredate='temp_'
temppostdate='_Meteo_1h_Rofental.nc'
rhpredate='rh_'
rhpostdate='_Meteo_1h_Rofental.nc'
wspredate='ws_'
wspostdate='_Meteo_1h_Rofental.nc'
precippredate='precip_'
precippostdate='_Meteo_1h_Rofental.nc'
swinpredate='swin_'
swinpostdate='_Meteo_1h_Rofental.nc'

# FOR POINTS: Define the name of meteo input file with all variables in one file!
preyear=''
postyear='.nc'

# Define the names of meteo input variables
tempvar='temp'
rhvar='hum'
wsvar='ws'
precipvar='precip'
swinvar='swin'

# Define unit for temperature input (K=Kelvin,C=degrees C)
temp_unit='K'

# Define precipitation gauge type (use 'standard' for European conditions in the Alps or see options in ESCIMOv2-R_Functions.R)
gaugetype='standard'

# Define projection-related information in static input file (used for NetCDF output)
crsvar='lambert_conformal_conic'
projected=0

# Define variable-realted information in static input file
xvar='lon'
yvar='lat'
elevationvar='elevation'
LAIeffvar='LAIeff'
landcovervar='landcover'
forestids=list(5,6,7)

# ************************************************************
# Define output variables for output grids
# ************************************************************

# Write all variables to file?
allout=0

# Define gridded outside canopy variables to be written to file
canopymaskout=0;outputmaskout=0;tempout=0;rhout=0;wsout=0;precipout=0;swinout=0;cloudcoverout=0;lwinout=0;
wetbulbtempout=0;shareliqprecipout=0;sharesolprecipout=0;liqprecipout=0;solprecipout=0;snowageout=0;snowalbout=0;
snowtempout=0;vappresairout=0;vappressnowout=0;swradbalout=0;lwradbalout=0;latfluxout=0;sensfluxout=0;
advfluxliqout=0;advfluxsolout=0;ebalsnowout=0;potmeltout=0;coldcontmmout=0;sublimout=0;meltout=0;
refreezeout=0;liqwatcontout=0;outflowout=0;sweout=0;snowdensout=0;snowdepthout=0

# Define gridded inside canopy variables to be written to file
radfracout=0;canfracout=0;canflowindexout=0;canswinout=0;canlwinout=0;canwsout=0;canrhout=0;
tminout=0;tmaxout=0;tmeanout=0;deltatout=0;canswabsout=0;reynoldsout=0;nusseltout=0;sherwoodout=0;
cansatvapout=0;satdensvapout=0;watvapdifout=0;omegaout=0;ratemasslossout=0;sublimlosscoeffout=0;
maxinterceptout=0;snowinterceptout=0;caninterceptloadout=0;canexposcoeffout=0;treesublimout=0;
treemeltout=0;groundliqprecipout=0;groundsolprecipout=0;throughsolprecipout=0;cansnowageout=0;
cansnowalbout=0;cansnowtempout=0;cansnowdeltatout=0;groundprecipout=0;canvappresairout=0;
canvappressnowout=0;canlatfluxout=0;canswradbalout=0;canlwradbalout=0;cansensfluxout=0;
canadvfluxliqout=0;canadvfluxsolout=0;canebalsnowout=0;cancoldcontmmout=0;cansublimout=0;
canpotmeltout=0;canmeltout=0;canrefreezeout=0;canliqwatcontout=0;canoutflowout=0;cansweout=0;
cansnowdensout=0;cansnowdepthout=0

# Define gridded combined variables (outside canopy+inside canopy) to be written to file
totsweout=1;totsnowdensout=0;totsnowdepthout=1

# ************************************************************
# Print out command line call
# ************************************************************

# Print message
print('')
print(paste("CMD-line call would be:"))
print(paste("Rscript ESCIMOv2-R.R ",meteoinput," ",staticinput," ",escimooutput," ",outputpath," ",modelstart," ",modelend," ",outputlocations," ",lwinopt," ",snowdensopt," ",precipcorropt,sep=""))
print('')

# ************************************************************
# Model parameters and constants
# ************************************************************

# Relevant for out- and inside the canopy
timeinc=3600                 # Time increment of each model timestep (s)
phasetranstemp=273.16        # Phase transition temperature (K)
albmin=0.5                   # Minimum albedo (0-1)
albadd=0.45                  # Additive albedo (0-1)
declinepos=-0.12             # Albedo decline parameter for positive temperatures (-)
declineneg=-0.05             # Albedo decline parameter for negative temperatures (-)
sigsnowfall=0.5              # Threshold for significant snowfall (mm/h)
melttemp=273.16              # Snow melt temperature (K)
LAIthreshold=1.25            # LAIeff threshold for canopy derivation (m2/m2)
forestLAIeff=7               # LAIeff for coniferous forest, only used if no LAIeff layer is provided (m2/m2)
meltheat=337500              # Melting heat (J/kg)
sublimheat=2835500           # (Re-)Sublimation heat (J/kg)
heatcapwater=4180            # Specific heat capacity of water (J/(kg*K))
heatcapsnow=2100             # Specific heat capacity of snow (J/(kg*K))
emissivitysnow=0.99          # Snow emissivity (-)
stefbolzcon=0.0000000567     # Stefan-Boltzmann-Constant (W/(m2*K^4))
snowdeltatmax=3              # Maximum snow temperature change per timestep (default=3 K from Cold De Porte data))
soilflux=2.0                 # Soil heat flux (W/m2)
snowdensfresh=100.           # Density of fresh snow (kg/m3)
fillvalue=-9999              # Value indicating missing values
initvalue=NaN                # Value used to initialize cells                
zerovalue=0                  # Value used to zero cells
kelvin=273.15                # Value used for temperature conversion
liqwatcap=0.025              # Liquid water holding capacity (0-1) -> values of 3-4% are found in Livneh et al (2010) (5% among the best in ESM-SnowMIP, 2.5% best for Saxony)
stepstmin=12                 # Timesteps considered for tmin estimation (steps) -> try 12h or 24h (12h among the best in ESM-SnowMIP, 12h best for Saxony)
transrange=0                 # Precipitation phase transition range (K) -> try 0K, 1K, 2K (0K used by Marke et al. 2016)

# Relevant for inside the canopy
extcoeff=0.71                # Vegetation-specific extinction coefficient (-)
scalfac=0.9                  # Scaling factor (-)
plantheight=40               # Plant height (m), here assuming spruce with heights between 30 and 50m (not used)
snowpartrad=0.0005           # Snow particle radius (m) (corresponds to 500 µm)
kinviscair=0.000013          # Kinematic viscosity of air (m2/s)
gasconstdryair=287.          # Gas constant of dry air (J/degree C*kg)
termcondatmo=0.024           # Thermal conductivity of the atmosphere (J/(m*s*K))
molweightwat=0.01801         # Molecular weight of water (kg/mole)
univgasconst=8.313           # Universal gas constant (J/(mole*K))
icedensity=917               # Ice density (kg/m^3)
tempmeltfac=0.05             # Temperature factor for snow melt (mm/(h*degree C))
albmeltfac=0.01              # Albedo factor for snow melt (mm/(W*h))
coeffshapesnow=0.01          # Coefficient related to the shape of the intercepted snow deposits [-]

# ************************************************************
# Process files
# ************************************************************

# --------------------------------------------------
# Process static input NetCDF
# --------------------------------------------------

# Open NetCDF file with static inputs
staticnc=open.nc(staticinput,write=FALSE,share=FALSE,prefill=TRUE)

# Check which variables exist
staticinfo=file.inq.nc(staticnc)
staticvars=sapply(0:(staticinfo$nvars-1),function(i) var.inq.nc(staticnc,i)$name)

# Read coordinate variables
if (xvar %in% staticvars) {
  xcoords=var.get.nc(staticnc,xvar,start=NA,count=NA,na.mode=4,unpack=TRUE)
} else {
  print(paste0('ESCIMO error: ','Variable not found in static input file: ',xvar))
  stop()
}
if (yvar %in% staticvars) {
  ycoords=var.get.nc(staticnc,yvar,start=NA,count=NA,na.mode=4,unpack=TRUE)
} else {
  print(paste0('ESCIMO error: ','Variable not found in static input file: ',yvar))
  stop()
}

# Get length of dimensions for static input file
ncols=length(xcoords)
nrows=length(ycoords)
gridres=abs(xcoords[2]-xcoords[1])

# Check if model is runing in gridded or point mode
if (ncols==1 & nrows==1) {
  print('Model running in point mode')
  grid=0
} else {
  print('Model running in grid mode')
  grid=1
}

# Read elevation [m]
elevation=array(initvalue,dim=c(ncols,nrows))              
if (elevationvar %in% staticvars) {
  elevation[1:ncols,1:nrows]=var.get.nc(staticnc,elevationvar,start=NA,count=NA,na.mode=4,unpack=TRUE)
} else {
  print(paste0('ESCIMO error: ','Variable not found in static input file: ',elevationvar))
  stop()
}

# Get effective LAI [m2/m2]
LAIeff=array(zerovalue,dim=c(ncols,nrows))   

# Read LAIeff from staic file if available
if (LAIeffvar %in% staticvars) {
  LAIeff[1:ncols,1:nrows]=var.get.nc(staticnc,'LAIeff',start=NA,count=NA,na.mode=4,unpack=TRUE)

# Set LAIeff based on landcover information
} else {
  print(paste0('ESCIMO info: ','Variable not found in static input file: ',LAIeffvar))
  print(paste0('ESCIMO info: ','Setting LAIeff based on forest class in lancover map!'))
  
  # Read landcover information
  landcover=array(initvalue,dim=c(ncols,nrows))              
  if (landcovervar %in% staticvars) {
    landcover[1:ncols,1:nrows]=var.get.nc(staticnc,landcovervar,start=NA,count=NA,na.mode=4,unpack=TRUE)
  } else {
    print(paste0('ESCIMO warning: ','Variable not found in static input file: ',landcovervar))
    print(paste0('ESCIMO warning: ','Setting LAIeff based on landcover information not possible!'))
    stop()
  }
  
  # Derive LAIeff from lancover map
  for (id in forestids) {
    forestmask=which(landcover==id,arr.ind=TRUE)
    LAIeff[forestmask]=forestLAIeff
  }
}

# --------------------------------------------------
# Get crs information from static input NetCDF
# --------------------------------------------------

# If NetCDF input includes projection information
if (projected==1) {
  
  # Get number of projection attributes
  ncrsattributes=var.inq.nc(staticnc,crsvar)
  ncrsattributes=as.numeric(ncrsattributes$natts)
  
  # Extract crs attributes (note: NetCDF ids start with 0, list items with 1)
  crsattributenames=list()
  crsattributetypes=list()
  crsattributevalues=list()
  
  for (n in 0:(ncrsattributes - 1)) {
    i=n+1
    crsattributenames[i]=att.inq.nc(staticnc, crsvar, n)$name
    crsattributetypes[i]=att.inq.nc(staticnc, crsvar, n)$type
    crsattributevalues[[i]]=att.get.nc(staticnc, crsvar, n)
  }
}

# --------------------------------------------------
# Process output locations
# --------------------------------------------------

# Get mask with output locations
outputmask=array(zerovalue,dim=c(ncols,nrows))

# Check if a filename has been provided
if (outputlocations != 'none') {
  
  # Open and read file with output locations
  outputlocationstable=read_excel(outputlocations)
  
  # Add a column for the row/col/id of locations
  outputlocationstable$ROW=0
  outputlocationstable$COL=0
  
  # Get number of output locations
  noutputlocations=nrow(outputlocationstable)
  
  # Loop over the rows in the table to find grid locations
  for(outputlocation in 1:noutputlocations) {
    
    # Get table information
    outputid=outputlocationstable$ID[outputlocation]
    xvalue=as.numeric(outputlocationstable$X[outputlocation])
    yvalue=as.numeric(outputlocationstable$Y[outputlocation])
    outputname=outputlocationstable$NAME[outputlocation]
    
    # Get row and col position
    position=col.row(xvalue,yvalue,gridres,xcoords,ycoords) 
    col=position[1]
    row=position[2]
    
    # Assign values to dataframe
    outputlocationstable$ROW[outputlocation]=row
    outputlocationstable$COL[outputlocation]=col
    
    # Modify output mask at col-row position
    outputmask[col,row]=1
  }
  
  # Delete rows in dataframe that contain stations out of the domain
  outputlocationstable=outputlocationstable[!is.na(outputlocationstable$ROW), ]
  
} else {
  
  # Create a dataframe including the required information to write results for the first grid cell
  outputlocationstable=data.frame(ID=c('1'),X=c(xcoords[1]),Y=c(ycoords[1]),Z=c(elevation[1]),NAME=c('FirstCell'),COL=c(1),ROW=c(1))
}

# Update number of output locations
noutputlocations=nrow(outputlocationstable)

# Write output locations to file (including row and colum information)
write_xlsx(outputlocationstable,path=paste(outputpath,"UsedOutputLocations.xlsx",sep=''))

# --------------------------------------------------
# Get information on the model period
# --------------------------------------------------

# Get datetime for start and end of model run
modelstartdatetime=as.POSIXct(paste(modelstart,' 00:00:00',sep=''),tz="UTC")
modelenddatetime=as.POSIXct(paste(modelend,' 00:00:00',sep=''),tz="UTC")

# Get the number of seconds between starttime and endtime
modelsecs=as.numeric(modelenddatetime-modelstartdatetime,units="secs")

# Get the number of model steps considering time increment
modelsteps=modelsecs/timeinc

# ************************************************************
# Once initialize continuous variables
# ************************************************************

# Variables used for outside canopy calculations
snowageprev=array(initvalue,dim=c(ncols,nrows))            # Snow age at previous model time step (days)
snowalbprev=array(initvalue,dim=c(ncols,nrows))            # Snow albedo at previous model time step (days)
snowtempprev=array(initvalue,dim=c(ncols,nrows))           # Snow temperature at previous model time step (K)
tempstack=array(initvalue,dim=c(ncols,nrows,24))           # Stack with air temperatures for the last 24h (K)

ebalsnowprev=array(zerovalue,dim=c(ncols,nrows))           # Snow energy balance at previous model time step (W/m2)
refreezprev=array(zerovalue,dim=c(ncols,nrows))            # Refreezing water at previous model time step (mm)
liqwatcontprev=array(zerovalue,dim=c(ncols,nrows))         # Liquid water content at previous model time step (mm)
sweprev=array(zerovalue,dim=c(ncols,nrows))                # Snow water equivalent at previous model time step (mm)
snowdepthprev=array(zerovalue,dim=c(ncols,nrows))          # Snow depth at previous model time step (m)
snowdensprev=array(zerovalue,dim=c(ncols,nrows))           # Snow density at previous model time step (m)

# Variables used for inside canopy calculations
cansnowageprev=array(initvalue,dim=c(ncols,nrows))         # Inside canopy snow age at previous model time step (days)
cansnowalbprev=array(initvalue,dim=c(ncols,nrows))         # Inside canopy snow albedo at previous model time step (days)
cansnowtempprev=array(initvalue,dim=c(ncols,nrows))        # Inside canopy snow temperature at previous model time step (K)
cantempstack=array(initvalue,dim=c(ncols,nrows,24))        # Stack with inside canopy air temperatures for the last 24h (K)

caninterceptloadprev=array(zerovalue,dim=c(ncols,nrows))   # Intercepted snow load at previous timestep (mm)
treesublimprev=array(zerovalue,dim=c(ncols,nrows))         # Sublimation from trees at previous timestep (mm)
meltunloadprev=array(zerovalue,dim=c(ncols,nrows))         # Melt-induced snow unload at previous timestep (mm)
canebalsnowprev=array(zerovalue,dim=c(ncols,nrows))        # Inside canopy energy balance of the snow cover at previous timestep (W/m2)
canrefreezprev=array(zerovalue,dim=c(ncols,nrows))         # Inside canopy refreezing liquid water at previous timestep (mm)
canliqwatcontprev=array(zerovalue,dim=c(ncols,nrows))      # Inside canopy liquid water content at previous model time step (mm)
cansweprev=array(zerovalue,dim=c(ncols,nrows))             # Inside canopy snow water equivalent at previous timestep (mm)
cansnowdepthprev=array(zerovalue,dim=c(ncols,nrows))       # Inside canopy snow depth at previous timestep (m)
cansnowdensprev=array(zerovalue,dim=c(ncols,nrows))        # Inside canopy snow depth at previous timestep (m)

# ************************************************************
# Preparations for static variables
# ************************************************************

# Declare static variables
radfrac=array(initvalue,dim=c(ncols,nrows))                # Fraction of incoming shortwave radiation reaching the surface (0-1)
canfrac=array(initvalue,dim=c(ncols,nrows))                # Canopy fraction (0-1)
canflowindex=array(initvalue,dim=c(ncols,nrows))           # Canopy flow index (-)

# Get masks for open areas and those with forest canopy
openmask=array(zerovalue,dim=c(ncols,nrows))
openmask[which(LAIeff<LAIthreshold,arr.ind=TRUE)]=1
canopymask=array(zerovalue,dim=c(ncols,nrows))
canopymask[which(LAIeff>=LAIthreshold,arr.ind=TRUE)]=1

# Get fraction of the incoming shortwave radiation reaching the inside canopy surface
radfrac=exp(-1*extcoeff*LAIeff)

# Get canopy fraction (0-1)
canfrac=0.55+0.29*log(LAIeff)
canfrac=ifelse(canfrac>1,1,canfrac)
canfrac=ifelse(canfrac<0,0,canfrac)

# Calculate canopy flow index
canflowindex=scalfac*LAIeff

# Calculate canopy reference level
canreflevel=0.6*plantheight

# ************************************************************
# Run model
# ************************************************************

# Get years to simulate
modelstartyear=as.numeric(substr(modelstart,1,4))
modelendyear=as.numeric(substr(modelend,1,4))

# --------------------------------------------------
# Loop over all model years
# --------------------------------------------------

# Initialize model timestep for total simulation period
timesteptot=0

# Loop over all years to simulate
for (modelyear in modelstartyear:modelendyear) {
  
  # Print information on current NetCDF date to screen
  print('')
  print(paste('************************************************************',sep=''))
  print(paste('Opening NetCDF input for: ',modelyear,sep=''))
  print(paste('************************************************************',sep=''))
  print('')
  
  # For simulations in grid mode
  if (grid==1) {
  
    # Define the meteorological input file names for this year
    tempinput=paste(meteoinput,temppredate,modelyear,temppostdate,sep='')
    rhinput=paste(meteoinput,rhpredate,modelyear,rhpostdate,sep='')
    wsinput=paste(meteoinput,wspredate,modelyear,wspostdate,sep='')
    precipinput=paste(meteoinput,precippredate,modelyear,precippostdate,sep='')
    swininput=paste(meteoinput,swinpredate,modelyear,swinpostdate,sep='')
    
    # Open NetCDF files with meteo inputs
    tempnc=open.nc(tempinput,write=FALSE,share=FALSE,prefill=TRUE)
    rhnc=open.nc(rhinput,write=FALSE,share=FALSE,prefill=TRUE)
    wsnc=open.nc(wsinput,write=FALSE,share=FALSE,prefill=TRUE)
    precipnc=open.nc(precipinput,write=FALSE,share=FALSE,prefill=TRUE)
    swinnc=open.nc(swininput,write=FALSE,share=FALSE,prefill=TRUE)
    
    # Extract time information for this year (here based on temperature data)
    time=var.get.nc(tempnc,'time',na.mode=4,unpack=TRUE)
    timeunits=att.get.nc(tempnc,"time","units")
  
  # For simulations in point mode  
  } else {
    
    # Define the meteorological input file name for this year
    allinput=paste(meteoinput,preyear,modelyear,postyear,sep='')
    
    # Open NetCDF file with meteo inputs
    allnc=open.nc(allinput,write=FALSE,share=FALSE,prefill=TRUE)
    
    # Extract time information for this year
    time=var.get.nc(allnc,'time',na.mode=4,unpack=TRUE)
    timeunits=att.get.nc(allnc,"time","units")
  }
  
  # Get reference time depending on time format in NetCDF file
  if (substr(timeunits,1,1)=='s') {
    referencetime=sub("seconds since ", "", timeunits)
    referenceunits='secs'
  } else if (substr(timeunits,1,1)=='h') {
    referencetime=sub("hours since ", "", timeunits)
    referenceunits='hours'
  } else if (substr(timeunits,1,1)=='d') {
    referencetime=sub("days since ", "", timeunits)
    referenceunits='days'
  }
  referencedatetime=ymd_hms(referencetime)
  
  # Convert time information to datetime
  datetime=as.POSIXct(referencedatetime,tz="UTC")+as.difftime(time,units=referenceunits)
  
  # Get the timesteps to be simulated for this year
  timemask=which(as.Date(datetime)>=modelstart & as.Date(datetime)<modelend,arr.ind=TRUE)
  yearsteps=length(timemask)
  yearstartstep=timemask[1]
  yearendstep=timemask[yearsteps]
  
  # --------------------------------------------------
  # Loop over all model timesteps in actual year
  # --------------------------------------------------
  
  # Loop over all timesteps to simulate
  for (timestepyear in 1:yearsteps) {
    
    # Raise model timestep for total simulation period
    timesteptot=timesteptot+1
    
    # --------------------------------------------------
    # Initialize all non-continuous variables
    # --------------------------------------------------
    
    # Time variables
    year=initvalue                                             # Model year
    month=initvalue                                            # Model month
    day=initvalue                                              # Model day
    hour=initvalue                                             # Model hour
     
    # Variables used for outside canopy calculations
    temp=array(initvalue,dim=c(ncols,nrows))                   # Air temperature (K)
    rh=array(initvalue,dim=c(ncols,nrows))                     # Air humidity (%)
    ws=array(initvalue,dim=c(ncols,nrows))                     # Wind speed (m/s)
    precip=array(initvalue,dim=c(ncols,nrows))                 # Precipitation (mm)
    swin=array(initvalue,dim=c(ncols,nrows))                   # Incoming shortwave radiation (W/m2)
    lwin=array(initvalue,dim=c(ncols,nrows))                   # Incoming longwave radiation (W/m2)
    wetbulbtemp=array(initvalue,dim=c(ncols,nrows))            # Wet bulb temperature (K)
    shareliqprecip=array(initvalue,dim=c(ncols,nrows))         # Share of liquid precipitation (0-1)
    sharesolprecip=array(initvalue,dim=c(ncols,nrows))         # Share of solid precipitation (0-1)
    liqprecip=array(initvalue,dim=c(ncols,nrows))              # Liquid precipitation (mm)
    solprecip=array(initvalue,dim=c(ncols,nrows))              # Solid precipitation (mm)
    cloudcover=array(initvalue,dim=c(ncols,nrows))             # Cloud cover (0-1)
    snowage=array(initvalue,dim=c(ncols,nrows))                # Snow age (days)
    snowalb=array(initvalue,dim=c(ncols,nrows))                # Snow albedo (0-1)
    snowtemp=array(initvalue,dim=c(ncols,nrows))               # Snow temperature (K)
    snowdeltat=array(initvalue,dim=c(ncols,nrows))             # Snow temperature change (K)
    vappresair=array(initvalue,dim=c(ncols,nrows))             # Vapor pressure of the air (hPa)
    vappressnow=array(initvalue,dim=c(ncols,nrows))            # Vapor pressure over the snow cover (hPa)
    swradbal=array(initvalue,dim=c(ncols,nrows))               # Shortwave radiation balance (W/m2)
    lwradbal=array(initvalue,dim=c(ncols,nrows))               # Longwave radiation balance (W/m2)
    latflux=array(initvalue,dim=c(ncols,nrows))                # Latent heat flux (W/m2)
    sensflux=array(initvalue,dim=c(ncols,nrows))               # Sensible heat flux (W/m2)
    advfluxliq=array(initvalue,dim=c(ncols,nrows))             # Advective flux from liquid precipitation (W/m2)
    advfluxsol=array(initvalue,dim=c(ncols,nrows))             # Advective flux from solid precipitation (W/m2)
    ebalsnow=array(initvalue,dim=c(ncols,nrows))               # Energy balance of the snow cover (W/m2)
    potmelt=array(initvalue,dim=c(ncols,nrows))                # Potential snow melt (mm)
    coldcontmm=array(initvalue,dim=c(ncols,nrows))             # Cold content of the snow cover (mm)
    sublim=array(initvalue,dim=c(ncols,nrows))                 # Sublimation of snow (mm)
    melt=array(initvalue,dim=c(ncols,nrows))                   # Actual snow melt (mm)
    refreeze=array(initvalue,dim=c(ncols,nrows))               # Refreezing liquid water (mm)
    liqwatcont=array(initvalue,dim=c(ncols,nrows))             # Liquid water content (mm)
    outflow=array(initvalue,dim=c(ncols,nrows))                # Melt water outflow (mm)
    snowdens=array(initvalue,dim=c(ncols,nrows))               # Simulated snow density (kg/m3)
    swe=array(zerovalue,dim=c(ncols,nrows))                    # Simulated snow water equivalent (mm)
    snowdepth=array(zerovalue,dim=c(ncols,nrows))              # Simulated snow depth (m)
    
    # Variables used for inside canopy calculations
    canswin=array(initvalue,dim=c(ncols,nrows))                # Inside canopy incoming shortwave radiation (W/m2)
    canlwin=array(initvalue,dim=c(ncols,nrows))                # Inside canopy incoming longwave radiation (W/m2)
    canws=array(initvalue,dim=c(ncols,nrows))                  # Inside canopy wind speed (m/s)
    canrh=array(initvalue,dim=c(ncols,nrows))                  # Inside canopy air humidity (%)
    tmin=array(initvalue,dim=c(ncols,nrows))                   # Outside canopy air temperature minimum (K)
    tmax=array(initvalue,dim=c(ncols,nrows))                   # Outside canopy air temperature maximum (K)
    tmean=array(initvalue,dim=c(ncols,nrows))                  # Outside canopy air temperature mean (K)
    deltat=array(initvalue,dim=c(ncols,nrows))                 # Inside canopy air temperature difference (K)
    canswabs=array(initvalue,dim=c(ncols,nrows))               # Inside canopy shortwave radiation absorption by snow particle (W/m2)
    reynolds=array(initvalue,dim=c(ncols,nrows))               # Reynolds number (-)
    nusselt=array(initvalue,dim=c(ncols,nrows))                # Nusselt number (-)
    sherwood=array(initvalue,dim=c(ncols,nrows))               # Sherwood number (-)
    cansatvap=array(initvalue,dim=c(ncols,nrows))              # Inside canopy saturation vapor pressure over ice (hPa)
    satdensvap=array(initvalue,dim=c(ncols,nrows))             # Saturation density of water vapor (kg/m3)
    watvapdif=array(initvalue,dim=c(ncols,nrows))              # Diffusivity of water vapor in the atmosphere (m2/s)
    omega=array(initvalue,dim=c(ncols,nrows))                  # Omega (-)
    ratemassloss=array(initvalue,dim=c(ncols,nrows))           # Rate of mass loss (kg/s)
    sublimlosscoeff=array(initvalue,dim=c(ncols,nrows))        # Sublimation loss rate coefficient (1/s)
    maxintercept=array(initvalue,dim=c(ncols,nrows))           # Maximum snow interception (mm)
    snowintercept=array(initvalue,dim=c(ncols,nrows))          # Snow interception (mm)
    caninterceptload=array(initvalue,dim=c(ncols,nrows))       # Intercepted snow load (mm)
    canexposcoeff=array(initvalue,dim=c(ncols,nrows))          # Coefficient related to the shape of the intercepted snow deposits (-)
    treesublim=array(initvalue,dim=c(ncols,nrows))             # Sublimation of snow from trees (mm)
    treemelt=array(initvalue,dim=c(ncols,nrows))               # Melt of snow from trees (mm)
    meltunload=array(initvalue,dim=c(ncols,nrows))             # Melt-induced snow unload (mm)
    groundliqprecip=array(initvalue,dim=c(ncols,nrows))        # Inside canopy liquid precipitation on the ground (mm)
    groundsolprecip=array(initvalue,dim=c(ncols,nrows))        # Inside canopy solid precipitation on the ground (mm)
    throughsolprecip=array(initvalue,dim=c(ncols,nrows))       # Inside canopy throughfall of solid precipitation (mm)
    cansnowage=array(initvalue,dim=c(ncols,nrows))             # Inside canopy age of snow (days)
    cansnowalb=array(initvalue,dim=c(ncols,nrows))             # Inside canopy snow albedo (0-1)
    cansnowtemp=array(initvalue,dim=c(ncols,nrows))            # Inside canopy snow temperature (K)
    cansnowdeltat=array(initvalue,dim=c(ncols,nrows))          # Inside canopy snow temperature change (K) 
    groundprecip=array(initvalue,dim=c(ncols,nrows))           # Inside canopy total precipitation on the ground (mm)
    canvappresair=array(initvalue,dim=c(ncols,nrows))          # Inside canopy vapor pressure of the air (hPa)
    canvappressnow=array(initvalue,dim=c(ncols,nrows))         # Inside canopy vapor pressure over the snow cover (hPa)
    canlatflux=array(initvalue,dim=c(ncols,nrows))             # Inside canopy latent heat flux (W/m2)
    canswradbal=array(initvalue,dim=c(ncols,nrows))            # Inside canopy shortwave radiation balance (W/m2)
    canlwradbal=array(initvalue,dim=c(ncols,nrows))            # Inside canopy longwave radiation balance (W/m2)
    cansensflux=array(initvalue,dim=c(ncols,nrows))            # Inside canopy sensible heat flux (W/m2)
    canadvfluxliq=array(initvalue,dim=c(ncols,nrows))          # Inside canopy advective flux from liquid precipitation (W/m2)
    canadvfluxsol=array(initvalue,dim=c(ncols,nrows))          # Inside canopy advective flux from solid precipitation (W/m2)
    canebalsnow=array(initvalue,dim=c(ncols,nrows))            # Inside canopy energy balance of the snow cover (W/m2)
    cancoldcontmm=array(initvalue,dim=c(ncols,nrows))          # Inside canopy cold content of the snow cover (mm)
    cansublim=array(initvalue,dim=c(ncols,nrows))              # Inside canopy sublimation of snow (mm)
    canpotmelt=array(initvalue,dim=c(ncols,nrows))             # Inside canopy potential snow melt (mm)
    canmelt=array(initvalue,dim=c(ncols,nrows))                # Inside canopy snow melt (mm)
    canrefreeze=array(initvalue,dim=c(ncols,nrows))            # Inside canopy refreezing liquid water (mm)
    canliqwatcont=array(initvalue,dim=c(ncols,nrows))          # Inside canopy liquid water content (mm)
    canoutflow=array(initvalue,dim=c(ncols,nrows))             # Inside canoy melt water outflow (mm)
    cansnowdens=array(initvalue,dim=c(ncols,nrows))            # Inside canopy simulated snow density (kg/m3)
    canswe=array(zerovalue,dim=c(ncols,nrows))                 # Inside canopy simulated snow water equivalent (mm)
    cansnowdepth=array(zerovalue,dim=c(ncols,nrows))           # Inside canopy simulated snow depth (m)
    
    # Combined variables (outside canopy+inside canopy)
    totsnowdens=array(zerovalue,dim=c(ncols,nrows))            # Total simulated snow density (kg/m3)
    totswe=array(zerovalue,dim=c(ncols,nrows))                 # Total simulated snow water equivalent (mm)
    totsnowdepth=array(zerovalue,dim=c(ncols,nrows))           # Total simulated snow depth (m)
    
    # --------------------------------------------------
    # Get model time for current timestep
    # --------------------------------------------------
    
    # Get timestep in the meteo input files
    filestep=(yearstartstep+timestepyear)-1
  
    # Get actual date
    modeldatetime=datetime[filestep]
    
    # Extract year, month, day and hour
    year=year(modeldatetime)
    month=month(modeldatetime)
    day=day(modeldatetime)
    hour=hour(modeldatetime)
    
    # --------------------------------------------------
    # Screen output
    # --------------------------------------------------
    
    # Print current timestep information to screen
    print(paste('------------------------------------------------------------',sep=''))
    print(paste('Model datetime: ',hour,'h ',day,'-',month,'-',year,' (timestep=',timestepyear,')',sep=''))
    
    # --------------------------------------------------
    # Get climate input for current timestep
    # --------------------------------------------------
    
    # Print out progress message
    print('-> Getting climate input for current timestep...')
    
    # Start and end for data range to extract from NetCDF input file
    inputdatastart=c(NA,NA,filestep)
    inputdatacount=c(NA,NA,1)
    
    # For simulations in grid mode
    if (grid==1) {
      
      # Get meteorological input data for current timestep from separate files for all variables
      temp[1:ncols,1:nrows]=var.get.nc(tempnc,tempvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      rh[1:ncols,1:nrows]=var.get.nc(rhnc,rhvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      ws[1:ncols,1:nrows]=var.get.nc(wsnc,wsvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      precip[1:ncols,1:nrows]=var.get.nc(precipnc,precipvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      swin[1:ncols,1:nrows]=var.get.nc(swinnc,swinvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      
    # For simulations in point mode  
    } else {
      
      # Get meteorological input data for current timestep from one file for all variables
      temp[1:ncols,1:nrows]=var.get.nc(allnc,tempvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      rh[1:ncols,1:nrows]=var.get.nc(allnc,rhvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      ws[1:ncols,1:nrows]=var.get.nc(allnc,wsvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      precip[1:ncols,1:nrows]=var.get.nc(allnc,precipvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
      swin[1:ncols,1:nrows]=var.get.nc(allnc,swinvar,start=inputdatastart,count=inputdatacount,na.mode=4,unpack=TRUE)
    }
    
    # Make sure wind speed is not smaller than 0.1 m/s (there is always some wind)
    ws=ifelse(ws<0.1,0.1,ws)
    
    # Make sure temperature is in Kelvin
    if (temp_unit!='K') {
      temp=temp+kelvin
    }
    
    # --------------------------------------------------
    # Perform calculations independent from snow presence
    # --------------------------------------------------
    
    # Print out progress message
    print('-> Performing calculations independent from snow presence...')
    
    # Modify 24h-temperature stack with actual air temperature
    tempstack[,,2:24]=tempstack[,,1:23]
    tempstack[,,1]=temp
    
    # Get outside canopy air temperature statistics from tempstack
    tmin=min3D(tempstack)
    tmax=max3D(tempstack)
    tmean=(tmin+tmax)*0.5
    deltat=(tmean-melttemp)/3
    deltat=ifelse(deltat>2,2,deltat)
    deltat=ifelse(deltat<(-2),-2,deltat)
    
    # Get inside canopy air temperature
    cantemp=temp-canfrac*(temp-(0.8*(temp-tmean)+tmean-deltat))
    
    # Modify 24h-temperature stack with actual inside canopy air temperature
    cantempstack[,,2:24]=cantempstack[,,1:23]
    cantempstack[,,1]=cantemp
    
    # Calculate cloud cover
    cloudcover=get.cloudcover(ncols,nrows,temp,rh,elevation,month)
    
    # Calculate incoming shortwave radiation using chosen option
    if (lwinopt==1) {
      
      # Get incoming longwave radiation following Swinbank (1963) with cloud modification by Jacobs (1978)
      lwin=incoming.longwaveradiation1(temp,rh,cloudcover)
      
    } else if (lwinopt==2) {
      
      # Get incoming longwave radiation following Dilley and O’Brien (1998) with cloud modification by Jacobs (1978)
      lwin=incoming.longwaveradiation2(temp,rh,cloudcover)
      
    } else if (lwinopt==3) {
      
      # Get incoming longwave radiation following Liston and Elder (2006) based on Iziomon et al. (2003)
      lwin=incoming.longwaveradiation3(ncols,nrows,temp,rh,cloudcover,elevation)
      
    } else if (lwinopt==4) {
      
      # Get incoming longwave radiation following Prata (1996) with cloud modification by Sugita and Brutsaert (1993)
      lwin=incoming.longwaveradiation4(temp,rh,cloudcover)
      
    } else if (lwinopt==5) {
      
      # Get incoming longwave radiation following Maykut and Church (1973)
      lwin=incoming.longwaveradiation5(temp,cloudcover)
      
    } else {
      
      # Print error
      print(paste0('ESCIMO error: ','Invalid option for calculating longwave radiation: ',lwinopt))
      stop()
    }
    
    # ************************************************************
    # Start of ESCIMOv2 model calculations
    # ************************************************************
    
    # --------------------------------------------------
    # Get shares of solid an liquid precipitation
    # --------------------------------------------------
    
    # Print out progress message
    print('-> Getting shares of solid an liquid precipitation...')
    
    # Get wetbulb temperature following Stull (2011) 
    wetbulbtemp=((temp-melttemp)*atan(0.151977*(rh+8.313659)^(1./2.))+
                   atan((temp-melttemp)+rh)-atan(rh-1.676331)+0.00391838*rh^(3./2.)*
                   atan(0.023101*rh)-4.686035) + melttemp
    
    # Get upper and lower boundaries for phase transition
    allliqt=phasetranstemp+transrange
    noliqt=phasetranstemp-transrange
    
    # Set all above upper boundary to liquid
    liquidcells=which(wetbulbtemp>=allliqt,arr.ind=TRUE)
    shareliqprecip[liquidcells]=1
    
    # Set all below lower boundary to solid
    solidcells=which(wetbulbtemp<noliqt,arr.ind=TRUE)
    shareliqprecip[solidcells]=0
    
    # In the transition range get mixed share
    mixedcells=which(wetbulbtemp>noliqt & wetbulbtemp<allliqt,arr.ind=TRUE)
    shareliqprecip[mixedcells]=(allliqt-wetbulbtemp[mixedcells])/(allliqt-noliqt)
    
    # Get share of solid precipitation
    sharesolprecip=1-shareliqprecip
    
    # Get liquid and solid precipitation
    liqprecip=precip*shareliqprecip
    solprecip=precip*sharesolprecip
    
    # Correct solid precipitation using the chosen option
    if (precipcorropt==0) {
      
      # No correction of solid precipitation
      solprecip=solprecip
      
    } else if (precipcorropt==1) {
      
      # Correct solid precipitation following Goodison et. al (1998)
      solprecip=correct.undercatch1(gaugetype,solprecip,temp,ws)
      
    } else if (precipcorropt==2) {
      
      # Correct solid precipitation following Kochendorfer et. al (2017)
      solprecip=correct.undercatch2(gaugetype,solprecip,temp,ws)
      
    } else {
      
      # Print error
      print(paste0('ESCIMO error: ','Invalid option for correcting solid precipitation: ',precipcorropt))
      stop()
    }
    
    # --------------------------------------------------
    # Simulating out of canopy snow processs
    # --------------------------------------------------
    
    # Define cells to consider (only if there snow falling or existing out of canopy)
    snowmask=array(zerovalue,dim=c(ncols,nrows))
    snowmask[which(solprecip>0 | sweprev>0,arr.ind=TRUE)]=1
    activemask=array(zerovalue,dim=c(ncols,nrows))
    activemask[which(snowmask==1 & openmask==1,arr.ind=TRUE)]=1
    activecells=which(activemask==1,arr.ind=TRUE)
    
    # Check if there are cells to simulate
    if (length(activecells)>0) {
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Screen output
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Print message to screen
      print('-> Simulating out of canopy snow processes...')
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow albedo and snow age
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Refresh albedo and snow age for all cells with significant snowfall
      sigsnowfallcells=which(activemask==1 & solprecip>sigsnowfall,arr.ind=TRUE)
      snowalb[sigsnowfallcells]=albmin+albadd
      snowage[sigsnowfallcells]=0
      
      # For all cells where there is no snow before but insignificant snowfall, we need to start with plausible values 
      insigsnowfallcells=which(activemask==1 & solprecip>0 & solprecip<=sigsnowfall & sweprev==0,arr.ind=TRUE)
      snowalb[insigsnowfallcells]=albmin+albadd  
      snowage[insigsnowfallcells]=0
      
      # For all cells where there is snow before but no or insignificant snowfall, decrease snow albedo and increase snow age 
      posinsigsnowfallcells=which(activemask==1 & solprecip<=sigsnowfall & sweprev>0 & temp>melttemp,arr.ind=TRUE)
      snowalb[posinsigsnowfallcells]=albmin+(snowalbprev[posinsigsnowfallcells]-albmin)*exp(declinepos*0.04167)
      snowage[posinsigsnowfallcells]=snowageprev[posinsigsnowfallcells]+0.04167
      
      neginsigsnowfallcells=which(activemask==1 & solprecip<=sigsnowfall & sweprev>0 & temp<=melttemp,arr.ind=TRUE)
      snowalb[neginsigsnowfallcells]=albmin+(snowalbprev[neginsigsnowfallcells]-albmin)*exp(declineneg*0.04167)
      snowage[neginsigsnowfallcells]=snowageprev[neginsigsnowfallcells]+0.04167
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow temperature
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # For cells with new snow initialize snow temperature
      newsnowcells=which(activemask==1 & sweprev==0,arr.ind=TRUE)
      snowtemp[newsnowcells]=ifelse(temp[newsnowcells]>melttemp,melttemp,temp[newsnowcells])
      
      # For cells with existing snow calculate snow temperature change 
      # (we use prev values so the later calculated energy balance matches with actual snow temperature)
      oldsnowcells=which(activemask==1 & sweprev>0,arr.ind=TRUE)
      snowdeltat[oldsnowcells]=((ebalsnowprev[oldsnowcells]*timeinc)+(refreezprev[oldsnowcells]*meltheat))/
        ((sweprev[oldsnowcells]+solprecip[oldsnowcells])*heatcapsnow)
      
      # Perform some checks on temperature change
      snowdeltat[oldsnowcells]=ifelse(snowdeltat[oldsnowcells]>snowdeltatmax,snowdeltatmax,snowdeltat[oldsnowcells])
      snowdeltat[oldsnowcells]=ifelse(snowdeltat[oldsnowcells]<(-1.*snowdeltatmax),(-1.*snowdeltatmax),snowdeltat[oldsnowcells])
      
      # Update snow temperature
      snowtemp[oldsnowcells]=snowtempprev[oldsnowcells]+snowdeltat[oldsnowcells]
      snowtemp[oldsnowcells]=ifelse(snowtemp[oldsnowcells]>melttemp,melttemp,snowtemp[oldsnowcells])
      
      # Check if snow temperature minimum should be ensured
      if (stepstmin>0) {
        
        # Get tcrit
        tcrit=min3D(tempstack[,,1:stepstmin,drop=FALSE])
        
        # Make sure tcrit is smaller than 273.16 and use it to limit snow temperature
        snowtemp[oldsnowcells]=ifelse(tcrit[oldsnowcells]<melttemp & snowtemp[oldsnowcells]<tcrit[oldsnowcells],tcrit[oldsnowcells],snowtemp[oldsnowcells])
      }
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Latent heat flux
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get vapor pressure of the air
      vappresair[activecells]=6.1078*exp((17.08085*(temp[activecells]-melttemp))/(234.175+(temp[activecells]-melttemp)))*(rh[activecells]/100)
      
      # Get vapor pressure over the snow
      vappressnow[activecells]=6.1078*exp((17.08085*(snowtemp[activecells]-melttemp))/(234.175+(snowtemp[activecells]-melttemp)))
      
      # Get latent heat flux
      latflux[activecells]=32.82*(0.18+0.098*ws[activecells])*(vappresair[activecells]-vappressnow[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Shortwave radiation balance
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate shortwave radiation balance
      swradbal[activecells]=(1-snowalb[activecells])*swin[activecells]
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Longwave radiation balance
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate shortwave radiation balance
      lwradbal[activecells]=lwin[activecells]-(emissivitysnow*stefbolzcon*snowtemp[activecells]^4)
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Sensible heat flux
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get sensible heat flux
      sensflux[activecells]=18.85*(0.18+0.098*ws[activecells])*(temp[activecells]-snowtemp[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Advective heat fluxes
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get advective heat from liquid precipitation
      advfluxliq[activecells]=heatcapwater*(temp[activecells]-melttemp)*liqprecip[activecells]/timeinc
      
      # Get advective heat from solid precipitation
      advfluxsol[activecells]=heatcapsnow*(temp[activecells]-snowtemp[activecells])*solprecip[activecells]/timeinc
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Energy balance
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get energy balance of the snow cover
      ebalsnow[activecells]=swradbal[activecells]+lwradbal[activecells]+sensflux[activecells]+latflux[activecells]+advfluxliq[activecells]+advfluxsol[activecells]+soilflux
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Cold content
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate cold content (mm) (defined to be negative)
      coldcontmm[activecells]=(snowtemp[activecells]-melttemp)*(sweprev[activecells]+solprecip[activecells])*heatcapsnow/meltheat
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # (Re-)Sublimation
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get (re-)sublimation of snow
      sublim[activecells]=latflux[activecells]*timeinc/sublimheat
      
      # If we see a mass loss (negative sublimation values) make sure it is not more than available
      sublim[activecells]=ifelse(sublim[activecells]<(-1*(sweprev[activecells]+solprecip[activecells])),(-1*(sweprev[activecells]+solprecip[activecells])),sublim[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow melt
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get potential melt from available energy
      potmelt[activecells]=ebalsnow[activecells]*timeinc/meltheat
      potmelt[activecells]=ifelse(potmelt[activecells]<0,0,potmelt[activecells])
      
      # Reduce potential melt to available water
      potmelt[activecells]=ifelse(potmelt[activecells]>(sweprev[activecells]+solprecip[activecells]+sublim[activecells]),(sweprev[activecells]+solprecip[activecells]+sublim[activecells]),potmelt[activecells])
      
      # Make sure no negative values are produced
      potmelt[activecells]=ifelse(potmelt[activecells]<0,0,potmelt[activecells])
      
      # Account for cold content and reduce melt accordingly (cold content is negative)
      melt[activecells]=potmelt[activecells]+coldcontmm[activecells]
      melt[activecells]=ifelse(melt[activecells]<0,0,melt[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Refreezing of liquid water
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get refreezing water
      refreeze[activecells]=ebalsnow[activecells]*timeinc/meltheat
      refreeze[activecells]=ifelse(refreeze[activecells]>0,0,refreeze[activecells])
      
      # The negative energy balance makes refreezing water negative, make it positive
      refreeze[activecells]=refreeze[activecells]*-1.
      
      # Make sure it is not more than available
      refreeze[activecells]=ifelse(refreeze[activecells]>liqwatcontprev[activecells],liqwatcontprev[activecells],refreeze[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Liquid water content
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate liquid water in the snow pack
      liqwatcont[activecells]=liqwatcontprev[activecells]+liqprecip[activecells]+melt[activecells]-refreeze[activecells]
      liqwatcont[activecells]=ifelse(liqwatcont[activecells]>(sweprev[activecells]*liqwatcap),(sweprev[activecells]*liqwatcap),liqwatcont[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Outflow
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate water outflow from snow pack
      outflow[activecells]=liqwatcontprev[activecells]+liqprecip[activecells]+melt[activecells]-refreeze[activecells]-(sweprev[activecells]*liqwatcap)
      outflow[activecells]=ifelse(outflow[activecells]<0,0,outflow[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow water equivalent
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get snow water equivalent
      swe[activecells]=sweprev[activecells]+solprecip[activecells]+liqprecip[activecells]+sublim[activecells]-outflow[activecells]
      swe[activecells]=ifelse(swe[activecells]<0,0,swe[activecells])  
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow density and snow depth
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # For cells with new snow initialize snow density with that of fresh snow
      newsnowcells=which(activemask==1 & sweprev==0,arr.ind=TRUE)
      snowdens[newsnowcells]=snowdensfresh
      
      # For cells with existing snow calculate bulk density
      oldsnowcells=which(activemask==1 & sweprev>0,arr.ind=TRUE)
      snowdens[oldsnowcells]=((sweprev[oldsnowcells]*snowdensprev[oldsnowcells])+(solprecip[oldsnowcells]*snowdensfresh))/(sweprev[oldsnowcells]+solprecip[oldsnowcells])
      
      # Update snow density using chosen option
      if (snowdensopt==1) {
        
        # Update snow density considering snow aging and overburden following Dawson et al. (2017)
        snowdens[activecells]=snow.density1(temp[activecells],swe[activecells],snowtemp[activecells],snowdepth[activecells],snowdens[activecells],timeinc)
        
      } else if (snowdensopt==2) {
        
        # Update snow density considering snow aging following Essery et al. (2013)
        snowdens[activecells]=snow.density2(swe[activecells],snowtemp[activecells],snowdens[activecells],timeinc)
        
      } else {
        
        # Print error
        print(paste0('ESCIMO error: ','Invalid option for calculating snow density: ',snowdensopt))
        stop()
      }
      
      # Make sure density is not lower than that of fresh snow
      snowdens[activecells]=ifelse(snowdens[activecells]<snowdensfresh,snowdensfresh,snowdens[activecells]) 
      
      # Update snow depth
      snowdepth[activecells]=swe[activecells]/snowdens[activecells]
      
      # For cells where there is no more snow (snow could have melted away during this timestep)
      nosnowcells=which(activemask==1 & swe==0,arr.ind=TRUE)
      snowdens[nosnowcells]=initvalue
      snowdepth[nosnowcells]=zerovalue
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Store snow values for next timestep
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Make sure we have information on previous timestep next loop
      sweprev[activecells]=swe[activecells]
      snowageprev[activecells]=snowage[activecells]
      snowalbprev[activecells]=snowalb[activecells]
      snowtempprev[activecells]=snowtemp[activecells]
      ebalsnowprev[activecells]=ebalsnow[activecells]
      refreezprev[activecells]=refreeze[activecells]
      liqwatcontprev[activecells]=liqwatcont[activecells]
      snowdepthprev[activecells]=snowdepth[activecells]
      snowdensprev[activecells]=snowdens[activecells]
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Reset variables if snow is gone
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Make sure we start with plausible values for a new snow cover
      snowgonecells=which(activemask==1 & swe==0,arr.ind=TRUE)
      sweprev[snowgonecells]=zerovalue
      snowageprev[snowgonecells]=zerovalue
      snowalbprev[snowgonecells]=initvalue
      snowtempprev[snowgonecells]=initvalue
      ebalsnowprev[snowgonecells]=zerovalue
      refreezprev[snowgonecells]=zerovalue
      liqwatcontprev[snowgonecells]=zerovalue
      snowdepthprev[snowgonecells]=zerovalue
      snowdensprev[snowgonecells]=initvalue
      
    } # End of simulation out of canopy snow processs
    
    # --------------------------------------------------
    # Simulating snow processs in trees
    # --------------------------------------------------
    
    # Define cells to consider (only if there snow falling or existing in or below canopy)
    snowmask=array(zerovalue,dim=c(ncols,nrows))
    snowmask[which(solprecip>0 | cansweprev>0 | caninterceptloadprev>0,arr.ind=TRUE)]=1
    activemask=array(zerovalue,dim=c(ncols,nrows))
    activemask[which(snowmask==1 & canopymask==1,arr.ind=TRUE)]=1
    activecells=which(activemask==1,arr.ind=TRUE)
    
    # Check if there are cells to simulate
    if (length(activecells)>0) {
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Screen output
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Print message to screen
      print('-> Simulating snow processes in trees...')
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow albedo and snow age inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Refresh albedo and snow age for all cells with significant snowfall
      sigsnowfallcells=which(activemask==1 & solprecip>sigsnowfall,arr.ind=TRUE)
      cansnowalb[sigsnowfallcells]=albmin+albadd
      cansnowage[sigsnowfallcells]=0
      
      # For all cells where there is no snow before but insignificant snowfall, we need to start with plausible values
      insigsnowfallcells=which(activemask==1 & solprecip>0 & solprecip<=sigsnowfall & (cansweprev==0 & caninterceptloadprev==0),arr.ind=TRUE)
      cansnowalb[insigsnowfallcells]=albmin+albadd
      cansnowage[insigsnowfallcells]=0
      
      # For all cells where there is snow before but no or insignificant snowfall, decrease snow albedo and increase snow age 
      posinsigsnowfallcells=which(activemask==1 & solprecip<=sigsnowfall & (cansweprev>0 | caninterceptloadprev>0) & temp>melttemp,arr.ind=TRUE)
      cansnowalb[posinsigsnowfallcells]=albmin+(cansnowalbprev[posinsigsnowfallcells]-albmin)*exp(declinepos*0.04167)
      cansnowage[posinsigsnowfallcells]=cansnowageprev[posinsigsnowfallcells]+0.04167
      
      neginsigsnowfallcells=which(activemask==1 & solprecip<=sigsnowfall & (cansweprev>0 | caninterceptloadprev>0) & temp<=melttemp,arr.ind=TRUE)
      cansnowalb[neginsigsnowfallcells]=albmin+(cansnowalbprev[neginsigsnowfallcells]-albmin)*exp(declineneg*0.04167)
      cansnowage[neginsigsnowfallcells]=cansnowageprev[neginsigsnowfallcells]+0.04167
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Inside canopy climate
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get inside canopy wind speed
      canws[activecells]=ws[activecells]*exp(-1*canflowindex[activecells]*(1-canreflevel/plantheight))
      
      # Get inside canopy air humidity
      canrh[activecells]=rh[activecells]*(1+0.1*canfrac[activecells])
      canrh[activecells]=ifelse(canrh[activecells]>100,100,canrh[activecells])  
      
      # Get incoming solar radiation inside canopy
      canswin[activecells]=swin[activecells]*radfrac[activecells]
      
      # Get incoming longwave radiation inside canopy  
      canlwin[activecells]=((1-canfrac[activecells])*lwin[activecells])+(canfrac[activecells]*stefbolzcon*cantemp[activecells]^4)
      
      # Calculate the inside canopy shortwave radiation absorption by snow particle
      canswabs[activecells]=pi*(snowpartrad^2)*(1-cansnowalb[activecells])*swin[activecells]
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow processes on trees
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate Reynolds number (0.7-10)
      reynolds[activecells]=2*snowpartrad*canws[activecells]/kinviscair
      reynolds[activecells]=ifelse(reynolds[activecells]<0.7,0.7,reynolds[activecells])
      reynolds[activecells]=ifelse(reynolds[activecells]>10,10,reynolds[activecells])
      
      # Get Nusselt number
      nusselt[activecells]=1.79+0.606*reynolds[activecells]^0.5
      
      # Get Sherwood number
      sherwood[activecells]=1.79+0.606*reynolds[activecells]^0.5
      
      # Calculate inside canopy saturation vapor pressure over ice
      cansatvap[activecells]=(611.15*exp(22.452*(cantemp[activecells]-melttemp)/(cantemp[activecells]-0.61))*0.01)
      
      # Get saturation density of water vapor
      satdensvap[activecells]=0.622*(cansatvap[activecells]*100/(gasconstdryair*cantemp[activecells]))
      
      # Calculate water vapor diffusivity
      watvapdif[activecells]=0.0000206*(cantemp[activecells]/273)^1.75
      
      # Get Omega
      omega[activecells]=((sublimheat*molweightwat)/(univgasconst*cantemp[activecells])-1)/(termcondatmo*cantemp[activecells]*nusselt[activecells])
      
      # Get rate of mass loss
      ratemassloss[activecells]=(2*pi*snowpartrad*(canrh[activecells]/100-1)-canswabs[activecells]*omega[activecells])/
        (sublimheat*omega[activecells]+(1/(watvapdif[activecells]*satdensvap[activecells]*sherwood[activecells])))
      
      # Calculate particle mass (kg)
      partmass=4./3.*pi*icedensity*snowpartrad^3
      
      # Calculate sublimation loss rate coefficient
      sublimlosscoeff[activecells]=ratemassloss[activecells]/partmass
      
      # Get maximum interception
      maxintercept[activecells]=4.4*LAIeff[activecells]
      
      # Calculate snow interception
      snowintercept[activecells]=0.7*(maxintercept[activecells]-caninterceptloadprev[activecells])*(1-exp((-solprecip[activecells])/maxintercept[activecells]))
      
      # Get total intercepted snow load
      caninterceptload[activecells]=(caninterceptloadprev[activecells]+snowintercept[activecells])-treesublimprev[activecells]-meltunloadprev[activecells]
      caninterceptload[activecells]=ifelse(caninterceptload[activecells]<0,0,caninterceptload[activecells])
      
      # Get cells with and without intercepted load
      loadcells=which(activemask==1 & caninterceptload>0,arr.ind=TRUE)
      noloadcells=which(activemask==1 & caninterceptload==0,arr.ind=TRUE)
      
      # Calculate canopy exposure coefficient
      canexposcoeff[loadcells]=coeffshapesnow*(caninterceptload[loadcells]/maxintercept[loadcells])^(-0.4)
      canexposcoeff[loadcells]=ifelse(canexposcoeff[loadcells]<0,0,canexposcoeff[loadcells])  
      
      # Get sublimation of snow on the trees (make sure it is positive as required for following calculations)
      treesublim[loadcells]=(canexposcoeff[loadcells]*caninterceptload[loadcells]*sublimlosscoeff[loadcells]*timeinc)*-1.
      treesublim[noloadcells]=0.
      
      # Calculate melt of snow in the trees
      treemelt[loadcells]=(tempmeltfac*(cantemp[loadcells]-273.16)+albmeltfac*(1-cansnowalb[loadcells])*canswin[loadcells])
      treemelt[loadcells]=ifelse(treemelt[loadcells]<0,0,treemelt[loadcells])
      treemelt[noloadcells]=0.
      
      # Calculate melt-induced snow unload
      meltunload[loadcells]=1.*treemelt[loadcells]+2.3*treemelt[loadcells]
      meltunload[loadcells]=ifelse(meltunload[loadcells]>caninterceptload[loadcells]-treesublim[loadcells],caninterceptload[loadcells]-treesublim[loadcells],meltunload[loadcells])
      meltunload[noloadcells]=0.
      
      # Get solid precipitation on the ground
      groundsolprecip[activecells]=solprecip[activecells]-snowintercept[activecells]
      
      # Calculate snow throughfall
      throughsolprecip[activecells]=groundsolprecip[activecells]+meltunload[activecells]
      
      # Get liquid precipitation on the ground
      groundliqprecip[activecells]=liqprecip[activecells]
      
      # Get total precipitation on the ground
      groundprecip[activecells]=groundsolprecip[activecells]+groundliqprecip[activecells]
      
    } # End of simulation of snow processs in trees
    
    # --------------------------------------------------
    # Simulating snow processs below trees
    # --------------------------------------------------
    
    # Define cells to consider (only if there is snow falling or existing on the ground below trees)
    snowmask=array(zerovalue,dim=c(ncols,nrows))
    snowmask[which(throughsolprecip>0 | cansweprev>0,arr.ind=TRUE)]=1
    activemask=array(zerovalue,dim=c(ncols,nrows))
    activemask[which(snowmask==1 & canopymask==1,arr.ind=TRUE)]=1
    activecells=which(activemask==1,arr.ind=TRUE)
    
    # Check if there are cells to simulate
    if (length(activecells)>0) {
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Screen output
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Print message to screen
      print('-> Simulating snow processes below trees...')
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow temperature inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # For cells with new snow initialize snow temperature and set snowdeltat to 0
      newsnowcells=which(activemask==1 & throughsolprecip>0 & cansweprev==0,arr.ind=TRUE)
      cansnowtemp[newsnowcells]=ifelse(cantemp[newsnowcells]>melttemp,melttemp,cantemp[newsnowcells])
      
      # For cells with existing snow calculate snow temperature change 
      # (we use prev values so the later calculated energy balance matches with actual snow temperature)
      oldsnowcells=which(activemask==1 & cansweprev>0,arr.ind=TRUE)
      cansnowdeltat[oldsnowcells]=((canebalsnowprev[oldsnowcells]*timeinc)+(canrefreezprev[oldsnowcells]*meltheat))/
        ((cansweprev[oldsnowcells]+throughsolprecip[oldsnowcells])*heatcapsnow)
      
      # Perform some checks on temperature change
      cansnowdeltat[oldsnowcells]=ifelse(cansnowdeltat[oldsnowcells]>snowdeltatmax,snowdeltatmax,cansnowdeltat[oldsnowcells])
      cansnowdeltat[oldsnowcells]=ifelse(cansnowdeltat[oldsnowcells]<(-1.*snowdeltatmax),(-1.*snowdeltatmax),cansnowdeltat[oldsnowcells])
      
      # Update snow temperature
      cansnowtemp[oldsnowcells]=cansnowtempprev[oldsnowcells]+cansnowdeltat[oldsnowcells]
      cansnowtemp[oldsnowcells]=ifelse(cansnowtemp[oldsnowcells]>melttemp,melttemp,cansnowtemp[oldsnowcells])  
      
      # Check if snow temperature minimum should be ensured
      if (stepstmin>0) {
        
        # Get tcrit
        tcrit=min3D(cantempstack[,,1:stepstmin,drop=FALSE])
        
        # Make sure tcrit is smaller than 273.16 and use it to limit snow temperature
        cansnowtemp[oldsnowcells]=ifelse(tcrit[oldsnowcells]<melttemp & cansnowtemp[oldsnowcells]<tcrit[oldsnowcells],tcrit[oldsnowcells],cansnowtemp[oldsnowcells])
      }
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Latent heat flux inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get vapor pressure of the air inside canopy
      canvappresair[activecells]=6.1078*exp((17.08085*(cantemp[activecells]-melttemp))/(234.175+(cantemp[activecells]-melttemp)))*(canrh[activecells]/100)
      
      # Get vapor pressure over the snow inside canopy
      canvappressnow[activecells]=6.1078*exp((17.08085*(cansnowtemp[activecells]-melttemp))/(234.175+(cansnowtemp[activecells]-melttemp)))
      
      # Get latent heat flux
      canlatflux[activecells]=32.82*(0.18+0.098*canws[activecells])*(canvappresair[activecells]-canvappressnow[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Shortwave radiation balance inside canopy 
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate shortwave radiation balance
      canswradbal[activecells]=(1-cansnowalb[activecells])*canswin[activecells]
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Longwave radiation balance inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate shortwave radiation balance
      canlwradbal[activecells]=canlwin[activecells]-(emissivitysnow*stefbolzcon*cansnowtemp[activecells]^4)
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Sensible heat flux inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get sensible heat flux
      cansensflux[activecells]=18.85*(0.18+0.098*canws[activecells])*(cantemp[activecells]-cansnowtemp[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Advective heat fluxes inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get advective heat from liquid precipitation
      canadvfluxliq[activecells]=heatcapwater*(cantemp[activecells]-melttemp)*liqprecip[activecells]/timeinc
      
      # Get advective heat from solid precipitation
      canadvfluxsol[activecells]=heatcapsnow*(cantemp[activecells]-cansnowtemp[activecells])*throughsolprecip[activecells]/timeinc
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Energy balance inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get energy balance of the snow cover
      canebalsnow[activecells]=canswradbal[activecells]+canlwradbal[activecells]+cansensflux[activecells]+canlatflux[activecells]+canadvfluxliq[activecells]+canadvfluxsol[activecells]+soilflux
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Cold content inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate cold content (mm) (defined to be negative)
      cancoldcontmm[activecells]=(cansnowtemp[activecells]-melttemp)*(cansweprev[activecells]+throughsolprecip[activecells])*heatcapsnow/meltheat
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # (Re-)Sublimation inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get (re-)sublimation of snow
      cansublim[activecells]=canlatflux[activecells]*timeinc/sublimheat
      
      # If we see a mass loss (negative sublimation values) make sure it is not more than available
      cansublim[activecells]=ifelse(cansublim[activecells]<(-1*(cansweprev[activecells]+throughsolprecip[activecells])),(-1*(cansweprev[activecells]+throughsolprecip[activecells])),cansublim[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow melt inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get potential melt from available energy
      canpotmelt[activecells]=canebalsnow[activecells]*timeinc/meltheat
      canpotmelt[activecells]=ifelse(canpotmelt[activecells]<0,0,canpotmelt[activecells])
      
      # Reduce potential melt to available water
      canpotmelt[activecells]=ifelse(canpotmelt[activecells]>(cansweprev[activecells]+throughsolprecip[activecells]+cansublim[activecells]),(cansweprev[activecells]+throughsolprecip[activecells]+cansublim[activecells]),canpotmelt[activecells])
      
      # Make sure no negative values are produced
      canpotmelt[activecells]=ifelse(canpotmelt[activecells]<0,0,canpotmelt[activecells])
      
      # Account for cold content and reduce melt accordingly (cold content is negative)
      canmelt[activecells]=canpotmelt[activecells]+cancoldcontmm[activecells]
      canmelt[activecells]=ifelse(canmelt[activecells]<0,0,canmelt[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Refreezing of liquid water inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get refreezing water
      canrefreeze[activecells]=canebalsnow[activecells]*timeinc/meltheat
      canrefreeze[activecells]=ifelse(canrefreeze[activecells]>0,0,canrefreeze[activecells])
      
      # The negative energy balance makes refreezing water negative, make it positive
      canrefreeze[activecells]=canrefreeze[activecells]*-1.
      
      # Make sure it is not more than available
      canrefreeze[activecells]=ifelse(canrefreeze[activecells]>canliqwatcontprev[activecells],canliqwatcontprev[activecells],canrefreeze[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Liquid water content inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate liquid water in the snow pack
      canliqwatcont[activecells]=canliqwatcontprev[activecells]+liqprecip[activecells]+canmelt[activecells]-canrefreeze[activecells]
      canliqwatcont[activecells]=ifelse(canliqwatcont[activecells]>(cansweprev[activecells]*liqwatcap),(cansweprev[activecells]*liqwatcap),canliqwatcont[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Outflow inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Calculate water outflow from snow pack
      canoutflow[activecells]=canliqwatcontprev[activecells]+liqprecip[activecells]+canmelt[activecells]-canrefreeze[activecells]-(cansweprev[activecells]*liqwatcap)
      canoutflow[activecells]=ifelse(canoutflow[activecells]<0,0,canoutflow[activecells])
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow water equivalent inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Get snow water equivalent
      canswe[activecells]=cansweprev[activecells]+throughsolprecip[activecells]+liqprecip[activecells]+cansublim[activecells]-canoutflow[activecells]
      canswe[activecells]=ifelse(canswe[activecells]<0,0,canswe[activecells])  
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Snow density and snow depth inside canopy
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # For cells with new snow initialize snow density with that of fresh snow
      newsnowcells=which(activemask==1 & throughsolprecip>0 & cansweprev==0,arr.ind=TRUE)
      cansnowdens[newsnowcells]=snowdensfresh
      
      # For cells with existing snow calculate bulk density
      oldsnowcells=which(activemask==1 & cansweprev>0,arr.ind=TRUE)
      cansnowdens[oldsnowcells]=((cansweprev[oldsnowcells]*cansnowdensprev[oldsnowcells])+(throughsolprecip[oldsnowcells]*snowdensfresh))/(cansweprev[oldsnowcells]+throughsolprecip[oldsnowcells])
      
      # Update snow density using chosen option
      if (snowdensopt==1) {
        
        # Update snow density considering snow aging and overburden following Dawson et al. (2017)
        cansnowdens[activecells]=snow.density1(cantemp[activecells],canswe[activecells],cansnowtemp[activecells],cansnowdepth[activecells],cansnowdens[activecells],timeinc)
        
      } else if (snowdensopt==2) {
        
        # Update snow density considering snow aging following Essery et al. (2013)
        cansnowdens[activecells]=snow.density2(canswe[activecells],cansnowtemp[activecells],cansnowdens[activecells],timeinc)
        
      } else {
        
        # Print error
        print(paste0('ESCIMO error: ','Invalid option for calculating snow density: ',snowdensopt))
        stop()
      }
      
      # Make sure density is not lower than that of fresh snow
      cansnowdens[activecells]=ifelse(cansnowdens[activecells]<snowdensfresh,snowdensfresh,cansnowdens[activecells])
      
      # Update snow depth
      cansnowdepth[activecells]=canswe[activecells]/cansnowdens[activecells]
      
      # For cells where there is no more snow (snow could have melted away during this timestep)
      nosnowcells=which(activemask==1 & canswe==0,arr.ind=TRUE)
      cansnowdens[nosnowcells]=initvalue
      cansnowdepth[nosnowcells]=zerovalue
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Store snow values for next timestep
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Variables that relate to canopy snow as well
      cansnowageprev[activecells]=cansnowage[activecells]
      cansnowalbprev[activecells]=cansnowalb[activecells]
      caninterceptloadprev[activecells]=caninterceptload[activecells]
      meltunloadprev[activecells]=meltunload[activecells]
      
      # Variables that relate to ground snow only
      groundsnowcells=which(activemask==1 & (solprecip>0 | cansweprev>0),arr.ind=TRUE)
      cansweprev[groundsnowcells]=canswe[groundsnowcells]
      cansnowtempprev[groundsnowcells]=cansnowtemp[groundsnowcells]
      canebalsnowprev[groundsnowcells]=canebalsnow[groundsnowcells]
      canrefreezprev[groundsnowcells]=canrefreeze[groundsnowcells]
      canliqwatcontprev[groundsnowcells]=canliqwatcont[groundsnowcells]
      cansnowdepthprev[groundsnowcells]=cansnowdepth[groundsnowcells]
      cansnowdensprev[groundsnowcells]=cansnowdens[groundsnowcells]
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Reset variables if snow is gone
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Make sure we start with plausible values for a new ground snow cover
      cansnowgonecells=which(activemask==1 & canswe==0,arr.ind=TRUE)
      cansweprev[cansnowgonecells]=zerovalue
      cansnowtempprev[cansnowgonecells]=initvalue
      canebalsnowprev[cansnowgonecells]=zerovalue
      canrefreezprev[cansnowgonecells]=zerovalue
      canliqwatcontprev[cansnowgonecells]=zerovalue
      cansnowdepthprev[cansnowgonecells]=zerovalue
      cansnowdensprev[cansnowgonecells]=initvalue
      
      # Make sure we start with plausible values for a tree snow load
      cansnowgonecells=which(activemask==1 & caninterceptload==0,arr.ind=TRUE)
      caninterceptloadprev[cansnowgonecells]=zerovalue
      meltunloadprev[cansnowgonecells]=zerovalue
      
      # Make sure we start with plausible values for snow albedo and age
      cansnowgonecells=which(activemask==1 & canswe==0 & caninterceptload==0,arr.ind=TRUE)
      cansnowageprev[cansnowgonecells]=zerovalue
      cansnowalbprev[cansnowgonecells]=initvalue
      
    } # End of simulation of snow processs below trees
    
    # --------------------------------------------------
    # Get totals combining in- and outside canopy values
    # --------------------------------------------------
    
    # Put outside canopy values
    totsnowdens[canopymask==0]=snowdens[canopymask==0]
    totswe[canopymask==0]=swe[canopymask==0]  
    totsnowdepth[canopymask==0]=snowdepth[canopymask==0]
    
    # Put inside canopy values
    totsnowdens[canopymask==1]=cansnowdens[canopymask==1]
    totswe[canopymask==1]=canswe[canopymask==1]  
    totsnowdepth[canopymask==1]=cansnowdepth[canopymask==1]  
    
    # ************************************************************
    # End of ESCIMO model calculations
    # ************************************************************
    
    # --------------------------------------------------
    # Write results for this timestep to NetCDF file
    # --------------------------------------------------
    
    # For the first timestep of the model run prepare NetCDF files for point output
    if (timesteptot==1) {
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Point output
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Print out progress message
      print('-> Preparing NetCDF files for point output at 1st timestep...')
      
      # Create list object to hold NetCDF IDs
      outputlocationsnc=list()
      
      # Loop over the output points in the outputlocation table
      for(outputlocation in 1:noutputlocations) {
        
        # Get table information
        outputid=outputlocationstable$ID[outputlocation]
        xvalue=as.numeric(outputlocationstable$X[outputlocation])
        yvalue=as.numeric(outputlocationstable$Y[outputlocation])
        outputname=outputlocationstable$NAME[outputlocation]
        outputcol=outputlocationstable$COL[outputlocation]
        outputrow=outputlocationstable$ROW[outputlocation]
        outputelevation=outputlocationstable$Z[outputlocation]
        
        # Define the file name for each NetCDF file
        outputlocationsfile=paste(outputpath,outputid,'_',outputname,'.nc',sep='')
        
        # Open NetCDF file for outputs
        pointoutputnc=create.nc(outputlocationsfile,clobber=TRUE,share=FALSE,prefill=TRUE,format='netcdf4')
        
        # Save NetCDF info for later access
        outputlocationsnc[[outputlocation]]=pointoutputnc
        
        # Define dimensions
        tdim=dim.def.nc(pointoutputnc,'time',modelsteps)
        ndim=dim.def.nc(pointoutputnc,'n',1)
        
        # Define static variables
        var.def.nc(pointoutputnc,'time',"NC_DOUBLE",c(tdim))
        var.def.nc(pointoutputnc,'x',"NC_DOUBLE",c(ndim))
        var.def.nc(pointoutputnc,'y',"NC_DOUBLE",c(ndim))
        var.def.nc(pointoutputnc,'zPoint',"NC_DOUBLE",c(ndim))
        var.def.nc(pointoutputnc,'zGrid',"NC_DOUBLE",c(ndim))
        var.def.nc(pointoutputnc,'LAIeff',"NC_DOUBLE",c(ndim))
        var.def.nc(pointoutputnc,'inCanopy',"NC_INT",c(ndim))
        var.def.nc(pointoutputnc,crsvar,"NC_INT",1L)
        
        # Put time variable
        outputtimeunits=paste('hours since ',as.Date(modeldatetime),' 00:00:00',sep='')
        outputtime=seq(0,modelsteps-1,1)
        var.put.nc(pointoutputnc,'time',outputtime)
        att.put.nc(pointoutputnc,'time',"long_name","NC_CHAR","Time values")
        att.put.nc(pointoutputnc,'time',"units","NC_CHAR",outputtimeunits)
        att.put.nc(pointoutputnc,'time',"calendar","NC_CHAR","Standard")
        
        # Put static variables
        var.put.nc(pointoutputnc,'x',xvalue)
        att.put.nc(pointoutputnc,'x',"long_name","NC_CHAR","X-coordinate")
        att.put.nc(pointoutputnc,"x","units","NC_CHAR","m")
        att.put.nc(pointoutputnc,"x","axis","NC_CHAR","X")
        var.put.nc(pointoutputnc,'y',yvalue)
        att.put.nc(pointoutputnc,'y',"long_name","NC_CHAR","Y-coordinate")
        att.put.nc(pointoutputnc,"y","units","NC_CHAR","m")
        att.put.nc(pointoutputnc,"y","axis","NC_CHAR","Y")
        var.put.nc(pointoutputnc,'zPoint',outputelevation)
        att.put.nc(pointoutputnc,'zPoint',"long_name","NC_CHAR","Z-coordinate of output point")
        var.put.nc(pointoutputnc,'zGrid',elevation[outputcol,outputrow])
        att.put.nc(pointoutputnc,'zGrid',"long_name","NC_CHAR","Z-coordinate of corresponding grid cell")
        var.put.nc(pointoutputnc,'LAIeff',LAIeff[outputcol,outputrow])
        att.put.nc(pointoutputnc,'LAIeff',"long_name","NC_CHAR","Effective LAI of corresponding grid cell")
        var.put.nc(pointoutputnc,'inCanopy',canopymask[outputcol,outputrow])
        att.put.nc(pointoutputnc,'inCanopy',"long_name","NC_CHAR","Canopy mask of corresponding grid cell")
        var.put.nc(pointoutputnc,crsvar,1)
        
        # If NetCDF input includes projection information
        if (projected == 1) {
          
          for (n in 0:(ncrsattributes - 1)) {
            i=n+1
            
            attributename=as.character(crsattributenames[[i]])
            attributetype=as.character(crsattributetypes[[i]])
            
            # get the element, not a sublist
            attributevalue=crsattributevalues[[i]]
            
            if (attributetype == "NC_CHAR") {
              attributevalue=as.character(attributevalue)
            } else {
              attributevalue=as.numeric(attributevalue)
            }
            
            att.put.nc(pointoutputnc, crsvar, attributename, attributetype, attributevalue)
          }
        }
        
        # Put global attributes (required to georeference data in Panoply)
        att.put.nc(pointoutputnc,"NC_GLOBAL","Conventions","NC_CHAR","CF-1.5")
        
        # Define and put all dynamic variables
        var.def.nc(pointoutputnc,'temp',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'temp',"long_name","NC_CHAR","Air temperature")
        att.put.nc(pointoutputnc,'temp',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'temp',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'temp',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'rh',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'rh',"long_name","NC_CHAR","Air humidity")
        att.put.nc(pointoutputnc,'rh',"units","NC_CHAR",'%')
        att.put.nc(pointoutputnc,'rh',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'rh',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'ws',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'ws',"long_name","NC_CHAR","Wind speed")
        att.put.nc(pointoutputnc,'ws',"units","NC_CHAR",'m/s')
        att.put.nc(pointoutputnc,'ws',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'ws',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'precip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'precip',"long_name","NC_CHAR","Precipitation")
        att.put.nc(pointoutputnc,'precip',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'precip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'precip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'swin',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'swin',"long_name","NC_CHAR","Incoming shortwave radiation")
        att.put.nc(pointoutputnc,'swin',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'swin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'swin',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cloudcover',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cloudcover',"long_name","NC_CHAR","Parameterized cloud cover")
        att.put.nc(pointoutputnc,'cloudcover',"units","NC_CHAR",'0-1')
        att.put.nc(pointoutputnc,'cloudcover',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cloudcover',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'lwin',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'lwin',"long_name","NC_CHAR","Incoming longwave radiation")
        att.put.nc(pointoutputnc,'lwin',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'lwin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'lwin',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'wetbulbtemp',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'wetbulbtemp',"long_name","NC_CHAR","Wet bulb temperature")
        att.put.nc(pointoutputnc,'wetbulbtemp',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'wetbulbtemp',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'wetbulbtemp',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'shareliqprecip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'shareliqprecip',"long_name","NC_CHAR","Share of liquid precipitation")
        att.put.nc(pointoutputnc,'shareliqprecip',"units","NC_CHAR",'0-1')
        att.put.nc(pointoutputnc,'shareliqprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'shareliqprecip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'sharesolprecip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'sharesolprecip',"long_name","NC_CHAR","Share of solid precipitation")
        att.put.nc(pointoutputnc,'sharesolprecip',"units","NC_CHAR",'0-1')
        att.put.nc(pointoutputnc,'sharesolprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'sharesolprecip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'liqprecip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'liqprecip',"long_name","NC_CHAR","Liquid precipitation")
        att.put.nc(pointoutputnc,'liqprecip',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'liqprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'liqprecip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'solprecip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'solprecip',"long_name","NC_CHAR","Solid precipitation")
        att.put.nc(pointoutputnc,'solprecip',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'solprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'solprecip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'snowage',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'snowage',"long_name","NC_CHAR","Snow age")
        att.put.nc(pointoutputnc,'snowage',"units","NC_CHAR",'days')
        att.put.nc(pointoutputnc,'snowage',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'snowage',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'snowalb',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'snowalb',"long_name","NC_CHAR","Snow albedo")
        att.put.nc(pointoutputnc,'snowalb',"units","NC_CHAR",'0-1')
        att.put.nc(pointoutputnc,'snowalb',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'snowalb',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'snowtemp',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'snowtemp',"long_name","NC_CHAR","Snow temperature")
        att.put.nc(pointoutputnc,'snowtemp',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'snowtemp',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'snowtemp',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'vappresair',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'vappresair',"long_name","NC_CHAR","Vapor pressure of the air")
        att.put.nc(pointoutputnc,'vappresair',"units","NC_CHAR",'hPa')
        att.put.nc(pointoutputnc,'vappresair',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'vappresair',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'vappressnow',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'vappressnow',"long_name","NC_CHAR","Vapor pressure over the snow cover")
        att.put.nc(pointoutputnc,'vappressnow',"units","NC_CHAR",'hPa')
        att.put.nc(pointoutputnc,'vappressnow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'vappressnow',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'swradbal',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'swradbal',"long_name","NC_CHAR","Shortwave radiation balance")
        att.put.nc(pointoutputnc,'swradbal',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'swradbal',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'swradbal',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'lwradbal',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'lwradbal',"long_name","NC_CHAR","Longwave radiation balance")
        att.put.nc(pointoutputnc,'lwradbal',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'lwradbal',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'lwradbal',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'latflux',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'latflux',"long_name","NC_CHAR","Latent heat flux")
        att.put.nc(pointoutputnc,'latflux',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'latflux',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'latflux',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'sensflux',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'sensflux',"long_name","NC_CHAR","Sensible heat flux")
        att.put.nc(pointoutputnc,'sensflux',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'sensflux',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'sensflux',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'advfluxliq',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'advfluxliq',"long_name","NC_CHAR","Advective flux from liquid precipitation")
        att.put.nc(pointoutputnc,'advfluxliq',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'advfluxliq',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'advfluxliq',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'advfluxsol',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'advfluxsol',"long_name","NC_CHAR","Advective flux from solid precipitation")
        att.put.nc(pointoutputnc,'advfluxsol',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'advfluxsol',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'advfluxsol',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'ebalsnow',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'ebalsnow',"long_name","NC_CHAR","Energy balance of the snow cover")
        att.put.nc(pointoutputnc,'ebalsnow',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'ebalsnow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'ebalsnow',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'potmelt',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'potmelt',"long_name","NC_CHAR","Potential snow melt")
        att.put.nc(pointoutputnc,'potmelt',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'potmelt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'potmelt',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'coldcontmm',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'coldcontmm',"long_name","NC_CHAR","Cold content of the snow cover")
        att.put.nc(pointoutputnc,'coldcontmm',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'coldcontmm',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'coldcontmm',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'sublim',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'sublim',"long_name","NC_CHAR","Sublimation of snow")
        att.put.nc(pointoutputnc,'sublim',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'sublim',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'sublim',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'melt',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'melt',"long_name","NC_CHAR","Actual snow melt")
        att.put.nc(pointoutputnc,'melt',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'melt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'melt',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'refreeze',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'refreeze',"long_name","NC_CHAR","Refreezing liquid water")
        att.put.nc(pointoutputnc,'refreeze',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'refreeze',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'refreeze',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'liqwatcont',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'liqwatcont',"long_name","NC_CHAR","Liquid water content")
        att.put.nc(pointoutputnc,'liqwatcont',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'liqwatcont',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'liqwatcont',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'outflow',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'outflow',"long_name","NC_CHAR","Melt water outflow")
        att.put.nc(pointoutputnc,'outflow',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'outflow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'outflow',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'swe',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'swe',"long_name","NC_CHAR","Simulated snow water equivalent")
        att.put.nc(pointoutputnc,'swe',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'swe',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'swe',"grid_mapping","NC_CHAR",crsvar) 
        
        var.def.nc(pointoutputnc,'snowdens',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'snowdens',"long_name","NC_CHAR","Simulated snow density")
        att.put.nc(pointoutputnc,'snowdens',"units","NC_CHAR",'kg/m3')
        att.put.nc(pointoutputnc,'snowdens',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'snowdens',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'snowdepth',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'snowdepth',"long_name","NC_CHAR","Simulated snow depth")
        att.put.nc(pointoutputnc,'snowdepth',"units","NC_CHAR",'m')
        att.put.nc(pointoutputnc,'snowdepth',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'snowdepth',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'radfrac',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'radfrac',"long_name","NC_CHAR","Fraction of incoming shortwave radiation reaching the surface")
        att.put.nc(pointoutputnc,'radfrac',"units","NC_CHAR",'0-1')
        att.put.nc(pointoutputnc,'radfrac',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'radfrac',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canfrac',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canfrac',"long_name","NC_CHAR","Canopy fraction")
        att.put.nc(pointoutputnc,'canfrac',"units","NC_CHAR",'0-1')
        att.put.nc(pointoutputnc,'canfrac',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canfrac',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canflowindex',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canflowindex',"long_name","NC_CHAR","Canopy flow index")
        att.put.nc(pointoutputnc,'canflowindex',"units","NC_CHAR",'-')
        att.put.nc(pointoutputnc,'canflowindex',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canflowindex',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canswin',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canswin',"long_name","NC_CHAR","Inside canopy incoming shortwave radiation")
        att.put.nc(pointoutputnc,'canswin',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canswin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canswin',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canlwin',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canlwin',"long_name","NC_CHAR","Inside canopy incoming longwave radiation")
        att.put.nc(pointoutputnc,'canlwin',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canlwin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canlwin',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canws',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canws',"long_name","NC_CHAR","Inside canopy wind speed")
        att.put.nc(pointoutputnc,'canws',"units","NC_CHAR",'m/s')
        att.put.nc(pointoutputnc,'canws',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canws',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canrh',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canrh',"long_name","NC_CHAR","Inside canopy air humidity")
        att.put.nc(pointoutputnc,'canrh',"units","NC_CHAR",'%')
        att.put.nc(pointoutputnc,'canrh',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canrh',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'tmin',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'tmin',"long_name","NC_CHAR","Outside canopy air temperature minimum")
        att.put.nc(pointoutputnc,'tmin',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'tmin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'tmin',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'tmax',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'tmax',"long_name","NC_CHAR","Outside canopy air temperature maximum")
        att.put.nc(pointoutputnc,'tmax',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'tmax',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'tmax',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'tmean',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'tmean',"long_name","NC_CHAR","Outside canopy air temperature mean")
        att.put.nc(pointoutputnc,'tmean',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'tmean',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'tmean',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'deltat',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'deltat',"long_name","NC_CHAR","Inside canopy air temperature difference")
        att.put.nc(pointoutputnc,'deltat',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'deltat',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'deltat',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canswabs',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canswabs',"long_name","NC_CHAR","Inside canopy shortwave radiation absorption by snow particle")
        att.put.nc(pointoutputnc,'canswabs',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canswabs',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canswabs',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'reynolds',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'reynolds',"long_name","NC_CHAR","Reynolds number")
        att.put.nc(pointoutputnc,'reynolds',"units","NC_CHAR",'-')
        att.put.nc(pointoutputnc,'reynolds',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'reynolds',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'nusselt',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'nusselt',"long_name","NC_CHAR","Nusselt number")
        att.put.nc(pointoutputnc,'nusselt',"units","NC_CHAR",'-')
        att.put.nc(pointoutputnc,'nusselt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'nusselt',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'sherwood',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'sherwood',"long_name","NC_CHAR","Sherwood number")
        att.put.nc(pointoutputnc,'sherwood',"units","NC_CHAR",'-')
        att.put.nc(pointoutputnc,'sherwood',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'sherwood',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansatvap',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansatvap',"long_name","NC_CHAR","Inside canopy saturation vapor pressure over ice")
        att.put.nc(pointoutputnc,'cansatvap',"units","NC_CHAR",'hPa')
        att.put.nc(pointoutputnc,'cansatvap',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansatvap',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'satdensvap',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'satdensvap',"long_name","NC_CHAR","Saturation density of water vapor")
        att.put.nc(pointoutputnc,'satdensvap',"units","NC_CHAR",'kg/m3')
        att.put.nc(pointoutputnc,'satdensvap',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'satdensvap',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'watvapdif',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'watvapdif',"long_name","NC_CHAR","Diffusivity of water vapor in the atmosphere")
        att.put.nc(pointoutputnc,'watvapdif',"units","NC_CHAR",'m2/s')
        att.put.nc(pointoutputnc,'watvapdif',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'watvapdif',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'omega',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'omega',"long_name","NC_CHAR","Omega")
        att.put.nc(pointoutputnc,'omega',"units","NC_CHAR",'-')
        att.put.nc(pointoutputnc,'omega',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'omega',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'ratemassloss',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'ratemassloss',"long_name","NC_CHAR","Rate of mass loss")
        att.put.nc(pointoutputnc,'ratemassloss',"units","NC_CHAR",'kg/s')
        att.put.nc(pointoutputnc,'ratemassloss',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'ratemassloss',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'sublimlosscoeff',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'sublimlosscoeff',"long_name","NC_CHAR","Sublimation loss rate coefficient")
        att.put.nc(pointoutputnc,'sublimlosscoeff',"units","NC_CHAR",'1/s')
        att.put.nc(pointoutputnc,'sublimlosscoeff',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'sublimlosscoeff',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'maxintercept',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'maxintercept',"long_name","NC_CHAR","Maximum snow interception")
        att.put.nc(pointoutputnc,'maxintercept',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'maxintercept',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'maxintercept',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'snowintercept',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'snowintercept',"long_name","NC_CHAR","Snow interception")
        att.put.nc(pointoutputnc,'snowintercept',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'snowintercept',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'snowintercept',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'caninterceptload',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'caninterceptload',"long_name","NC_CHAR","Intercepted snow load")
        att.put.nc(pointoutputnc,'caninterceptload',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'caninterceptload',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'caninterceptload',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canexposcoeff',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canexposcoeff',"long_name","NC_CHAR","Coefficient related to the shape of the intercepted snow deposits")
        att.put.nc(pointoutputnc,'canexposcoeff',"units","NC_CHAR",'-')
        att.put.nc(pointoutputnc,'canexposcoeff',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canexposcoeff',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'treesublim',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'treesublim',"long_name","NC_CHAR","Sublimation of snow from trees")
        att.put.nc(pointoutputnc,'treesublim',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'treesublim',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'treesublim',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'treemelt',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'treemelt',"long_name","NC_CHAR","Melt of snow from trees")
        att.put.nc(pointoutputnc,'treemelt',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'treemelt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'treemelt',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'groundliqprecip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'groundliqprecip',"long_name","NC_CHAR","Inside canopy liquid precipitation on the ground")
        att.put.nc(pointoutputnc,'groundliqprecip',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'groundliqprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'groundliqprecip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'groundsolprecip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'groundsolprecip',"long_name","NC_CHAR","Inside canopy solid precipitation on the ground")
        att.put.nc(pointoutputnc,'groundsolprecip',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'groundsolprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'groundsolprecip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'throughsolprecip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'throughsolprecip',"long_name","NC_CHAR","Inside canopy throughfall of solid precipitation")
        att.put.nc(pointoutputnc,'throughsolprecip',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'throughsolprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'throughsolprecip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansnowage',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansnowage',"long_name","NC_CHAR","Inside canopy age of snow")
        att.put.nc(pointoutputnc,'cansnowage',"units","NC_CHAR",'days')
        att.put.nc(pointoutputnc,'cansnowage',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansnowage',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansnowalb',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansnowalb',"long_name","NC_CHAR","Inside canopy snow albedo")
        att.put.nc(pointoutputnc,'cansnowalb',"units","NC_CHAR",'0-1')
        att.put.nc(pointoutputnc,'cansnowalb',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansnowalb',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansnowtemp',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansnowtemp',"long_name","NC_CHAR","Inside canopy snow temperature")
        att.put.nc(pointoutputnc,'cansnowtemp',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'cansnowtemp',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansnowtemp',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansnowdeltat',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansnowdeltat',"long_name","NC_CHAR","Inside canopy snow temperature change")
        att.put.nc(pointoutputnc,'cansnowdeltat',"units","NC_CHAR",'K')
        att.put.nc(pointoutputnc,'cansnowdeltat',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansnowdeltat',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'groundprecip',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'groundprecip',"long_name","NC_CHAR","Inside canopy total precipitation on the ground")
        att.put.nc(pointoutputnc,'groundprecip',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'groundprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'groundprecip',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canvappresair',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canvappresair',"long_name","NC_CHAR","Inside canopy vapor pressure of the air")
        att.put.nc(pointoutputnc,'canvappresair',"units","NC_CHAR",'hPa')
        att.put.nc(pointoutputnc,'canvappresair',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canvappresair',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canvappressnow',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canvappressnow',"long_name","NC_CHAR","Inside canopy vapor pressure over the snow cover")
        att.put.nc(pointoutputnc,'canvappressnow',"units","NC_CHAR",'hPa')
        att.put.nc(pointoutputnc,'canvappressnow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canvappressnow',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canlatflux',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canlatflux',"long_name","NC_CHAR","Inside canopy latent heat flux")
        att.put.nc(pointoutputnc,'canlatflux',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canlatflux',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canlatflux',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canswradbal',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canswradbal',"long_name","NC_CHAR","Inside canopy shortwave radiation balance")
        att.put.nc(pointoutputnc,'canswradbal',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canswradbal',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canswradbal',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canlwradbal',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canlwradbal',"long_name","NC_CHAR","Inside canopy longwave radiation balance")
        att.put.nc(pointoutputnc,'canlwradbal',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canlwradbal',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canlwradbal',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansensflux',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansensflux',"long_name","NC_CHAR","Inside canopy sensible heat flux")
        att.put.nc(pointoutputnc,'cansensflux',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'cansensflux',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansensflux',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canadvfluxliq',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canadvfluxliq',"long_name","NC_CHAR","Inside canopy advective flux from liquid precipitation")
        att.put.nc(pointoutputnc,'canadvfluxliq',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canadvfluxliq',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canadvfluxliq',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canadvfluxsol',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canadvfluxsol',"long_name","NC_CHAR","Inside canopy advective flux from solid precipitation")
        att.put.nc(pointoutputnc,'canadvfluxsol',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canadvfluxsol',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canadvfluxsol',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canebalsnow',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canebalsnow',"long_name","NC_CHAR","Inside canopy energy balance of the snow cover")
        att.put.nc(pointoutputnc,'canebalsnow',"units","NC_CHAR",'W/m2')
        att.put.nc(pointoutputnc,'canebalsnow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canebalsnow',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cancoldcontmm',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cancoldcontmm',"long_name","NC_CHAR","Inside canopy cold content of the snow cover")
        att.put.nc(pointoutputnc,'cancoldcontmm',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'cancoldcontmm',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cancoldcontmm',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansublim',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansublim',"long_name","NC_CHAR","Inside canopy sublimation of snow")
        att.put.nc(pointoutputnc,'cansublim',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'cansublim',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansublim',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canpotmelt',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canpotmelt',"long_name","NC_CHAR","Inside canopy potential snow melt")
        att.put.nc(pointoutputnc,'canpotmelt',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'canpotmelt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canpotmelt',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canmelt',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canmelt',"long_name","NC_CHAR","Inside canopy snow melt")
        att.put.nc(pointoutputnc,'canmelt',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'canmelt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canmelt',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canrefreeze',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canrefreeze',"long_name","NC_CHAR","Inside canopy refreezing liquid water")
        att.put.nc(pointoutputnc,'canrefreeze',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'canrefreeze',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canrefreeze',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canliqwatcont',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canliqwatcont',"long_name","NC_CHAR","Inside canopy liquid water content")
        att.put.nc(pointoutputnc,'canliqwatcont',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'canliqwatcont',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canliqwatcont',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canoutflow',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canoutflow',"long_name","NC_CHAR","Inside canoy melt water outflow")
        att.put.nc(pointoutputnc,'canoutflow',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'canoutflow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canoutflow',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'canswe',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'canswe',"long_name","NC_CHAR","Inside canopy simulated snow water equivalent")
        att.put.nc(pointoutputnc,'canswe',"units","NC_CHAR",'mm')
        att.put.nc(pointoutputnc,'canswe',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'canswe',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansnowdens',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansnowdens',"long_name","NC_CHAR","Inside canopy simulated snow density")
        att.put.nc(pointoutputnc,'cansnowdens',"units","NC_CHAR",'kg/m3')
        att.put.nc(pointoutputnc,'cansnowdens',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansnowdens',"grid_mapping","NC_CHAR",crsvar)
        
        var.def.nc(pointoutputnc,'cansnowdepth',outputprecision,c(tdim))
        att.put.nc(pointoutputnc,'cansnowdepth',"long_name","NC_CHAR","Inside canopy simulated snow depth")
        att.put.nc(pointoutputnc,'cansnowdepth',"units","NC_CHAR",'m')
        att.put.nc(pointoutputnc,'cansnowdepth',"_FillValue",outputprecision,fillvalue)
        att.put.nc(pointoutputnc,'cansnowdepth',"grid_mapping","NC_CHAR",crsvar)
      }
    }  
    
    # For the first timestep of every year prepare NetCDF files for gridded output
    if (timestepyear==1) {  
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Gridded output
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Print out progress message
      print('-> Preparing NetCDF files for gridded output at 1st timestep...')
      
      # Open NetCDF file for outputs
      outputfile=paste(outputpath,modelyear,'_',escimooutput,sep='')
      gridoutputnc=create.nc(outputfile,clobber=TRUE,share=FALSE,prefill=TRUE,format='netcdf4')
      
      # Define dimensions
      tdim=dim.def.nc(gridoutputnc,'time',yearsteps)
      xdim=dim.def.nc(gridoutputnc,'x',ncols)
      ydim=dim.def.nc(gridoutputnc,'y',nrows)
      
      # Define grid variables
      var.def.nc(gridoutputnc,'x',"NC_DOUBLE",c(xdim))
      var.def.nc(gridoutputnc,'y',"NC_DOUBLE",c(ydim))
      var.def.nc(gridoutputnc,'time',"NC_DOUBLE",c(tdim))
      var.def.nc(gridoutputnc,crsvar,"NC_INT",1L)
      
      # Put coordinate variable
      var.put.nc(gridoutputnc,'x',xcoords)
      att.put.nc(gridoutputnc,'x',"long_name","NC_CHAR","X-coordinate")
      att.put.nc(gridoutputnc,"x","units","NC_CHAR","m")
      att.put.nc(gridoutputnc,"x","axis","NC_CHAR","X")
      var.put.nc(gridoutputnc,'y',ycoords)
      att.put.nc(gridoutputnc,'y',"long_name","NC_CHAR","Y-coordinate")
      att.put.nc(gridoutputnc,"y","units","NC_CHAR","m")
      att.put.nc(gridoutputnc,"y","axis","NC_CHAR","Y")
      
      # Define time variable
      outputtimeunits=paste('hours since ',as.Date(modeldatetime),' 00:00:00',sep='')
      outputtime=seq(0,yearsteps-1,1)
      
      # Put time variable
      var.put.nc(gridoutputnc,'time',outputtime)
      att.put.nc(gridoutputnc,'time',"long_name","NC_CHAR","Time values")
      att.put.nc(gridoutputnc,'time',"units","NC_CHAR",outputtimeunits)
      att.put.nc(gridoutputnc,'time',"calendar","NC_CHAR","Standard")
   
      # If NetCDF input includes projection information
      if (projected == 1) {
        
        for (n in 0:(ncrsattributes - 1)) {
          i=n+1
          
          attributename=as.character(crsattributenames[[i]])
          attributetype=as.character(crsattributetypes[[i]])
          
          # get the element, not a sublist
          attributevalue=crsattributevalues[[i]]
          
          if (attributetype == "NC_CHAR") {
            attributevalue=as.character(attributevalue)
          } else {
            attributevalue=as.numeric(attributevalue)
          }
          
          att.put.nc(gridoutputnc, crsvar, attributename, attributetype, attributevalue)
        }
      }
      
      # Put global attributes (required to georeference data in Panoply)
      att.put.nc(gridoutputnc,"NC_GLOBAL","Conventions","NC_CHAR","CF-1.5")
      
      # Put selected static variables to file
      if (canopymaskout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canopymask',outputprecision,c(xdim,ydim))
        att.put.nc(gridoutputnc,'canopymask',"long_name","NC_CHAR","Canopy mask")
        att.put.nc(gridoutputnc,'canopymask',"units","NC_CHAR",'0/1')
        att.put.nc(gridoutputnc,'canopymask',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canopymask',"grid_mapping","NC_CHAR",crsvar)
        var.put.nc(gridoutputnc,'canopymask',canopymask,na.mode=4,pack=FALSE)
      }
      if (outputmaskout==1 | allout==1) {
        var.def.nc(gridoutputnc,'outputmask',outputprecision,c(xdim,ydim))
        att.put.nc(gridoutputnc,'outputmask',"long_name","NC_CHAR","Output mask")
        att.put.nc(gridoutputnc,'outputmask',"units","NC_CHAR",'0/1')
        att.put.nc(gridoutputnc,'outputmask',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'outputmask',"grid_mapping","NC_CHAR",crsvar)
        var.put.nc(gridoutputnc,'outputmask',outputmask,na.mode=4,pack=FALSE)
      }
      
      # Put selected dynamic variables to file
      if (tempout==1 | allout==1) {
        var.def.nc(gridoutputnc,'temp',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'temp',"long_name","NC_CHAR","Air temperature")
        att.put.nc(gridoutputnc,'temp',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'temp',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'temp',"grid_mapping","NC_CHAR",crsvar)
      }
      if (rhout==1 | allout==1) {
        var.def.nc(gridoutputnc,'rh',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'rh',"long_name","NC_CHAR","Air humidity")
        att.put.nc(gridoutputnc,'rh',"units","NC_CHAR",'%')
        att.put.nc(gridoutputnc,'rh',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'rh',"grid_mapping","NC_CHAR",crsvar)
      }
      if (wsout==1 | allout==1) {
        var.def.nc(gridoutputnc,'ws',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'ws',"long_name","NC_CHAR","Wind speed")
        att.put.nc(gridoutputnc,'ws',"units","NC_CHAR",'m/s')
        att.put.nc(gridoutputnc,'ws',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'ws',"grid_mapping","NC_CHAR",crsvar)
      }
      if (precipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'precip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'precip',"long_name","NC_CHAR","Precipitation")
        att.put.nc(gridoutputnc,'precip',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'precip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'precip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (swinout==1 | allout==1) {
        var.def.nc(gridoutputnc,'swin',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'swin',"long_name","NC_CHAR","Incoming shortwave radiation")
        att.put.nc(gridoutputnc,'swin',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'swin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'swin',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cloudcoverout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cloudcover',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cloudcover',"long_name","NC_CHAR","Parameterized cloud cover")
        att.put.nc(gridoutputnc,'cloudcover',"units","NC_CHAR",'0-1')
        att.put.nc(gridoutputnc,'cloudcover',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cloudcover',"grid_mapping","NC_CHAR",crsvar)
      }
      if (lwinout==1 | allout==1) {
        var.def.nc(gridoutputnc,'lwin',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'lwin',"long_name","NC_CHAR","Incoming longwave radiation")
        att.put.nc(gridoutputnc,'lwin',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'lwin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'lwin',"grid_mapping","NC_CHAR",crsvar)
      }
      if (wetbulbtempout==1 | allout==1) {
        var.def.nc(gridoutputnc,'wetbulbtemp',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'wetbulbtemp',"long_name","NC_CHAR","Wet bulb temperature")
        att.put.nc(gridoutputnc,'wetbulbtemp',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'wetbulbtemp',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'wetbulbtemp',"grid_mapping","NC_CHAR",crsvar)
      }
      if (shareliqprecipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'shareliqprecip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'shareliqprecip',"long_name","NC_CHAR","Share of liquid precipitation")
        att.put.nc(gridoutputnc,'shareliqprecip',"units","NC_CHAR",'0-1')
        att.put.nc(gridoutputnc,'shareliqprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'shareliqprecip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (sharesolprecipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'sharesolprecip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'sharesolprecip',"long_name","NC_CHAR","Share of solid precipitation")
        att.put.nc(gridoutputnc,'sharesolprecip',"units","NC_CHAR",'0-1')
        att.put.nc(gridoutputnc,'sharesolprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'sharesolprecip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (liqprecipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'liqprecip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'liqprecip',"long_name","NC_CHAR","Liquid precipitation")
        att.put.nc(gridoutputnc,'liqprecip',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'liqprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'liqprecip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (solprecipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'solprecip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'solprecip',"long_name","NC_CHAR","Solid precipitation")
        att.put.nc(gridoutputnc,'solprecip',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'solprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'solprecip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (snowageout==1 | allout==1) {
        var.def.nc(gridoutputnc,'snowage',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'snowage',"long_name","NC_CHAR","Snow age")
        att.put.nc(gridoutputnc,'snowage',"units","NC_CHAR",'days')
        att.put.nc(gridoutputnc,'snowage',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'snowage',"grid_mapping","NC_CHAR",crsvar)
      }
      if (snowalbout==1 | allout==1) {
        var.def.nc(gridoutputnc,'snowalb',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'snowalb',"long_name","NC_CHAR","Snow albedo")
        att.put.nc(gridoutputnc,'snowalb',"units","NC_CHAR",'0-1')
        att.put.nc(gridoutputnc,'snowalb',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'snowalb',"grid_mapping","NC_CHAR",crsvar)
      }
      if (snowtempout==1 | allout==1) {
        var.def.nc(gridoutputnc,'snowtemp',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'snowtemp',"long_name","NC_CHAR","Snow temperature")
        att.put.nc(gridoutputnc,'snowtemp',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'snowtemp',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'snowtemp',"grid_mapping","NC_CHAR",crsvar)
      }
      if (vappresairout==1 | allout==1) {
        var.def.nc(gridoutputnc,'vappresair',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'vappresair',"long_name","NC_CHAR","Vapor pressure of the air")
        att.put.nc(gridoutputnc,'vappresair',"units","NC_CHAR",'hPa')
        att.put.nc(gridoutputnc,'vappresair',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'vappresair',"grid_mapping","NC_CHAR",crsvar)
      }
      if (vappressnowout==1 | allout==1) {
        var.def.nc(gridoutputnc,'vappressnow',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'vappressnow',"long_name","NC_CHAR","Vapor pressure over the snow cover")
        att.put.nc(gridoutputnc,'vappressnow',"units","NC_CHAR",'hPa')
        att.put.nc(gridoutputnc,'vappressnow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'vappressnow',"grid_mapping","NC_CHAR",crsvar)
      }
      if (swradbalout==1 | allout==1) {
        var.def.nc(gridoutputnc,'swradbal',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'swradbal',"long_name","NC_CHAR","Shortwave radiation balance")
        att.put.nc(gridoutputnc,'swradbal',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'swradbal',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'swradbal',"grid_mapping","NC_CHAR",crsvar)
      }
      if (lwradbalout==1 | allout==1) {
        var.def.nc(gridoutputnc,'lwradbal',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'lwradbal',"long_name","NC_CHAR","Longwave radiation balance")
        att.put.nc(gridoutputnc,'lwradbal',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'lwradbal',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'lwradbal',"grid_mapping","NC_CHAR",crsvar)
      }
      if (latfluxout==1 | allout==1) {
        var.def.nc(gridoutputnc,'latflux',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'latflux',"long_name","NC_CHAR","Latent heat flux")
        att.put.nc(gridoutputnc,'latflux',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'latflux',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'latflux',"grid_mapping","NC_CHAR",crsvar)
      }
      if (sensfluxout==1 | allout==1) {
        var.def.nc(gridoutputnc,'sensflux',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'sensflux',"long_name","NC_CHAR","Sensible heat flux")
        att.put.nc(gridoutputnc,'sensflux',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'sensflux',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'sensflux',"grid_mapping","NC_CHAR",crsvar)
      }
      if (advfluxliqout==1 | allout==1) {
        var.def.nc(gridoutputnc,'advfluxliq',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'advfluxliq',"long_name","NC_CHAR","Advective flux from liquid precipitation")
        att.put.nc(gridoutputnc,'advfluxliq',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'advfluxliq',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'advfluxliq',"grid_mapping","NC_CHAR",crsvar)
      }
      if (advfluxsolout==1 | allout==1) {
        var.def.nc(gridoutputnc,'advfluxsol',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'advfluxsol',"long_name","NC_CHAR","Advective flux from solid precipitation")
        att.put.nc(gridoutputnc,'advfluxsol',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'advfluxsol',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'advfluxsol',"grid_mapping","NC_CHAR",crsvar)
      }
      if (ebalsnowout==1 | allout==1) {
        var.def.nc(gridoutputnc,'ebalsnow',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'ebalsnow',"long_name","NC_CHAR","Energy balance of the snow cover")
        att.put.nc(gridoutputnc,'ebalsnow',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'ebalsnow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'ebalsnow',"grid_mapping","NC_CHAR",crsvar)
      }
      if (potmeltout==1 | allout==1) {
        var.def.nc(gridoutputnc,'potmelt',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'potmelt',"long_name","NC_CHAR","Potential snow melt")
        att.put.nc(gridoutputnc,'potmelt',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'potmelt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'potmelt',"grid_mapping","NC_CHAR",crsvar)
      }
      if (coldcontmmout==1 | allout==1) {
        var.def.nc(gridoutputnc,'coldcontmm',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'coldcontmm',"long_name","NC_CHAR","Cold content of the snow cover")
        att.put.nc(gridoutputnc,'coldcontmm',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'coldcontmm',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'coldcontmm',"grid_mapping","NC_CHAR",crsvar)
      }
      if (sublimout==1 | allout==1) {
        var.def.nc(gridoutputnc,'sublim',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'sublim',"long_name","NC_CHAR","Sublimation of snow")
        att.put.nc(gridoutputnc,'sublim',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'sublim',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'sublim',"grid_mapping","NC_CHAR",crsvar)
      }
      if (meltout==1 | allout==1) {
        var.def.nc(gridoutputnc,'melt',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'melt',"long_name","NC_CHAR","Actual snow melt")
        att.put.nc(gridoutputnc,'melt',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'melt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'melt',"grid_mapping","NC_CHAR",crsvar)
      }
      if (refreezeout==1 | allout==1) {
        var.def.nc(gridoutputnc,'refreeze',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'refreeze',"long_name","NC_CHAR","Refreezing liquid water")
        att.put.nc(gridoutputnc,'refreeze',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'refreeze',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'refreeze',"grid_mapping","NC_CHAR",crsvar)
      }
      if (liqwatcontout==1 | allout==1) {
        var.def.nc(gridoutputnc,'liqwatcont',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'liqwatcont',"long_name","NC_CHAR","Liquid water content")
        att.put.nc(gridoutputnc,'liqwatcont',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'liqwatcont',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'liqwatcont',"grid_mapping","NC_CHAR",crsvar)
      }
      if (outflowout==1 | allout==1) {
        var.def.nc(gridoutputnc,'outflow',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'outflow',"long_name","NC_CHAR","Melt water outflow")
        att.put.nc(gridoutputnc,'outflow',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'outflow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'outflow',"grid_mapping","NC_CHAR",crsvar)
      }
      if (sweout==1 | allout==1) {
        var.def.nc(gridoutputnc,'swe',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'swe',"long_name","NC_CHAR","Simulated snow water equivalent")
        att.put.nc(gridoutputnc,'swe',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'swe',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'swe',"grid_mapping","NC_CHAR",crsvar) 
      }
      if (snowdensout==1 | allout==1) {
        var.def.nc(gridoutputnc,'snowdens',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'snowdens',"long_name","NC_CHAR","Simulated snow density")
        att.put.nc(gridoutputnc,'snowdens',"units","NC_CHAR",'kg/m3')
        att.put.nc(gridoutputnc,'snowdens',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'snowdens',"grid_mapping","NC_CHAR",crsvar)
      }
      if (snowdepthout==1 | allout==1) {
        var.def.nc(gridoutputnc,'snowdepth',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'snowdepth',"long_name","NC_CHAR","Simulated snow depth")
        att.put.nc(gridoutputnc,'snowdepth',"units","NC_CHAR",'m')
        att.put.nc(gridoutputnc,'snowdepth',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'snowdepth',"grid_mapping","NC_CHAR",crsvar)
      }
      if (radfracout==1 | allout==1) {
        var.def.nc(gridoutputnc,'radfrac',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'radfrac',"long_name","NC_CHAR","Fraction of incoming shortwave radiation reaching the surface")
        att.put.nc(gridoutputnc,'radfrac',"units","NC_CHAR",'0-1')
        att.put.nc(gridoutputnc,'radfrac',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'radfrac',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canfracout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canfrac',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canfrac',"long_name","NC_CHAR","Canopy fraction")
        att.put.nc(gridoutputnc,'canfrac',"units","NC_CHAR",'0-1')
        att.put.nc(gridoutputnc,'canfrac',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canfrac',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canflowindexout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canflowindex',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canflowindex',"long_name","NC_CHAR","Canopy flow index")
        att.put.nc(gridoutputnc,'canflowindex',"units","NC_CHAR",'-')
        att.put.nc(gridoutputnc,'canflowindex',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canflowindex',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canswinout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canswin',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canswin',"long_name","NC_CHAR","Inside canopy incoming shortwave radiation")
        att.put.nc(gridoutputnc,'canswin',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canswin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canswin',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canlwinout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canlwin',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canlwin',"long_name","NC_CHAR","Inside canopy incoming longwave radiation")
        att.put.nc(gridoutputnc,'canlwin',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canlwin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canlwin',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canwsout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canws',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canws',"long_name","NC_CHAR","Inside canopy wind speed")
        att.put.nc(gridoutputnc,'canws',"units","NC_CHAR",'m/s')
        att.put.nc(gridoutputnc,'canws',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canws',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canrhout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canrh',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canrh',"long_name","NC_CHAR","Inside canopy air humidity")
        att.put.nc(gridoutputnc,'canrh',"units","NC_CHAR",'%')
        att.put.nc(gridoutputnc,'canrh',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canrh',"grid_mapping","NC_CHAR",crsvar)
      }
      if (tminout==1 | allout==1) {
        var.def.nc(gridoutputnc,'tmin',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'tmin',"long_name","NC_CHAR","Outside canopy air temperature minimum")
        att.put.nc(gridoutputnc,'tmin',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'tmin',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'tmin',"grid_mapping","NC_CHAR",crsvar)
      }
      if (tmaxout==1 | allout==1) {
        var.def.nc(gridoutputnc,'tmax',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'tmax',"long_name","NC_CHAR","Outside canopy air temperature maximum")
        att.put.nc(gridoutputnc,'tmax',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'tmax',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'tmax',"grid_mapping","NC_CHAR",crsvar)
      }
      if (tmeanout==1 | allout==1) {
        var.def.nc(gridoutputnc,'tmean',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'tmean',"long_name","NC_CHAR","Outside canopy air temperature mean")
        att.put.nc(gridoutputnc,'tmean',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'tmean',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'tmean',"grid_mapping","NC_CHAR",crsvar)
      }
      if (deltatout==1 | allout==1) {
        var.def.nc(gridoutputnc,'deltat',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'deltat',"long_name","NC_CHAR","Inside canopy air temperature difference")
        att.put.nc(gridoutputnc,'deltat',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'deltat',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'deltat',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canswabsout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canswabs',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canswabs',"long_name","NC_CHAR","Inside canopy shortwave radiation absorption by snow particle")
        att.put.nc(gridoutputnc,'canswabs',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canswabs',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canswabs',"grid_mapping","NC_CHAR",crsvar)
      }
      if (reynoldsout==1 | allout==1) {
        var.def.nc(gridoutputnc,'reynolds',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'reynolds',"long_name","NC_CHAR","Reynolds number")
        att.put.nc(gridoutputnc,'reynolds',"units","NC_CHAR",'-')
        att.put.nc(gridoutputnc,'reynolds',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'reynolds',"grid_mapping","NC_CHAR",crsvar)
      }
      if (nusseltout==1 | allout==1) {
        var.def.nc(gridoutputnc,'nusselt',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'nusselt',"long_name","NC_CHAR","Nusselt number")
        att.put.nc(gridoutputnc,'nusselt',"units","NC_CHAR",'-')
        att.put.nc(gridoutputnc,'nusselt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'nusselt',"grid_mapping","NC_CHAR",crsvar)
      }
      if (sherwoodout==1 | allout==1) {
        var.def.nc(gridoutputnc,'sherwood',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'sherwood',"long_name","NC_CHAR","Sherwood number")
        att.put.nc(gridoutputnc,'sherwood',"units","NC_CHAR",'-')
        att.put.nc(gridoutputnc,'sherwood',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'sherwood',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansatvapout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansatvap',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansatvap',"long_name","NC_CHAR","Inside canopy saturation vapor pressure over ice")
        att.put.nc(gridoutputnc,'cansatvap',"units","NC_CHAR",'hPa')
        att.put.nc(gridoutputnc,'cansatvap',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansatvap',"grid_mapping","NC_CHAR",crsvar)
      }
      if (satdensvapout==1 | allout==1) {
        var.def.nc(gridoutputnc,'satdensvap',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'satdensvap',"long_name","NC_CHAR","Saturation density of water vapor")
        att.put.nc(gridoutputnc,'satdensvap',"units","NC_CHAR",'kg/m3')
        att.put.nc(gridoutputnc,'satdensvap',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'satdensvap',"grid_mapping","NC_CHAR",crsvar)
      }
      if (watvapdifout==1 | allout==1) {
        var.def.nc(gridoutputnc,'watvapdif',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'watvapdif',"long_name","NC_CHAR","Diffusivity of water vapor in the atmosphere")
        att.put.nc(gridoutputnc,'watvapdif',"units","NC_CHAR",'m2/s')
        att.put.nc(gridoutputnc,'watvapdif',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'watvapdif',"grid_mapping","NC_CHAR",crsvar)
      }
      if (omegaout==1 | allout==1) {
        var.def.nc(gridoutputnc,'omega',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'omega',"long_name","NC_CHAR","Omega")
        att.put.nc(gridoutputnc,'omega',"units","NC_CHAR",'-')
        att.put.nc(gridoutputnc,'omega',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'omega',"grid_mapping","NC_CHAR",crsvar)
      }
      if (ratemasslossout==1 | allout==1) {
        var.def.nc(gridoutputnc,'ratemassloss',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'ratemassloss',"long_name","NC_CHAR","Rate of mass loss")
        att.put.nc(gridoutputnc,'ratemassloss',"units","NC_CHAR",'kg/s')
        att.put.nc(gridoutputnc,'ratemassloss',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'ratemassloss',"grid_mapping","NC_CHAR",crsvar)
      }
      if (sublimlosscoeffout==1 | allout==1) {
        var.def.nc(gridoutputnc,'sublimlosscoeff',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'sublimlosscoeff',"long_name","NC_CHAR","Sublimation loss rate coefficient")
        att.put.nc(gridoutputnc,'sublimlosscoeff',"units","NC_CHAR",'1/s')
        att.put.nc(gridoutputnc,'sublimlosscoeff',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'sublimlosscoeff',"grid_mapping","NC_CHAR",crsvar)
      }
      if (maxinterceptout==1 | allout==1) {
        var.def.nc(gridoutputnc,'maxintercept',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'maxintercept',"long_name","NC_CHAR","Maximum snow interception")
        att.put.nc(gridoutputnc,'maxintercept',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'maxintercept',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'maxintercept',"grid_mapping","NC_CHAR",crsvar)
      }
      if (snowinterceptout==1 | allout==1) {
        var.def.nc(gridoutputnc,'snowintercept',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'snowintercept',"long_name","NC_CHAR","Snow interception")
        att.put.nc(gridoutputnc,'snowintercept',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'snowintercept',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'snowintercept',"grid_mapping","NC_CHAR",crsvar)
      }
      if (caninterceptloadout==1 | allout==1) {
        var.def.nc(gridoutputnc,'caninterceptload',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'caninterceptload',"long_name","NC_CHAR","Intercepted snow load")
        att.put.nc(gridoutputnc,'caninterceptload',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'caninterceptload',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'caninterceptload',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canexposcoeffout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canexposcoeff',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canexposcoeff',"long_name","NC_CHAR","Coefficient related to the shape of the intercepted snow deposits")
        att.put.nc(gridoutputnc,'canexposcoeff',"units","NC_CHAR",'-')
        att.put.nc(gridoutputnc,'canexposcoeff',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canexposcoeff',"grid_mapping","NC_CHAR",crsvar)
      }
      if (treesublimout==1 | allout==1) {
        var.def.nc(gridoutputnc,'treesublim',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'treesublim',"long_name","NC_CHAR","Sublimation of snow from trees")
        att.put.nc(gridoutputnc,'treesublim',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'treesublim',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'treesublim',"grid_mapping","NC_CHAR",crsvar)
      }
      if (treemeltout==1 | allout==1) {
        var.def.nc(gridoutputnc,'treemelt',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'treemelt',"long_name","NC_CHAR","Melt of snow from trees")
        att.put.nc(gridoutputnc,'treemelt',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'treemelt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'treemelt',"grid_mapping","NC_CHAR",crsvar)
      }
      if (groundliqprecipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'groundliqprecip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'groundliqprecip',"long_name","NC_CHAR","Inside canopy liquid precipitation on the ground")
        att.put.nc(gridoutputnc,'groundliqprecip',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'groundliqprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'groundliqprecip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (groundsolprecipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'groundsolprecip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'groundsolprecip',"long_name","NC_CHAR","Inside canopy solid precipitation on the ground")
        att.put.nc(gridoutputnc,'groundsolprecip',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'groundsolprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'groundsolprecip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (throughsolprecipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'throughsolprecip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'throughsolprecip',"long_name","NC_CHAR","Inside canopy throughfall of solid precipitation")
        att.put.nc(gridoutputnc,'throughsolprecip',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'throughsolprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'throughsolprecip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansnowageout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansnowage',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansnowage',"long_name","NC_CHAR","Inside canopy age of snow")
        att.put.nc(gridoutputnc,'cansnowage',"units","NC_CHAR",'days')
        att.put.nc(gridoutputnc,'cansnowage',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansnowage',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansnowalbout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansnowalb',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansnowalb',"long_name","NC_CHAR","Inside canopy snow albedo")
        att.put.nc(gridoutputnc,'cansnowalb',"units","NC_CHAR",'0-1')
        att.put.nc(gridoutputnc,'cansnowalb',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansnowalb',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansnowtempout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansnowtemp',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansnowtemp',"long_name","NC_CHAR","Inside canopy snow temperature")
        att.put.nc(gridoutputnc,'cansnowtemp',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'cansnowtemp',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansnowtemp',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansnowdeltatout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansnowdeltat',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansnowdeltat',"long_name","NC_CHAR","Inside canopy snow temperature change")
        att.put.nc(gridoutputnc,'cansnowdeltat',"units","NC_CHAR",'K')
        att.put.nc(gridoutputnc,'cansnowdeltat',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansnowdeltat',"grid_mapping","NC_CHAR",crsvar)
      }
      if (groundprecipout==1 | allout==1) {
        var.def.nc(gridoutputnc,'groundprecip',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'groundprecip',"long_name","NC_CHAR","Inside canopy total precipitation on the ground")
        att.put.nc(gridoutputnc,'groundprecip',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'groundprecip',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'groundprecip',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canvappresairout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canvappresair',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canvappresair',"long_name","NC_CHAR","Inside canopy vapor pressure of the air")
        att.put.nc(gridoutputnc,'canvappresair',"units","NC_CHAR",'hPa')
        att.put.nc(gridoutputnc,'canvappresair',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canvappresair',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canvappressnowout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canvappressnow',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canvappressnow',"long_name","NC_CHAR","Inside canopy vapor pressure over the snow cover")
        att.put.nc(gridoutputnc,'canvappressnow',"units","NC_CHAR",'hPa')
        att.put.nc(gridoutputnc,'canvappressnow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canvappressnow',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canlatfluxout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canlatflux',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canlatflux',"long_name","NC_CHAR","Inside canopy latent heat flux")
        att.put.nc(gridoutputnc,'canlatflux',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canlatflux',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canlatflux',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canswradbalout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canswradbal',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canswradbal',"long_name","NC_CHAR","Inside canopy shortwave radiation balance")
        att.put.nc(gridoutputnc,'canswradbal',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canswradbal',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canswradbal',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canlwradbalout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canlwradbal',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canlwradbal',"long_name","NC_CHAR","Inside canopy longwave radiation balance")
        att.put.nc(gridoutputnc,'canlwradbal',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canlwradbal',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canlwradbal',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansensfluxout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansensflux',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansensflux',"long_name","NC_CHAR","Inside canopy sensible heat flux")
        att.put.nc(gridoutputnc,'cansensflux',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'cansensflux',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansensflux',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canadvfluxliqout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canadvfluxliq',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canadvfluxliq',"long_name","NC_CHAR","Inside canopy advective flux from liquid precipitation")
        att.put.nc(gridoutputnc,'canadvfluxliq',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canadvfluxliq',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canadvfluxliq',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canadvfluxsolout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canadvfluxsol',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canadvfluxsol',"long_name","NC_CHAR","Inside canopy advective flux from solid precipitation")
        att.put.nc(gridoutputnc,'canadvfluxsol',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canadvfluxsol',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canadvfluxsol',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canebalsnowout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canebalsnow',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canebalsnow',"long_name","NC_CHAR","Inside canopy energy balance of the snow cover")
        att.put.nc(gridoutputnc,'canebalsnow',"units","NC_CHAR",'W/m2')
        att.put.nc(gridoutputnc,'canebalsnow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canebalsnow',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cancoldcontmmout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cancoldcontmm',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cancoldcontmm',"long_name","NC_CHAR","Inside canopy cold content of the snow cover")
        att.put.nc(gridoutputnc,'cancoldcontmm',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'cancoldcontmm',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cancoldcontmm',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansublimout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansublim',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansublim',"long_name","NC_CHAR","Inside canopy sublimation of snow")
        att.put.nc(gridoutputnc,'cansublim',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'cansublim',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansublim',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canpotmeltout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canpotmelt',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canpotmelt',"long_name","NC_CHAR","Inside canopy potential snow melt")
        att.put.nc(gridoutputnc,'canpotmelt',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'canpotmelt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canpotmelt',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canmeltout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canmelt',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canmelt',"long_name","NC_CHAR","Inside canopy snow melt")
        att.put.nc(gridoutputnc,'canmelt',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'canmelt',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canmelt',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canrefreezeout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canrefreeze',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canrefreeze',"long_name","NC_CHAR","Inside canopy refreezing liquid water")
        att.put.nc(gridoutputnc,'canrefreeze',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'canrefreeze',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canrefreeze',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canliqwatcontout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canliqwatcont',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canliqwatcont',"long_name","NC_CHAR","Inside canopy liquid water content")
        att.put.nc(gridoutputnc,'canliqwatcont',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'canliqwatcont',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canliqwatcont',"grid_mapping","NC_CHAR",crsvar)
      }
      if (canoutflowout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canoutflow',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canoutflow',"long_name","NC_CHAR","Inside canoy melt water outflow")
        att.put.nc(gridoutputnc,'canoutflow',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'canoutflow',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canoutflow',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansweout==1 | allout==1) {
        var.def.nc(gridoutputnc,'canswe',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'canswe',"long_name","NC_CHAR","Inside canopy simulated snow water equivalent")
        att.put.nc(gridoutputnc,'canswe',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'canswe',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'canswe',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansnowdensout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansnowdens',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansnowdens',"long_name","NC_CHAR","Inside canopy simulated snow density")
        att.put.nc(gridoutputnc,'cansnowdens',"units","NC_CHAR",'kg/m3')
        att.put.nc(gridoutputnc,'cansnowdens',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansnowdens',"grid_mapping","NC_CHAR",crsvar)
      }
      if (cansnowdepthout==1 | allout==1) {
        var.def.nc(gridoutputnc,'cansnowdepth',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'cansnowdepth',"long_name","NC_CHAR","Inside canopy simulated snow depth")
        att.put.nc(gridoutputnc,'cansnowdepth',"units","NC_CHAR",'m')
        att.put.nc(gridoutputnc,'cansnowdepth',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'cansnowdepth',"grid_mapping","NC_CHAR",crsvar)
      }
      if (totsweout==1 | allout==1) {
        var.def.nc(gridoutputnc,'totswe',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'totswe',"long_name","NC_CHAR","Total simulated snow water equivalent")
        att.put.nc(gridoutputnc,'totswe',"units","NC_CHAR",'mm')
        att.put.nc(gridoutputnc,'totswe',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'totswe',"grid_mapping","NC_CHAR",crsvar)
      }
      if (totsnowdensout==1 | allout==1) {
        var.def.nc(gridoutputnc,'totsnowdens',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'totsnowdens',"long_name","NC_CHAR","Total simulated snow density")
        att.put.nc(gridoutputnc,'totsnowdens',"units","NC_CHAR",'kg/m3')
        att.put.nc(gridoutputnc,'totsnowdens',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'totsnowdens',"grid_mapping","NC_CHAR",crsvar)
      }
      if (totsnowdepthout==1 | allout==1) {
        var.def.nc(gridoutputnc,'totsnowdepth',outputprecision,c(xdim,ydim,tdim))
        att.put.nc(gridoutputnc,'totsnowdepth',"long_name","NC_CHAR","Total simulated snow depth")
        att.put.nc(gridoutputnc,'totsnowdepth',"units","NC_CHAR",'m')
        att.put.nc(gridoutputnc,'totsnowdepth',"_FillValue",outputprecision,fillvalue)
        att.put.nc(gridoutputnc,'totsnowdepth',"grid_mapping","NC_CHAR",crsvar)
      }
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Point output
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Print out progress message
    print('-> Writing point output for timestep...')
    
    # Start and end for data range to put into NetCDF output file
    outputlocationsdatastart=c(timesteptot)
    outputlocationsdatacount=c(1)
    
    # Loop over the output points in the outputlocation table
    for(outputlocation in 1:noutputlocations) {
      
      # Get table information
      outputcol=outputlocationstable$COL[outputlocation]
      outputrow=outputlocationstable$ROW[outputlocation]
      
      # Put all dynamic variables
      var.put.nc(outputlocationsnc[[outputlocation]],'temp',temp[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'rh',rh[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'ws',ws[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'precip',precip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'swin',swin[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cloudcover',cloudcover[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'lwin',lwin[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'wetbulbtemp',wetbulbtemp[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'shareliqprecip',shareliqprecip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'sharesolprecip',sharesolprecip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'liqprecip',liqprecip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'solprecip',solprecip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'snowage',snowage[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'snowalb',snowalb[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'snowtemp',snowtemp[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'vappresair',vappresair[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'vappressnow',vappressnow[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'swradbal',swradbal[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'lwradbal',lwradbal[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'latflux',latflux[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'sensflux',sensflux[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'advfluxliq',advfluxliq[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'advfluxsol',advfluxsol[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'ebalsnow',ebalsnow[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'potmelt',potmelt[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'coldcontmm',coldcontmm[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'sublim',sublim[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'melt',melt[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'refreeze',refreeze[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'liqwatcont',liqwatcont[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'outflow',outflow[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'swe',swe[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'snowdens',snowdens[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'snowdepth',snowdepth[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'radfrac',radfrac[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canfrac',canfrac[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canflowindex',canflowindex[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canswin',canswin[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canlwin',canlwin[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canws',canws[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canrh',canrh[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'tmin',tmin[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'tmax',tmax[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'tmean',tmean[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'deltat',deltat[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canswabs',canswabs[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'reynolds',reynolds[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'nusselt',nusselt[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'sherwood',sherwood[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansatvap',cansatvap[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'satdensvap',satdensvap[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'watvapdif',watvapdif[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'omega',omega[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'ratemassloss',ratemassloss[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'sublimlosscoeff',sublimlosscoeff[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'maxintercept',maxintercept[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'snowintercept',snowintercept[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'caninterceptload',caninterceptload[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canexposcoeff',canexposcoeff[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'treesublim',treesublim[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'treemelt',treemelt[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'groundliqprecip',groundliqprecip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'groundsolprecip',groundsolprecip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'throughsolprecip',throughsolprecip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansnowage',cansnowage[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansnowalb',cansnowalb[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansnowtemp',cansnowtemp[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansnowdeltat',cansnowdeltat[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'groundprecip',groundprecip[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canvappresair',canvappresair[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canvappressnow',canvappressnow[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canlatflux',canlatflux[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canswradbal',canswradbal[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canlwradbal',canlwradbal[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansensflux',cansensflux[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canadvfluxliq',canadvfluxliq[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canadvfluxsol',canadvfluxsol[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canebalsnow',canebalsnow[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cancoldcontmm',cancoldcontmm[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansublim',cansublim[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canpotmelt',canpotmelt[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canmelt',canmelt[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canrefreeze',canrefreeze[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canliqwatcont',canliqwatcont[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canoutflow',canoutflow[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'canswe',canswe[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansnowdens',cansnowdens[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
      var.put.nc(outputlocationsnc[[outputlocation]],'cansnowdepth',cansnowdepth[outputcol,outputrow],start=outputlocationsdatastart,count=outputlocationsdatacount,na.mode=4,pack=FALSE)
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Gridded output
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Print out progress message
    print('-> Writing gridded output for timestep...')
    
    # Start and end for data range to put into NetCDF output file
    outputdatastart=c(NA,NA,timestepyear)
    outputdatacount=c(NA,NA,1)
    
    # Put selected variables to grid file
    if (tempout==1 | allout==1) {
      var.put.nc(gridoutputnc,'temp',temp,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (rhout==1 | allout==1) {
      var.put.nc(gridoutputnc,'rh',rh,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (wsout==1 | allout==1) {
      var.put.nc(gridoutputnc,'ws',ws,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (precipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'precip',precip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (swinout==1 | allout==1) {
      var.put.nc(gridoutputnc,'swin',swin,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cloudcoverout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cloudcover',cloudcover,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (lwinout==1 | allout==1) {
      var.put.nc(gridoutputnc,'lwin',lwin,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (wetbulbtempout==1 | allout==1) {
      var.put.nc(gridoutputnc,'wetbulbtemp',wetbulbtemp,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (shareliqprecipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'shareliqprecip',shareliqprecip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (sharesolprecipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'sharesolprecip',sharesolprecip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (liqprecipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'liqprecip',liqprecip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (solprecipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'solprecip',solprecip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (snowageout==1 | allout==1) {
      var.put.nc(gridoutputnc,'snowage',snowage,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (snowalbout==1 | allout==1) {
      var.put.nc(gridoutputnc,'snowalb',snowalb,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (snowtempout==1 | allout==1) {
      var.put.nc(gridoutputnc,'snowtemp',snowtemp,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (vappresairout==1 | allout==1) {
      var.put.nc(gridoutputnc,'vappresair',vappresair,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (vappressnowout==1 | allout==1) {
      var.put.nc(gridoutputnc,'vappressnow',vappressnow,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (swradbalout==1 | allout==1) {
      var.put.nc(gridoutputnc,'swradbal',swradbal,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (lwradbalout==1 | allout==1) {
      var.put.nc(gridoutputnc,'lwradbal',lwradbal,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (latfluxout==1 | allout==1) {
      var.put.nc(gridoutputnc,'latflux',latflux,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (sensfluxout==1 | allout==1) {
      var.put.nc(gridoutputnc,'sensflux',sensflux,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (advfluxliqout==1 | allout==1) {
      var.put.nc(gridoutputnc,'advfluxliq',advfluxliq,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (advfluxsolout==1 | allout==1) {
      var.put.nc(gridoutputnc,'advfluxsol',advfluxsol,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (ebalsnowout==1 | allout==1) {
      var.put.nc(gridoutputnc,'ebalsnow',ebalsnow,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (potmeltout==1 | allout==1) {
      var.put.nc(gridoutputnc,'potmelt',potmelt,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (coldcontmmout==1 | allout==1) {
      var.put.nc(gridoutputnc,'coldcontmm',coldcontmm,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (sublimout==1 | allout==1) {
      var.put.nc(gridoutputnc,'sublim',sublim,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (meltout==1 | allout==1) {
      var.put.nc(gridoutputnc,'melt',melt,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (refreezeout==1 | allout==1) {
      var.put.nc(gridoutputnc,'refreeze',refreeze,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (liqwatcontout==1 | allout==1) {
      var.put.nc(gridoutputnc,'liqwatcont',liqwatcont,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (outflowout==1 | allout==1) {
      var.put.nc(gridoutputnc,'outflow',outflow,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (sweout==1 | allout==1) {
      var.put.nc(gridoutputnc,'swe',swe,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (snowdensout==1 | allout==1) {
      var.put.nc(gridoutputnc,'snowdens',snowdens,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (snowdepthout==1 | allout==1) {
      var.put.nc(gridoutputnc,'snowdepth',snowdepth,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (radfracout==1 | allout==1) {
      var.put.nc(gridoutputnc,'radfrac',radfrac,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canfracout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canfrac',canfrac,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canfracout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canfrac',canfrac,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canflowindexout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canflowindex',canflowindex,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canswinout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canswin',canswin,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canlwinout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canlwin',canlwin,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canwsout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canws',canws,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canrhout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canrh',canrh,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (tminout==1 | allout==1) {
      var.put.nc(gridoutputnc,'tmin',tmin,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (tmaxout==1 | allout==1) {
      var.put.nc(gridoutputnc,'tmax',tmax,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (tmeanout==1 | allout==1) {
      var.put.nc(gridoutputnc,'tmean',tmean,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (deltatout==1 | allout==1) {
      var.put.nc(gridoutputnc,'deltat',deltat,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canswabsout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canswabs',canswabs,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (reynoldsout==1 | allout==1) {
      var.put.nc(gridoutputnc,'reynolds',reynolds,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (nusseltout==1 | allout==1) {
      var.put.nc(gridoutputnc,'nusselt',nusselt,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (sherwoodout==1 | allout==1) {
      var.put.nc(gridoutputnc,'sherwood',sherwood,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansatvapout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansatvap',cansatvap,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (satdensvapout==1 | allout==1) {
      var.put.nc(gridoutputnc,'satdensvap',satdensvap,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (watvapdifout==1 | allout==1) {
      var.put.nc(gridoutputnc,'watvapdif',watvapdif,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (omegaout==1 | allout==1) {
      var.put.nc(gridoutputnc,'omega',omega,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (ratemasslossout==1 | allout==1) {
      var.put.nc(gridoutputnc,'ratemassloss',ratemassloss,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (sublimlosscoeffout==1 | allout==1) {
      var.put.nc(gridoutputnc,'sublimlosscoeff',sublimlosscoeff,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (maxinterceptout==1 | allout==1) {
      var.put.nc(gridoutputnc,'maxintercept',maxintercept,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (snowinterceptout==1 | allout==1) {
      var.put.nc(gridoutputnc,'snowintercept',snowintercept,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (caninterceptloadout==1 | allout==1) {
      var.put.nc(gridoutputnc,'caninterceptload',caninterceptload,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canexposcoeffout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canexposcoeff',canexposcoeff,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (treesublimout==1 | allout==1) {
      var.put.nc(gridoutputnc,'treesublim',treesublim,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (treemeltout==1 | allout==1) {
      var.put.nc(gridoutputnc,'treemelt',treemelt,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (groundliqprecipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'groundliqprecip',groundliqprecip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (groundsolprecipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'groundsolprecip',groundsolprecip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (throughsolprecipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'throughsolprecip',throughsolprecip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansnowageout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansnowage',cansnowage,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansnowalbout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansnowalb',cansnowalb,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansnowtempout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansnowtemp',cansnowtemp,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansnowdeltatout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansnowdeltat',cansnowdeltat,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (groundprecipout==1 | allout==1) {
      var.put.nc(gridoutputnc,'groundprecip',groundprecip,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canvappresairout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canvappresair',canvappresair,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canvappressnowout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canvappressnow',canvappressnow,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canlatfluxout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canlatflux',canlatflux,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canswradbalout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canswradbal',canswradbal,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canlwradbalout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canlwradbal',canlwradbal,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansensfluxout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansensflux',cansensflux,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canadvfluxliqout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canadvfluxliq',canadvfluxliq,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canadvfluxliqout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canadvfluxliq',canadvfluxliq,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canadvfluxsolout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canadvfluxsol',canadvfluxsol,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canebalsnowout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canebalsnow',canebalsnow,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cancoldcontmmout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cancoldcontmm',cancoldcontmm,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansublimout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansublim',cansublim,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canpotmeltout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canpotmelt',canpotmelt,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canmeltout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canmelt',canmelt,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canrefreezeout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canrefreeze',canrefreeze,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canliqwatcontout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canliqwatcont',canliqwatcont,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (canoutflowout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canoutflow',canoutflow,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansweout==1 | allout==1) {
      var.put.nc(gridoutputnc,'canswe',canswe,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansnowdensout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansnowdens',cansnowdens,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (cansnowdepthout==1 | allout==1) {
      var.put.nc(gridoutputnc,'cansnowdepth',cansnowdepth,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (totsweout==1 | allout==1) {
      var.put.nc(gridoutputnc,'totswe',totswe,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (totsnowdensout==1 | allout==1) {
      var.put.nc(gridoutputnc,'totsnowdens',totsnowdens,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
    if (totsnowdepthout==1 | allout==1) {
      var.put.nc(gridoutputnc,'totsnowdepth',totsnowdepth,start=outputdatastart,count=outputdatacount,na.mode=4,pack=FALSE)
    }
  } # End of loop over all timesteps in a year
  
  # Close files with meteo input at the end of a model year
  if (grid==1) {
    
    # Close files with meteo inputs if working in grid mode
    close.nc(tempnc)
    close.nc(rhnc)
    close.nc(wsnc)
    close.nc(precipnc)
    close.nc(swinnc)
    
  } else {
    
    # Close file with meteo input if working in point mode
    close.nc(allnc)
  }
  
  # Print out progress message
  print('-> Closing output files...')
  
  # Close output file for gridded values
  close.nc(gridoutputnc)
  
} # End of loop over all years

# Loop over the output points in the outputlocation table
for(outputlocation in 1:noutputlocations) {
  
  # Close output files for point values
  close.nc(outputlocationsnc[[outputlocation]])
}

print('')
print("----------------------------------------")
print("... End of model simulations!")
print("----------------------------------------")

# Record end time
endtime=Sys.time()

# Calculate time difference
elapsedtime=as.numeric(difftime(endtime,starttime,units="secs"))

# Print time information
print('')
print('Time required from first to last simulated timestep:')
print(paste('In seconds: ',round(as.numeric(elapsedtime),2),sep=''))
print(paste('In minutes: ',round(as.numeric(elapsedtime/60),2),sep=''))
print(paste('In hours: ',round(as.numeric(elapsedtime/(60*60)),2),sep=''))