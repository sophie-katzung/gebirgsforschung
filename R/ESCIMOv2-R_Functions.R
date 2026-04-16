# **************************************************************
# Function for cloud cover estimation
# **************************************************************
# Liston, G. and Elder, K. (2006): A meteorological distribution 
# system for high-resolution terrestrial modeling (MicroMet). 
# Journal of Hydrometeorology. 7, 217-234.
# **************************************************************

get.cloudcover <- function(ncols,nrows,temp,rh,elevation,month) {
  
  # Define kelvin
  kelvin=273.15
  
  # Define monthly temperature lapse rate (deg C/km)
  templapserates=c(2.6,3.5,4.7,5.3,5.2,5.3,4.9,4.7,4.2,3.3,3.5,3.1)
  
  # Define monthly dew point temperature lapse rate (deg C/km)
  dewtemplapserates=c(4.4,4.6,4.9,4.8,4.6,4.7,4.3,4.2,4.5,4.4,4.7,4.6)
  
  # Get actual temperature lapse rate (deg C/km)
  templapserate=templapserates[month]
  
  # Get actual dewpoint temperature lapse rate (deg C/km)
  dewtemplapserate=dewtemplapserates[month]
  
  # Convert temperature values to cloud level (700hPa, roughly 3000 m.a.s.l in a standard atmosphere)
  tempcloudlevel=temp-(templapserate*0.001*(3000.-elevation))
  
  # Set coefficients for humidity calculations for the surface (Buck 1981)
  overwater=which(temp>=kelvin,arr.ind=T)
  overice=which(temp<kelvin,arr.ind=T)
  
  asurf=array(NaN,dim=c(ncols,nrows))
  asurf[overwater]=611.21
  asurf[overice]=611.15
  
  bsurf=array(NaN,dim=c(ncols,nrows))
  bsurf[overwater]=17.502
  bsurf[overice]=22.452
  
  csurf=array(NaN,dim=c(ncols,nrows))
  csurf[overwater]=240.97
  csurf[overice]=272.55
  
  # Get saturated vapor pressure (Pa)
  es=asurf*exp((bsurf*(temp-kelvin))/(csurf+(temp-kelvin)))
  
  # Get actual vapor pressure (Pa)
  ea=rh/100*es
  
  # Get dew point temperature from actual vapor pressure (K)
  dewtemp=(csurf*log(ea/asurf))/(bsurf-log(ea/asurf))+kelvin
  
  # Convert dewpoint temperature values to cloud level (700hPa, roughly 3000 m.a.s.l in a standard atmosphere)
  dewtempcloudlevel=dewtemp-(dewtemplapserate*0.001*(3000.-elevation))
  
  # Set coefficients for humidity calculations for the cloud level (Buck 1981)
  overwater=which(tempcloudlevel>=kelvin,arr.ind=T)
  overice=which(tempcloudlevel<kelvin,arr.ind=T)
  
  acloudlevel=array(NaN,dim=c(ncols,nrows))
  acloudlevel[overwater]=611.21
  acloudlevel[overice]=611.15
  
  bcloudlevel=array(NaN,dim=c(ncols,nrows))
  bcloudlevel[overwater]=17.502
  bcloudlevel[overice]=22.452
  
  ccloudlevel=array(NaN,dim=c(ncols,nrows))
  ccloudlevel[overwater]=240.97
  ccloudlevel[overice]=272.55
  
  # Get saturated and actual vapor pressure at cloud level (Pa)
  escloudlevel=acloudlevel*exp((bcloudlevel*(tempcloudlevel-kelvin))/(ccloudlevel+(tempcloudlevel-kelvin)))
  eacloudlevel=acloudlevel*exp((bcloudlevel*(dewtempcloudlevel-kelvin))/(ccloudlevel+(dewtempcloudlevel-kelvin)))
  
  # Get relative humidty at cloud level (%)  
  rhcloudlevel=eacloudlevel/escloudlevel*100
  rhcloudlevel[rhcloudlevel>100]=100
  
  # Get cloud cover (0-1)
  cloudcover=0.832*exp((rhcloudlevel-100)/41.6)
  
  # Return results
  return(cloudcover)
}

# **************************************************************
# Function for longwave radiation estimation
# **************************************************************
# Swinbank, W. C. (1963): Long-wave radiation from clear skies, Q. J. Roy.
# Meteor. Soc., 89, 330–348.
#
# Jacobs, J. D. (1978): Radiation climate of Broughton Island, in: Energy
# Budget Studies in Relation to Fast-ice Breakup Processes in
# Davis Strait, edited by: Barry, R. G. and Jacobs, J. D., Inst. of
# Arctic and Alp. Res. Occas. Paper No. 26, University of Col-
# orado, Boulder, 105–120.
# **************************************************************

incoming.longwaveradiation1 <- function(temp,rh,cloudcover) {
  
  # Define kelvin
  kelvin=273.15
  
  # Set Stefan-Boltzmann Constant (W/(m2*K^4))
  stefbolzcon=0.0000000567
  
  # Set coefficients for humidity calculations for the surface (Buck 1981)
  overwater=which(temp>=kelvin,arr.ind=T)
  overice=which(temp<kelvin,arr.ind=T)
  
  asurf=array(NaN,dim=c(ncols,nrows))
  asurf[overwater]=611.21
  asurf[overice]=611.15
  
  bsurf=array(NaN,dim=c(ncols,nrows))
  bsurf[overwater]=17.502
  bsurf[overice]=22.452
  
  csurf=array(NaN,dim=c(ncols,nrows))
  csurf[overwater]=240.97
  csurf[overice]=272.55
  
  # Get saturated vapor pressure (Pa)
  es=asurf*exp((bsurf*(temp-kelvin))/(csurf+(temp-kelvin)))
  
  # Get actual vapor pressure (Pa)
  ea=rh/100*es
  
  # Get clear sky longwave radiation (W/m2) (Swinbank 1963)
  lwinclearsky=5.31*10^(-13)*temp^6
  
  # Modify for cloud cover effects (Jacobs 1978)
  a=0.26
  lwin=(1+a*cloudcover)*lwinclearsky
  
  # Return results
  return(lwin)
}

# **************************************************************
# Function for longwave radiation estimation
# **************************************************************
# Dilley, A. C. and O’Brien, D. M. (1998): Estimating downward clear sky
# long-wave irradiance at the surface from screen temperature and
# precipitable water, Q. J. Roy. Meteor. Soc., 124, 1391–1401,
# https://doi.org/10.1002/qj.49712454903.
#
# Jacobs, J. D. (1978): Radiation climate of Broughton Island, in: Energy
# Budget Studies in Relation to Fast-ice Breakup Processes in
# Davis Strait, edited by: Barry, R. G. and Jacobs, J. D., Inst. of
# Arctic and Alp. Res. Occas. Paper No. 26, University of Col-
# orado, Boulder, 105–120.
# **************************************************************

incoming.longwaveradiation2 <- function(temp,rh,cloudcover) {
  
  # Define kelvin
  kelvin=273.15
  
  # Set Stefan-Boltzmann Constant (W/(m2*K^4))
  stefbolzcon=0.0000000567
  
  # Set coefficients for humidity calculations for the surface (Buck 1981)
  overwater=which(temp>=kelvin,arr.ind=T)
  overice=which(temp<kelvin,arr.ind=T)
  
  asurf=array(NaN,dim=c(ncols,nrows))
  asurf[overwater]=611.21
  asurf[overice]=611.15
  
  bsurf=array(NaN,dim=c(ncols,nrows))
  bsurf[overwater]=17.502
  bsurf[overice]=22.452
  
  csurf=array(NaN,dim=c(ncols,nrows))
  csurf[overwater]=240.97
  csurf[overice]=272.55
  
  # Get saturated vapor pressure (Pa)
  es=asurf*exp((bsurf*(temp-kelvin))/(csurf+(temp-kelvin)))
  
  # Get actual vapor pressure (Pa)
  ea=rh/100*es
  
  # Get clear sky longwave radiation (W/m2) (Dilley and O’Brien 1998)
  lwinclearsky=59.38+113.7*(temp/273.16)^6+96.96*sqrt(46.5*((ea*0.01)/temp)/2.5)
  
  # Modify for cloud cover effects (Jacobs 1978)
  a=0.26
  lwin=(1+a*cloudcover)*lwinclearsky
  
  # Return results
  return(lwin)
}

# **************************************************************
# Function for longwave radiation estimation
# **************************************************************
# Liston, G. and Elder, K. (2006): A meteorological distribution 
# system for high-resolution terrestrial modeling (MicroMet). 
# Journal of Hydrometeorology. 7, 217-234.
#
# Iziomon, M. G., Mayer, H., and Matzarakis, A. (2003): Downward atmo-
# spheric longwave irradiance under clear and cloudy skies: mea-
# surement and parameterization, J. Atmos. Sol.-Terr. Phys., 65,
# 1107–1116.
# **************************************************************

incoming.longwaveradiation3 <- function(ncols,nrows,temp,rh,cloudcover,elevation) {
  
  # Define kelvin
  kelvin=273.15
  
  # Set Stefan-Boltzmann Constant (W/(m2*K^4))
  stefbolzcon=0.0000000567
  
  # Set required constants
  E1=200.0
  X1=0.35
  Y1=0.100
  Z1=0.224
  
  E2=3000.0
  X2=0.51
  Y2=0.130
  Z2=1.100
  
  # Set coefficients for humidity calculations for the surface (Buck 1981)
  overwater=which(temp>=kelvin,arr.ind=T)
  overice=which(temp<kelvin,arr.ind=T)
  
  asurf=array(NaN,dim=c(ncols,nrows))
  asurf[overwater]=611.21
  asurf[overice]=611.15
  
  bsurf=array(NaN,dim=c(ncols,nrows))
  bsurf[overwater]=17.502
  bsurf[overice]=22.452
  
  csurf=array(NaN,dim=c(ncols,nrows))
  csurf[overwater]=240.97
  csurf[overice]=272.55
  
  # Get saturated vapor pressure (Pa)
  es=asurf*exp((bsurf*(temp-kelvin))/(csurf+(temp-kelvin)))
  
  # Get actual vapor pressure (Pa)
  ea=rh/100*es
  
  # Compute elevation levels
  level1=which(elevation<E1,arr.ind=T)
  level2=which(elevation>=E1 & elevation<=E2,arr.ind=T)
  level3=which(elevation>E2,arr.ind=T)
  
  # Compute required constants depending on elevation
  Xs=array(NaN,dim=c(ncols,nrows))
  Ys=array(NaN,dim=c(ncols,nrows))
  Zs=array(NaN,dim=c(ncols,nrows))
  
  Xs[level1]=X1
  Ys[level1]=Y1
  Zs[level1]=Z1
  
  Xs[level2]=X1+(elevation[level2]-E1)*((X2-X1)/(E2-E1))
  Ys[level2]=Y2+(elevation[level2]-E1)*((Y2-Y1)/(E2-E1))
  Zs[level2]=Z2+(elevation[level2]-E1)*((Z2-Z1)/(E2-E1))
  
  Xs[level3]=X2
  Ys[level3]=Y2
  Zs[level3]=Z2
  
  # Compute incoming longwave radiation (W/m2)
  alfa=1.0 # no adjustment to site specific conditions (alfa=1.083) at Walton Creek as proposed by Liston and Elder (2006)
  emisscloud=alfa*(1.0-Xs*exp((-Ys)*ea/temp))*(1.0+Zs*cloudcover**2)
  lwin=emisscloud*stefbolzcon*temp**4
  
  # Return results
  return(lwin)
}

# **************************************************************
# Function for longwave radiation estimation
# **************************************************************
# Prata, A. J. (1996): A new long-wave formula for estimating downward
# clear-sky radiation at the surface, Q. J. Roy. Meteor. Soc., 122,
# 1127–1151.
#
# Sugita, M. and Brutsaert, W. (1993): Cloud effect in the estimation of in-
# stantaneous downward longwave radiation, Water Resour. Res.,
# 29, 599-605, https://doi.org/10.1029/92wr02352.
# **************************************************************

incoming.longwaveradiation4 <- function(temp,rh,cloudcover) {
  
  # Define kelvin
  kelvin=273.15
  
  # Set Stefan-Boltzmann Constant (W/(m2*K^4))
  stefbolzcon=0.0000000567
  
  # Set coefficients for humidity calculations for the surface (Buck 1981)
  overwater=which(temp>=kelvin,arr.ind=T)
  overice=which(temp<kelvin,arr.ind=T)
  
  asurf=array(NaN,dim=c(ncols,nrows))
  asurf[overwater]=611.21
  asurf[overice]=611.15
  
  bsurf=array(NaN,dim=c(ncols,nrows))
  bsurf[overwater]=17.502
  bsurf[overice]=22.452
  
  csurf=array(NaN,dim=c(ncols,nrows))
  csurf[overwater]=240.97
  csurf[overice]=272.55
  
  # Get saturated vapor pressure (Pa)
  es=asurf*exp((bsurf*(temp-kelvin))/(csurf+(temp-kelvin)))
  
  # Get actual vapor pressure (Pa)
  ea=rh/100*es
  
  # Get clear sky longwave radiation (W/m2) (Prata 1996)
  lwinclearsky=(1-(1+46.5*(ea*0.01)/temp)*exp(-1*(1.2+3*46.5*(ea*0.01)/temp)^0.5))*stefbolzcon*temp^4
  
  # Modify for cloud cover effects (Sugita and Brutsaert 1993)
  a=0.0496
  b=2.45
  lwin=(1+a*cloudcover^b)*lwinclearsky
  
  # Return results
  return(lwin)
}

# **************************************************************
# Function for longwave radiation estimation
# **************************************************************
# Maykut, G.A. and Church, P.E. (1973): Radiation Climate of 
# Barrow Alaska, 1962–66. Journal of Applied Meteorology and 
# Climatology, 620-628, DOI: https://doi.org/10.1175/
# 1520-0450(1973)012<0620:RCOBA>2.0.CO;2
# **************************************************************

incoming.longwaveradiation5 <- function(temp,cloudcover) {
  
  # Set coefficients
  a=0.7855
  b=0.000312
  c=2.75
  
  # Set Stefan-Boltzmann Constant (W/(m2*K^4))
  stefbolzcon=0.0000000567
  
  # get longwave radiation (W/m2)
  lwin=stefbolzcon*temp^4*(a+b*cloudcover^c)
  
  # return results
  return(lwin)
}

# **************************************************************
# Function for snow density estimation
# **************************************************************
# This function represents a simplified version of the SNOW-17
# density paramerterisation by Anderson (2006) as described in 
# Dawson et al. (2017):
# Dawson, N, Broxton, P. and X. Zeng (2017): A New Snow Density
# Parameterization for Land Data Initialization. Journal of 
# Hydrometeorlogy, DOI: 10.1175/JHM-D-16-0166.1, https://journals.
# ametsoc.org/view/journals/hydr/18/1/jhm-d-16-0166_1.pdf
# **************************************************************

snow.density1 <- function(temp,swe,snowtemp,snowdepth,snowdens,timeinc) {
  
  # Define constants and parameters
  kelvin=273.15
  soiltempC=0
  
  # Convert snow density from kg/m3 to g/cm3
  snowdens=snowdens/1000
  
  # Convert snow depth from meters to cm
  snowdepth=snowdepth*100
  
  # Convert swe from kg/m2 (=mm) to g/cm2
  swe=swe*1000/(100*100)
  
  # Correct for snow aging and overburden 
  C1=0.01 # 1/cm*h
  C2=21   # cm3/g
  B=(timeinc/3600)*C1*exp(0.08*(soiltempC+(snowtemp-kelvin)/2)-C2*snowdens)  
  correction=(exp(B*swe)-1)/(B*swe)  
  snowdens=snowdens*correction  
  
  # Include effect of snow melt
  dw=0.13*(timeinc/3600)/24
  meltcells=which(snowtemp>=273.15,arr.ind=T)
  snowdens[meltcells]=snowdens[meltcells]*(1-dw)+dw
  
  # Convert density from g/cm3 to kg/m3
  snowdens=snowdens*1000
  
  # Return results
  return(snowdens)
}

# **************************************************************
# Function for snow density estimation
# **************************************************************
# Essery, R., Morin, S., Lejeune, Y. and Menard, C. (2013): 
# A comparison of 1701 snow models using observations from an 
# alpine site. Advances in Water Resources, 55, 131-148.
# **************************************************************

snow.density2 <- function(swe,snowtemp,sdensprev,timeinc) {
  
  # Define parameters
  maxdensmelt=500.         # Maximum density of melting snow (kg/m3)
  maxdenscold=300.         # Maximum density of cold snow (kg/m3)
  
  snowcomptimescaleh=200.  # Snow compaction time scale (h)
  
  # Get comapction time scale in seconds
  snowcomptimescales=snowcomptimescaleh*3600
  
  # Initialize snow density with previous values
  sdens=sdensprev
  
  # Density for melting snow
  meltingsnow=which(swe>0 & snowtemp>=273.15,arr.ind=T)
  sdens[meltingsnow]=maxdensmelt+(sdensprev[meltingsnow]-maxdensmelt)*exp(-timeinc/snowcomptimescales)
  sdens[meltingsnow]=ifelse(sdens[meltingsnow]>maxdensmelt,maxdensmelt,sdens[meltingsnow])
  
  # Density for cold snow
  coldsnow=which(swe>0 & snowtemp<273.15,arr.ind=T)
  sdens[coldsnow]=maxdenscold+(sdensprev[coldsnow]-maxdenscold)*exp(-timeinc/snowcomptimescales)
  sdens[coldsnow]=ifelse(sdens[coldsnow]>maxdenscold,maxdenscold,sdens[coldsnow])
  
  # Return results
  return(sdens)
}

# **************************************************************
# Function for precipitation undercatch correction
# **************************************************************
# Goodison, B. E., Louie, P., & Yang, D. (1998). WMO solid
# precipitation measurement intercomparison (World Meteorological
# Organization, p. 212). World Meteorological Organization.
# https://library.wmo.int/records/item/28336-wmo-solid-precipitation-measurement-intercomparison
#
# Note: some corrections use Tmin or Tmax as used for daily corrections, 
# we use Tmean as we operate hourly
# **************************************************************

correct.undercatch1 <- function(gauge_type,precipitation_input,temperature_input,windspeed_input) {
  
  # set closest gauge type for European conditions in the Alps
  if (gauge_type == 'standard') {
    gauge_type='us_sh'
  }
  
  # get meteo conditions at positive precipitation (snowfall!!!) values
  positive_precipitation=which(precipitation_input>0,arr.ind=TRUE)
  precipitation=precipitation_input[positive_precipitation]
  windspeed=windspeed_input[positive_precipitation]
  temperature=temperature_input[positive_precipitation]-273.15
  
  # make sure we account for the maximum wind speeds the formula is valid for (7 m/s)
  wind_max=7
  tooHigh=which(windspeed > wind_max,arr.ind=TRUE)
  windspeed[tooHigh]=wind_max
  
  # set the correction functions depending on gauge type (the standard gauge in Austria/Rofental should be closest to us_sh)
  if (gauge_type == 'us_un') {              # US unshielded
    catch_ratio=exp(4.61-0.16*windspeed^1.28)
  } else if (gauge_type == 'us_sh') {       # US shielded
    catch_ratio=exp(4.61-0.04*windspeed^1.75)
  } else if (gauge_type == 'he_un') {       # Hellmann unshielded
    catch_ratio=100.00+1.13*windspeed^2-19.45*windspeed
  } else if (gauge_type == 'ni_sh') {       # Nipher shielded (integrated shield)
    catch_ratio=100.00-0.44*windspeed^2-1.98*windspeed
  } else if (gauge_type == 'tr_sh') {       # Tretyakov shielded
    catch_ratio=103.11-8.67*windspeed+0.30*temperature
  } else {
    print('Unknown gauge type!')
    stop()
  }
  
  # correct precipitation data (account for catch_ratio beeing in %)
  precipitation_corrected=precipitation_input
  precipitation_corrected[positive_precipitation]=precipitation/(catch_ratio*0.01)
  
  # return results
  return(precipitation_corrected)
}

# **************************************************************
# Function for precipitation undercatch correction
# **************************************************************
# Kochendorfer, J., Rasmussen, R., Wolff, M., Baker, B., 
# Hall, M. E., Meyers, T., Landolt, S., Jachcik, A., Isaksen, K., 
# Brækkan, R., & Leeper, R. (2017): The quantification and 
# correction of wind-induced precipitation measurement errors.
# Hydrology and Earth System Sciences, 21(4), 1973–1989. 
# https://doi.org/10.5194/hess-21-1973-2017
#
# The original correction function published has been corrected 
# in a corrigendum, see:
# https://hess.copernicus.org/articles/21/1973/2017/
# hess-21-1973-2017-corrigendum.pdf
# **************************************************************

correct.undercatch2 <- function(gauge_type,precipitation_input,temperature_input,windspeed_input) {
  
  # set closest gauge type for European conditions in the Alps
  if (gauge_type == 'standard') {
    gauge_type='all_sa'
  }
  
  # set the correction parameters depending on gauge type (the standard gauge in Austria/Rofental should be closest to all_sa)
  if (gauge_type == 'us_un') {              #  US unshielded
    a=0.045
    b=1.21
    c=0.66
    wind_max=8
  } else if (gauge_type == 'nor_sa') {      # Norwegian single-Alter
    a=0.05
    b=0.66
    c=0.23
    wind_max=12
  } else if (gauge_type == 'us_sa') {       # US single-Alter
    a=0.03
    b=1.06
    c=0.63
    wind_max=12
  } else if (gauge_type == 'all_sa') {      # combined US and NOR single-Alter
    a=0.03
    b=1.04
    c=0.57
    wind_max=12
  } else if (gauge_type == 'us_da') {       # US double-Alter
    a=0.021
    b=0.74
    c=0.66
    wind_max=8
  } else if (gauge_type == 'us_bda') {      # US Belfort double-Alter
    a=0.01
    b=0.48
    c=0.51
    wind_max=8    
  } else if (gauge_type == 'us_sdfir') {    # US small Double Fence Intercomparison Reference
    a=0.004
    b=0.00
    c=0.00
    wind_max=8
  } else {
    print('Unknown gauge type!')
    stop()
  }
  
  # get meteo conditions at positive precipitation values
  positive_precipitation=which(precipitation_input>0,arr.ind=TRUE)
  precipitation=precipitation_input[positive_precipitation]
  windspeed=windspeed_input[positive_precipitation]
  temperature=temperature_input[positive_precipitation]-273.15
  
  # make sure we account for the maximum wind speeds the formula is valid for 
  tooHigh=which(windspeed > wind_max,arr.ind=TRUE)
  windspeed[tooHigh]=wind_max
  
  # calculate the catch efficiency (ratio of precipitation measured by a gauge and actual precipitation)
  catch_efficiency=exp(-a*windspeed*(1-atan(b*temperature)+c))
  
  # correct precipitation data
  precipitation_corrected=precipitation_input
  precipitation_corrected[positive_precipitation]=precipitation/catch_efficiency
  
  # return results
  return(precipitation_corrected)
}

# **************************************************************
# Function to get get colum and row position from coordinates
# **************************************************************

col.row <- function(xvalue,yvalue,res,xcoords,ycoords) {
  
  # Get length of array in both dimensions
  nrows=length(ycoords)
  ncols=length(xcoords)
  
  # Get distance to all y-coordinates
  ydistance=array(NaN,dim=c(nrows))
  for (i in 1:nrows) {
    ydistance[i]=ycoords[i]-yvalue
  }
  
  # Get absolute values
  ydistance=abs(ydistance)
  
  # Get minimum y-distance
  minydistance=min(ydistance)
  
  # Check if y-coordinate is in the domain
  if (minydistance<res*0.5) {
    
    # Get y-location of the minimum distance
    row=which(ydistance==minydistance)
    
  } else {
    
    # The y-location is ut of the domain
    row=NA
  }
  
  # Get distance to all x-coordinates
  xdistance=array(NaN,dim=c(ncols))
  for (j in 1:ncols) {
    xdistance[j]=xcoords[j]-xvalue
  }
  
  # Get absolute values
  xdistance=abs(xdistance)
  
  # Get minimum x-distance
  minxdistance=min(xdistance)
  
  # Check if x-coordinate is in the domain
  if (minxdistance<res*0.5) {
    
    # Get x-location of the minimum distance
    col=which(xdistance==minxdistance)
    
  } else {
    
    # The x-location is out of the domain
    col=NA
  }
  
  # Check if either col or row is NA
  if(is.na(col) | is.na(row)) {
    
    # Set row an dcol values to NA
    row=NA
    col=NA
  }
  
  # Define function result
  result=c(col,row)
}

# **************************************************************
# Function to get minimum over 3rd dimension of an array
# **************************************************************

min3D <- function(input3D) {
  
  library(matrixStats)
  
  # Reshape to 2D: rows=ncols*nrows,cols=nLayers
  reshaped=matrix(input3D,nrow=prod(dim(input3D)[1:2]),ncol=dim(input3D)[3])
  
  # Row-wise min with NA removed
  minvals=rowMins(reshaped,na.rm=TRUE)
  
  # Reshape back to 2D
  min2D=matrix(minvals,nrow=dim(input3D)[1],ncol=dim(input3D)[2])
  
  # Return results
  return(min2D)
}

# **************************************************************
# Function to get maximum over 3rd dimension of an array
# **************************************************************

max3D <- function(input3D) {
  
  library(matrixStats)
  
  # Reshape to 2D: rows=ncols*nrows,cols=nLayers
  reshaped=matrix(input3D,nrow=prod(dim(input3D)[1:2]),ncol=dim(input3D)[3])
  
  # Row-wise min with NA removed
  maxvals=rowMaxs(reshaped,na.rm=TRUE)
  
  # Reshape back to 2D
  max2D=matrix(maxvals,nrow=dim(input3D)[1],ncol=dim(input3D)[2])
  
  # Return results
  return(max2D)
}