# Workflow for Campbell et al. Oregon Offshore Wind groundfish model

# set working directory to source file location

library(ggplot2)
library(devtools)
#install_local("Model2package",force=TRUE) 
#library(Model2package)
#library(matlib)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(mvtnorm) # Load the mvtnorm package for multivariate normal functions
library(sf)
library(sp)
#library(viridis)
#library(MASS)

# Load some helper functions
source('leslie_matrix.R')
source('HR_2D.R')
source('spatial_model2.R')

source('DataReadIn.R') # This assumes there is a directory 'Data' within the working directory

source('RuntheModel.R') # this will call the model later

Sp_name = c('lcod','dsole')
Options = c('B','C')
Options2 = c('Constant','Increase')

nruns = 500

for (s in 1:2){

  if (s == 1){  
# Species 1: Lingcod
source('lcod_X.R')
source('avg_lingcod.R')
  }else{
    # Species 2: Dover sole
    source('dver_X.R')
    source('doversole.R')
  }

  for (o in 1:2){
    for (oo in 1:2){
#Option "B" - p coos & brookings SCENARIO 1
#Option "C" - p coos & p brookings SCENARIO 2
Option <- Options[o]
Option2 <- Options2[oo]
Sp <- Sp_name[s]

Barchart.Filename = paste0(Sp,'_barcharts_option',Option,'_effort_',Option2,'_nruns',toString(nruns),'.eps')
Barchart.Title = paste(Sp,' option',Option,'_effort_',Option2,'nruns',toString(nruns))
Line.Graph.Name = paste(Sp,'_linecharts_option',Option,'_effort_',Option2,'_nruns',toString(nruns),'.pdf')
# Map filenames: (only for deterministic case)
Harvest.before.name = paste0(Sp,'_harvest_before_map_option',Option,'_effort_',Option2,'.eps')
Harvest.after.name = paste0(Sp,'_harvest_after_map_option',Option,'_effort_',Option2,'.eps')
Harvest.change.name = paste0(Sp,'_harvest_change_map_option',Option,'_effort_',Option2,'.eps')
Biomass.before.name = paste0(Sp,'_biomass_before_map_option',Option,'_effort_',Option2,'.eps')
Biomass.after.name = paste0(Sp,'_biomass_after_map_option',Option,'_effort_',Option2,'.eps')
Biomass.change.name = paste0(Sp,'_biomass_change_map_option',Option,'_effort_',Option2,'.eps')
Yield.before.name = paste0(Sp,'_yield_before_map_option',Option,'_effort_',Option2,'.eps')
Yield.change.name = paste0(Sp,'_yield_change_map_option',Option,'_effort_',Option2,'.eps')
Yield.after.name = paste0(Sp,'_yield_after_map_option',Option,'_effort_',Option2,'.eps')

source('ModelSetup.R')

if (nruns == 1){
  params$sigma_r = 0 # make these runs deterministic
}

RunTheModel(nruns,Tmax,X,hab_means,HR,f,a,params,trips,D,Option,Option2,windfarms)
} # end loop over options 2
} # end loop over options
} # end loop over species
  