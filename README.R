# #README
# to run the model first decide which species you want to run -> Lingcod or Dover sole
# Step 1: Run DataReadIn.R (all needed packages are at the top of this file)
#   you will need to have 5 datasets downloaded and correct file pathway:
#   the SDM data downloaded (OSU_offshorewind_5_species.rds), 
#   the OWF parcel data downloaded (OWF_Parcelled.xlx),
#   the OR Coastline shapefile (or_coastline_proj.shp)
#   the trawl data downloaded (Trawl 05-22 Fixed_E.xlsx) 
#   the fixed gear data downloaded (FG 05-22 Fixed_E.xlsx)
    #SDM data from: Liu, O. R., E. J. Ward, S. C. Anderson, K. S. Andrews, 
    #L. A. K. Barnett, S. Brodie, G. Carroll, J. Fiechter, M. A. Haltuch, 
    #C. J. Harvey, E. L. Hazen, P.-Y. Hernvann, M. Jacox, I. C. Kaplan, 
    #S. Matson, K. Norman, M. P. Buil, R. L. Selden, A. Shelton, and J. R. Samhouri. 
    #(2023). Species redistribution creates unequal outcomes for multispecies 
    #fisheries under projected climate change. Science Advances. 
    #trawl and fixed gear dataset from the Oregon Deparment of Fish and Wildlife 
# Step 2: Run HR_2D.R
# Step 3: Run leslie_matrix.R
# Step 4: Run spatial_model2.R
# Step 5: If running Lingcod run lcod_X.R OR If running Dover sole run dver_X.R
# Step 6: If running Lingcod run avg_lingcod.R OR If running Dover sole run doversole.R
# Step 7: Now you can run the model in RuntheModel.R
# Step 8: Look at data - all code for graphing is in RuntheModel.R
