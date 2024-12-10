# Script for creating mapping data related to SB5466 data request
#
# It attaches various dummies and capacity info to the input parcels file
# and writes it out. 
#
# Last update: 12/09/2024
# Hana Sevcikova

if(! "data.table" %in% installed.packages())
    install.packages("data.table")

library(data.table)

# Run this script from the directory of this file, unless data_dir is set as an absolute path
#setwd("J:/Projects/Bill-Analysis/2023/scripts")
setwd("~/psrc/R/bill-analysis/scripts")

# Settings
write.allparcels.file <- TRUE
#write.parcels.file <- FALSE
#write.summary.files.to.csv <- FALSE
#write.summary.files.to.excel <- FALSE

data_dir <- "../data2023" # directory where the data files below live 
                      # (it's a relative path to the script location; can be also set as an absolute path)
parcels_file_name <- "parcels_for_bill_analysis_2024-12-09.csv"

cities_file_name <- "cities.csv"

plan_type_file_name <- "plan_type_id_summary_r109_script.csv"


# size restriction to be included in parts 2-4 of the bill
min_parcel_sqft_for_analysis <- 5000

# market factor used for comparing land value to improvement value
market_factor <- 1.5

# max FAR that would trigger re-development
max_far_to_redevelop <- 0.5

# max FAR for limiting mostly messy data
upper_far_limit <- 50

# sqft per unit assumption
sqft_per_du <-1200

# FAR used for computing capacity measures in (hct-1, hct-2)
#potential_far_for_capacity <- c(6, 4)
#potential_far_for_capacity <- c(5.1, 3.4)
potential_far_for_capacity <- c(3, 2.5)


# object used to identify directories/files with the various scenarios
scenario_string <- paste0("_lotsize", min_parcel_sqft_for_analysis, 
                          "_mfactor", market_factor, 
                          "_far", max_far_to_redevelop,
                          #paste0("_potfar", potential_far_for_capacity[1], "_", potential_far_for_capacity[2])
                          paste0("_potfar", potential_far_for_capacity[1])
                          )

# directory name where results should be written
output_dir <- paste0("../results-SB5466")
# name of the output file
output_parcels_file_name <- paste0("all_parcels_for_mapping", scenario_string, "_", Sys.Date(), ".csv") 

                          
# Read input files
parcels_for_bill_analysis <- fread(file.path(data_dir, parcels_file_name)) # parcels
cities <- fread(file.path(data_dir, cities_file_name)) # cities table
plan_type <- fread(file.path(data_dir, plan_type_file_name)) #plan_type table

#Replace "-" with NAs in plan_type table
for(col in setdiff(colnames(plan_type), c("plan_type_id", "zoned_use", "zoned_use_sf_mf"))){ # iterate over all columns except 3 columns
    plan_type[get(col) == "-", (col) := NA]
    plan_type[, (col) := gsub(",", "", get(col))] # remove ','
    plan_type[, (col) := as.numeric(get(col))] # convert to numeric
}

parcels_updated <- copy(parcels_for_bill_analysis)
parcels_updated[, vision_hct := as.integer(tod_id > 0 & control_hct_id > 1000)]

#Adds new fields from "plan_type" table to final parcel table
parcels_updated <- merge(parcels_updated, plan_type[, .(plan_type_id,max_du,max_far,is_mixed_use_2,zoned_use,zoned_use_sf_mf)],  by = "plan_type_id")
parcels_updated[is.na(max_far), max_far := 0]
parcels_updated[is.na(max_du), max_du := 0]

# lock all parks
parcels_updated[land_use_type_id == 19, zoned_use := "other"]

# tag parcels outside of UGB as not HCT
parcels_updated[is_inside_urban_growth_boundary == 0, vision_hct := 0]

#Creates "land_greater_improvement" field to denotes parcels that have land value that is greater than the improvement value
parcels_updated[is.na(improvement_value), improvement_value := 0]
parcels_updated[, land_greater_improvement := 0]
parcels_updated[land_value > market_factor * improvement_value, land_greater_improvement := 1] 

#Creates a dummy based on the parcel size restriction
parcels_updated[, developable_sq_ft := 0]
parcels_updated[parcel_sqft >= min_parcel_sqft_for_analysis, developable_sq_ft := 1]

#Maximum zoned dwelling units per acre on mixed use parcels, assuming 50%/50% res/nonres development split
parcels_updated[, max_du_mixed := max_du/2]

#Maximum zoned non-residential floor area ratio (FAR) per acre on mixed use parcels, assuming 50%/50% res/nonres development split
parcels_updated[, max_far_mixed := max_far/2]

# Compute zoned DUs 
parcels_updated[, zoned_du_res := 0]
parcels_updated[zoned_use == "residential", zoned_du_res := max_du * parcel_sqft/43560]

parcels_updated[, zoned_du_mixed := 0]
parcels_updated[zoned_use == "mixed", zoned_du_mixed := max_du_mixed * parcel_sqft/43560]

parcels_updated[, zoned_du := zoned_du_res + zoned_du_mixed]

# Compute zoned residential sqft
parcels_updated[, zoned_sqft_res_only := 0]
parcels_updated[zoned_use == "residential", zoned_sqft_res_only := zoned_du_res * sqft_per_du]

parcels_updated[, zoned_sqft_res_mixed := 0]
parcels_updated[zoned_use == "mixed", zoned_sqft_res_mixed := zoned_du_mixed * sqft_per_du]

parcels_updated[, zoned_sqft_res := zoned_sqft_res_only + zoned_sqft_res_mixed]

# Compute zoned non-res sqft
parcels_updated[, zoned_sqft_nonres_only := 0]
parcels_updated[! zoned_use %in% c("residential", "mixed"), zoned_sqft_nonres_only := max_far * parcel_sqft]

parcels_updated[, zoned_sqft_nonres_mixed := 0]
parcels_updated[zoned_use == "mixed", zoned_sqft_nonres_mixed := max_far_mixed * parcel_sqft]

parcels_updated[, zoned_sqft_nonres := zoned_sqft_nonres_only + zoned_sqft_nonres_mixed]

#Formulas to convert maximum zoned capacity into FAR values
# For small parcels this value can be huge. Thus, we restrict it to be upper_far_limit (50) at max.
parcels_updated[, zoned_far_res := 0]
parcels_updated[zoned_use == "residential" & parcel_sqft > 0, zoned_far_res := pmin(upper_far_limit, zoned_du_res * sqft_per_du/parcel_sqft)]

parcels_updated[, zoned_far_res_mixed := 0]
parcels_updated[zoned_use == "mixed" & parcel_sqft > 0, zoned_far_res_mixed := pmin(upper_far_limit, zoned_du_mixed * sqft_per_du/parcel_sqft)] # don't divide by two since max_du_mixed has been already halfed

parcels_updated[, zoned_far_nonres := 0]
parcels_updated[! zoned_use %in% c("residential", "mixed"), zoned_far_nonres := max_far]

parcels_updated[, zoned_far_nonres_mixed := 0]
parcels_updated[zoned_use == "mixed", zoned_far_nonres_mixed := max_far_mixed]

parcels_updated[, zoned_far_mixed := 0]
parcels_updated[zoned_use == "mixed", zoned_far_mixed := pmin(upper_far_limit, zoned_far_res_mixed + zoned_far_nonres_mixed)]

parcels_updated[,zoned_far := pmin(upper_far_limit, zoned_far_res + zoned_far_mixed + zoned_far_nonres)]

parcels_updated[, zoned_far_lt_6 := 0]
parcels_updated[zoned_far < 6, zoned_far_lt_6 := 1]

parcels_updated[, zoned_far_lt_4 := 0]
parcels_updated[zoned_far < 4, zoned_far_lt_4 := 1]

parcels_updated[, zoned_far_for_tier := 0]
parcels_updated[((zoned_far_lt_6 == 0) & (vision_hct == 1)) | ((zoned_far_lt_4 == 0) & (vision_hct %in% c(2,3))), zoned_far_for_tier := 1]

#Formulas to convert built square footage into FAR values
parcels_updated[, current_far_res := 0]
parcels_updated[parcel_sqft > 0, current_far_res := pmin(upper_far_limit, residential_sqft/parcel_sqft)]

parcels_updated[, current_far_nonres := 0]
parcels_updated[parcel_sqft > 0, current_far_nonres := pmin(upper_far_limit, non_residential_sqft/parcel_sqft)]

parcels_updated[, current_far_mixed := 0]
parcels_updated[current_far_res > 0 & current_far_nonres > 0, current_far_mixed := pmin(upper_far_limit, current_far_res + current_far_nonres)]
parcels_updated[, current_far := pmin(upper_far_limit, current_far_res + current_far_nonres + current_far_mixed)]

#Current built square footage(FAR) less than 1.0 (Market criteria #2)
parcels_updated[, current_far_to_redevelop := 0]
parcels_updated[current_far < max_far_to_redevelop, current_far_to_redevelop := 1]

#Parcel meets both market criteria 1 and 2
parcels_updated[, both_value_size := 0]
parcels_updated[land_greater_improvement == 1 & current_far_to_redevelop == 1, both_value_size := 1]

# Dummy to indicate if parcel is included in the bill regardless of size restriction, or not
parcels_updated[, is_in_bill_no_size := 0]
parcels_updated[zoned_use %in%  c("residential", "commercial", "mixed") & zoned_far_for_tier == 0 & vision_hct > 0, is_in_bill_no_size := 1]

# Dummy that adds a size restriction to the previous filter
parcels_updated[, is_in_bill := 0]
parcels_updated[is_in_bill_no_size == 1 & developable_sq_ft == 1, is_in_bill := 1]

parcels_updated[, is_yrbuilt_for_redevelop := Nblds > 0 & (is.na(max_year_built) | max_year_built < 1600 | (max_year_built > 1945 & max_year_built < 1990))]


# upzone eligible parcels
parcels_updated[, `:=`(potential_far_res_mixed = zoned_far_res_mixed, potential_far_res = zoned_far_res, 
                       potential_far_nonres_mixed = zoned_far_nonres_mixed, potential_far_nonres = zoned_far_nonres, 
                       potential_far = zoned_far)]
parcels_updated[is_in_bill_no_size & vision_hct == 1 & zoned_use == "mixed", `:=`(potential_far_res_mixed = pmax(potential_far_for_capacity[1]/2, potential_far_res_mixed),
                                                                          potential_far_nonres_mixed = pmax(potential_far_for_capacity[1]/2, potential_far_nonres_mixed)
                                                                                                         )]
parcels_updated[is_in_bill_no_size & vision_hct == 1 & zoned_use != "mixed", `:=`(potential_far_res = pmax(potential_far_for_capacity[1], potential_far_res),
                                                                          potential_far_nonres = pmax(potential_far_for_capacity[1], potential_far_nonres))]
parcels_updated[is_in_bill_no_size & vision_hct == 2 & zoned_use == "mixed", `:=`(potential_far_res_mixed = pmax(potential_far_for_capacity[2]/2, potential_far_res_mixed),
                                                                          potential_far_nonres_mixed = pmax(potential_far_for_capacity[2]/2, potential_far_nonres_mixed)
                                                                            )]
parcels_updated[is_in_bill_no_size & vision_hct == 2 & zoned_use != "mixed", `:=`(potential_far_res = pmax(potential_far_for_capacity[2], potential_far_res),
                                                                          potential_far_nonres = pmax(potential_far_for_capacity[2], potential_far_nonres))]
parcels_updated[, potential_far := potential_far_res_mixed + potential_far_nonres_mixed + potential_far_res + potential_far_nonres]

# Compute potential DUs & sqft
parcels_updated[, `:=`(potential_du = zoned_du, potential_sqft_res = zoned_sqft_res, potential_sqft_nonres = zoned_sqft_nonres)]
parcels_updated[zoned_use %in% c("residential", "mixed"), `:=`(potential_du = (potential_far_res + potential_far_res_mixed) * parcel_sqft/sqft_per_du,
                                                               potential_sqft_res = (potential_far_res + potential_far_res_mixed) * parcel_sqft)]
parcels_updated[zoned_use != "residential", `:=`(potential_sqft_nonres = (potential_far_nonres + potential_far_nonres_mixed) * parcel_sqft)]

# Compute net DU, sqft
parcels_updated[, `:=`(net_du = pmax(potential_du - residential_units, 0),
                       net_res_sqft = pmax(potential_sqft_res - residential_sqft, 0),
                       net_nonres_sqft = pmax(potential_sqft_nonres - non_residential_sqft, 0)
                       )]

#Adds city_name field to final parcel table
parcels_final <- merge(parcels_updated, cities[, .(city_id, city_name)],  by = "city_id")

if(write.allparcels.file) {
    if(!dir.exists(output_dir))
        dir.create(output_dir)
    fwrite(parcels_final, file.path(output_dir, output_parcels_file_name))
    cat("\nParcels written into ", file.path(output_dir, output_parcels_file_name), "\n")
}


