# Script for data analysis related to HB1110 data request
#
# Important: Run this script from the directory of this file, unless data_dir is set as an absolute path
#
# Last update: 02/06/2023
# Drew Hanson & Hana Sevcikova

if(! "data.table" %in% installed.packages())
    install.packages("data.table")

library(data.table)

# Settings
write.parcels.file <- TRUE
write.summary.files <- TRUE

data_dir <- "../data" # directory where the data files below live 
                      # (it's a relative path to the script location; can be also set as an absolute path)
                      # Important: if using a relative path, run this script from the directory of this file
parcels_file_name <- "parcels_for_bill_analysis.csv" 
parcel_vision_hct_file_name <- "parcel_vision_hct.csv"
cities_file_name <- "cities18.csv"
output_parcels_file_name <- paste0("parcels_for_bill_analysis_updated-", Sys.Date(), ".csv") # will be written into data_dir


# Read input files
parcels_for_bill_analysis <- fread(file.path(data_dir, parcels_file_name)) # parcels
parcel_vision_hct  <- fread(file.path(data_dir, parcel_vision_hct_file_name)) # HCT locations
cities <- fread(file.path(data_dir, cities_file_name)) # cities table
                  
# Codes records that fall in cities/geographies excluded from HB1110 bill requirements with 0's and and everything else with 1's 
parcels_for_bill_analysis[ , applicable_city := 1] # assign 1 to all records
parcels_for_bill_analysis[city_id %in% c(8,19,36,39,44,47,48,51,52,54,64,66,68,72,74,78,81,82,83,92,95,96), applicable_city := 0 ] # exclude selected cities

# Creates updated parcel table with "hct_vision" field added
parcels_updated <- merge(parcels_for_bill_analysis, parcel_vision_hct, all=TRUE)

#Creates "cities_hct_combined" field that denotes records that fall inside an applicable city + and hct areas (1's-in,0's-out)
parcels_updated[, cities_hct_combined := 0] 
parcels_updated[applicable_city == 1 & vision_hct == 1, cities_hct_combined := 1]

#Creates "res_zone" field that denotes residential zoned parcels
parcels_updated[, res_zone := 0]
parcels_updated[DUcap > 0 & is_mixed_cap == 0, res_zone := 1]

#Creates "mix_zone" field that denotes mixed-use zoned parcels
parcels_updated[, mixed_zone := 0]
parcels_updated[DUcap > 0 & is_mixed_cap == 1, mixed_zone := 1]

#Creates "already_zoned" field to denotes parcels that are already zoned to meet requirements of bill (Step 3 in Methodology document)
parcels_updated[, already_zoned := 0]
parcels_updated[(vision_hct == 1 & DUcap >= 6.0) | (vision_hct == 0 & DUcap >= 4.0), already_zoned := 1]

#Creates "sq_ft_less_2500" field denoting records with parcel square footage of less then 2,500
parcels_updated[, sq_ft_less_2500 := 0]
parcels_updated[parcel_sqft < 2500, sq_ft_less_2500 := 1]

#Creates "sf_use" field denoting single family parcels
parcels_updated[, sf_use := 0]
parcels_updated[residential_units == 1, sf_use := 1]

#Creates "vacant" field denoting vacant parcels
parcels_updated[, vacant := 0]
parcels_updated[Nblds == 0, vacant := 1]

#Creates "land_greater_improvement" field to denotes parcels that have land value that is greater than the improvement value
parcels_updated[is.na(improvement_value), improvement_value := 0]
parcels_updated[, land_greater_improvement := 0]
parcels_updated[land_value >= improvement_value, land_greater_improvement := 1] # we use '>=' to catch cases when both are 0

#Creates "built_sqft_less_1400" field to denotes parcels that have a built square footage of less than 1,400
parcels_updated[, built_sqft_less_1400 := 0]
parcels_updated[residential_sqft < 1400, built_sqft_less_1400 := 1]

#Fixes City ID error in "parcels_for_bill_analysis" table
parcels_updated[city_id==95, city_id := 96]

#Adds city_name field to final parcel table
parcels_final <- merge(parcels_updated, cities[, .(city_id, city_name)],  by = "city_id")

#Writes a csv output file
if(write.parcels.file) 
    fwrite(parcels_final, file.path(data_dir, output_parcels_file_name))


# Create summaries
create_summary <- function(dt){
    summary_all <- dt[, .(
        total_parcels = .N, 
        already_zoned = sum(already_zoned)
    ), by = "city_id"][order(city_id)]
    
    summary_hct <- dt[cities_hct_combined == 1 & already_zoned == 0, .(
        hct_total_parcels = .N, 
        hct_res_vacant = sum(res_zone == 1 & vacant == 1),
        hct_res_sf_use = sum(res_zone == 1 & sf_use == 1),
        hct_res_other_use = sum(res_zone == 1 & vacant == 0 & sf_use == 0),
        hct_mix_vacant = sum(mixed_zone == 1 & vacant == 1),
        hct_mix_sf_use = sum(mixed_zone == 1 & sf_use == 1),
        hct_mix_other_use = sum(mixed_zone == 1 & vacant == 0 & sf_use == 0),
        hct_other = sum(res_zone == 0 & mixed_zone == 0)
    ), by = "city_id"][order(city_id)]
    
    summary_nonhct <- dt[cities_hct_combined == 0 & already_zoned == 0, .(
        nhct_total_parcels = .N, 
        nhct_res_vacant = sum(res_zone == 1 & vacant == 1),
        nhct_res_sf_use = sum(res_zone == 1 & sf_use == 1),
        nhct_res_other_use = sum(res_zone == 1 & vacant == 0 & sf_use == 0),
        nhct_mix_vacant = sum(mixed_zone == 1 & vacant == 1),
        nhct_mix_sf_use = sum(mixed_zone == 1 & sf_use == 1),
        nhct_mix_other_use = sum(mixed_zone == 1 & vacant == 0 & sf_use == 0),
        nhct_other = sum(res_zone == 0 & mixed_zone == 0)
    ), by = "city_id"][order(city_id)]
    
    summary_final <- merge(merge(cities[, .(city_id, city_name)], summary_all, by = "city_id"),
                           merge(summary_hct, summary_nonhct, by = "city_id"),
                           by = "city_id")
    return(summary_final)
}

summary_all_parcels <- create_summary(parcels_final)
summary_filter_parcel_sqft <- create_summary(parcels_final[sq_ft_less_2500 == 0])
summary_filter_under1400 <- create_summary(parcels_final[sq_ft_less_2500 == 0 & built_sqft_less_1400 == 1])
summary_filter_land_value <- create_summary(parcels_final[sq_ft_less_2500 == 0 & land_greater_improvement == 1])
summary_filter_both_mkt <- create_summary(parcels_final[sq_ft_less_2500 == 0 & land_greater_improvement == 1 & built_sqft_less_1400 == 1])

if(write.summary.files){
    #TODO: write out the summary files
}



