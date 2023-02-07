# Script for data analysis related to HB1110 data request
#
# It attaches various dummies to the input parcels file
# and writes two output parcels file, one with all parcels included 
# in the bill analysis and one with parcels likely to develop.
# It then creates several city-level summaries and exports 
# the into csv files.
#
# Last update: 02/07/2023
# Drew Hanson & Hana Sevcikova

if(! "data.table" %in% installed.packages())
    install.packages("data.table")

library(data.table)

# Run this script from the directory of this file, unless data_dir is set as an absolute path
setwd("J:/Projects/Bill-Analysis/2023/scripts")
#setwd("~/psrc/R/bill-analysis/scripts")

# Settings
write.parcels.file <- TRUE
write.summary.files <- TRUE

data_dir <- "../data" # directory where the data files below live 
                      # (it's a relative path to the script location; can be also set as an absolute path)
parcels_file_name <- "parcels_for_bill_analysis.csv" 
parcel_vision_hct_file_name <- "parcel_vision_hct.csv"
cities_file_name <- "cities18.csv"
# name of the output files; should include "XXX" which will be replaced by "in_bill" and "to_develop" to distinguish the two files
output_parcels_file_name <- paste0("selected_parcels_for_mapping_XXX-", Sys.Date(), ".csv") # will be written into data_dir
# directory name where summary files should be written
output_summary_dir <- paste0("csv_summaries-", Sys.Date())


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
parcels_updated[land_value > improvement_value, land_greater_improvement := 1] 

#Creates "built_sqft_less_1400" field to denotes parcels that have a built square footage of less than 1,400
parcels_updated[, built_sqft_less_1400 := 0]
parcels_updated[residential_sqft < 1400, built_sqft_less_1400 := 1]

#Fixes City ID error in "parcels_for_bill_analysis" table (might not be needed after it's corrected in the input file)
parcels_updated[city_id==95, city_id := 96]

#Adds city_name field to final parcel table
parcels_final <- merge(parcels_updated, cities[, .(city_id, city_name)],  by = "city_id")

# split parcels into two sets: 1. all parcels included in the bill, 2. parcels likely to develop
parcels_in_bill <- parcels_final[applicable_city == 1 & already_zoned == 0 & (res_zone == 1 | mixed_zone == 1)]
parcels_likely_to_develop <- parcels_final[applicable_city == 1 & already_zoned == 0 & (res_zone == 1 | mixed_zone == 1) & sq_ft_less_2500 == 0 & land_greater_improvement == 1 & built_sqft_less_1400 == 1 &  (vacant == 1 | sf_use == 1), ]

#Writes csv output files
if(write.parcels.file) {
    # reduce amount of data to be saved (remove city_name and reduce decimal digits for capacity columns)
    parcels_in_bill_to_save <- copy(parcels_in_bill)[, `:=`(city_name = NULL, DUcap = round(DUcap, 3), DUnetcap = round(DUnetcap, 3))]
    parcels_likely_to_develop_to_save <- copy(parcels_likely_to_develop)[, `:=`(city_name = NULL, DUcap = round(DUcap, 3), DUnetcap = round(DUnetcap, 3))]
    # write to disk
    fwrite(parcels_in_bill_to_save, file.path(data_dir, gsub("XXX", "in_bill", output_parcels_file_name)))
    fwrite(parcels_likely_to_develop_to_save, file.path(data_dir, gsub("XXX", "to_develop", output_parcels_file_name)))
    cat("\nParcels written into ", file.path(data_dir, output_parcels_file_name), "\n")
}


# Functions for generating summaries
create_summary_detail <- function(dt, col_prefix){
    detail <- dt[, .(
        total_parcels = .N, 
        res_vacant = sum(res_zone == 1 & vacant == 1),
        res_sf_use = sum(res_zone == 1 & sf_use == 1),
        res_other_use = sum(res_zone == 1 & vacant == 0 & sf_use == 0),
        mix_vacant = sum(mixed_zone == 1 & vacant == 1),
        mix_sf_use = sum(mixed_zone == 1 & sf_use == 1),
        mix_other_use = sum(mixed_zone == 1 & vacant == 0 & sf_use == 0),
        other = sum(res_zone == 0 & mixed_zone == 0)
    ), by = "city_id"][order(city_id)]
    
    # add prefix to column names (excluding city_id which is first)
    setnames(detail, colnames(detail)[-1], paste(col_prefix, colnames(detail)[-1], sep = "_"))
    return(detail)
}

create_summary <- function(dt){
    # part that involves all parcels
    summary_all <- dt[, .(
        total_parcels = .N, 
        already_zoned = sum(already_zoned)
    ), by = "city_id"][order(city_id)]
    
    # generate HCT and nonHCT parts of the summary
    summary_hct <- create_summary_detail(dt[cities_hct_combined == 1 & already_zoned == 0], col_prefix = "hct")
    summary_nonhct <- create_summary_detail(dt[cities_hct_combined == 0 & already_zoned == 0], col_prefix = "nhct")
    
    # merge together and add city_name
    summary_final <- merge(merge(cities[, .(city_id, city_name)], summary_all, by = "city_id"),
                           merge(summary_hct, summary_nonhct, by = "city_id"),
                           by = "city_id")
    return(summary_final)
}

# Create summaries
summary_all_parcels <- create_summary(parcels_final)
summary_filter_parcel_sqft <- create_summary(parcels_final[sq_ft_less_2500 == 0])
summary_filter_under1400 <- create_summary(parcels_final[sq_ft_less_2500 == 0 & built_sqft_less_1400 == 1])
summary_filter_land_value <- create_summary(parcels_final[sq_ft_less_2500 == 0 & land_greater_improvement == 1])
summary_filter_both_mkt <- create_summary(parcels_final[sq_ft_less_2500 == 0 & land_greater_improvement == 1 & built_sqft_less_1400 == 1])

# create a dataset of existing units
existing_units <- merge(cities[, .(city_id, city_name)], 
                        parcels_in_bill[, .(total = sum(residential_units), 
                                                  HCT = sum(residential_units * cities_hct_combined), 
                                                  nonHCT = sum(residential_units * (cities_hct_combined == 0))),
                                              by = "city_id"],
                        by = "city_id")
# add regional totals as the first row
existing_units <- rbind(data.table(city_id = 0, city_name = "Region", 
                                   total = existing_units[, sum(total)], 
                                   HCT = existing_units[, sum(HCT)],
                                   nonHCT = existing_units[, sum(nonHCT)]),
                        existing_units)

if(write.summary.files){
    summary_dir <- file.path(data_dir, output_summary_dir)
    if(!dir.exists(summary_dir)) dir.create(summary_dir) # create directory if not exists
    fwrite(summary_all_parcels, file = file.path(summary_dir, "all_parcels.csv"))
    fwrite(summary_filter_parcel_sqft, file = file.path(summary_dir, "filter_parcel_sqft.csv"))
    fwrite(summary_filter_under1400, file = file.path(summary_dir, "filter_parcel_sqft_under1400.csv"))
    fwrite(summary_filter_land_value, file = file.path(summary_dir, "filter_parcel_sqft_land_value.csv"))
    fwrite(summary_filter_both_mkt, file = file.path(summary_dir, "filter_all.csv"))
    fwrite(existing_units, file = file.path(summary_dir, "existing_units.csv"))
    cat("\nSummary files written into ", summary_dir, "\n")
}


