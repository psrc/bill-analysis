# Script for data analysis related to HB1110 data request
#
# It attaches various dummies to the input parcels file
# and writes two output parcels file, one with all parcels included 
# in the bill analysis and one with parcels likely to develop.
# It then creates several city-level summaries and exports 
# the into csv files.
#
# Last update: 02/08/2023
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
tier_file_name <- "cities_coded.csv"

# tier definitions
tier_constraints <- list(`1` = c(4, 2), # in the form c(hct_constraint, non-hct_constraint)
                         `2` = c(6, 4)
                         #`1` = c(6, 4) # original constraint
                         )
#tier_column <- "Original"
tier_column <- "Substitute"

# name of the output files; should include "XXX" which will be replaced by "in_bill" and "to_develop" to distinguish the two files
output_parcels_file_name <- paste0("selected_parcels_for_mapping_XXX-", Sys.Date(), ".csv") # will be written into data_dir
# directory name where summary files should be written
output_summary_dir <- paste0("csv_summaries-", Sys.Date())



# Read input files
parcels_for_bill_analysis <- fread(file.path(data_dir, parcels_file_name)) # parcels
parcel_vision_hct  <- fread(file.path(data_dir, parcel_vision_hct_file_name)) # HCT locations
cities <- fread(file.path(data_dir, cities_file_name)) # cities table
tiers_by_city <- fread(file.path(data_dir, tier_file_name))[, c("city_id", tier_column), with = FALSE] # table containing tier assignment to cities
setnames(tiers_by_city, tier_column, "tier") # rename the tier column to "tier" for simpler access
cities <- merge(cities, tiers_by_city)

# Get all non-zero tiers and check if there is a definition for them
tiers <- as.character(unique(cities[tier > 0, tier]))
if(any(! tiers %in% names(tier_constraints))) 
    stop("Missing definition of tier(s) ", paste(tiers[! tiers %in% names(tier_constraints)], collapse = ", "), " in the tier_constraints object.")

#Fixes City ID error in the input dataset (might not be needed after it's corrected in the input file)
parcels_for_bill_analysis[city_id==95, city_id := 96]

# Codes records that fall in cities/geographies excluded from HB1110 bill requirements with 0's and and everything else with 1's 
parcels_for_bill_analysis[cities, city_tier := i.tier, on = "city_id"]
    
# Creates updated parcel table with "hct_vision" field added
parcels_updated <- merge(parcels_for_bill_analysis, parcel_vision_hct, all=TRUE)

#Creates "res_zone" field that denotes residential zoned parcels
parcels_updated[, res_zone := 0]
parcels_updated[DUcap > 0 & is_mixed_cap == 0, res_zone := 1]

#Creates "mix_zone" field that denotes mixed-use zoned parcels
parcels_updated[, mixed_zone := 0]
parcels_updated[DUcap > 0 & is_mixed_cap == 1, mixed_zone := 1]

#Creates "already_zoned" field to denote parcels that are already zoned to meet requirements of bill (Step 3 in Methodology document)
parcels_updated[, already_zoned := 0]
for(tier in tiers) { # iterate over the non-zero city tiers
    parcels_updated[(vision_hct == 1 & DUcap >= tier_constraints[[tier]][1]) | 
                        (vision_hct == 0 & DUcap >= tier_constraints[[tier]][2]), already_zoned := 1]
}

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


#Adds city_name field to final parcel table
parcels_final <- merge(parcels_updated, cities[, .(city_id, city_name)],  by = "city_id")

# split parcels into two sets: 1. all parcels included in the bill, 2. parcels likely to develop
parcels_in_bill <- parcels_final[city_tier > 0 & already_zoned == 0 & (res_zone == 1 | mixed_zone == 1)]
parcels_likely_to_develop <- parcels_final[city_tier > 0 & already_zoned == 0 & (res_zone == 1 | mixed_zone == 1) & sq_ft_less_2500 == 0 & land_greater_improvement == 1 & built_sqft_less_1400 == 1 &  (vacant == 1 | sf_use == 1), ]

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
    setnames(detail, colnames(detail)[-1], paste0(col_prefix, colnames(detail)[-1]))
    return(detail)
}

create_summary <- function(dt){
    # part that involves all parcels
    summary_all <- dt[, .(
        tier = mean(city_tier),
        total_parcels = .N, 
        already_zoned = sum(already_zoned)
    ), by = "city_id"][order(city_id)]
    
    # generate HCT and nonHCT parts of the summary
    summary_hct <- create_summary_detail(dt[vision_hct == 1 & already_zoned == 0], col_prefix = "hct_")
    summary_nonhct <- create_summary_detail(dt[vision_hct == 0 & already_zoned == 0], col_prefix = "nhct_")
    
    # merge together and add city_name
    summary_final <- merge(merge(cities[, .(city_id, city_name)], summary_all, by = "city_id", all = TRUE),
                           merge(summary_hct, summary_nonhct, by = "city_id", all = TRUE),
                           by = "city_id", all = TRUE)
    return(summary_final)
}

# Create summaries
summary_all_parcels <- create_summary(parcels_final)
summary_filter_parcel_sqft <- create_summary(parcels_final[sq_ft_less_2500 == 0])
summary_filter_under1400 <- create_summary(parcels_final[sq_ft_less_2500 == 0 & built_sqft_less_1400 == 1])
summary_filter_land_value <- create_summary(parcels_final[sq_ft_less_2500 == 0 & land_greater_improvement == 1])
summary_filter_both_mkt <- create_summary(parcels_final[sq_ft_less_2500 == 0 & land_greater_improvement == 1 & built_sqft_less_1400 == 1])

# create a dataset of existing units
existing_units <- merge(cities[, .(city_id, tier, city_name)], 
                        parcels_in_bill[, .(total = sum(residential_units), 
                                                  HCT = sum(residential_units * vision_hct == 1), 
                                                  nonHCT = sum(residential_units * (vision_hct == 0))),
                                              by = "city_id"],
                        by = "city_id")
# add regional totals as the first row
existing_units <- rbind(data.table(city_id = 0, tier = 0, city_name = "Region", 
                                   total = existing_units[tier > 0, sum(total)], 
                                   HCT = existing_units[tier > 0, sum(HCT)],
                                   nonHCT = existing_units[tier > 0, sum(nonHCT)]),
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


