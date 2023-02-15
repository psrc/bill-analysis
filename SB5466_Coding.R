# Script for data analysis related to SB5466 data request
#
# It attaches various dummies to the input parcels file
# and writes two output parcels file, one with all parcels included 
# in the bill analysis and one with parcels likely to develop.
# It then creates several city-level summaries and exports 
# the into csv files.
#
# Last update: 02/15/2023
# Drew Hanson & Hana Sevcikova

if(! "data.table" %in% installed.packages())
    install.packages("data.table")

library(data.table)

# Run this script from the directory of this file, unless data_dir is set as an absolute path
setwd("J:/Projects/Bill-Analysis/2023/scripts")
#setwd("~/psrc/R/bill-analysis/scripts")

# Settings
write.parcels.file <- TRUE
write.summary.files.to.csv <- TRUE
write.summary.files.to.excel <- TRUE

data_dir <- "../data" # directory where the data files below live 
                      # (it's a relative path to the script location; can be also set as an absolute path)
parcels_file_name <- "parcels_for_bill_analysis.csv" 
parcel_vision_hct_file_name <- "parcel_vision_hct_5466.csv"
cities_file_name <- "cities.csv"
tier_file_name <- "cities_coded_all.csv"
plan_type_file_name <- "plan_type_id_summary_r109_script.csv"

tier_column <- "Original"
#tier_column <- "Substitute"

# market factor used for comparing land value to improvement value
market_factor <- 1

# name of the output files; should include "XXX" which will be replaced by "in_bill" and "to_develop" to distinguish the two files
output_parcels_file_name <- paste0("selected_parcels_for_mapping_SB5466_XXX-", Sys.Date(), ".csv") # will be written into data_dir
# directory name where summary files should be written
output_summary_dir <- paste0("summaries_SB5466_mfactor", market_factor, "-", Sys.Date())

upper_far_limit <- 50

# Read input files
parcels_for_bill_analysis <- fread(file.path(data_dir, parcels_file_name)) # parcels
parcel_vision_hct  <- fread(file.path(data_dir, parcel_vision_hct_file_name)) # HCT locations
cities <- fread(file.path(data_dir, cities_file_name)) # cities table
tiers_by_city <- fread(file.path(data_dir, tier_file_name))[, c("city_id", tier_column), with = FALSE] # table containing tier assignment to cities
plan_type <- fread(file.path(data_dir, plan_type_file_name)) #plan_type table
setnames(tiers_by_city, tier_column, "tier") # rename the tier column to "tier" for simpler access
cities <- merge(cities, tiers_by_city, all = TRUE)
cities[is.na(tier), tier := 0]

#Replace "-" with NAs in plan_type table
for(col in setdiff(colnames(plan_type), c("plan_type_id", "zoned_use", "zoned_use_sf_mf"))){ # iterate over all columns except 3 columns 
    plan_type[get(col) == "-", (col) := NA]
    plan_type[, (col) := gsub(",", "", get(col))] # remove ',' 
    plan_type[, (col) := as.numeric(get(col))] # convert to numeric
}

#Fixes City ID error in the input dataset (might not be needed after it's corrected in the input file)
parcels_for_bill_analysis[city_id==95, city_id := 96]

parcels_for_bill_analysis[cities, city_tier := i.tier, on = "city_id"]
    
# Creates updated parcel table with "hct_vision" field added
parcels_updated <- merge(parcels_for_bill_analysis, parcel_vision_hct,by.x = "parcel_id",by.y = "pin_1", 
                         all.x=TRUE, all.y = FALSE)
parcels_updated[is.na(vision_hct), vision_hct := 0]

#Adds new fields from "plan_type" table to final parcel table
parcels_updated <- merge(parcels_updated, plan_type[, .(plan_type_id,max_du,max_far,is_mixed_use_2,zoned_use,zoned_use_sf_mf)],  by = "plan_type_id")

#Creates "land_greater_improvement" field to denotes parcels that have land value that is greater than the improvement value
parcels_updated[is.na(improvement_value), improvement_value := 0]
parcels_updated[, land_greater_improvement := 0]
parcels_updated[land_value > market_factor * improvement_value, land_greater_improvement := 1] 

#Creates "sq_ft_10000" field to denotes parcels that have developable land area of at least 10,000 sqft
parcels_updated[, sq_ft_10000 := 0]
parcels_updated[parcel_sqft >= 10000, sq_ft_10000 := 1]

#Maximum zoned dwelling units per acre on mixed use parcels, assuming 50%/50% res/nonres development split
parcels_updated[, max_du_mixed := max_du/2]

#Maximum zoned non-residential floor area ratio (FAR) per acre on mixed use parcels, assuming 50%/50% res/nonres development split
parcels_updated[, max_far_mixed := max_far/2]

#Formulas to convert maximum zoned capacity into FAR values
# For small parcels this value can be huge. Thus, we restrict it to be upper_far_limit (50) at max.
parcels_updated[, zoned_far_res := 0]
parcels_updated[zoned_use == "residential" & parcel_sqft > 0, zoned_far_res := pmin(upper_far_limit, max_du * 1452/parcel_sqft)]

parcels_updated[, zoned_far_res_mixed := 0]
parcels_updated[zoned_use == "mixed" & parcel_sqft > 0, zoned_far_res_mixed := pmin(upper_far_limit, max_du_mixed * 1452/parcel_sqft)] # don't divide by two since max_du_mixed has been already halfed

parcels_updated[, zoned_far_nonres := 0]
parcels_updated[zoned_use == "commercial", zoned_far_nonres := pmin(upper_far_limit, max_far * parcel_sqft/43560)]

parcels_updated[, zoned_far_nonres_mixed := 0]
parcels_updated[zoned_use == "mixed", zoned_far_nonres_mixed := pmin(upper_far_limit, max_far_mixed * parcel_sqft/43560)]

parcels_updated[, zoned_far_mixed := 0]
parcels_updated[zoned_use == "mixed", zoned_far_mixed := pmin(upper_far_limit, zoned_far_res_mixed + zoned_far_nonres_mixed)]

parcels_updated[,zoned_far := pmin(upper_far_limit, zoned_far_res + zoned_far_mixed + zoned_far_nonres)]

parcels_updated[, zoned_far_6 := 0]
parcels_updated[zoned_far < 6, zoned_far_6 := 1]

parcels_updated[, zoned_far_4 := 0]
parcels_updated[zoned_far < 4, zoned_far_4 := 1]

parcels_updated[, zoned_far_for_tier := 0]
parcels_updated[((zoned_far_6 == 0) & (vision_hct == 1)) | ((zoned_far_4 == 0) & (vision_hct %in% c(2,3))), zoned_far_for_tier := 1]

#Formulas to convert built square footage into FAR values
parcels_updated[, current_far_res := 0]
parcels_updated[parcel_sqft > 0,current_far_res := pmin(upper_far_limit, residential_sqft/parcel_sqft)]

parcels_updated[, current_far_nonres := 0]
parcels_updated[parcel_sqft > 0, current_far_nonres := pmin(upper_far_limit, non_residential_sqft/parcel_sqft)]

parcels_updated[, current_far_mixed := 0]
parcels_updated[current_far_res > 0 & current_far_nonres > 0, current_far_mixed := pmin(upper_far_limit, current_far_res + current_far_nonres)]
parcels_updated[, current_far := pmin(upper_far_limit, current_far_res + current_far_nonres + current_far_mixed)]
parcels_updated[, net_far := pmax(zoned_far - current_far, 0)]

parcels_updated[, zoned_du := zoned_far_res * 30]
parcels_updated[, net_du := pmax(zoned_du - residential_units, 0)]

parcels_updated[, zoned_nonres_sqft := zoned_far_nonres * 43560]
parcels_updated[, net_nonres_sqft := pmax(zoned_nonres_sqft - non_residential_sqft, 0)]


#Current built square footage(FAR) less than 1.0 (Market criteria #2)
parcels_updated[, current_far_1 := 0]
parcels_updated[current_far < 1, current_far_1 := 1]

#Parcel meets both market criteria 1 and 2
parcels_updated[, both_value_size := 0]
parcels_updated[land_greater_improvement == 1 & current_far_1 == 1, both_value_size := 1]


#Adds city_name field to final parcel table
parcels_final <- merge(parcels_updated, cities[, .(city_id, city_name)],  by = "city_id")

# split parcels into two sets: 1. all parcels included in the bill, 2. parcels likely to develop
parcels_in_bill <- parcels_final[(zoned_use %in%  c("residential", "commercial", "mixed") & sq_ft_10000 == 1 & zoned_far_for_tier == 0) & (vision_hct > 0)]
parcels_likely_to_develop <- parcels_in_bill[both_value_size == 1, ]

#Writes csv output files
if(write.parcels.file) {
    #remove all fields from parcel tables except parcel_id, vision_hct
    parcels_in_bill_to_save <- parcels_in_bill[, c("parcel_id", "vision_hct")]
    parcels_likely_to_develop_to_save <- parcels_likely_to_develop[, c("parcel_id", "vision_hct")]
    # write to disk
    fwrite(parcels_in_bill_to_save, file.path(data_dir, gsub("XXX", "in_bill", output_parcels_file_name)))
    fwrite(parcels_likely_to_develop_to_save, file.path(data_dir, gsub("XXX", "to_develop", output_parcels_file_name)))
    cat("\nParcels written into ", file.path(data_dir, output_parcels_file_name), "\n")
}


# Functions for generating summaries
create_summary_detail <- function(dt, col_prefix, column_to_sum = "one", decimal = 0){
    detail <- dt[, .(
        total = round(sum((zoned_use %in%  c("residential", "commercial", "mixed") & sq_ft_10000 == 1 & zoned_far_for_tier == 0)*get(column_to_sum)), decimal),
        res = round(sum((zoned_use == "residential" & sq_ft_10000 == 1 & zoned_far_for_tier == 0)*get(column_to_sum)), decimal),
        mixed = round(sum((zoned_use == "mixed" & sq_ft_10000 == 1 & zoned_far_for_tier == 0)*get(column_to_sum)), decimal),
        comm = round(sum((zoned_use == "commercial" & sq_ft_10000 == 1 & zoned_far_for_tier == 0)*get(column_to_sum)), decimal),
        zoned_for_far = round(sum((zoned_use %in%  c("residential", "commercial", "mixed") & sq_ft_10000 == 1 & zoned_far_for_tier == 1)*get(column_to_sum)), decimal),
        sqft_lt_10T = round(sum((zoned_use %in%  c("residential", "commercial", "mixed") & sq_ft_10000 == 0)*get(column_to_sum)), decimal),
        industrial = round(sum((! zoned_use %in%  c("residential", "commercial", "mixed"))*get(column_to_sum)), decimal)
    ), by = "city_id"][order(city_id)]
    
    # add prefix to column names (excluding city_id which is first)
    setnames(detail, colnames(detail)[-1], paste0(col_prefix, colnames(detail)[-1]))
    return(detail)
}

create_summary <- function(dt, column_to_sum = "one", decimal = 0){
    # part that involves all parcels
    summary_all <- dt[, .(
        total_parcels = round(sum(get(column_to_sum)), decimal), 
        subject_to_proposal = round(sum((zoned_use %in%  c("residential", "commercial", "mixed") & sq_ft_10000 == 1 & zoned_far_for_tier == 0 & vision_hct > 0)*get(column_to_sum)), decimal),
        not_in_hct = round(sum((vision_hct == 0)*get(column_to_sum)), decimal)
    ), by = "city_id"][order(city_id)]
    
    # generate tier 1 and tier 2 parts of the summary
    summary_tier1 <- create_summary_detail(dt[vision_hct == 1], col_prefix = "tier1_", column_to_sum = column_to_sum, decimal = decimal)
    summary_tier2 <- create_summary_detail(dt[vision_hct %in% c(2,3)], col_prefix = "tier2_", column_to_sum = column_to_sum, decimal = decimal)
    
    # merge together and add city_name
    summary_final <- merge(merge(cities[, .(city_id, city_name, tier)], summary_all, by = "city_id", all = TRUE),
                           merge(summary_tier1, summary_tier2, by = "city_id", all = TRUE),
                           by = "city_id", all = TRUE)[order(-tier, city_id)]
    return(summary_final)
}

# Create summaries
parcels_final[, one := 1] # dummy for summing # of parcels

summaries <- list()
summaries[["parcel_count"]] <- create_summary(parcels_final) # Table 1
summaries[["gross_far"]] <- create_summary(parcels_final, column_to_sum = "zoned_far", decimal = 1) # Table 2
summaries[["existing_far"]] <- create_summary(parcels_final, column_to_sum = "current_far", decimal = 1) # Table 3
summaries[["net_far"]] <- create_summary(parcels_final, column_to_sum = "net_far", decimal = 1) # Table 4
summaries[["parcel_count_land_value"]] <- create_summary(parcels_final[land_greater_improvement == 1]) # Table 5
summaries[["parcel_count_far_1"]]  <- create_summary(parcels_final[current_far_1 == 1]) # Table 6
parcels_meeting_market_cond <- parcels_final[both_value_size == 1]
summaries[["parcel_count_market"]]  <- create_summary(parcels_meeting_market_cond) # Table 7
summaries[["res_far_market"]]  <- create_summary(parcels_meeting_market_cond, 
                                                           column_to_sum = "zoned_far_res", decimal = 1) # Table 8
summaries[["zoned_du_market"]]  <- create_summary(parcels_meeting_market_cond, 
                                                           column_to_sum = "zoned_du") # Table 9
summaries[["exist_du_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                            column_to_sum = "residential_units") # Table 10
summaries[["net_du_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                    column_to_sum = "net_du") # Table 11
summaries[["zoned_nonres_far_market"]] <- create_summary(parcels_meeting_market_cond, 
                                               column_to_sum = "zoned_far_nonres") # Table 12
summaries[["zoned_nonres_sqft_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                   column_to_sum = "zoned_nonres_sqft") # Table 13
summaries[["exist_nonres_sqft_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                    column_to_sum = "non_residential_sqft") # Table 14
summaries[["net_nonres_sqft_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                      column_to_sum = "net_nonres_sqft") # Table 15

if(write.summary.files.to.csv || write.summary.files.to.excel){
    summary_dir <- file.path(data_dir, output_summary_dir)
    if(!dir.exists(summary_dir)) dir.create(summary_dir) # create directory if not exists
    if(write.summary.files.to.csv) {
        for(table in names(summaries))
            fwrite(summaries[[table]], file = file.path(summary_dir, paste0(table, ".csv")))
    }
    if(write.summary.files.to.excel) {
        library(openxlsx)
        write.xlsx(summaries, file = file.path(summary_dir, 
                                               paste0("SB5466_mfactor", market_factor, "_summaries_all_tables.xlsx")))
    }
    cat("\nSummaries written into ", summary_dir, "\n")
}


