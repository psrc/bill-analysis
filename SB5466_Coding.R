# Script for data analysis related to SB5466 data request
#
# It attaches various dummies to the input parcels file
# and writes two output parcels file, one with all parcels included 
# in the bill analysis and one with parcels likely to develop.
# It then creates several city-level summaries and exports 
# the into csv files and an excel file.
#
# Last update: 02/28/2023
# Drew Hanson & Hana Sevcikova

if(! "data.table" %in% installed.packages())
    install.packages("data.table")

library(data.table)

# Run this script from the directory of this file, unless data_dir is set as an absolute path
#setwd("J:/Projects/Bill-Analysis/2023/scripts")
setwd("~/psrc/R/bill-analysis/scripts")

# Settings
write.parcels.file <- FALSE
write.summary.files.to.csv <- FALSE
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

# size restriction to be included in parts 2-4 of the bill
min_parcel_sqft_for_analysis <- 5000

# market factor used for comparing land value to improvement value
market_factor <- 1

# max FAR that would trigger re-development
max_far_to_redevelop <- 1

# max FAR for limiting mostly messy data
upper_far_limit <- 50

# name of the output files; should include "XXX" which will be replaced by "in_bill" and "to_develop" to distinguish the two files
output_parcels_file_name <- paste0("selected_parcels_for_mapping_SB5466_XXX-", Sys.Date(), ".csv") # will be written into data_dir
# directory name where results should be written
output_dir <- paste0("SB5466_results-", Sys.Date())

# object used to identify directories/files with the various scenarios
scenario_string <- paste0("_lotsize", min_parcel_sqft_for_analysis, 
                          "_mfactor", market_factor, 
                          "_far", max_far_to_redevelop)

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

# add city_tier column
parcels_for_bill_analysis[cities, city_tier := i.tier, on = "city_id"]
    
# remove duplicates from parcel_vision_hct
parcel_vision_hct_unique <- parcel_vision_hct[, .(vision_hct = min(vision_hct)), by = "pin_1"] # for each parcel take the minimum hct tier

# Creates updated parcel table with "hct_vision" field added
parcels_updated <- merge(parcels_for_bill_analysis, parcel_vision_hct_unique, by.x = "parcel_id",by.y = "pin_1", 
                         all.x=TRUE, all.y = FALSE)
parcels_updated[is.na(vision_hct), vision_hct := 0]

#Adds new fields from "plan_type" table to final parcel table
parcels_updated <- merge(parcels_updated, plan_type[, .(plan_type_id,max_du,max_far,is_mixed_use_2,zoned_use,zoned_use_sf_mf)],  by = "plan_type_id")

# lock all parks
parcels_updated[land_use_type_id == 19, zoned_use := "other"]

# remove parcels outside of UGB
parcels_updated <- parcels_updated[is_inside_urban_growth_boundary == 1]

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

# Compute zoned non-res sqft
parcels_updated[, zoned_sqft_nonres := 0]
parcels_updated[zoned_use == "commercial", zoned_sqft_nonres := max_far * parcel_sqft]

parcels_updated[, zoned_sqft_nonres_mixed := 0]
parcels_updated[zoned_use == "mixed", zoned_sqft_nonres_mixed := max_far_mixed * parcel_sqft]

parcels_updated[, zoned_sqft := zoned_sqft_nonres + zoned_sqft_nonres_mixed]

#Formulas to convert maximum zoned capacity into FAR values
# For small parcels this value can be huge. Thus, we restrict it to be upper_far_limit (50) at max.
parcels_updated[, zoned_far_res := 0]
parcels_updated[zoned_use == "residential" & parcel_sqft > 0, zoned_far_res := pmin(upper_far_limit, zoned_du_res * 1452/parcel_sqft)]

parcels_updated[, zoned_far_res_mixed := 0]
parcels_updated[zoned_use == "mixed" & parcel_sqft > 0, zoned_far_res_mixed := pmin(upper_far_limit, zoned_du_mixed * 1452/parcel_sqft)] # don't divide by two since max_du_mixed has been already halfed

parcels_updated[, zoned_far_nonres := 0]
parcels_updated[zoned_use == "commercial", zoned_far_nonres := max_far]

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

parcels_updated[, net_far := pmax(zoned_far - current_far, 0)]
parcels_updated[, net_du := pmax(zoned_du - residential_units, 0)]
parcels_updated[, net_nonres_sqft := pmax(zoned_sqft - non_residential_sqft, 0)]

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

#Adds city_name field to final parcel table
parcels_final <- merge(parcels_updated, cities[, .(city_id, city_name)],  by = "city_id")

# split parcels into two sets: 1. all parcels included in the bill, 2. parcels likely to develop
parcels_in_bill <- parcels_final[is_in_bill == 1]
parcels_likely_to_develop <- parcels_in_bill[both_value_size == 1, ]

#Writes csv output files
if(write.parcels.file) {
    #remove all fields from parcel tables except parcel_id, vision_hct
    parcels_in_bill_to_save <- parcels_in_bill[, c("parcel_id", "vision_hct")]
    parcels_likely_to_develop_to_save <- parcels_likely_to_develop[, c("parcel_id", "vision_hct")]
    # write to disk into a subdirectory of output_dir
    pcl_dir <- file.path(data_dir, output_dir, paste0("parcels", scenario_string))
    if(!dir.exists(pcl_dir)) dir.create(pcl_dir, recursive = TRUE)
    fwrite(parcels_in_bill_to_save, file.path(pcl_dir, gsub("XXX", "in_bill", output_parcels_file_name)))
    fwrite(parcels_likely_to_develop_to_save, file.path(pcl_dir, 
                                                        gsub("XXX", "to_develop", output_parcels_file_name)))
    cat("\nParcels written into ", file.path(pcl_dir, output_parcels_file_name), "\n")
}


# Functions for generating summaries
create_summary_detail <- function(dt, col_prefix, column_to_sum = "one", 
                                  include_size = TRUE, decimal = 0){
    if(include_size) {
        in_bill_column <- "is_in_bill" 
        size_column <- "developable_sq_ft"
    } else {
        in_bill_column <- "is_in_bill_no_size"
        size_column <- "one"
    }
    detail <- dt[, .(
        total = round(sum((get(in_bill_column) == 1)*get(column_to_sum)), decimal),
        res = round(sum((zoned_use == "residential" & get(in_bill_column) == 1)*get(column_to_sum)), decimal),
        mixed = round(sum((zoned_use == "mixed" & get(in_bill_column) == 1)*get(column_to_sum)), decimal),
        comm = round(sum((zoned_use == "commercial" & get(in_bill_column) == 1)*get(column_to_sum)), decimal),
        sqft_lt_threshold = round(sum((zoned_use %in%  c("residential", "commercial", "mixed") & zoned_far_for_tier == 1 & get(size_column) == 0)*get(column_to_sum)), decimal),
        zoned_for_far = round(sum((zoned_use %in%  c("residential", "commercial", "mixed") & zoned_far_for_tier == 1)*get(column_to_sum)), decimal),
        industrial = round(sum((! zoned_use %in%  c("residential", "commercial", "mixed"))*get(column_to_sum)), decimal)
    ), by = "city_id"][order(city_id)]
    if(include_size){
        # replace "threshold" in column names with the right number
        colnames(detail) <- gsub("threshold", min_parcel_sqft_for_analysis, colnames(detail))
    } else # remove the sqft_lt_threshold column
        detail[, sqft_lt_threshold := NULL]
    # add prefix to column names (excluding city_id which is first)
    setnames(detail, colnames(detail)[-1], paste0(col_prefix, colnames(detail)[-1]))
    return(detail)
}

create_summary <- function(dt, column_to_sum = "one", include_size = TRUE, decimal = 0){
    in_bill_column <- if(include_size) "is_in_bill" else "is_in_bill_no_size"
    # part that involves all parcels
    summary_all <- dt[, .(
        total_parcels = round(sum(get(column_to_sum)), decimal), 
        subject_to_proposal = round(sum((get(in_bill_column) == 1)*get(column_to_sum)), decimal),
        not_in_hct = round(sum((vision_hct == 0)*get(column_to_sum)), decimal)
    ), by = "city_id"][order(city_id)]
    
    # generate tier 1 and tier 2 parts of the summary
    summary_tier1 <- create_summary_detail(dt[vision_hct == 1], col_prefix = "tier1_", column_to_sum = column_to_sum, 
                                           include_size = include_size, decimal = decimal)
    summary_tier2 <- create_summary_detail(dt[vision_hct %in% c(2,3)], col_prefix = "tier2_", column_to_sum = column_to_sum, 
                                           include_size = include_size, decimal = decimal)
    
    # merge together and add city_name
    summary_final <- merge(merge(cities[, .(city_id, city_name, tier)], summary_all, by = "city_id", all = TRUE),
                           merge(summary_tier1, summary_tier2, by = "city_id", all = TRUE),
                           by = "city_id", all = TRUE)[order(-tier, city_id)]
    
    return(summary_final)
}

create_summary_by_lot_area <- function(dt, ...){
    sum_by_lot_area <- NULL
    groups <- c(0, 2500, 5000, 7500, 10000, 15000, 20000, max(dt[,max(parcel_sqft)])+1)
    for(i in 2:length(groups)){
        gdt <- create_summary(dt[parcel_sqft >= groups[i-1] & parcel_sqft < groups[i]], include_size = FALSE)
        gdt[, `:=`(total_parcels = NULL, not_in_hct = NULL, tier1_zoned_for_far = NULL, tier1_industrial = NULL,
                   tier2_zoned_for_far = NULL, tier2_industrial = NULL)]
        sum_by_lot_area <- rbind(sum_by_lot_area, 
                                 data.table(sqft = if(i == length(groups)) paste0(groups[i-1], "+") else paste(groups[i-1], groups[i]-1, sep = "-"),
                                            gdt)
                                )
    }
    summary_tot <- sum_by_lot_area[, lapply(.SD, sum, na.rm = TRUE), 
                                 .SDcols = setdiff(colnames(sum_by_lot_area), c("city_id", "city_name", "tier", "sqft")),
                                 by = "sqft"]
    return(summary_tot)
}

# Create summaries
parcels_final[, one := 1] # dummy for summing # of parcels

summaries <- list()
summaries[["parcel_count"]] <- create_summary(parcels_final, include_size = FALSE) # Table 1
summaries[["parcel_count_by_lot_area"]] <- create_summary_by_lot_area(parcels_final)
summaries[["zoned_du"]] <- create_summary(parcels_final, column_to_sum = "zoned_du") # 
summaries[["exist_du"]] <- create_summary(parcels_final, column_to_sum = "residential_units")
summaries[["net_du"]] <- create_summary(parcels_final, column_to_sum = "net_du")
summaries[["zoned_nonres_sqft"]] <- create_summary(parcels_final, column_to_sum = "zoned_sqft")
summaries[["exist_nonres_sqft"]] <- create_summary(parcels_final, column_to_sum = "non_residential_sqft")
summaries[["net_nonres_sqft"]] <- create_summary(parcels_final, column_to_sum = "net_nonres_sqft")
summaries[["parcel_count_size"]] <- create_summary(parcels_final, include_size = TRUE) # 
#summaries[["gross_far"]] <- create_summary(parcels_final, column_to_sum = "zoned_far", decimal = 1) # Table 2
#summaries[["existing_far"]] <- create_summary(parcels_final, column_to_sum = "current_far", decimal = 1) # Table 3
#summaries[["net_far"]] <- create_summary(parcels_final, column_to_sum = "net_far", decimal = 1) # Table 4
summaries[[paste0("parcel_count_land_impr_ratio_", market_factor)]] <- create_summary(parcels_final[land_greater_improvement == 1]) # Table 5
summaries[[paste0("parcel_count_far_", max_far_to_redevelop)]]  <- create_summary(parcels_final[current_far_to_redevelop == 1]) # Table 6
parcels_meeting_market_cond <- parcels_final[both_value_size == 1]
summaries[["parcel_count_market"]]  <- create_summary(parcels_meeting_market_cond) # Table 7
#summaries[["res_far_market"]]  <- create_summary(parcels_meeting_market_cond, 
#                                                           column_to_sum = "zoned_far_res", decimal = 1) # Table 8
summaries[["zoned_du_market"]]  <- create_summary(parcels_meeting_market_cond, 
                                                           column_to_sum = "zoned_du") # Table 9
summaries[["exist_du_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                            column_to_sum = "residential_units") # Table 10
summaries[["net_du_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                    column_to_sum = "net_du") # Table 11
#summaries[["zoned_nonres_far_market"]] <- create_summary(parcels_meeting_market_cond, 
#                                              column_to_sum = "zoned_far_nonres") # Table 12
summaries[["zoned_nonres_sqft_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                   column_to_sum = "zoned_sqft") # Table 13
summaries[["exist_nonres_sqft_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                    column_to_sum = "non_residential_sqft") # Table 14
summaries[["net_nonres_sqft_market"]] <- create_summary(parcels_meeting_market_cond, 
                                                      column_to_sum = "net_nonres_sqft") # Table 15

# create top page with regional summaries
top_page <- NULL
for(sheet in setdiff(names(summaries), "parcel_count_by_lot_area")){
    top_page <- rbind(top_page, data.table(indicator = sheet, 
                                           summaries[[sheet]][, lapply(.SD, sum, na.rm = TRUE), 
                                                 .SDcols = setdiff(colnames(summaries[[sheet]]), c("city_id", "city_name", "tier"))]),
                      fill = TRUE)
}
description <- list(
    parcel_count = "Total number of parcels",
    zoned_du = "Gross allowable dwelling units",
    exist_du = "Existing dwelling units",
    net_du = "Net allowable dwelling units",
    zoned_nonres_sqft = "Gross allowable non-residential sqft",
    exist_nonres_sqft = "Existing non-residential sqft",
    net_nonres_sqft = "Net allowable non-residential sqft",
    parcel_count_size = paste0("Number of parcels larger than ", min_parcel_sqft_for_analysis, " sqft"),
    parcel_count_land_impr_ratio_MFACTOR = paste0("Number of parcels with land/improvement ratio > ", market_factor,
                                        " (market 1) and > ", min_parcel_sqft_for_analysis, " sqft"),
    parcel_count_far_MAXFAR = paste0("Number of parcels with current FAR < ", max_far_to_redevelop,
                               " (market 2) and > ", min_parcel_sqft_for_analysis, " sqft"),
    parcel_count_market = "Number of parcels passing both market criteria as well as the size condition.",
    zoned_du_market = "Gross allowable dwelling units for parcels passing the market & size criteria",
    exist_du_market = "Existing dwelling units  for parcels passing the market & size criteria",
    net_du_market = "Net allowable dwelling units for parcels passing the market & size criteria",
    zoned_nonres_sqft_market = "Gross allowable non-residential sqft for parcels passing the market & size criteria",
    exist_nonres_sqft_market = "Existing non-residential sqft for parcels passing the market & size criteria",
    net_nonres_sqft_market = "Net allowable non-residential sqft for parcels passing the market & size criteria"
)
descr <- cbind(data.table(description), indicator = names(description))
descr[, indicator := gsub("MFACTOR", market_factor, indicator)]
descr[, indicator := gsub("MAXFAR", max_far_to_redevelop, indicator)]
top_page <- merge(descr, top_page, by = "indicator", sort = FALSE)

setcolorder(top_page, c(c("indicator", "description"), intersect(colnames(summaries[["parcel_count_size"]]), colnames(top_page)))) # re-order columns

summaries <- c(list(Region = top_page), summaries) # set the regional summaries as the first sheet

if(write.summary.files.to.csv || write.summary.files.to.excel){
    summary_dir <- file.path(data_dir, output_dir)
    if(!dir.exists(summary_dir)) dir.create(summary_dir) # create directory if not exists
    if(write.summary.files.to.csv) {
        csvdir <- file.path(summary_dir, paste0("csv", scenario_string)) # put csv files into a sub-directory identified by the scenario
        if(!dir.exists(csvdir)) dir.create(csvdir) # create sub-directory if not exists
        for(table in names(summaries))
            fwrite(summaries[[table]], file = file.path(csvdir, paste0(table, ".csv")))
    }
    if(write.summary.files.to.excel) {
        library(openxlsx)
        # style of the header
        style <- createStyle(
            textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12, fgFill = "#4F80BD"
            #fontName = "Arial Narrow", 
            )
        # set the width of columns and how many columns should be freezed
        colwidths <- list()
        firstcol <- list()
        for(sheet in names(summaries)){
            colwidths[[sheet]] <- rep(10, ncol(summaries[[sheet]]))
            colwidths[[sheet]][colnames(summaries[[sheet]]) == "city_id"] <- 7
            colwidths[[sheet]][colnames(summaries[[sheet]]) == "tier"] <- 5
            colwidths[[sheet]][colnames(summaries[[sheet]]) == "city_name"] <- 20
            colwidths[[sheet]][colnames(summaries[[sheet]]) == "sqft"] <- 15
            firstcol[[sheet]] <- 4
        }
        colwidths[["Region"]][1:2] <- c(25, 40)
        colwidths[["Region"]][-c(1,2)] <- 15
        firstcol[["Region"]] <- 3
        firstcol[["parcel_count_by_lot_area"]] <- 2
        write.xlsx(summaries, file = file.path(summary_dir, 
                                               paste0("SB5466_all_tables", scenario_string, ".xlsx")),
                   headerStyle = style, colWidths = colwidths, firstActiveRow = 2, firstActiveCol = firstcol)
    }
    cat("\nResults written into ", summary_dir, "\n")
}


