# Script for data analysis related to HB1110 data request
#
# It attaches various dummies to the input parcels file
# and writes two output parcels file, one with all parcels included 
# in the bill analysis and one with parcels likely to develop.
# It then creates several city-level summaries and exports 
# the into csv files.
#
# Last update: 04/06/2023
# Drew Hanson & Hana Sevcikova

if(! "data.table" %in% installed.packages())
    install.packages("data.table")

library(data.table)

# Run this script from the directory of this file, unless data_dir is set as an absolute path
setwd("J:/Projects/Bill-Analysis/2023/scripts")
#setwd("~/psrc/R/bill-analysis/scripts")

# Settings
write.parcels.file <- TRUE
write.summary.files.to.csv <- FALSE
write.summary.files.to.excel <- TRUE

data_dir <- "../data" # directory where the data files below live 
                      # (it's a relative path to the script location; can be also set as an absolute path)
parcels_file_name <- "parcels_for_bill_analysis.csv" 
#parcel_vision_hct_file_name <- "parcel_vision_hct.csv"
parcel_vision_hct_file_name <- "revised_buffers_1110_v4.csv"
cities_file_name <- "cities.csv"
tier_file_name <- "cities_coded_all_20230405.csv"

# tier definitions
tier_constraints <- list(`1` = c(4, 2.7, 2, 1.5), # in the form c(hct_constraint, hct_mixed_constraint, non-hct_constraint, non-hct_mixed_constraint)
                         `2` = c(6, 4, 4, 3),
                         `3` = c(2, 1.3, 2, 1.5)
                         #`1` = c(6, 4) # original constraint
                         )
#tier_column <- "Original"
#tier_column <- "Substitute"
tier_column <- "Rev_swm"

# size restriction for parcels to be included
min_parcel_sqft_for_analysis <- 2000

# name of the output files; should include "XXX" which will be replaced by "in_bill" and "to_develop" to distinguish the two files
output_parcels_file_name <- paste0("selected_parcels_for_mapping_XXX-", Sys.Date(), ".csv") # will be written into data_dir
# directory name where results should be written
output_dir <- paste0("HB1110_results-", Sys.Date())

# Read input files
parcels_for_bill_analysis <- fread(file.path(data_dir, parcels_file_name)) # parcels
parcel_vision_hct  <- fread(file.path(data_dir, parcel_vision_hct_file_name)) # HCT locations
cities <- fread(file.path(data_dir, cities_file_name)) # cities table
tiers_by_city <- fread(file.path(data_dir, tier_file_name))[, c("city_id", tier_column), with = FALSE] # table containing tier assignment to cities
setnames(tiers_by_city, tier_column, "tier") # rename the tier column to "tier" for simpler access
cities <- merge(cities, tiers_by_city, all = TRUE)
cities[is.na(tier), tier := 0]

# Get all non-zero tiers and check if there is a definition for them
tiers <- as.character(unique(cities[tier > 0, tier]))
if(any(! tiers %in% names(tier_constraints))) 
    stop("Missing definition of tier(s) ", paste(tiers[! tiers %in% names(tier_constraints)], collapse = ", "), " in the tier_constraints object.")

#Fixes City ID error in the input dataset (might not be needed after it's corrected in the input file)
parcels_for_bill_analysis[city_id==95, city_id := 96]

# Codes records that fall in cities/geographies excluded from HB1110 bill requirements with 0's and and everything else with 1's 
parcels_for_bill_analysis[cities, city_tier := i.tier, on = "city_id"]
    
# Creates updated parcel table with "hct_vision" field added
#parcel_vision_hct[, vision_hct := any(hct_quarter_mile, parks, schools)]
#parcels_updated <- merge(parcels_for_bill_analysis, parcel_vision_hct[, .(parcel_id, vision_hct)], all=TRUE)
parcels_updated <- copy(parcels_for_bill_analysis)
parcels_updated[, vision_hct := 0]
parcels_updated[parcel_id %in% parcel_vision_hct[vision_hct == 1, parcel_id], vision_hct := 1]

#Creates "res_zone" field that denotes residential zoned parcels
parcels_updated[, res_zone := 0]
parcels_updated[DUcap > 0 & is_mixed_cap == 0, res_zone := 1]

#Creates "mix_zone" field that denotes mixed-use zoned parcels
parcels_updated[, mixed_zone := 0]
parcels_updated[DUcap > 0 & is_mixed_cap == 1, mixed_zone := 1]
parcels_updated[, DUaltnetcap := DUcap]

# update net capacity for mixed use (use 70% of total capacity)
parcels_updated[mixed_zone == 1, DUaltnetcap := DUaltnetcap * 0.7]

# exclude all parks
parcels_updated[land_use_type_id == 19, `:=`(res_zone = 0, mixed_zone = 0)]

#Creates a field of potential DUs and denote parcels that are already zoned to meet requirements of bill (Step 3 in Methodology document)
parcels_updated[, `:=`(potential_du = DUaltnetcap, already_zoned = 0, zoning_constr = 0)]
for(tier in tiers) { # iterate over the non-zero city tiers
    parcels_updated[city_tier == tier & vision_hct == 1 & mixed_zone == 0, `:=`(potential_du = pmax(potential_du, tier_constraints[[tier]][1]), zoning_constr = tier_constraints[[tier]][1])]
    parcels_updated[city_tier == tier & vision_hct == 1 & mixed_zone == 1, `:=`(potential_du = pmax(potential_du, tier_constraints[[tier]][2]), zoning_constr = tier_constraints[[tier]][1])]
    parcels_updated[city_tier == tier & vision_hct == 0 & mixed_zone == 0, `:=`(potential_du = pmax(potential_du, tier_constraints[[tier]][3]), zoning_constr = tier_constraints[[tier]][3])]
    parcels_updated[city_tier == tier & vision_hct == 0 & mixed_zone == 1, `:=`(potential_du = pmax(potential_du, tier_constraints[[tier]][4]), zoning_constr = tier_constraints[[tier]][3])]
}
#parcels_updated[city_tier > 0 & DUaltnetcap >= potential_du, already_zoned := 1]
parcels_updated[city_tier > 0 & DUcap >= zoning_constr, already_zoned := 1] # compare 100% capacity with 100% potential zoning
parcels_updated[, net_du := pmax(0, potential_du - residential_units)] # compute potential net capacity
parcels_updated[, current_net_du := pmax(0, DUaltnetcap - residential_units)] # current net capacity
                    
#Creates a field denoting records with parcel square footage of less then a threshold (2000sf)
parcels_updated[, sq_ft_lt_threshold := 0]
parcels_updated[parcel_sqft < min_parcel_sqft_for_analysis, sq_ft_lt_threshold := 1]

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
parcels_likely_to_develop <- parcels_final[city_tier > 0 & already_zoned == 0 & (res_zone == 1 | mixed_zone == 1) & sq_ft_lt_threshold == 0 & land_greater_improvement == 1 & built_sqft_less_1400 == 1 &  (vacant == 1 | sf_use == 1), ]

#Writes csv output files
if(write.parcels.file) {
    #remove all fields from parcel tables except parcel_id,vision_hct,and city_tier
    parcels_in_bill_to_save <- parcels_in_bill[, c("parcel_id", "vision_hct","city_tier")]
    parcels_likely_to_develop_to_save <- parcels_likely_to_develop[, c("parcel_id", "vision_hct","city_tier")]
    # write to disk into a subdirectory of output_dir
    pcl_dir <- file.path(data_dir, output_dir, "parcels")
    if(!dir.exists(pcl_dir)) dir.create(pcl_dir, recursive = TRUE)
    fwrite(parcels_in_bill_to_save, file.path(pcl_dir, gsub("XXX", "in_bill", output_parcels_file_name)))
    fwrite(parcels_likely_to_develop_to_save, file.path(pcl_dir, gsub("XXX", "to_develop", output_parcels_file_name)))
    cat("\nParcels written into ", file.path(pcl_dir, output_parcels_file_name), "\n")
}


# Functions for generating summaries
create_summary_detail <- function(dt, col_prefix, column_to_sum = "one", decimal = 0){
    detail <- dt[, .(
        total_parcels = round(sum(get(column_to_sum)), decimal), 
        res_vacant = round(sum((res_zone == 1 & vacant == 1)*get(column_to_sum)), decimal),
        res_sf_use = round(sum((res_zone == 1 & sf_use == 1)*get(column_to_sum)), decimal),
        res_other_use = round(sum((res_zone == 1 & vacant == 0 & sf_use == 0)*get(column_to_sum)), decimal),
        mix_vacant = round(sum((mixed_zone == 1 & vacant == 1)*get(column_to_sum)), decimal),
        mix_sf_use = round(sum((mixed_zone == 1 & sf_use == 1)*get(column_to_sum)), decimal),
        mix_other_use = round(sum((mixed_zone == 1 & vacant == 0 & sf_use == 0)*get(column_to_sum)), decimal),
        other = round(sum((res_zone == 0 & mixed_zone == 0)*get(column_to_sum)), decimal)
    ), by = "city_id"][order(city_id)]
    
    # add prefix to column names (excluding city_id which is first)
    setnames(detail, colnames(detail)[-1], paste0(col_prefix, colnames(detail)[-1]))
    return(detail)
}

create_summary <- function(dt, column_to_sum = "one", decimal = 0){
    # part that involves all parcels
    summary_all <- dt[, .(
        total_parcels = round(sum(get(column_to_sum)), decimal), 
        already_zoned = round(sum((already_zoned == 1)*get(column_to_sum)), decimal)
    ), by = "city_id"][order(city_id)]
    
    # generate HCT and nonHCT parts of the summary
    summary_hct <- create_summary_detail(dt[vision_hct == 1 & already_zoned == 0], col_prefix = "hct_", 
                                         column_to_sum = column_to_sum, decimal = decimal)
    summary_nonhct <- create_summary_detail(dt[vision_hct == 0 & already_zoned == 0], col_prefix = "nhct_", 
                                            column_to_sum = column_to_sum, decimal = decimal)
    
    # merge together and add city_name
    summary_final <- merge(merge(cities[, .(city_id, city_name, tier)], summary_all, by = "city_id", all = TRUE),
                           merge(summary_hct, summary_nonhct, by = "city_id", all = TRUE),
                           by = "city_id", all = TRUE)[order(-tier, city_id)]
    return(summary_final)
}

# Create summaries
parcels_final[, one := 1] # dummy for summing # of parcels
summaries <- list()
summaries[["all_parcels"]] <- create_summary(parcels_final)
summaries[["filter_parcel_sqft"]] <- create_summary(parcels_final[sq_ft_lt_threshold == 0])
summaries[["filter_parcel_sqft_under1400"]] <- create_summary(parcels_final[sq_ft_lt_threshold == 0 & built_sqft_less_1400 == 1])
summaries[["filter_parcel_sqft_land_value"]] <- create_summary(parcels_final[sq_ft_lt_threshold == 0 & land_greater_improvement == 1])
parcels_filtered_all <- parcels_final[sq_ft_lt_threshold == 0 & land_greater_improvement == 1 & built_sqft_less_1400 == 1]
summaries[["filter_all"]] <- create_summary(parcels_filtered_all)
summaries[["zoned_du_fltr_sqft"]] <- create_summary(parcels_final[sq_ft_lt_threshold == 0], column_to_sum = "potential_du")
summaries[["exist_du_fltr_sqft"]] <- create_summary(parcels_final[sq_ft_lt_threshold == 0], column_to_sum = "residential_units")
summaries[["net_du_fltr_sqft"]] <- create_summary(parcels_final[sq_ft_lt_threshold == 0], column_to_sum = "net_du")
summaries[["zoned_du_fltr_all"]] <- create_summary(parcels_filtered_all, column_to_sum = "potential_du")
summaries[["exist_du_fltr_all"]] <- create_summary(parcels_filtered_all, column_to_sum = "residential_units")
summaries[["net_du_fltr_all"]] <- create_summary(parcels_filtered_all, column_to_sum = "net_du")
summaries[["current_net_du_fltr_sqft"]] <- create_summary(parcels_final[sq_ft_lt_threshold == 0], column_to_sum = "current_net_du")
summaries[["current_net_du_fltr_all"]] <- create_summary(parcels_filtered_all, column_to_sum = "current_net_du")


# create top page with regional summaries
top_page <- top_page_total <- NULL
for(sheet in names(summaries)){
    top_page_total <- rbind(top_page_total, data.table(indicator = sheet, 
                                           tier = -1,
                                           summaries[[sheet]][tier > 0, lapply(.SD, sum, na.rm = TRUE), 
                                                              .SDcols = setdiff(colnames(summaries[[sheet]]), c("city_id", "city_name", "tier"))]),
                      fill = TRUE)
}
description <- list(
    all_parcels = "Total number of parcels",
    filter_parcel_sqft = paste("Parcels larger than", min_parcel_sqft_for_analysis, "sqft"),
    filter_parcel_sqft_under1400 = paste("Parcels larger than", min_parcel_sqft_for_analysis, "sqft that have built residential sqft smaller than 1400"),
    filter_parcel_sqft_land_value = paste("Parcels larger than", min_parcel_sqft_for_analysis, "sqft with land value > improvement value"),
    filter_all = paste("Parcels larger than", min_parcel_sqft_for_analysis, "sqft passing both market criteria"),
    zoned_du_fltr_sqft = paste("Gross allowable dwelling units on parcels larger than", min_parcel_sqft_for_analysis, "sqft"),
    exist_du_fltr_sqft = paste("Existing dwelling units on parcels larger than", min_parcel_sqft_for_analysis, "sqft"),
    net_du_fltr_sqft = paste("Net allowable dwelling units on parcels larger than", min_parcel_sqft_for_analysis, "sqft"),
    zoned_du_fltr_all = paste("Gross allowable dwelling units on parcels larger than", min_parcel_sqft_for_analysis, "sqft passing both market criteria"),
    exist_du_fltr_all = paste("Existing dwelling units on parcels larger than", min_parcel_sqft_for_analysis, "sqft passing both market criteria"),
    net_du_fltr_all = paste("Net allowable dwelling units on parcels larger than", min_parcel_sqft_for_analysis, "sqft passing both market criteria"),
    current_net_du_fltr_sqft = paste("Net dwelling units allowable by current zoning on parcels larger than", min_parcel_sqft_for_analysis, "sqft"),
    current_net_du_fltr_all = paste("Net dwelling units allowable by current zoning on parcels larger than", min_parcel_sqft_for_analysis, "sqft passing both market criteria")
)

descr <- cbind(data.table(description), indicator = names(description))
top_page_total <- merge(descr, top_page_total, by = "indicator", sort = FALSE)

# summaries by tier
for(sheet in names(summaries)){
    top_page <- rbind(top_page, top_page_total[indicator == sheet])
    top_page <- rbind(top_page, data.table(indicator = "", 
                                           description = "",
                                           summaries[[sheet]][, lapply(.SD, sum, na.rm = TRUE), 
                                                              .SDcols = setdiff(colnames(summaries[[sheet]]), c("city_id", "city_name", "tier")), by = "tier"]),
                      fill = TRUE)
}
top_page[,tier := as.character(tier)][tier == -1, tier := "1,2,3"]
#summaries[["existing_units"]] <- existing_units
summaries <- c(list(Region = top_page), summaries) # set the regional summaries as the first sheet

if(write.summary.files.to.csv || write.summary.files.to.excel){
    summary_dir <- file.path(data_dir, output_dir)
    if(!dir.exists(summary_dir)) dir.create(summary_dir) # create directory if not exists
    if(write.summary.files.to.csv) {
        csvdir <- file.path(summary_dir, "csv")
        if(!dir.exists(csvdir)) dir.create(csvdir) # create sub-directory if not exists
        for(table in names(summaries))
            fwrite(summaries[[table]], file = file.path(csvdir, paste0(table, ".csv")))
    }
    if(write.summary.files.to.excel) {
        library(openxlsx)
        # style of the header
        style <- createStyle(
            textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12, fgFill = "#4F80BD"
        )
        # set the width of columns and how many columns should be freezed
        colwidths <- list()
        firstcol <- list()
        for(sheet in names(summaries)){
            colwidths[[sheet]] <- rep(10, ncol(summaries[[sheet]]))
            colwidths[[sheet]][colnames(summaries[[sheet]]) == "city_id"] <- 7
            colwidths[[sheet]][colnames(summaries[[sheet]]) == "tier"] <- 5
            colwidths[[sheet]][colnames(summaries[[sheet]]) == "city_name"] <- 20
            firstcol[[sheet]] <- 4
        }
        colwidths[["Region"]][1:2] <- c(25, 40)
        colwidths[["Region"]][-(1:3)] <- 15
        write.xlsx(summaries, file = file.path(summary_dir, "HB1110_all_tables.xlsx"),
                   headerStyle = style, colWidths = colwidths, firstActiveRow = 2, firstActiveCol = firstcol)
    }
    cat("\nSummary files written into ", summary_dir, "\n")
}


