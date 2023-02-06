install.packages("data.table", 
                 repos = "https://Rdatatable.github.io/data.table", type = "source")
install.packages("rlist")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("writexl")
install.packages("stringr")

library(dplyr)
library(tidyverse)
library(writexl)
library(data.table)
library(rlist)
library(stringr)


#Codes records that fall in cities/geographies excluded from HB1110 bill requirements with 0's and and everything else with 1's 
parcels_for_bill_analysis$applicable_city[parcels_for_bill_analysis$city_id %in% c(8,19,36,39,44,47,48,51,52,54,64,66,68,72,74,78,81,82,83,92,95,96) ] <- 0
parcels_for_bill_analysis$applicable_city[is.na(parcels_for_bill_analysis$applicable_city)] <- 1

#Creates updated parcel table with "hct_vision" field added ('parcel_vision_hct' table needs to be added beforehand)
parcels_updated <- merge(parcels_for_bill_analysis,parcel_vision_hct,all=TRUE)

#Creates "cities_hct_combined" field that denotes records that fall inside an applicable city + and hct areas (1's-in,0's-out)
parcels_updated$cities_hct_combined[parcels_updated$applicable_city==1&parcels_updated$vision_hct==1] <- 1
parcels_updated$cities_hct_combined[is.na(parcels_updated$cities_hct_combined)] <- 0

#Creates "res_zone" field that denotes residential zoned parcels
parcels_updated$res_zone[parcels_updated$DUcap>0 & parcels_updated$is_mixed_cap==0] <- 1
parcels_updated$res_zone[is.na(parcels_updated$res_zone)] <- 0

#Creates "mix_zone" field that denotes mixed-use zoned parcels
parcels_updated$mixed_zone[parcels_updated$DUcap>0 & parcels_updated$is_mixed_cap==1] <- 1
parcels_updated$mixed_zone[is.na(parcels_updated$mixed_zone)] <- 0

#Creates "already_zoned" field to denotes parcels that are already zoned to meet requirements of bill (Step 3 in Methodology document)
parcels_updated$already_zoned[parcels_updated$vision_hct==1 & parcels_updated$DUcap >= 6.0] <- 1
parcels_updated$already_zoned[parcels_updated$vision_hct==0 & parcels_updated$DUcap >= 4.0] <- 1
parcels_updated$already_zoned[is.na(parcels_updated$already_zoned)] <- 0

#Creates "sq_ft_less_2500" field denoting records with parcel square footage of less then 2,500
parcels_updated$sq_ft_less_2500[parcels_updated$parcel_sqft<2500] <- 1
parcels_updated$sq_ft_less_2500[is.na(parcels_updated$sq_ft_less_2500)] <- 0

#Creates "sf_use" field denoting single family parcels
parcels_updated$sf_use[parcels_updated$residential_units==1] <- 1
parcels_updated$sf_use[is.na(parcels_updated$sf_use)] <- 0

#Creates "vacant" field denoting vacant parcels
parcels_updated$vacant[parcels_updated$Nblds==0] <- 1
parcels_updated$vacant[is.na(parcels_updated$vacant)] <- 0

#Creates "land_greater_improvement" field to denotes parcels that have land value that is greater than the improvement value
parcels_updated$land_greater_improvement[parcels_updated$land_value > parcels_updated$improvement_value] <- 1
parcels_updated$land_greater_improvement[is.na(parcels_updated$improvement_value)] <- 0
parcels_updated$land_greater_improvement[is.na(parcels_updated$land_greater_improvement)] <- 0

#Creates "built_sqft_less_1400" field to denotes parcels that have a built square footage of less than 1,400
parcels_updated$built_sqft_less_1400[parcels_updated$residential_sqft <1400] <- 1
parcels_updated$built_sqft_less_1400[is.na(parcels_updated$built_sqft_less_1400)] <- 0

#Fixes City ID error in "parcels_for_bill_analysis" table
parcels_updated$city_id[parcels_updated$city_id==95] <- 96

#Cleans up city table (add "cities18.cvs" file manually) and adds city_name field to final parcel table
cities_clean <- select(cities18,c(city_id,city_name))
parcels_final <- merge(parcels_updated,cities_clean,by.x="city_id",by.y="city_id")

#Writes a csv output file
fwrite(parcels_final,"J:/Projects/Bill-Analysis/2023/data/parcels_for_bill_analysis_updated.csv",col.names = TRUE)



