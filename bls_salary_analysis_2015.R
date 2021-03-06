########################################
#### Load libraries ----            ####
########################################
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(scales)
library(ggmap)

########################################
#### Load Raw Data Files ----       ####
########################################

# I had trouble open xlsx file from R directly, so I converted the xlsx file to csv before loading it in
data <-  read.csv("oesm15ma/MSA_M2015_dl.csv", stringsAsFactors =F)
names(data) <- tolower(names(data))

# Convert numbers with comma into real numbers
numIndex <- 7:23
names(data)[numIndex]
data[,numIndex] <- lapply(data[,numIndex], function(x){as.numeric(gsub(",", "", x))})

# Load cost-of-living index
# Source: c2er-coli, 2015 annual average
# Data was cleaned into csv format from pdf: http://www.bvep.org/images/content/docs/COLI2012AnnualReview.pdf
coli <- read.csv("oesm15ma/coli_2015.csv", stringsAsFactors = F)
str(coli)

########################################
#### Merging BLS and COLI data ---- ####
########################################

# Reformat the metro names to get better match between salary data (BLS) and cost of living data (C2ER)
unique(data$area_name)
unique(coli$metro_micro)

# Remove trailing strings such as "Metro Div", ... etc from the metro area name
coli[!grepl("[A-Z][A-Z]\ [Metro|Micro]", coli$metro_micro),]$metro_micro
coli$metro_micro <- gsub("\ Metro|\ Micro|\ Metro\ Div\\.", "", coli$metro_micro)

data$area_name <- gsub(" Metropolitan Division", "", data$area_name)
data$area_name <- gsub(" NECTA Division", "", data$area_name)
data$area_name <- gsub(",", "", data$area_name)

# get primary (first) city name and state into different column
coli$metro_city <- gsub("\ [A-Z]{2}(-[A-Z]{2})?", "", coli$metro_micro)
coli$metro_first_city <- sapply(strsplit(coli$metro_city, "-"), "[[", 1)

r <- regexpr("[A-Z]{2}(-[A-Z]{2})?", coli$metro_micro)
coli$metro_state <- regmatches(coli$metro_micro, r)

data$metro_city <- gsub("\ [A-Z]{2}(-[A-Z]{2})?", "", data$area_name)
data$metro_first_city <- sapply(strsplit(data$metro_city, "-"), "[[", 1)

r <- regexpr("[A-Z]{2}(-[A-Z]{2})?", data$area_name)
data$metro_state <- regmatches(data$area_name, r)

# Concatenate the primary city name and state-name into its own column
coli$metro_short <- paste(coli$metro_first_city, coli$metro_state)
data$metro_short <- paste(data$metro_first_city, data$metro_state)

bls_name <- unique(data$metro_short)
coli_name <- unique(coli$metro_short)

table(bls_name %in% coli_name)
table(coli_name %in% bls_name)

sort(coli_name[!(coli_name %in% bls_name)])
sort(bls_name[!(bls_name %in% coli_name)])

# Manual correction of data city names to correct some typo
data[with(data, metro_short == "Louisville/Jefferson County KY-IN"),]$metro_short <- "Louisville KY-IN"
data[with(data, metro_short == "Mayag\xfc\xbe\x99\xa3\xa4\xbcez PR"),]$metro_short <- "Mayaguez PR"
data[with(data, metro_short == "San Germ\xfc\xbe\x8d\xb3\xa0\xbcn PR"),]$metro_short <- "San German PR"

coli[!(coli$metro_short %in% bls_name), c("metro_short", "metro_micro")]
coli$metro_short <- paste(coli$metro_first_city, coli$metro_state)

# For coli with multiple city in one metro (eg. NYC-Manhattan, NYC-Brooklyn, etc), keep only the city with the highest composite index (most likely to be core metro)
coli <- coli[!coli$metro_micro == "NonMetro US",]
coli_city_freq <- as.data.frame(table(coli$metro_short))
coli_duplicate_city <- subset(coli_city_freq, Freq > 1)$Var1
coli[coli$metro_short %in% coli_duplicate_city, ] 
coli[coli$metro_short %in% coli_duplicate_city, c("metro_micro", "urban_area_state", "composite")]

coli <- as.data.frame(coli %>%
    group_by(metro_micro) %>%
    slice(which.max(composite))
#     top_n(n = 1, wt = composite)
)

# Merge salary data with coli data
# metro_short is our join key
# Keep only some columns of interest

salary <- merge(select(data, metro_short, prim_state:loc.quotient, a_mean:mean_prse, a_pct10:a_pct90), select(coli, metro_short, metro_micro, composite:misc), by = "metro_short", all.x = T, sort = F)

salary <- arrange(salary, prim_state, area, occ_code)

# Calculate adjusted salary, based on COLI
names(salary)[grepl("a_median", names(salary))] <- "a_pct50"
for (pct in c(10, 25, 50, 75, 90)){
    var = paste0("a_pct", pct)
    var_adj = paste0(var, "_adj")
    salary[, var_adj] <- round(100 * salary[, var] /salary$composite)
}

# Rename the variables to make them more comprehensible for shiny app
salary <- select(salary, 
                 primary_city = metro_short, 
                 metro_name = area_name,
                 state = prim_state, 
                 area,
                 occ_code, 
                 occ_job_title = occ_title,
                 occ_group = occ_group,
                 total_employment = tot_emp,
                 emp_prse,
                 job_per_1000 = jobs_1000,
                 location_quotient = loc.quotient,
                 wage_mean = a_mean,
                 mean_prse,
                 wage_pct10 = a_pct10,
                 wage_pct25 = a_pct25,
                 wage_median = a_pct50,
                 wage_pct75 = a_pct75,
                 wage_pct90 = a_pct90,
                 wage_pct10_adj = a_pct10_adj,
                 wage_pct25_adj = a_pct25_adj,
                 wage_median_adj = a_pct50_adj,
                 wage_pct75_adj = a_pct75_adj,
                 wage_pct90_adj = a_pct90_adj,
                 coli_composite = composite,
                 coli_grocery = grocery,
                 coli_housing = housing,
                 coli_utilities = utilities,
                 coli_transportation = transportation,
                 coli_healthcare = healthcare,
                 coli_misc = misc)

########################################
#### Geocoding using Google API ----####
########################################

metro_lat_long <- data.frame(metro = as.character(unique(salary$primary_city)),
                             lon = NA, lat = NA,
                             stringsAsFactors = F)

for (i in 1:dim(metro_lat_long)[1]){
    pair = geocode(metro_lat_long$metro[i], source = "google")
    metro_lat_long$lon[i] <- pair$lon
    metro_lat_long$lat[i] <- pair$lat
}

salary <- merge(salary, metro_lat_long, by.x = "primary_city", by.y = "metro", all.x = T, sort = F)

saveRDS(object = salary, file = "data/salary_coli_2015.RDS")
