########################################
#### Load libraries ----            ####
########################################
library(xlsx)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(scales)

########################################
#### Load Raw Data Files ----       ####
########################################

# Download zip file from BLS website and open the enclosed xlsx file
temp <- tempfile()
download.file("http://www.bls.gov/oes/special.requests/oesm14ma.zip", temp)
unzip(temp)

# I had trouble open xlsx file from R directly, so I converted the xlsx file to csv before loading it in
data <-  read.csv("oesm14ma/MSA_M2014_dl.csv", stringsAsFactors =F)
names(data) <- tolower(names(data))

# Convert numbers with comma into real numbers
numIndex <- 7:23
names(data)[numIndex]
data[,numIndex] <- lapply(data[,numIndex], function(x){as.numeric(gsub(",", "", x))})

# Load cost-of-living index
# Source: c2er-coli, 2014 annual average
# Data was cleaned into csv format from pdf: http://www.bvep.org/images/content/docs/COLI2012AnnualReview.pdf
coli <- read.csv("oesm14ma/coli_2014.csv", stringsAsFactors = F)
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

# bls_name <- unique(data$metro_short)
# coli_name <- unique(coli$metro_short)
# 
# table(bls_name %in% coli_name)
# table(coli_name %in% bls_name)
# 
# sort(coli_name[!(coli_name %in% bls_name)])
# sort(bls_name[!(bls_name %in% coli_name)]) ###

# Manual correction of coli city names to get better match
coli[!(coli$metro_short %in% bls_name), c("metro_short", "metro_micro")]

coli[with(coli, metro_first_city == "Anaheim"),]$metro_first_city <- "Santa Ana"
coli[with(coli, metro_first_city == "Urban Honolulu"),]$metro_first_city <- "Honolulu"
coli[with(coli, metro_first_city == "Silver Spring"),]$metro_first_city <- "Bethesda"
coli[with(coli, metro_first_city == "Myrtle Beach"),]$metro_state <- "SC"

coli$metro_short <- paste(coli$metro_first_city, coli$metro_state)

unique(data[grepl("Portland", data$area_name),]$area_name)

# For coli index, keep only the city with the highest composite index (most likely to be core metro)
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

salary <- merge(select(data, metro_short, prim_state:loc.quotient, a_mean:mean_prse, a_pct10:a_pct90), select(coli, metro_short, metro_micro, composite:misc), by = "metro_short", sort = F)

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
                 metro_name = metro_micro,
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
       

(occ_title, occ_group, tot_emp, jobs_1000, loc.quotient, a_mean, a_pct_50, a_pct_50_adj, compsite)

saveRDS(object = salary, file = "data/salary_coli.RDS")

########################################
#### Quick Analysis of SDE salary --####
########################################

# Find the top metros with most software engineering jobs

# Software engineers are narrowly defined as:
# "Software Developers, Applications" and "Software Developers, Systems Software" in BLS definition
unique(data[grep("Software", data$occ_title),]$occ_title)

# Top 7 metro with most total_employment of software engineers are:
# San Jose, Washington DC, Seattle, New York, Boston, Chicago, San Francisco
swe_by_metro <- data[grepl("Software", data$occ_title), c("area_name", "tot_emp")] %>%
    group_by(area_name) %>%
    summarise(sum_emp = sum(tot_emp)) %>%
    arrange(desc(sum_emp))

metro_names <- swe_by_metro$area_name[1:7]
metro_names

# Keep only swe-salary data in these 7 metros 
data <- subset(data, area_name %in% metro_names)
data <- data[grepl("Software", data$occ_title),]

# Compute weighted mean of the salary in each quantile
data_dist <- as.data.frame(data %>%
    group_by(area_name) %>%
    summarise(total_emp = sum(tot_emp),
              pct10 = weighted.mean(a_pct10, tot_emp, na.rm = T),
              pct25 = weighted.mean(a_pct25, tot_emp, na.rm = T),
              pct50 = weighted.mean(a_median, tot_emp, na.rm = T),
              pct75 = weighted.mean(a_pct75, tot_emp, na.rm = T),
              pct90 = weighted.mean(a_pct90, tot_emp, na.rm = T)))

data_dist$area_abb <- sapply(strsplit(data_dist$area_name, "-"), "[[", 1)

# Calculate mean and stdev with 25% and 75% quantiles (assuming normal distribution)

data_dist
data_dist$mu <- with(data_dist, (pct25 + pct75)/2)
data_dist$sigma <- with(data_dist, (pct75-mu)/qnorm(0.75))

# How much does a 1% engineer make (base salary)?
data_dist$pct99 <- qnorm(0.99, mean = data_dist$mu, sd = data_dist$sigma)

# What fraction of people makes over 200k a year? How about 150k?
data_dist$frac.200k <- pnorm(200000, mean = data_dist$mu, sd = data_dist$sigma, lower.tail = F)
data_dist$frac.150k <- pnorm(150000, mean = data_dist$mu, sd = data_dist$sigma, lower.tail = F)

# Plot the distributions
data_plot <- data.frame(x = seq(0, 300000, 1000))
for (metro in data_dist$area_abb){
    m = subset(data_dist, area_abb == metro)$mu
    s = subset(data_dist, area_abb == metro)$sigma
    data_plot[, metro] <- dnorm(data_plot$x, mean = m, sd = s)
}

# Convert to long format for ggplot2
data_plot <- melt(data_plot, id.vars = c("x"))

ggplot(data = data_plot) +
    geom_line(aes(x = x, y = value, color = variable), size = 1.5) + 
    scale_color_manual("Metro", values = brewer.pal(8, "Set1")[c(1:5, 7:8)]) + 
    scale_x_continuous("Annual Salary", labels=dollar) + 
    scale_y_continuous("Probability Density (A.U.)") + 
    theme(axis.text.y = element_blank())
