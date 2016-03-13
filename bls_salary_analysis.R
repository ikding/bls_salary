# Load libraries
# install.packages("xlsx")
library(xlsx)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(scales)

# Download zip file from BLS website and open the enclosed xlsx file
temp <- tempfile()
download.file("http://www.bls.gov/oes/special.requests/oesm14ma.zip", temp)
unzip(temp)
data <-  read.csv("oesm14ma/MSA_M2014_dl.csv", stringsAsFactors =F)
names(data) <- tolower(names(data))

# Convert numbers with comma into real numbers
numIndex <- 7:23
names(data)[numIndex]
data[,numIndex] <- lapply(data[,numIndex], function(x){as.numeric(gsub(",", "", x))})

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
