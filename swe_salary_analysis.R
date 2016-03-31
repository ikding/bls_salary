########################################
#### Load libraries ----            ####
########################################
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(scales)

########################################
#### Quick Analysis of SDE salary --####
########################################
# Answer for quora question:
# https://www.quora.com/What-percentage-of-software-engineers-make-at-least-200-000/

# Load 2014 and 2015 data
salary_14 <- readRDS("data/salary_coli_2014.RDS")
salary_15 <- readRDS("data/salary_coli_2015.RDS")
salary_14$year <- 2014
salary_15$year <- 2015
salary <- rbind.fill(salary_14, salary_15)

# Find the top metros with most software engineering jobs

# Software engineers are narrowly defined as:
# "Software Developers, Applications" and "Software Developers, Systems Software" in BLS definition
unique(salary[grep("Software", salary$occ_job_title),]$occ_job_title)

# Get group_by summaries
salary_swe <- salary[grepl("Software", salary$occ_job_title), c("metro_name", "year", "total_employment")] %>%
    group_by(year, metro_name, add = TRUE) %>%
    summarise(sum_employment = sum(total_employment))

# Top 7 metro with most total_employment of software engineers are:
# 2014: San Jose, Washington DC, Seattle, New York, Boston, Chicago, San Francisco
# 2015: San Jose, New York, Seattle, Washington, Boston, San Francisco, Atlanta
# But I wonder if that is partly due to boundary change in BLS data between 2014 and 2015?
salary_swe %>%
    filter(year == 2014) %>%
#     filter(year == 2015) %>%
    arrange(desc(sum_employment))

metro_names <- salary_swe[grep(pattern = "San Jose|Washington|Seattle|New York|Boston|Chicago|San Francisco|Dallas|Atlanta", salary_swe$metro_name),]$metro_name
metro_names

# Keep only swe-salary data in these 9 metros 
salary_swe <- subset(salary, metro_name %in% metro_names)
salary_swe <- salary_swe[grepl("Software", salary_swe$occ_job_title) & !is.na(salary_swe$wage_mean),]

# Compute weighted mean of the salary in each quantile
salary_dist <- as.data.frame(salary_swe %>%
                                 group_by(year, metro_name) %>%
                                 summarise(total_emp = sum(total_employment),
                                           pct10 = weighted.mean(wage_pct10, total_employment, na.rm = T),
                                           pct25 = weighted.mean(wage_pct25, total_employment, na.rm = T),
                                           pct50 = weighted.mean(wage_median, total_employment, na.rm = T),
                                           pct75 = weighted.mean(wage_pct75, total_employment, na.rm = T),
                                           pct90 = weighted.mean(wage_pct90, total_employment, na.rm = T),
                                           pct10_adj = weighted.mean(wage_pct10_adj, total_employment, na.rm = T),
                                           pct25_adj = weighted.mean(wage_pct25_adj, total_employment, na.rm = T),
                                           pct50_adj = weighted.mean(wage_median_adj, total_employment, na.rm = T),
                                           pct75_adj = weighted.mean(wage_pct75_adj, total_employment, na.rm = T),
                                           pct90_adj = weighted.mean(wage_pct90_adj, total_employment, na.rm = T)
                                 ))

salary_dist$metro_abb <- sapply(strsplit(salary_dist$metro_name, "-"), "[[", 1)

# Calculate mean and stdev with 25% and 75% quantiles (assuming normal distribution)

salary_dist$mu <- with(salary_dist, (pct25 + pct75)/2)
salary_dist$sigma <- with(salary_dist, (pct75-mu)/qnorm(0.75))

salary_dist$mu_adj <- with(salary_dist, (pct25_adj + pct75_adj)/2)
salary_dist$sigma_adj <- with(salary_dist, (pct75_adj-mu_adj)/qnorm(0.75))

# How much does a 1% engineer make (base salary)?
salary_dist$pct99 <- qnorm(0.99, mean = salary_dist$mu, sd = salary_dist$sigma)

# What fraction of people makes over 200k a year? How about 150k?
salary_dist$frac.150k <- pnorm(150000, mean = salary_dist$mu, sd = salary_dist$sigma, lower.tail = F)
salary_dist$frac.200k <- pnorm(200000, mean = salary_dist$mu, sd = salary_dist$sigma, lower.tail = F)

# Reshape the dataframe for copy & paste into website
salary_dist_long = melt(salary_dist, id.vars = c("metro_abb", "year"), measure.vars = c("frac.200k"))
salary_dist_wide = dcast(salary_dist_long, metro_abb ~ year, )
cbind(salary_dist_wide[,c(1)], round(salary_dist_wide[,c(2:3)]*100, 2))

# Plot the distributions (unadjusted salary)
salary_plot <- list()
for (metro in unique(salary_dist$metro_abb)){
    for (y in unique(salary_dist$year)){
        salary_df <- data.frame(x = seq(0, 300000, 1000), metro = metro, year = y)
        m = subset(salary_dist, metro_abb == metro & year == y)$mu
        s = subset(salary_dist, metro_abb == metro & year == y)$sigma
        salary_df[, "y"] <- dnorm(salary_df$x, mean = m, sd = s)
        salary_plot[[paste0(metro, "_", y)]] = salary_df
    }
}

ggplot(data = plyr::rbind.fill(salary_plot)) +
    geom_line(aes(x = x, y = y, color = metro), size = 1.5) + 
    scale_color_brewer(palette = "Set1", name = "Metro") +
    #     scale_color_manual("Metro", values = brewer.pal(8, "Set1")[c(1:5, 7:8)]) + 
    facet_grid(year ~ .) +
    scale_x_continuous("Annual Salary", labels=dollar) + 
    scale_y_continuous("Probability Density (A.U.)") + 
    theme(axis.text.y = element_blank())

# Plot the distributions (adjusted salary)

salary_plot_adj <- list()
for (metro in unique(salary_dist$metro_abb)){
    for (y in unique(salary_dist$year)){
        salary_df <- data.frame(x = seq(0, 175000, 1000), metro = metro, year = y)
        m_adj = subset(salary_dist, metro_abb == metro & year == y)$mu_adj
        s_adj = subset(salary_dist, metro_abb == metro & year == y)$sigma_adj
        salary_df[, "y"] <- dnorm(salary_df$x, mean = m_adj, sd = s_adj)
        salary_plot_adj[[paste0(metro, "_", y)]] = salary_df
    }
}

ggplot(data = plyr::rbind.fill(salary_plot_adj)) +
    geom_line(aes(x = x, y = y, color = metro), size = 1.5) + 
    scale_color_brewer(palette = "Set1", name = "Metro") +
    #     scale_color_manual("Metro", values = brewer.pal(8, "Set1")[c(1:5, 7:8)]) + 
    facet_grid(year ~ .) +
    scale_x_continuous("Annual Salary (after COLI adjustment)", labels=dollar) + 
    scale_y_continuous("Probability Density (A.U.)") + 
    theme(axis.text.y = element_blank())
