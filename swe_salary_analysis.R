########################################
#### Quick Analysis of SDE salary --####
########################################
# Answer for quora question:
# https://www.quora.com/What-percentage-of-software-engineers-make-at-least-200-000/

# Find the top metros with most software engineering jobs

# Software engineers are narrowly defined as:
# "Software Developers, Applications" and "Software Developers, Systems Software" in BLS definition
unique(salary[grep("Software", salary$occ_job_title),]$occ_job_title)

# Top 7 metro with most total_employment of software engineers are:
# San Jose, Washington DC, Seattle, New York, Boston, Chicago, San Francisco
salary_swe <- salary[grepl("Software", salary$occ_job_title), c("metro_name", "total_employment")] %>%
    group_by(metro_name) %>%
    summarise(sum_employment = sum(total_employment)) %>%
    arrange(desc(sum_employment))

metro_names <- salary_swe$metro_name[1:7]
metro_names

# Keep only swe-salary data in these 7 metros 
salary_swe <- subset(salary, metro_name %in% metro_names)
salary_swe <- salary_swe[grepl("Software", salary_swe$occ_job_title) & !is.na(salary_swe$wage_mean),]

# Compute weighted mean of the salary in each quantile
salary_dist <- as.data.frame(salary_swe %>%
                                 group_by(metro_name) %>%
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

# Plot the distributions (unadjusted numbers)
salary_plot <- data.frame(x = seq(0, 300000, 1000))
for (metro in salary_dist$metro_abb){
    m = subset(salary_dist, metro_abb == metro)$mu
    s = subset(salary_dist, metro_abb == metro)$sigma
    salary_plot[, metro] <- dnorm(salary_plot$x, mean = m, sd = s)
}

salary_plot_adj <- data.frame(x = seq(0, 175000, 1000))
for (metro in salary_dist$metro_abb){
    m_adj = subset(salary_dist, metro_abb == metro)$mu_adj
    s_adj = subset(salary_dist, metro_abb == metro)$sigma_adj
    salary_plot_adj[, metro] <- dnorm(salary_plot_adj$x, mean = m_adj, sd = s_adj)
}


# Convert to long format for ggplot2
salary_plot <- melt(salary_plot, id.vars = c("x"))
salary_plot_adj <- melt(salary_plot_adj, id.vars = c("x"))

ggplot(data = salary_plot) +
    geom_line(aes(x = x, y = value, color = variable), size = 1.5) + 
    scale_color_manual("Metro", values = brewer.pal(8, "Set1")[c(1:5, 7:8)]) + 
    scale_x_continuous("Annual Salary", labels=dollar) + 
    scale_y_continuous("Probability Density (A.U.)") + 
    theme(axis.text.y = element_blank())

ggplot(data = salary_plot_adj) +
    geom_line(aes(x = x, y = value, color = variable), size = 1.5) + 
    scale_color_manual("Metro", values = brewer.pal(8, "Set1")[c(1:5, 7:8)]) + 
    scale_x_continuous("Annual Salary (after COLI adjustment)", labels=dollar) + 
    scale_y_continuous("Probability Density (A.U.)") + 
    theme(axis.text.y = element_blank())
