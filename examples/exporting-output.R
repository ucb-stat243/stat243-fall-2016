# Various examples for exporting data and output form R


# change this directory!!!
setwd('~/Dropbox/course_stat243/stat243-fall-2016/data')

dat <- read.csv('cpds.csv')

# subset Australia
australia <- subset(dat, country == "Australia")
summary(australia)

# export a CSV file
write.table(australia, "australia.txt", row.names = FALSE)

# export a CSV file
write.csv(australia, "australia.csv", row.names = FALSE)

head(australia)

# summary statistics of unemp
min(australia$unemp)
max(australia$unemp)
median(australia$unemp)
mean(australia$unemp)
sd(australia$unemp)


# summary statistics of unemp
aus_min <- min(australia$unemp)
aus_max <- max(australia$unemp)
aus_med <- median(australia$unemp)
aus_avg <- mean(australia$unemp)
aus_sd <- sd(australia$unemp)

outfile <- "australia-statistics.txt"
file.create(outfile)
cat("Australia Unemployment Statistics\n\n", file = outfile)
cat("Minimum:", aus_min, "\n", file = outfile, append = TRUE)
cat("Maximum:", aus_max, "\n", file = outfile, append = TRUE)
cat("Median :", aus_med, "\n", file = outfile, append = TRUE)
cat("Mean   :", aus_avg, "\n", file = outfile, append = TRUE)
cat("Std Dev:", aus_sd, "\n", file = outfile, append = TRUE)

sprintf('Minimum: %s', aus_min)
sprintf('Minimum: %f', aus_min)
sprintf('Minimum: %0.2f', aus_min)
sprintf('Mean: %s', aus_avg)
sprintf('Mean: %f', aus_avg)
sprintf('Minimum: %0.2f', aus_avg)


cat("Australia Unemployment Statistics\n\n", file = outfile)
cat(sprintf('Minimum: %0.2f', aus_min), "\n", file = outfile, append = TRUE)
cat(sprintf('Maximum: %0.2f', aus_max), "\n", file = outfile, append = TRUE)
cat(sprintf('Median : %0.2f', aus_med), "\n", file = outfile, append = TRUE)
cat(sprintf('Mean   : %0.2f', aus_avg), "\n", file = outfile, append = TRUE)
cat(sprintf('Std Dev: %0.2f', aus_sd), "\n", file = outfile, append = TRUE)


# summary statistics of unemp
aus_stats <- list(
  Minimum = min(australia$unemp),
  Maximum = max(australia$unemp),
  Median = median(australia$unemp),
  Mean = mean(australia$unemp),
  Stdev = sd(australia$unemp)
)

cat("Australia Unemployment Statistics\n\n", file = outfile)
for (s in 1:length(aus_stats)) {
  cat(names(aus_stats)[s], sprintf(': %0.2f', aus_stats[[s]]), "\n", 
      file = outfile, 
      append = TRUE)
}



# Sink
sink(file = "australia-stats2.txt")
# summary statistics of unemp
summary(australia)
sink()


# linear regression
reg <- lm(realgdpgr ~ unemp, data = dat)
reg

summary(reg)


# R package xtable
reg_table <- xtable(reg)
# default
print(reg_table)


print(reg_table, type = "html", file = "reg-table.html")


sink("reg-table.txt")
print(reg_table)
sink()


