# Read data from a text file into a data frame
pm0 <- read.table("C:/Users/Hp/Documents/R/AIr-pollution-case-study/RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")

# Print the dimensions of the data frame
dim(pm0)

# Print the first few rows of the data frame
head(pm0)

# Read the column names from the first line of the text file
cnames <- readLines("C:/Users/Hp/Documents/R/AIr-pollution-case-study/RD_501_88101_1999-0.txt", 1)
print(cnames)

# Split the column names by the pipe character "|"
cnames <- strsplit(cnames, "|", fixed = TRUE)
print(cnames)

# Assign the column names to the data frame
names(pm0) <- make.names(cnames[[1]])
head(pm0)

# Extract the 'Sample.Value' column from the data frame
x0 <- pm0$Sample.Value

# Check the class of the 'x0' variable
class(x0)

# Display the structure of the 'x0' variable
str(x0)

# Display the summary statistics of the 'x0' variable
summary(x0)

# Calculate the proportion of missing values in 'x0'
mean(is.na(x0))*100 ## Are missing values important here?

# Read data from another text file into a data frame
pm1 <- read.table("C:/Users/Hp/Documents/R/AIr-pollution-case-study/RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "", nrow = 1304290)
names(pm1) <- make.names(cnames[[1]])
head(pm1)
dim(pm1)

# Extract the 'Sample.Value' column from the data frame
x1 <- pm1$Sample.Value

# Check the class of the 'x1' variable
class(x1)

# Display the summary statistics of the 'x1' variable
summary(x1)

# Calculate the proportion of missing values in 'x1'
mean(is.na(x1))  ## Are missing values important here?

# Create a boxplot of 'x0' and 'x1'
boxplot(x0, x1)

# Create a boxplot of the logarithm of 'x0' and 'x1'
boxplot(log10(x0), log10(x1))

# Display the summary statistics of 'x1'
summary(x1)

# Find the negative values in 'x1'
negative <- x1 < 0
sum(negative, na.rm = T)
mean(negative, na.rm = T)*100

# Extract the 'Date' column from 'pm1'
dates <- pm1$Date
str(dates)

# Convert 'dates' to Date format
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)

# Create a histogram of 'dates' by month
hist(dates, "month")  ## Check what's going on in months 1--6

# Find a monitor in New York State that exists in both datasets
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")
str(site0)
str(site1)
both <- intersect(site0, site1)
print(both)

# Calculate the number of observations available at each monitor in 'pm0'
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

# Choose a specific monitor in New York State from 'pm1' and 'pm0'
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dim(pm1sub)
dim(pm0sub)

# Plot the data for 2012
dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
str(dates1)
plot(dates1, x1sub)

# Plot the data for 1999
dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

# Plot the data for both years in the same panel
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20)  ## Whoa! Different ranges
abline(h = median(x1sub, na.rm = T))

# Find the global range of 'x0sub' and 'x1sub'
rng <- range(x0sub, x1sub, na.rm = T)
rng
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))

# Calculate state-wide means for both years
head(pm0)
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn1)

# Create separate data frames for states and their mean values
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
dim(mrg)
head(mrg)

# Connect lines between the mean values of both years
par(mfrow = c(1, 1))
with(mrg, plot(rep(1, 52), mrg[, 2], xlim = c(.5, 2.5)))
with(mrg, points(rep(2, 52), mrg[, 3]))
segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])

