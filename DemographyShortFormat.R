## Working with data that are in a data frame in SHORT format

# Data frames are special matrices that contain additional information about its content such as row and column names. In short, data frames are R’s equivalent to a an EXCEL spreadsheet. The columns (typically representing different measurements) can be of different types (e.g., one column could be the date of measurement, another the weight of the individual, or the volume of the cell, or the treatment of the sample), while the rows typically represent different samples.

# When you import a spreadsheet file into R, it is automatically stored as a data frame. The difference between a matrix and a data frame is that in a matrix all the values are of the same type (e.g., all numeric), while in a data frame each column can be of a different type.

#Let us look at a data set that comes with R, called trees.
# Create vectors for time, object IDs, and measurements

axType1 <- c(1000, 990, 970, 940, 900, 850, 750, 500, 200, 40, 1, 0)
axType2 <- c(2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 0)
axType3 <- c(10000, 100, 30, 20, 18, 17, 16, 15, 14, 13, 12, 0)

LifeTable <- data.frame(age=0:11, axType1, axType2, axType3)

lxType1 <- axType1/axType1[1]

lxType2 <- axType2/axType2[1]

lxType3 <- axType3/axType3[1]

gxType1 <- c(lxType1)

lxType1[-1]

lxType1[-length(lxType1)]

gxType1 <- c(lxType1[-1]/lxType1[-length(lxType1)], 0)
gxType2 <- c(lxType2[-1]/lxType2[-length(lxType2)], 0)
gxType3 <- c(lxType3[-1]/lxType3[-length(lxType3)], 0)

LifeTable <-cbind(LifeTable, lxType1, lxType2, lxType3)

LifeTable <-cbind(LifeTable, gxType1, gxType2, gxType3)


LifeTable

lxType2[-1]
lxType2[-length(lxType2)]


averagelxType1 <- (lxType1[-length(lxType1)] + lxType1[-1]) / 2
averagelxType2 <- (lxType2[-length(lxType2)] + lxType2[-1]) / 2
averagelxType3 <- (lxType3[-length(lxType3)] + lxType3[-1]) / 2

averagelx <- c(averagelxType1, averagelxType2, averagelxType3)
averagelx

lifeexpectancy <- function(age, averagelx, lx) {
  sum(averagelx[age:11]) / lx[age]
}

e1 <- c()
for (i in 0:11) {
  ex <- lifeexpectancy(i, averagelxType1, lxType1)
  e1 <- c(e1, ex)
}

e2 <- c()
for (i in 0:11) {
  ex <- lifeexpectancy(i, averagelxType2, lxType2)
  e2 <- c(e2, ex)
}

plot(e2)

e3 <- c()
for (i in 0:11) {
  ex <- lifeexpectancy(i, averagelxType3, lxType3)
  e3 <- c(e3, ex)
}

plot(e3)

lifeexp <- data.frame(0:10, e1, e2, e3)

lifeexp

plot(0:10, lifeexp$e1, type = "b", pch = 0, col="red", xlab = "age", ylab = "standardized survivorship, lx", main="Life Expectancy")
lines(0:10, lifeexp$e2, type = "b", pch = 1, col="blue")
lines(0:10, lifeexp$e3, type = "b", pch = 2, col="orange")
legend("topright", c("ex, type 1", "ex, type 2", "ex, type 3"), col =c("red", "blue", "orange"), pch = c(0, 1, 2)) # adds a plot legend

plot(LifeTable$age, LifeTable$lxType1, type = "b", pch = 0, col="red", xlab = "age", ylab = "standardized survivorship, lx", main="Survivorship Curves")
lines(LifeTable$age, LifeTable$lxType1, AgeSpecificLifeExpectancy$lxType2, type = "b", pch = 1, col="blue")
lines(LifeTable$age, LifeTable$lxType1, AgeSpecificLifeExpectancy$lxType3, type = "b", pch = 2, col="orange")
legend("topright", c("lx, type 1", "lx, type 2", "lx, type 3"), col =c("red", "blue", "orange"), pch = c(0, 1, 2)) # adds a plot legend

plot(AgeSpecificLifeExpectancy$age, AgeSpecificLifeExpectancy$lxType1, log="y", type = "b", pch = 0, col="red", xlab = "age", ylab = "standardized survivorship, lx", main="Survivorship Curves")
lines(AgeSpecificLifeExpectancy$age, AgeSpecificLifeExpectancy$lxType2, type = "b", pch = 1, col="blue")
lines(AgeSpecificLifeExpectancy$age, AgeSpecificLifeExpectancy$lxType3, type = "b", pch = 2, col="orange")
legend("topright", c("lx, type 1", "lx, type 2", "lx, type 3"), col =c("red", "blue", "orange"), pch = c(0, 1, 2)) # adds a plot legend

plot(LifeTable$age, LifeTable$gxType1, type = "b", pch = 0, col="red", xlab = "age", ylab = "Age specific survival, gx", main="Age Specific Survival")
lines(LifeTable$age, LifeTable$gxType2, type = "b", pch = 1, col="blue")
lines(LifeTable$age, LifeTable$gxType3, type = "b", pch = 2, col="orange")
legend("topright", c("lx, type 1", "lx, type 2", "lx, type 3"), col =c("red", "blue", "orange"), pch = c(0, 1, 2)) # adds a plot legend

AgeSpecificLifeExpectancy
data(trees) # load data set "trees"
str(trees) # structure of data frame ’data.frame’
ncol(trees) # number of columns
nrow(trees) # number of rows
head(trees) # print the first few rows Girth Height Volume
trees$Girth # select column by name
# select column by name; return first 5 elements
trees$Height[1:5]
trees[1:3, ] # select rows 1 through 3
trees[trees$Height > 80, ] # select all rows that contain trees taller than 80 ft
# select rows 1 through 3; return column Volume
trees[1:3, ]$Volume
trees <- rbind(trees, c(13.25, 76, 30.17)) # add a row
trees_double <- cbind(trees, trees) # combine columns
head(trees_double)
# change column names
colnames(trees) <- c("girth", "height", "volume")
head(trees