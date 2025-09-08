# Working with data that are in a data frame in long format

axType1 <- c(1000, 990, 970, 940, 900, 850, 750, 500, 200, 40, 1, 0)
axType2 <- c(2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 0)
axType3 <- c(10000, 100, 30, 20, 18, 17, 16, 15, 14, 13, 12, 0)

age <- rep(0:11, times = 3)            # Repeats 1 to 11 for each of 3 objects
species <- rep(1:3, each = 12)                # Repeats object IDs 1, 2, 3 each for 11 times
ax <- c(
  axType1,    # measurements for object 1
  axType2,     # measurements for object 2
  axType3     # measurements for object 3
)

LifeTable <- data.frame(
  species = species,
  age = age,
  ax = ax
)

LifeTable

LifeTable$lx <- with(LifeTable, ax / ave(ax, species, FUN = function(x) x[1])
)

species_list <- unique(LifeTable$species)
colors <- c("red", "blue", "green")

plot(NULL, xlim = range(LifeTable$age), ylim = range(LifeTable$lx),
     xlab = "Age", ylab = "lx", main = "Survival Curves by Species")

for (i in seq_along(species_list)) {
  subset_data <- subset(LifeTable, species == species_list[i])
  lines(subset_data$age, subset_data$lx, col = colors[i], lwd = 2)
}
