# Just a test file to show some functionalities of Rstudio

# Generate random data
set.seed(6)
x <- rexp(50)

# Assign two rows and two columns for plot
par(mfcol = c(2, 2))

# Define plots lots
hist(x, main = "Top left")                   # Top left
boxplot(x, main = "Bottom left")             # Bottom left
plot(x, main = "Top right")                  # Top right
pie(table(round(x)), main = "Bottom right")  # Bottom right

# Back to the original graphics device
par(mfcol = c(1, 1))
