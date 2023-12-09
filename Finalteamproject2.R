
house_data <- read.csv("house_sales.csv")

houdim(house_data)
str(house_data)
summary(house_data)
head(house_data)


house_data <- read.csv("house_sales.csv", na.strings = "")

colSums(is.na(house_data))

numeric_columns <- sapply(house_data, is.numeric)

for (col in names(house_data)[numeric_columns]) {
  house_data[is.na(house_data[, col]), col] <- mean(house_data[, col], na.rm = TRUE)
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

house_data_normalized <- as.data.frame(lapply(house_data[, c("sqft_living", "sqft_lot", "sqft_above")], normalize))

house_data$price_category <- cut(house_data$price, breaks = c(0, 500000, 1000000, max(house_data$price)), labels = c("Low", "Medium", "High"))
#finding numerical columns
numeric_columns2 <- house_data[, sapply(house_data, is.numeric)]
#mean of the numerical columns
means_by_column <- colMeans(numeric_columns2, na.rm = TRUE)
medians_by_column <- apply(numeric_columns2, 2, median, na.rm = TRUE)
#standard deviation
sd_values <- sapply(numeric_columns2, sd, na.rm = TRUE)
#calculate IQR for each numerical column
iqr_values <- sapply(numeric_columns2, IQR, na.rm = TRUE)

# Create a separate histogram for each numeric column
for (col in names(numeric_columns2)) {
  hist_data <- numeric_columns2[[col]]
  hist(hist_data, main = paste("Histogram for", col), xlab = "Values", col = "lightblue", border = "black")
}

lapply(numeric_columns, function(col_data) {
  boxplot(col_data, main = "Boxplot", col = "lightgreen", border = "black")
})

correlation_matrix <- cor(numeric_columns2, use = "complete.obs")
print(correlation_matrix)

install.packages("corrplot")
library(corrplot)3
corrplot(correlation_matrix, method = "circle", type = "upper", tl.cex = 0.7)
