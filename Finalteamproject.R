house_data <- read.csv("house_sales.csv")

dim(house_data)
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
