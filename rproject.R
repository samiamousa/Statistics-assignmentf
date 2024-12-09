colnames(Purchase)
colnames(Purchase) <- c("ID", "PurchaseFrequency", "Gender", "Age", "ShoppingSatisfaction", "Column6", "Column7", "Column8", "Column9", "Column10", "Column11", "Column12", "Column13")
data <- Purchase[, 2:10]
data$PurchaseFrequency <- as.numeric(factor(data$PurchaseFrequency))
data$Gender <- as.numeric(factor(data$Gender))
data$ShoppingSatisfaction <- as.numeric(factor(data$ShoppingSatisfaction))
summary(data)
boxplot(data$ShoppingSatisfaction, main = "Boxplot of Shopping Satisfaction")ٍ
# Ensure the columns are numeric
data$Age <- as.numeric(as.character(data$Age))
data$ShoppingSatisfaction <- as.numeric(as.character(data$ShoppingSatisfaction))

# Calculate correlation
correlation <- cor(data$Age, data$ShoppingSatisfaction, use = "complete.obs")
print(correlation)
# Ensure the columns are factors
data$PurchaseFrequency <- factor(data$PurchaseFrequency)
data$Gender <- factor(data$Gender)

# Combine low-frequency levels (if needed)
freq_table <- table(data$PurchaseFrequency, data$Gender)
if (any(freq_table < 5)) {
  # Example: Combine sparse levels (adjust logic as needed)
  levels(data$PurchaseFrequency)[freq_table < 5] <- "Other"
}

# Perform the Chi-squared test
chisq_test <- chisq.test(table(data$PurchaseFrequency, data$Gender))
print(chisq_test)
barplot(table(data$PurchaseFrequency), main = "Purchase Frequency Distribution", col = "blue")
plot(data$Age, data$ShoppingSatisfaction, main = "Age vs Shopping Satisfaction", xlab = "Age", ylab = "Satisfaction", pch = 19, col = "red")
model <- lm(ShoppingSatisfaction ~ Age + PurchaseFrequency, data = data)
summary(model)
mse <- mean(model$residuals^2)
summary(model)$adj.r.squared
write.csv(data, "cleaned_data.csv")

         

