library(ggplot2)

# Read sales csv file
data <- read.csv("sales.csv")

print("Here is a preview of the data: ")
print(head(data)) #shows top few rows



# Numeric datatype: advertising column
numeric_advertising <- data$Advertising
cat("\nAdvertising Values:\n")
print(numeric_advertising)


# Integer datatype: converts Sales to integers
integer_sales <- as.integer(data$Sales)
cat("\nSales Values in Integers:\n")
print(integer_sales)


# Character datatype: creates campaign labels
campaign_labels <- paste("Campaign", 1:nrow(data))
cat("\nCharacter Campaign Labels:\n")
print(campaign_labels)


# Logical datatype: checks which values are greater than or equal to 40
high_sales <- data$Sales >= 40
cat("\nLogical Values (Sales => 40): TRUE means sales met or exceeded 40\n")
for (i in 1:length(high_sales)) {
  cat(paste(campaign_labels[i], ":", high_sales[i], "\n"))
}


# List datatype: combines the campaign labels into a list
summary_list <- list(
  Advertising = numeric_advertising,
  Sales = integer_sales,
  HighSalesFlag = high_sales,
  Campaign = campaign_labels
)
cat("\nSummary:\n")
print(summary_list)


# Loop 1: prints message for eaach campaign based on sales
cat("\nSales Performance by Campaign:\n")
for (i in 1:nrow(data)) {
  message <- paste(campaign_labels[i], "had sales of", data$Sales[i], ".")
  if (high_sales[i]) {
    message <- paste(message, "This campaign performed well!")
  }
  else {
    message <- paste(message, "This campaign had low performance.")
  }
  print(message)
}

# Loop 2: categorizes each advertisement trial by performance
cat("\nSales Performance Category:\n")
for (i in 1:nrow(data)) {
  performance <- if (data$Sales[i] >= 55) {
    "Excellent"
  }
  else if (data$Sales[i] >= 40) {
    "Good"
  }
  else {
    "Poor"
  }
  print(paste(campaign_labels[i], "-Performance: ", performance))
}


# Count how many campaigns were successful
num_successful <- sum(high_sales)
cat("\nNumber of successful campaigns:\n")
print(num_successful)


# Create the linear regression model
model <- lm(Sales ~ Advertising, data = data)



# Print the model summary
cat("\nLinear Regression Model Summary:\n")
print(summary(model))

# Plot the data with regression line
plot <- ggplot(data, aes(x = Advertising, y = Sales)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color =  "red") +
  labs(title =  "Advertising vs Sales",
  x = "Advertising Budget",
  y = "Sales")


# Creates png of graph
ggsave("regression_plot.png", plot = plot, width = 6, height = 4)

# Display the plot
print(plot)