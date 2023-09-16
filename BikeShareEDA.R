library(tidyverse)
library(corrplot)
library(readr)
library(patchwork)
test <- vroom("test.csv")
sampleSubmission <- read_csv("sampleSubmission.csv")
train <- vroom("train.csv")



DataExplorer::plot_intro(train) #- visualization of glimpse()
DataExplorer::plot_correlation(train) #- correlation heat map between variables
DataExplorer::plot_bar(train) #- bar charts of all discrete variables
DataExplorer::plot_histrograms(train) #- histograms of all numerical variables
DataExplorer::plot_missing(train) #- percent missing in each column
view(train)

plot(train)
view(train)

corrplot(cor(train))

# (plot1 + plot2) / (plot1 + plot2) #4 panel plot
plot1 <- ggplot(test, aes(x = season)) +
  geom_bar() +
  labs(
    title = "Quantity of Each Value in 'Season'",
    x = "Season",
    y = "Quantity"
  ) +
  theme_minimal()

plot2 <- DataExplorer::plot_missing(train)
plot3 <- ggplot(variance_df, aes(x = Column, y = Variance)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            labs(
              title = "Variances of Columns in 'train' Data",
              x = "Column Names",
              y = "Variance"
            ) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot4 <- corrplot(cor(test %>% select(-season, -holiday, -workingday, -weather, -datetime)))

#### Creating Plot 4 ####
data <- reshape2::melt(cor(test %>% select(-season, -holiday, -workingday, -weather, -datetime)))

plot4 <- ggplot(data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "black") +
  labs(x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Creating Plot 3 ####
train_new <- train %>%
  select(-datetime)  # Remove the 'datetime' variable

# Calculate variances for each column except 'datetime'
variances <- sapply(train_new, var)

# Create a data frame for plotting
variance_df <- data.frame(Column = names(train_new), Variance = variances)

# Create the ggplot
ggplot(variance_df, aes(x = Column, y = Variance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Variances of Columns in 'train' Data (Excluding 'datetime')",
    x = "Column Names",
    y = "Variance"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### Create 2x2 Plot ####
(plot2 + plot3) / (plot4 + plot1)


