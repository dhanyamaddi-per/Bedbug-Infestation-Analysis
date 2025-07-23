
library(tidyverse)
library(glmnet)

##### Importing dataset
Bedbug<-read.csv("Bedbug_Reporting_cleaned.csv")
head(Bedbug)
Bedbug<-na.omit(Bedbug)

str(Bedbug)
head(Bedbug$Filing.Date)

# Load required libraries
library(ggplot2)
library(dplyr)


##### Exploratory Analysis

Bedbug$Filing.Date <- as.Date(Bedbug$Filing.Date, format = "%m/%d/%Y")

# Convert 'Filing Date' to Date format
Bedbug$Filing.Date <- as.Date(Bedbug$Filing.Date)
Bedbug$Month <- format(Bedbug$Filing.Date, "%m")
Bedbug<-na.omit(Bedbug)


Bedbug$Filing.Period.Start.Date <- as.Date(Bedbug$Filing.Period.Start.Date, format = "%m/%d/%Y")

Bedbug$Filling.Period.End.Date<- as.Date(Bedbug$Filling.Period.End.Date,format="%m/%d/%Y")



# Plotting
ggplot(data = Bedbug, aes(x = Month, y = Infested.Dwelling.Unit.Count, fill = Borough)) +
  geom_bar(stat="identity")+
  labs(title = "Bedbug Infestations Over Time in Different Boroughs",
       x = "Month",
       y = "Infested Dwelling Unit Count",
       color = "Borough") +
  theme_minimal()+
  facet_grid(~Borough)+
  scale_y_continuous(labels = scales::comma_format(scale = 0.001))

#### create a bar graph for Borough
ggplot(Bedbug,aes(x=Borough))+
  geom_bar(color="black",fill="cyan")+
  labs(title = "Total number of bedbug cases reported by each Borough in New York",x= "Borough",y="Frequency")

##### Histogram of BIN(Building Identification Number) 
ggplot(Bedbug,aes(x=BIN))+
  geom_histogram(binwidth=1000000,color="black",fill="red")+
  scale_x_continuous(labels = scales::comma_format(scale=0.001)) +
  labs(title = "Histogram of Building Identification Number", x = "Building Identification Number", y = "Frequency") 
 
##### Histogram of BBL (Borough-Block-Lot) 
ggplot(Bedbug,aes(x=BBL))+
  geom_histogram(binwidth=1000000000,color="black",fill="blue")+
  scale_x_continuous(labels = scales::comma_format(scale=0.0001)) +
  labs(title = "Histogram of Borough-Block-Lot", x = "Borough-Block-Lot", y = "Frequency") 

##### Bargraph of 2010 census tract
ggplot(Bedbug,aes(x=X2010.Census.Tract,fill=Borough))+
  geom_bar(color="black",)+
  coord_cartesian(xlim = c(0, 50))+
  facet_grid(~Borough)+
  theme(legend.position ="bottom")+labs(
    title = "Distribution of Census Tract by Borough",
    x = "X2010 Census Tract",
    y = "Count",
    fill = "Borough",
    caption = "Source: Bedbug Reporting"
  ) 


##### barplot for re-infestion with repestive Borough
ggplot(Bedbug, aes(y= Re.infested..Dwelling.Unit.Count, x = Month, fill = Borough))+ geom_bar(stat="identity") +labs(
    title = "Monthly Distribution of Re-infested Dwelling Unit Count by Borough",
    y = "Re-infested Dwelling Unit Count",
    x = "Month",
    color = "Borough",
    caption = "Source: Bedbug Reporting"
  )+
  coord_flip() +
  coord_cartesian(ylim = c(0, 50))
  
library(ggplot2)


### Frequency polygon for Infestated Dwell units
ggplot(Bedbug, aes(x = Infested.Dwelling.Unit.Count, color = Borough)) +
  geom_freqpoly(binwidth = 5000, size = 1.00) +
  scale_color_manual(
    values = c("red", "orange", rgb(0, 0.75, 0), "blue", "violet"),
    na.value = "gray30"
  ) +
  labs(
    title = "Frequency Polygon of Infested Dwelling Unit Count by Borough",
    x = "Infested Dwelling Unit Count",
    y = "Frequency",
    color = "Borough",
    caption = "\n \n   Figure : Distribution of Infested Dwelling Unit Count by Borough"
  ) +
  theme_minimal()

#### Scatterplot between dwell units and re-infested dwell units
ggplot(Bedbug,aes(x=X..of.Dwelling.Units,y=Re.infested..Dwelling.Unit.Count))+
  geom_point(
    shape = 21,
    fill = "violet",
    size = 3,
    color = "black"
  )+
  scale_y_continuous(limits =c(-10,100))+
  scale_x_continuous(limits=c(-10,2500))+
  labs(
    title = "Bedbug Scatter Plot",
    x = "Number of Dwelling Units",
    y = "Re-infested Dwelling Unit Count",
    caption = "Source: Bedbug Reporting"
  )


##### Linear Regression
ggplot(Bedbug, aes(x = Infested.Dwelling.Unit.Count, y = Re.infested..Dwelling.Unit.Count)) +
  geom_point(shape = 20, fill="orange", size = 3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression: Re-infested Dwelling Units vs Infested Dwelling Units",
       x = "Infested Dwelling Unit Count",
       y = "Re-infested Dwelling Unit Count") +
  scale_y_continuous(limits = c(-10, 100)) +
  scale_x_continuous(limits = c(-10, 500)) +
  theme_minimal() +  # Example: Change the theme to minimal
  theme(legend.position = "none")  # Example: Remove legend if not needed


# Plot the time-series or line series

Bedbug$Year <- lubridate::year(Bedbug$Filing.Date)
Bedbug$Year
Bedbug$EndYear<- lubridate::year(Bedbug$Filling.Period.End.Date )
Bedbug$EndYear



borough_colors <- c("red", "navy", "lightblue", "violet", "orange")

# Plot the bar plot with different colors for each borough
ggplot(Bedbug, aes(x = as.factor(Year), y = Infested.Dwelling.Unit.Count, fill = Borough)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Plot of Infested Dwelling Unit Count by filling Year",
       x = "Filing Year",
       y = "\n Infested Dwelling Unit Count",
       fill = "Borough",
       caption = "Source: Bedbug Reporting") +
  scale_fill_manual(values = borough_colors) +  # Use the defined colors
  coord_flip() +
  theme_minimal()


# Plot 1
ggplot(Bedbug, aes(y = StartYear, fill = Borough)) +
  geom_boxplot() +
  labs(title = "Boxplot of Filing StartYear by Borough",
       y = "Start Year",
       fill = "Borough",
       caption = "Figure : Distribution of Filing StartYear by Borough") +
  theme_minimal()

# Plot 2
ggplot(Bedbug, aes(y = Year, fill = Borough)) +
  geom_boxplot() +
  labs(title = "Boxplot of Filing Year by Borough",
       y = "Year",
       fill = "Borough",
       caption = "Figure : Distribution of Filing Year by Borough") +
  theme_minimal()

# Plot 3
ggplot(Bedbug, aes(y = Latitude, fill = Borough)) +
  geom_boxplot() +
  labs(title = "Boxplot of Latitude by Borough",
       y = "Latitude",
       fill = "Borough",
       caption = "Figure : Distribution of Latitude by Borough") +
  theme_minimal()

# Plot 4
ggplot(Bedbug, aes(y = Longitude, fill = Borough)) +
  geom_boxplot() +
  labs(title = "Boxplot of Longitude by Borough",
       y = "Longitude",
       fill = "Borough",
       caption = "\n Figure : Distribution of Longitude by Borough") +
  theme_minimal()


############ Correlation between selected columns

# Select specific columns for correlation
selected_columns <- c("Infested.Dwelling.Unit.Count", "Eradicated.Unit.Count", "X..of.Dwelling.Units")

# Calculate the correlation matrix for selected columns
cor_matrix_selected <- cor(Bedbug[selected_columns])

# Print the correlation matrix for selected columns
print(cor_matrix_selected)




########## Summary Statistics for selected columns

# Install and load the e1071 library
install.packages("e1071")
library(e1071)

summary(Bedbug$Infested.Dwelling.Unit.Count)

selected_columns <- c("Infested.Dwelling.Unit.Count", "Eradicated.Unit.Count", "X..of.Dwelling.Units")

# Compute summary statistics, including skewness, kurtosis, and standard deviation
summary_stats <- describe(Bedbug[selected_columns])

# Print the summary statistics
print(summary_stats)

# Install and load the e1071 and moments libraries
install.packages(c("e1071", "moments"))
library(e1071)
library(moments)

# Select specific columns for analysis
selected_columns <- c("Infested.Dwelling.Unit.Count", "Eradicated.Unit.Count", "X..of.Dwelling.Units")

# Compute skewness, kurtosis, and standard deviation
skewness_values <- sapply(Bedbug[selected_columns], skewness)
kurtosis_values <- sapply(Bedbug[selected_columns], kurtosis)
sd_values <- sapply(Bedbug[selected_columns], sd)

# Combine the results into a data frame
summary_stats <- data.frame(
  Skewness = skewness_values,
  Kurtosis = kurtosis_values,
  Standard_Deviation = sd_values
)

# Print the summary statistics
print(summary_stats)



############# Correlation PLOT
install.packages("corrplot")
# Load the corrplot library
library(corrplot)

# Select numeric columns for correlation analysis
numeric_columns <- sapply(Bedbug, is.numeric)
correlation_data <- Bedbug[, numeric_columns]

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data)

# Create a correlation plot
corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust")


####### K-means Clusterings

cluster_data <- Bedbug[, c("Infested.Dwelling.Unit.Count", "Eradicated.Unit.Count", "Re.infested..Dwelling.Unit.Count", "X..of.Dwelling.Units", "Latitude", "Longitude")]

# Remove rows with missing values
cluster_data <- na.omit(cluster_data)

# Standardize the data
scaled_data <- scale(cluster_data)

# Determine the optimal number of clusters using the elbow method
wss <- numeric(20)
for (i in 1:20) {
  kmeans_model <- kmeans(scaled_data, centers = i)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow method
plot(1:20, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters", ylab = "Within-cluster sum of squares")

# Based on the plot, choose the number of clusters (e.g., where the elbow occurs)

# Perform k-means clustering with the chosen number of clusters
k <- 7  # You can choose the appropriate number of clusters based on the elbow plot
kmeans_model <- kmeans(scaled_data, centers = k)

# Add cluster assignments to the original data frame
Bedbug$Cluster <- as.factor(kmeans_model$cluster)

# Visualize the clusters on a scatter plot
library(ggplot2)
ggplot(Bedbug, aes(x = Longitude, y = Latitude, color = Cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering of Bedbug Data",
       x = "Longitude", y = "Latitude") +
  scale_color_discrete(name = "Cluster")


