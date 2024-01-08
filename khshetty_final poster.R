# FINAL POSTER

# Khushi Shetty
# IST 719: Information Visualization

# Video Game Sale Datset

# Loading data
data.dir <- "C:/Users/Khushi/OneDrive/Desktop/poster/"
video_game <- read.csv(paste0(data.dir,"Video_Games_Sales.csv"))

# Load the packages
library(ggplot2)
library(dplyr)

# View the first few rows of the data
head(video_game)
# View colomn names
column_names <- names(video_game)  
print(column_names)

# Count the NA values in each column
na_count <- colSums(is.na(video_game))
# Display the number of NA values for each column
print(na_count)

# We note that there are too many missing values.
# This is because the data set used is a combination 
# of two different sets where many of the original observations 
# did not have corresponding data collected in the second data set.
# Since the analysis is also focused on the scores obtained by each title, 
# we are not interested in games for which we do not have this data. 
# We therefore eliminate records that have missing values.

# Remove rows with NA values
video_game <- na.omit(video_game)
# Now, the 'video_game' dataset contains rows without NA values

na_count <- colSums(is.na(video_game))
# Display the number of NA values for each column
print(na_count)
# We see that there are no more NA values.

summary(video_game)
str(video_game)

# It seems that the user score is not numeric even though it should be.
# let us convert it to numeric
video_game$User_Score <- as.numeric(video_game$User_Score)

# It seems that the year of release is not numeric even though it should be. 
# Let us look at the values of this variable:

unique(video_game$Year_of_Release)

# We see that there is a string "N/A" that was missed when we checked for NA values. 
# These need to be removed just like other NA values:

video_game <- video_game[video_game$Year_of_Release != "N/A", ]
unique(video_game$Year_of_Release)
# There are no more "NA" values. 

# We can now convert and store the variable as an integer:
video_game$Year_of_Release <- as.integer(video_game$Year_of_Release)

# We have to check other string variables for the same problem of having N/As
sum(video_game$Publisher=="N/A")
sum(video_game$Developer == "N/A")
sum(video_game$Rating == "N/A")

# As we can see, the Pub;lisher column has 1 N/A value. 
# Therefore remove the record

video_game <- video_game[video_game$Publisher != "N/A", ]
sum(video_game$Publisher=="N/A")
# There are no  more null values in the dataset

#Let's view the structure of the data again
str(video_game)
summary(video_game)
# We notice that the user score is out of ten whil ethe critic score is out of 100. 
# It makes sense to have them both using the same scale.
video_game$User_Score <- video_game$User_Score * 10

#Viewing the data again
str(video_game)
summary(video_game)


# There are too many genres. 
# Let us create a new genre variable that collapses these values into 
# seven categories: Fighting, Adventure, Simulation, Misc, Action, Racing, Sports:

video_game <- video_game %>% mutate(Genre2 = case_when(
  Genre %in% c("Shooter", "Fighting") ~ "Fighting",
  Genre %in% c("Adventure", "Platform") ~ "Adventure",
  Genre %in% c("Simulation", "Role-Playing") ~ "Simulation",
  Genre %in% c("Misc", "Puzzle", "Strategy") ~ "Misc",
  Genre == "Action" ~ "Action",
  Genre == "Racing" ~ "Racing",
  Genre == "Sports" ~ "Sports"
))

# There are too many platforms. 
# Let us create a new platform variable that collapses these values into 
# four categories: Nintendo, PS, XBox, PC, and Sega:

video_game <- video_game %>% mutate(platform2 = case_when(
  Platform %in% c("Wii", "DS", "3DS", "WiiU", "GC", "GBA", "DC") ~ "Nintendo",
  Platform %in% c("X360", "XB", "XOne") ~ "XBox",
  Platform %in% c("PS3", "PS4", "PS2", "PS", "PSP", "PSV") ~ "PS",
  Platform == "PC" ~ "PC"
))

# View colomn names
column_names <- names(video_game)  
print(column_names)

# View the first few rows of the data
head(video_game)


# KEY VISUALIZATION

# Horizontal Bar Graph: Percentage of Sales in Each Region by Platforms
video_game$platform2 <- factor(video_game$platform2)

# Aggregate data by platform and calculate total sales in each region
agg_data_region <- video_game %>%
  group_by(platform2) %>%
  summarise(total_na_sales = sum(NA_Sales),
            total_eu_sales = sum(EU_Sales),
            total_jp_sales = sum(JP_Sales),
            total_other_sales = sum(Other_Sales))

# Calculate the percentage of sales for each region
agg_data_region_percentage <- agg_data_region %>%
  mutate(
    perc_na_sales = total_na_sales / sum(total_na_sales) * 100,
    perc_eu_sales = total_eu_sales / sum(total_eu_sales) * 100,
    perc_jp_sales = total_jp_sales / sum(total_jp_sales) * 100,
    perc_other_sales = total_other_sales / sum(total_other_sales) * 100
  ) %>%
  pivot_longer(cols = starts_with("perc_"), names_to = "Region", 
               values_to = "Percentage_Sales")

# Create a horizontal grouped bar plot for percentage of sales in each region
p_perc_sales_per_region_horizontal <- ggplot(agg_data_region_percentage, 
                                             aes(y = platform2, x = Percentage_Sales, 
                                                 fill = Region)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  labs(title = "Percentage of Sales in Each Region by Platforms",
       y = "Platforms",
       x = "Percentage of Sales",
       fill = "Region") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1))  

print(p_perc_sales_per_region_horizontal)


# SUPPORTING VISUALIZATIONS

# 1. COMBINED BAR AND LINE GRAPH

# Bar plot: Global Sales by Genre

# Aggregate global sales by genre
genre_sales <- video_game %>%
  group_by(Genre2) %>%
  summarize(Total_Sales = sum(Global_Sales))

# Create a bar plot
ggplot(genre_sales, aes(x = Genre2, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Bar Plot: Total Global Sales by Genre",
       x = "Genre",
       y = "Total Global Sales") +
  theme(axis.text.x = element_text(angle = 45))

# Line plot
video_game$Genre2 <- factor(video_game$Genre2, 
                            levels = c("Action", "Fighting", "Sports", 
                                       "Simulation", "Misc", "Racing", "Adventure"))

# Aggregate data by genre and calculate average scores
aggregate_data <- video_game %>%
  group_by(Genre2) %>%
  summarise(Avg_Critic_Score = mean(Critic_Score, na.rm = TRUE),
            Avg_User_Score = mean(User_Score, na.rm = TRUE))

# Line graph for Average Critic Score and User Score by Genre2
line_plot_avg_scores <- ggplot(aggregate_data, aes(x = Genre2)) +
  geom_line(aes(y = Avg_Critic_Score, color = "Critic Score"), group = 1) +
  geom_point(aes(y = Avg_Critic_Score, color = "Critic Score"), group = 1) +
  geom_line(aes(y = Avg_User_Score, color = "User Score"), group = 2) +
  geom_point(aes(y = Avg_User_Score, color = "User Score"), group = 2) +
  labs(title = "Line Graph: Average Critic and User Scores by Genre",
       x = "Genre",
       y = "Average Score") +
  scale_color_manual(values = c("Critic Score" = "#FF5733", "User Score" = "#33A2FF"),
                     name = "Score Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(line_plot_avg_scores)


# 2. 4 PIE CHARTS REPRESENTING 4 REGIONS 

video_game$Genre2 <- factor(video_game$Genre2)

# Specify the custom color palette
custom_colors <- c("#12d0f9", "#d25dd3", "#FF905C", "#28eca1", "#0f67ec", "#ebca75", "#f9418e")

# North America Sales

# Aggregate data by genre and calculate total NA sales
agg_data_na_sales <- video_game %>%
  group_by(Genre2) %>%
  summarise(total_na_sales = sum(NA_Sales))

# Create a pie chart for NA Sales distribution by genre with custom colors
pie_chart_na_sales <- ggplot(agg_data_na_sales, aes(x = "", y = total_na_sales, fill = Genre2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(title = "NA Sales Distribution by Genre") +
  guides(fill = guide_legend(title = "Genre2")) +  # Add legend
  theme_minimal()

print(pie_chart_na_sales)

# North America Sales Percentage
na_sales_percentage <- video_game %>%
  group_by(Genre2) %>%
  summarise(total_na_sales = sum(NA_Sales)) %>%
  mutate(percentage = (total_na_sales / sum(total_na_sales)) * 100)
# View the result
print(na_sales_percentage)


# JP Sales

# Aggregate data by genre and calculate total JP sales
agg_data_jp_sales <- video_game %>%
  group_by(Genre2) %>%
  summarise(total_jp_sales = sum(JP_Sales))

# Create a pie chart for JP Sales distribution by genre with custom colors
pie_chart_jp_sales <- ggplot(agg_data_jp_sales, aes(x = "", y = total_jp_sales, 
                                                    fill = Genre2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(title = "JP Sales Distribution by Genre") +
  guides(fill = guide_legend(title = "Genre")) +  # Add legend
  theme_minimal()

print(pie_chart_jp_sales)

# Japan Sales Percentage
jp_sales_percentage <- video_game %>%
  group_by(Genre2) %>%
  summarise(total_jp_sales = sum(JP_Sales)) %>%
  mutate(percentage = (total_jp_sales / sum(total_jp_sales)) * 100)
# View the result
print(jp_sales_percentage)


# EU SALES

# Aggregate data by genre and calculate total EU sales
agg_data_eu_sales <- video_game %>%
  group_by(Genre2) %>%
  summarise(total_eu_sales = sum(EU_Sales))

# Create a pie chart for EU Sales distribution by genre with custom colors
pie_chart_eu_sales <- ggplot(agg_data_eu_sales, aes(x = "", y = total_eu_sales, fill = Genre2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(title = "EU Sales Distribution by Genre") +
  guides(fill = guide_legend(title = "Genre")) +  # Add legend
  theme_minimal()

print(pie_chart_eu_sales)

# European Union Sales Percentage
eu_sales_percentage <- video_game %>%
  group_by(Genre2) %>%
  summarise(total_eu_sales = sum(EU_Sales)) %>%
  mutate(percentage = (total_eu_sales / sum(total_eu_sales)) * 100)
# View the result
print(eu_sales_percentage)


# OTHER SALES

# Aggregate data by genre and calculate total Other sales
agg_data_other_sales <- video_game %>%
  group_by(Genre2) %>%
  summarise(total_other_sales = sum(Other_Sales))

# Create a pie chart for Other Sales distribution by genre with custom colors
pie_chart_other_sales <- ggplot(agg_data_other_sales, aes(x = "", y = total_other_sales, fill = Genre2)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(title = "Other Sales Distribution by Genre") +
  guides(fill = guide_legend(title = "Genre")) +  # Add legend
  theme_minimal()

print(pie_chart_other_sales)

# Other Sales Percentage
other_sales_percentage <- video_game %>%
  group_by(Genre2) %>%
  summarise(total_other_sales = sum(Other_Sales)) %>%
  mutate(percentage = (total_other_sales / sum(total_other_sales)) * 100)
# View the result
print(other_sales_percentage)


# DATA DESCRIPTIVE PLOTS

# 1. Scatter plot for Critic Score vs. User Score
ggplot(video_game, aes(x = Critic_Score, y = User_Score, color = platform2)) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatter Plot: Critic Score vs. User Score",
       x = "Critic Score",
       y = "User Score") +
  scale_color_manual(values = c("#f9478e", "#2bee9f", "#fccc4f", "#6f34be"), name = "Platform")

# 2.Pie chart for Sales Distribution by Area

# Your code for summarizing the data
summarized_data <- video_game %>% gather(area, sales, NA_Sales:Other_Sales, 
                                         factor_key = TRUE) %>% 
  group_by(area, Year_of_Release) %>%
  summarise(sales = sum(sales))

# Create a pie chart
ggplot(summarized_data, aes(x = "", y = sales, fill = area)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Sales Distribution by Area",
       fill = "Area") +
  theme_minimal()

# Calculation of sales percentage by region
video_game$Genre <- factor(video_game$Genre)

# Calculate total global sales
total_global_sales <- sum(video_game$Global_Sales)

# Aggregate data by region and calculate total sales in each region
agg_data_region <- video_game %>%
  summarise(
    total_na_sales = sum(NA_Sales),
    total_eu_sales = sum(EU_Sales),
    total_jp_sales = sum(JP_Sales),
    total_other_sales = sum(Other_Sales)
  )

# Calculate the percentage of sales for each region based on total global sales

perc_na_sales <- agg_data_region$total_na_sales / total_global_sales * 100
perc_eu_sales <- agg_data_region$total_eu_sales / total_global_sales * 100
perc_jp_sales <- agg_data_region$total_jp_sales / total_global_sales * 100
perc_other_sales <- agg_data_region$total_other_sales / total_global_sales * 100

# Create a data frame for the percentages
perc_sales_data <- data.frame(
  Region = c("NA", "EU", "JP", "Other"),
  Percentage = c(perc_na_sales, perc_eu_sales, perc_jp_sales, perc_other_sales)
)

# Print the percentage of sales in each region based on total global sales
print(perc_sales_data)