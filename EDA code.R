
#----------------EDA--------------# 

#1 contract type customer are less likely to churn 

#Contract status plot 
# plotGraph  <- ggplot(churn_clean, aes(x = Contract)) + 
#   geom_bar(aes(fill = Churn)) + 
#   geom_text(aes(y = ..count.. -200,  
#                 label = paste0(round(prop.table(..count..),4) * 100, '%')),  
#             stat = 'count',  
#             position = position_dodge(.1),  
#             size = 3) 
# 
# #Plot contract data within a grid 
# plot(plotGraph) 


library(ggplot2)

# Create the base plot with Contract on x-axis and fill based on Churn
plotGraph <- ggplot(churn_clean, aes(x = Contract, fill = Churn)) + 
  geom_bar() + 
  geom_text(
    aes(y = after_stat(count) - 200, 
        label = paste0(round(after_stat(prop.table(count)), 4) * 100, '%')),
    stat = 'count',
    position = position_dodge(width = 0.9),
    size = 3
  ) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) + # Optional: Customize the fill colors
  theme_minimal() + # Optional: Apply a minimal theme for a clean look
  labs(
    title = "Churn Distribution by Contract Type",
    x = "Contract Type",
    y = "Count"
  )

# Plot the graph
plot(plotGraph)

--------------------------------------------------------------------------------------------
  

#2. Is there a significant difference in monthly charges for customers with different tenure lengths?  
#   
#   # Convert 'tenure' to a factor variable to represent different groups 
#   churn_clean$tenure_group <- cut(churn_clean$tenure, breaks = c(0, 20, 40, 60, Inf), labels = 
#                                     c("0-20", "21-40", "41-60", "61+")) 
# 
# 
# # Perform ANOVA 
# anova_result <- aov(MonthlyCharges ~ tenure_group, data = churn_clean) 
# 
# # Check the summary 
# summary(anova_result) 
# 
# posthoc_result <- TukeyHSD(anova_result) 
# 
# # Check the post-hoc test results 
# print(posthoc_result) 
# 
# ggplot(churn_clean, aes(x = tenure_group, y = MonthlyCharges)) + 
#   geom_boxplot(fill = "lightblue", color = "blue") + 
#   labs(title = "Monthly Charges Across Tenure Groups", 
#        x = "Tenure Group", 
#        y = "Monthly Charges") + 
#   theme_minimal() 

library(ggplot2)
library(dplyr)

# Convert 'tenure' to a factor variable to represent different groups using dplyr's case_when
churn_clean <- churn_clean %>%
  mutate(tenure_group = case_when(
    tenure <= 20 ~ "0-20",
    tenure <= 40 ~ "21-40",
    tenure <= 60 ~ "41-60",
    TRUE ~ "61+"
  ))

# Perform ANOVA using lm function
anova_result <- lm(MonthlyCharges ~ tenure_group, data = churn_clean)

# Check the summary of the ANOVA
summary(anova_result)

# Perform the post-hoc test using TukeyHSD
posthoc_result <- TukeyHSD(aov(anova_result))

# Check the post-hoc test results
print(posthoc_result)

# Create the boxplot using a different color palette and theme
ggplot(churn_clean, aes(x = tenure_group, y = MonthlyCharges)) + 
  geom_boxplot(aes(fill = tenure_group), color = "darkblue") + 
  scale_fill_brewer(palette = "Set3") +  # Change color palette for the fill
  labs(title = "Monthly Charges by Tenure Group", 
       x = "Tenure Group", 
       y = "Monthly Charges") + 
  theme_classic()  # Use a classic theme for a different look




----------------------------------------------------------------------------------------------------
#3. Which type of internet service influence more on churn rate ? (EDA) 
# 
# Graph <- ggplot(churn_clean, aes(x = InternetService)) + 
#   geom_bar(aes(fill = Churn)) + 
#   geom_text(aes(y = ..count.. -200,  
#                 label = paste0(round(prop.table(..count..),4) * 100, '%')),  
#             stat = 'count',  
#             position = position_dodge(.1),  
#             size = 3) 
# 
# plot(Graph)
library(ggplot2)

# Create the bar plot with InternetService on the x-axis and fill based on Churn
Graph <- ggplot(churn_clean, aes(x = InternetService, fill = Churn)) + 
  geom_bar(position = "dodge") + 
  geom_text(
    aes(y = after_stat(count) - 200, 
        label = paste0(round(after_stat(prop.table(count)), 4) * 100, '%')),
    stat = 'count',
    position = position_dodge(width = 0.9),
    size = 3
  ) +
  scale_fill_manual(values = c("Yes" = "#FF5733", "No" = "#33FF57")) +  # Custom colors for Churn categories
  theme_light() + 
  labs(
    title = "Churn Distribution by Internet Service",
    x = "Internet Service",
    y = "Count"
  )

# Plot the graph
plot(Graph)



------------------------------------------------------------------------------------------------ 
  
  #----------------correlation analysis-------------# 
  #Is there a correlation between the customer’s Contract type and churn rate? 
#   

# churn_clean$Churn <- as.numeric(mapvalues(churn_clean$Churn,from=c("No", "Yes"),to=c("0", "1")))
# churn_clean$Churn
# 
# churn_clean$Contract <- as.numeric(mapvalues(churn_clean$Contract,from=c("Month-to-month", "One year","Two year"),to=c("0", "1", "2")))
# churn_clean$Contract
# 
# Correlation_matrix <- cor(churn_clean[,c("Contract","Churn")])
# corrplot(Correlation_matrix,method = "color")
# summary(Correlation_matrix)
# 
# churn_correlation_result <- cor.test(churn_clean$Contract,churn_clean$Churn)
# print(churn_correlation_result)
# 
# churn_clean %>%
#   dplyr::select (Contract, MonthlyCharges, Churn) %>%
#   cor() %>%
#   corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7) 

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(corrplot)
library(dplyr)
library(plyr)

# Convert Churn and Contract to numeric
churn_clean$Churn <- as.numeric(mapvalues(churn_clean$Churn, from=c("No", "Yes"), to=c("0", "1")))
churn_clean$Contract <- as.numeric(mapvalues(churn_clean$Contract, from=c("Month-to-month", "One year", "Two year"), to=c("0", "1", "2")))

# Calculate the correlation matrix
Correlation_matrix <- cor(churn_clean[,c("Contract", "Churn")])

# Visualize the correlation matrix using ggplot2
corr_data <- melt(Correlation_matrix)
ggplot(data = corr_data, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()

# Summary of the correlation matrix
summary(Correlation_matrix)

# Perform the correlation test
churn_correlation_result <- cor.test(churn_clean$Contract, churn_clean$Churn)
print(churn_correlation_result)

# Extended correlation plot with ggplot2 for more variables
extended_correlation_matrix <- churn_clean %>%
  dplyr::select(Contract, MonthlyCharges, Churn) %>%
  cor()

extended_corr_data <- melt(extended_correlation_matrix)
ggplot(data = extended_corr_data, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()










  #-----------------Hypothesis  testing---------------------# 
# 6. Customers who have been with the company for a longer time are less likely to leave,
# Compared to clients who have been with the firm for a shorter time
# head(churn_clean)
# 
# hypertension_group <- churn_clean$tenure[churn_clean$Churn == "1"]
# no_hypertension_group <- churn_clean$tenure[churn_clean$Churn == "0"]
# 
# # Perform t-test
# t.testing <- t.test(hypertension_group, no_hypertension_group)
# print(t.testing)
# 
# 
# ggplot(churn_clean, aes(x = as.factor(Churn), y = tenure, fill = as.factor(Churn))) +
#   geom_boxplot() +
# 
# 
#   labs(title = "Tenure by Churn ",
#        x = "Churn",
#        y = "tenure") +
#   theme_minimal()

  library(ggplot2)

# Perform the t-test
hypertension_group <- churn_clean$tenure[churn_clean$Churn == "1"]
no_hypertension_group <- churn_clean$tenure[churn_clean$Churn == "0"]

t.testing <- t.test(hypertension_group, no_hypertension_group)
print(t.testing)

# Create an enhanced boxplot with additional elements
ggplot(churn_clean, aes(x = as.factor(Churn), y = tenure, fill = as.factor(Churn))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +  # Customize outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jittered points for individual data points
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue", fill = "blue") +  # Add mean points
  labs(title = "Tenure by Churn",
       x = "Churn",
       y = "Tenure") +
  scale_fill_manual(values = c("1" = "#FF9999", "0" = "#9999FF")) +  # Use soft color palette
  theme_classic() +
  theme(legend.position = "none")  # Remove legend since it's not necessary



---------------------------------------------------------------------------------------- 
  #-------------------Linear Regression-------------# 
  #8. Is there any linear relationship between Monthly service Charges  
  #and the total service charges that Customer are paying 
  
  # Perform linear regression 
  regression_model <- lm(TotalCharges ~ MonthlyCharges + tenure, data = churn_clean) 

# Summary of the regression model 
summary(regression_model) 
#MonthlyCharges <- as.numeric(churn_clean$MonthlyCharges) 
# Plotting the regression line 

ggplot(churn_clean, aes(x = MonthlyCharges, y = TotalCharges, color = tenure)) + 
  scale_color_gradient(low = "lightblue", high = "red") +  # Gradient from light blue to dark blue
  labs(title = "Scatter Plot: MonthlyCharges vs. TotalCharges",
       x = "MonthlyCharges",
       y = "TotalCharges") +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Linear Regression: MonthlyCharges vs. TotalCharges", 
       x = "MonthlyCharges", 
       y = "TotalCharges") + 
  theme_minimal()
