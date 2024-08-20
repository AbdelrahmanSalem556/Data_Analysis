library(dplyr)
library(ggplot2)
library(tidyr)

# Read the data
data <- read.csv("D:/FIRST R/business.retailsales2.csv")

# Clean the data
data <- na.omit(data)

# Inspect Data Distribution
summary(data)
boxplot(data$Net.Sales, main="Boxplot of Net Sales")
boxplot(data$Total.Sales, main="Boxplot of Total Sales")
table(data$Month)
table(data$Year)

# Monthly and Yearly Trends
monthly_net_sales <- data %>%
  group_by(Month, Year) %>%
  summarise(monthly_net_sales = sum(Net.Sales, na.rm = TRUE))

monthly_total_sales <- data %>%
  group_by(Month, Year) %>%
  summarise(monthly_total_sales = sum(Total.Sales, na.rm = TRUE))

ggplot(monthly_net_sales, aes(x=paste(Year, Month, sep="-"), y=monthly_net_sales)) +
  geom_line(color="blue") +
  labs(title="Monthly Net Sales", x="Month-Year", y="Net Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(monthly_total_sales, aes(x=paste(Year, Month, sep="-"), y=monthly_total_sales)) +
  geom_line(color="green") +
  labs(title="Monthly Total Sales After Shipping", x="Month-Year", y="Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Yearly Analysis
yearly_sales <- data %>%
  group_by(Year) %>%
  summarise(total_net_sales = sum(Net.Sales, na.rm = TRUE),
            total_total_sales = sum(Total.Sales, na.rm = TRUE))

ggplot(yearly_sales, aes(x=Year)) +
  geom_bar(aes(y=total_net_sales), stat="identity", fill="blue", alpha=0.5) +
  geom_line(aes(y=total_total_sales * 0.1), color="green", size=1) +
  labs(title="Yearly Net Sales and Total Sales", x="Year", y="Sales") +
  theme_minimal()

# Analyze the impact of discounts and returns
discounts_vs_returns <- data %>%
  summarise(total_discounts = sum(Discounts, na.rm = TRUE),
            total_returns = sum(Returns, na.rm = TRUE),
            total_gross_sales = sum(Gross.Sales, na.rm = TRUE)) %>%
  mutate(discount_impact = total_discounts / total_gross_sales * 100,
         return_impact = total_returns / total_gross_sales * 100)

impact_data <- discounts_vs_returns %>%
  pivot_longer(cols = c(discount_impact, return_impact), names_to = "Type", values_to = "Impact")

ggplot(impact_data, aes(x=Type, y=Impact, fill=Type)) +
  geom_bar(stat="identity") +
  labs(title="Impact of Discounts and Returns on Gross Sales", x="Type", y="Impact (%)") +
  theme_minimal()

# Correlation Analysis
correlations <- cor(data %>% select(Net.Sales, Total.Sales, Discounts, Returns, Gross.Sales))
print(correlations)
