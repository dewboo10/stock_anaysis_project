# stock_anaysis_project
This is simple and short project of data analysis of 4 stocks movement (language R)
---
title: "Stock Analysis Report"
author: "By Devanshu Saxena"
date: "`r Sys.Date()`"
output: html_document
---

## **Introduction**
This report provides insights into stock price movements using **data visualization and statistical summaries**.  

### **Objectives**  
1. Load required libraries for data visualization and manipulation.  
2. Read and clean the stock dataset.  
3. Generate statistical summaries (min, max, mean, median).  
4. Visualize overall stock trends and individual stock movements.  
5. Use LOESS smoothing to observe price trends for selected stocks.  

---

### **1. Load Libraries**
*We are using `ggplot2` for visualization and `tidyverse` for data handling.*

```{r setup, echo=TRUE, message=FALSE, warning=FALSE}
# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)

# Display a message to indicate successful loading
cat("Libraries loaded successfully!\n")
# Import data from CSV file
allstocks <- read.csv("C://Users//dewboo//Downloads//stocks.csv")


# Display column names
cat("### Column Names in Dataset:\n")
print(colnames(allstocks))

# Convert 'Date' column to Date format
allstocks$Date <- as.Date(allstocks$Date)

# Display structure of dataset
cat("### Data Structure:\n")
str(allstocks)

# Show the count of stocks for each Ticker
cat("### Stock Count by Ticker:\n")
print(table(allstocks$Ticker))
# Compute min, mean, max, and median for each stock
stats <- allstocks %>%
  group_by(Ticker) %>%
  summarise(
    Min_Close = min(Close, na.rm = TRUE),
    Mean_Close = mean(Close, na.rm = TRUE),
    Max_Close = max(Close, na.rm = TRUE),
    Median_Close = median(Close, na.rm = TRUE)
  )

# Display the result in a nicely formatted table
cat("### Summary Statistics for Each Stock:\n")
stats %>%
  kable(caption = "Stock Summary Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"),
                full_width = FALSE) %>%
  column_spec(1, bold = TRUE, color = "white", background = "skyblue") %>%
  row_spec(0, bold = TRUE)
cat("### Overall Stock Closing Prices:\n")

# Line plot of stock movements
ggplot(data = allstocks, aes(x = Date , y = Close , color = Ticker)) +
  geom_line() +
  labs(title = "Stock Closing Prices", x = "Date", y = "Closing Price") +
  theme_minimal()
cat("### Individual Stock Closing Prices:\n")

ggplot(data = allstocks, aes(x = Date , y = Close , color = Ticker)) +
  geom_line() +
  facet_wrap(~Ticker, scales = "free_y") +  
  labs(title = "Stock Closing Prices (Individual)", x = "Date", y = "Closing Price") +
  theme_minimal()
cat("### LOESS Smoothed Trend Lines for AAPL, GOOG, MSFT, and NFLX:\n")

stocks_to_plot <- c("AAPL", "GOOG", "MSFT", "NFLX")

for (stock in stocks_to_plot) {
  cat(paste("####", stock, "Stock Price with Trend Line\n"))
  
  print(
    ggplot(data = allstocks %>% filter(Ticker == stock), aes(x = Date, y = Close)) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = paste(stock, "Stock Price with Trend Line"), x = "Date", y = "Closing Price") +
      theme_minimal()
  )
}
