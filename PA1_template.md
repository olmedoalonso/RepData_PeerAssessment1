---
title: "Reproducible Research Week 2 Course Project 1"
author: "Olmedo Alonso Madrigales"
date: "11/09/2020"
output: html_document
---



# Course Project 1

## Introduction

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. The data 
consists of two months of data from an anonymous individual collected during the 
months of October and November, 2012 and include the number of steps taken in 
5 minute intervals each day. The data for this assignment was provided from the 
course web site and the Professor R. Peng´s repository which I forked to my personal 
computer. 

I think is important in this moment, recall the packages that are going to be needed.
Here I detail the library calls I would need. I do it before anything else.

```{r libraries, echo=TRUE, results='hide', message=FALSE, warning=FALSE}
library(dplyr)
library(tidyr)
library(httr)
library(RCurl)
library(data.table)
library(lubridate)
library(ggplot2)
library(knitr)
```

## Assignment

This assignment is described in multiple parts. I write this report with the answers to the questions detailed below. I include the codes used to generate the output I present. Also, I am including the code chunks in the document with echo = TRUE so that my peer evaluators are able to review the code for the analysis.

The assignment consists of the following 5 tasks.

### 1. Loading and preprocessing the data

I am departing from the point where the file is already downloaded in my directory 
as a csv file. The code unzip directly the "activity" file. 
In this part, I also changed the format to the "date" variable as well as the "steps" to numeric.

```{r filing, echo=TRUE, message=FALSE, warning=FALSE}
if (!file.exists("activity.csv") ) {
   unzip("activity.zip")
}
Activity <- read.csv("activity.csv", na.strings = TRUE)
Activity$date <- as.Date(Activity$date, format = "%Y-%m-%d")
Activity$steps <- as.numeric(Activity$steps)
```

Then I explore the data with functions ‘str’, ‘head’, ‘summary’ (for each column) in order to get acquainted with it.

### 2. What is mean total number of steps taken per day?

I calculated the total number of steps taken per day as a data frame with two columns
"date" and "total.steps"

```{r calculation, echo=TRUE, results='hide', message=FALSE, warning=FALSE}
Activity_Steps_Total <- Activity %>% filter(!steps =="NA") %>% group_by(date) %>% summarise(total.steps = sum(steps))
print(Activity_Steps_Total)
```

Then I made a histogram with the frequency of the total amounts of steps.

```{r plotting, echo=TRUE, fig.align='center'}
hist(Activity_Steps_Total$total.steps, main = "Histogram of Total Activity Steps",
     xlab = "Step Amounts", col = c("Yellow","Green", "Blue", "Red", "Black"))
        abline(v = mean(Activity_Steps_Total$total.steps), col = "Red", lwd = 2)
        abline(v = median(Activity_Steps_Total$total.steps), col = "Black", lwd = 1)
```

I also calculated the mean and the median for this data frame.

```{r calculating mean, echo=TRUE}
Activity_mean <- mean(Activity_Steps_Total$total.steps)
print(Activity_mean)
```

```{r calculating median, echo=TRUE}
Activity_median <- median(Activity_Steps_Total$total.steps)
print(Activity_median)
```

### 3. What is the average daily activity pattern?

In this point, I calculated the mean for the daily activities with the following code.

```{r activity mean, echo=TRUE, results='hide', message=FALSE, warning=FALSE}
Activity_Interval_Mean <- Activity %>% filter(!steps =="NA") %>% group_by(interval)%>%
  summarise(max.avg.steps = mean(steps))
print(Activity_Interval_Mean)
```

Then I plotted the data with the following code.  In this I made a time series plot type
= "l", of the 5-minute interval (x-axis) and the average number of steps taken, averaged
across all days (y-axis)

```{r plotting avg, echo=TRUE, fig.dim=c(8,6), fig.align='center'}
max_avg_interval <- qplot(interval, max.avg.steps, data = Activity_Interval_Mean) + 
  geom_line(colour = "blue") + geom_point(colour = "blue") + 
  ggtitle("MAX AVERAGE NUMBER OF STEPS TAKEN") + xlab("Intervals") + 
  ylab("Avg. Steps Taken") + theme(plot.title = element_text(hjust = 0.5))
print(max_avg_interval)
```

I also got the 5-minute interval that on average across all the days in the dataset,
contains the maximum number of steps.  I did this with the following code.

```{r Activity Interval Mean, echo=TRUE, message=FALSE, warning=FALSE}
Activity_Interval_Mean_Max <- Activity %>% filter(!steps =="NA") %>% 
  group_by(interval) %>% summarise(max.avg.steps = mean(steps)) %>% 
  arrange(desc(max.avg.steps)) %>% head(1)
print(Activity_Interval_Mean_Max)
```

### 4. Imputing missing values

It is important to mention that there are a number of days/intervals where there are missing values (NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

I report in this part the total number of missing values in this dataset that is the total number of rows with NAs.  After reviewing the dataset with the summary function, I concluded that all the NAs where in only one row.  I show it with the following code.

```{r imputing NA, echo=TRUE}
Activity_Nas <- sum(is.na(Activity$steps))
print(Activity_Nas)
```

I deviced a strategy for filling in all of the missing values in the dataset. I verify
the amount of NA´s by date and interval.  Looking at the data I could understand that
there are 8 NA´s per interval and 288 NA´s per date. So, I decide to impute the NA´s by
the **mean** for that interval.  I do this because I could have more data available for the intervals regardless of the date.  

I put this imputed data in a new data frame called Activity_New.  The code follows.

```{r activity new, echo=TRUE, results='hide', message=FALSE, warning=FALSE}
Activity_New <- Activity %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

Then I calculated the total number of steps taken each day and put it in a new data 
set called Activity_New_Steps_Total and changed the variable "step" to a numeric.  See 
the code below.

```{r activity new steps, echo=TRUE, results='hide', message=FALSE, warning=FALSE}
Activity_New_Steps_Total <- Activity_New %>% group_by(date) %>% summarise(total.steps = sum(steps))
Activity_New_Steps_Total$total.steps <- as.numeric(Activity_New_Steps_Total$total.steps)
```

I also made the histogram with the new frequencies of total number of steps after imputing.  See the code and plot below.

```{r plotting new, echo=TRUE, fig.dim= c(6,4), fig.align='center'}
hist(Activity_New_Steps_Total$total.steps, main = "Histogram of New Activity Steps", xlab = "Step Amounts", col = c("Yellow","Green", "Blue", "Red", "Black"))
    abline(v = mean(Activity_New_Steps_Total$total.steps), col = "Red", lwd = 2)
    abline(v = median(Activity_New_Steps_Total$total.steps), col = "Black", lwd = 1)
```

Then I calculated the new mean and median of the total number of steps after imputing.

```{r new mean, echo=TRUE}
Activity_New_Mean <- mean(Activity_New_Steps_Total$total.steps)
    print(Activity_New_Mean)
Activity_New_Median <- median(Activity_New_Steps_Total$total.steps)
    print(Activity_New_Median)
```

As I can see, even though I placed new figures, the results are almost the same as 
before filling NA values. The median actually changed a bit and became basically the 
same as to the mean.

### 5. Are there differences in activity patterns between weekdays and weekends?

I have created a new factor variable in the original data set called "day" with two levels,  "weekday" and "weekend", indicating whether a given date is a weekday or weekend day.  I had to use words in Spanish for Saturday and Sunday. See the code as follows.

```{r new activity, echo=TRUE, results='hide'}
Activity_New$day <- ifelse(weekdays(Activity_New$date) %in% c("sábado","domingo"), "weekend", "weekday") %>% factor()
```

Finally, I arranged the data in a new data set in order to make a panel plot containing a time series plot, type = "l", of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r new interval mean, echo=TRUE, results='hide'}
Activity_New_Interval_Mean_Day <- aggregate(steps ~ interval + day, data = Activity_New, mean)
```

```{r plottin wkd vs wkn, echo=TRUE, fig.dim= c(6,6), fig.align='center'}
weekdays_vs_weekends <- ggplot(Activity_New_Interval_Mean_Day, aes(interval, steps)) +  geom_line(colour = "blue") + facet_grid(day ~.) + ggtitle("AVERAGE ACTIVITY PATTERN") + xlab("Intervals") + ylab("Avg Steps Taken") + theme(plot.title = element_text(hjust = 0.5))
print(weekdays_vs_weekends)
```

So in this plot, it looks like the number of steps on weekends are higher on average during the day, but the peak in the morning is higher on weekdays.

