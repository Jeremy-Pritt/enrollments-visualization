#### read in the data
totals <- read.csv("totals.csv")

# import libraries
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(plotly)

#### convert data to proper data types
totals$Enrollments <- as.numeric(gsub(",", "", totals$Enrollments))
totals$Revenue <- as.numeric(gsub(",", "", totals$Revenue))
totals$Month <- as.Date(totals$Month, "%m/%d/%Y")

#### create a percent-increase column for enrollemnts to add to the dataframe

# create an empty vector to populate with values

class(totals$Enrollments)

result_vec <- rep(NA, times = length(totals$Enrollments))

for (i in 1:length(totals$Enrollments)) {
  if (i == 1) {
    result_vec[i] <- 0
  }
  else {
    result_vec[i] <- ((totals$Enrollments[i] - totals$Enrollments[i-1]) / totals$Enrollments[i-1]) * 100
  }
}

# attach the new vector to the df
totals$percent_change_enrollments <- result_vec


##### create a percent-increase column for revenue to add to the dataframe

# create an empty vector to populate with values

class(totals$Revenue)

result_vec <- rep(NA, times = length(totals$Revenue))

for (i in 1:length(totals$Revenue)) {
  if (i == 1) {
    result_vec[i] <- 0
  }
  else {
    result_vec[i] <- ((totals$Revenue[i] - totals$Revenue[i-1]) / totals$Revenue[i-1]) * 100
  }
}

# attach the new vector to the df
totals$percent_change_rev <- result_vec



##### subset the totals to show only observations starting this year
totals_cleaned <- totals[totals$Month >= "2022-01-01", ]

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug")

##### visualize the data with ggplot2
ggplotly(ggplot(totals_cleaned, aes(x=Month, y=percent_change_enrollments)) +
  geom_line(aes(x=Month, y=percent_change_enrollments), color="#0D5992", size=4) + 
  geom_point(aes(x=Month, y=percent_change_enrollments), color="grey", fill="grey", size=9) +
  labs(title="Enrollments - 2022", x="Month", y="Percentage Change") +
  theme_bw() +
  theme_ipsum() + 
  theme(plot.title=element_text(size=30, face="bold", hjust=0.5), 
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=15, face="bold", hjust=0.5),
        axis.title.y = element_text(size=15, face="bold", hjust=0.5)) +
  scale_x_continuous(labels=month_names, breaks=totals_cleaned$Month)
)



