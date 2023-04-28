install.packages('rvest')
install.packages('dplyr')
installed.packages('xml2')
install.packages("tidyverse")

library(rvest)
library(dplyr)
library(xml2)
library(tidyverse)

content <- read_html("https://www.patriotsoftware.com/blog/accounting/average-cost-living-by-state/")

table <- content %>% html_table(fill = TRUE)
first_table <- table[[1]]

View(first_table)

write.csv(first_table, file = "T5_dataset.csv")

first_table = read.csv("T5_dataset.csv")

#renaming tables

names(first_table)
names(first_table)[1] <- "Index"
names(first_table)[2] <- "States"
names(first_table)[3] <- "Annual_mean_wage"
names(first_table)[4] <- "Average_monthly_rent"
names(first_table)[5] <- "Value_of_$100"
names(first_table)[6] <- "X"

#delete column with NA value
df = subset(first_table, select = -c(X))

#checking where is the missing value as NA


is.na(df[,1:5])

#delete NA row

df <- na.omit(df)


View(df)

library(dplyr)
df<-df %>% mutate(Type =
                            case_when(Annual_mean_wage < 40000 ~ "Poor wage",
                                      Annual_mean_wage < 59999 ~ "Good wage ",
                                      Annual_mean_wage >= 60050 ~ "Excellent wage"))

#mean

mean(df$Annual_mean_wage)

mean(df$Average_monthly_rent)

mean(df$`Value_of_$100`)

#median

median(df$Annual_mean_wage)

median(df$Average_monthly_rent)

median(df$`Value_of_$100`)

#Range

max(df$Annual_mean_wage)-min(df$Annual_mean_wage)

max(df$Average_monthly_rent)-min(df$Average_monthly_rent)

max(df$`Value_of_$100`)-min(df$`Value_of_$100`)

#variance

var(df$Annual_mean_wage)
var(df$Average_monthly_rent)
var(df$`Value_of_$100`)

#standard deviation

sd(df$Annual_mean_wage)
sd(df$Average_monthly_rent)
sd(df$`Value_of_$100`)

#Quartiles

quantile(df$Annual_mean_wage)
quantile(df$Average_monthly_rent)
quantile(df$`Value_of_$100`)

#InterQuartile
IQR(df$Annual_mean_wage)
IQR(df$Average_monthly_rent)
IQR(df$`Value_of_$100`)


install.packages(c("mosaicData","ggplot2"))


library(ggplot2)

library(mosaicData)
#states_wage
ggplot(df)+geom_bar(aes(x = df$States , y = df$Annual_mean_wage ),stat="identity" ,fill = "purple")+
  labs(title = "U.S. cost of living comparison by state",caption = "Data source: www.patriotsoftware.com") + coord_flip()

#States_rent
ggplot(df)+geom_bar(aes(x = df$States , y = df$Average_monthly_rent ),stat="identity" ,fill = "blue")+
  labs(title = "U.S. cost of living comparison by state",caption = "Data source: www.patriotsoftware.com") + coord_flip()

#States_valueOf_$100
ggplot(df)+geom_bar(aes(x = df$States , y = df$`Value_of_$100` ),stat="identity" ,fill = "#5F4B8BFF")+
  labs(title = "U.S. cost of living comparison by state",caption = "Data source: www.patriotsoftware.com") + coord_flip()
