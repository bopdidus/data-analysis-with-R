#INSTALLATION OF USEFUL LIBRARY

install.packages("dplyr", repos = '"https://cloud.r-project.org')
install.packages('plyr')

#LOADING LIBRARY
library(dplyr)
library(plyr)


#LOAD DATA INTO DATAFRAME

df<- read.csv(file="C:/Users/SoftwareEngineer/Documents/R/csv/RKI_COVID19.csv", header=TRUE, na.strings = c("", "NA"))

View(df)

gender <- group_by(df, Geschlecht)



#DATA CLEANING !False = True, !True = False
is.na(df$AnzahlFall)
sum(is.na(df$AnzahlFall))
c(is.na(df$AnzahlFall))

df<-df[!c(is.na(df$AnzahlFall)),]
sum(is.na(df$AnzahlFall))
View(df)

#***************DESCRIPTIVE STATISTICS*****************

install.packages("psych")
library(psych)

#CALCULATE MEDIAN, MEAN, RANGE, variance and standard deviation


mean(df$AnzahlFall)
summary_by_Gender <- summary(df, mean_by_gender = mean(AnzahlFall)) # FOR NUMBER OF CASE mean = avg  mean (df$AnzahlFall, na.rm=true)
View(summary_by_Gender)

df[, c(3,7)]

median(df$AnzahlFall) 
range(df$AnzahlFall)
var(df$AnzahlFall)
sd(df$AnzahlFall)

describe(df)

summary(df)

#median_by_gender = median(AnzahlFall), 
#range_by_gender = range(AnzahlFall)
#var_by_gender = var(AnzahlFall)
#sd_by_gender = sd(AnzahlFall)

summary_by_Gender1 <- summarise(gender, mean_by_gender = mean(AnzahlFall))
summary_by_Gender1 <- summarise(gender, var_by_gender = var(AnzahlFall))
summary_by_Gender1 <- summarise(gender, mean_by_gender = mean(AnzahlFall), var_by_gender = var(AnzahlFall), sd_by_gender = sd(AnzahlFall))
summary_by_Gender1 <- summarise(gender, mean_by_gender = mean(AnzahlFall), var_by_gender = var(AnzahlFall), sd_by_gender = sd(AnzahlFall))
View(summary_by_Gender1)


#DATA VIZUALISATION

dataset<-df %>% select(Bundesland, AnzahlFall, Geschlecht)

install.packages("lessR")
install.packages("ggplot2")
install.packages("hrbrthemes")
install.packages("viridis")

library(lessR)
library(ggplot2)

library(hrbrthemes)
library(viridis)

df1<- df %>% 
  select(Geschlecht, AnzahlTodesfall, Bundesland ) %>%
  filter( Geschlecht =="M" | Geschlecht=="W") %>%
  group_by(Geschlecht)


BarChart(x=Bundesland, y=AnzahlFall, data=dataset, by=Geschlecht, beside=TRUE,  xlab = "Bundesland", ylab = "Average of infected people", main = "Average of infected people by bundesland and by sex")
BarChart(x=Bundesland, y=AnzahlTodesfall, data=df1, by=Geschlecht, beside=TRUE,  xlab = "Bundesland", ylab = "Average of infected people", main = "Average of dead people by bundesland and by sex")

df<-group_by(df, Meldedatum)

df%>% select(Bundesland, AnzahlFall, Geschlecht, Meldedatum) %>%
      filter(Bundesland == "Hamburg") %>%
      head(20) %>%
    ggplot(aes(x=Meldedatum, y=AnzahlFall, group=Geschlecht, color=Geschlecht)) +
      geom_line() 

#************Inferential statistics: Hypothesis test***************************#


View(df1)

t.test(data=df1, AnzahlTodesfall ~ Geschlecht)

df%>%ggplot(aes(x = Bundesland, y = AnzahlTodesfall, col= Geschlecht)) +geom_point() + facet_wrap(~Bundesland)


