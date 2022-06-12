#Data Source: https//www.kaggle.com/tunguz/data-on-covid19-coronavirus


library(data.table)
library(funModeling)
library(Hmisc)
library(ggplot2)


#######################
## Loading the data ###
#######################
df <- fread("owid-covid-data.csv")

#######################
## Viewing Data #######
#######################
View(df)
str(df)


# 1. Begin Profiling
df_status(df)


# 2. Profiling categorical variables
freq(df)


# 3. Profiling numerical variables 
plot_num(df)

##############################
## Choose data of interest ###
##############################
sa <- df[location == 'South Africa']

# Selecting specific columns

sa <- sa[,.(iso_code, location, date, total_vaccinations, people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred)]
sa <- sa[!is.na(total_vaccinations)]
sa <- sa[complete.cases(sa)]

#####################################################################
#Plotting the graph, the rate of vaccinated people in South Africa ##
#####################################################################

pl <- ggplot(data = sa, aes(x = date, y = total_vaccinations, color = location))
pl + geom_line(size=1.2) + #scale_y_continuous(breaks = c(0,20,40,60), labels = scales::comma) +
  labs(title = 'total number of doses distributed in South Africa', x = 'Date', y = '% Vaccinated people')

#######################################################
# Comparing it to at least Three countries in Europe ##
#######################################################

pl <- ggplot(data = df[location %in% c("South Africa","United Kingdom", "France", "Russia") & !is.na(total_vaccinations)], aes(x = date, y = total_vaccinations, color = location))
pl + geom_line(size=1.2) + #scale_y_continuous(breaks = c(0,20,40,60), labels = scales::comma) +
  labs(title = 'total number of doses distributed in the 4 countires', x = 'Date', y = '% Vaccinated people')


######################################################################
## Conclusion: South Africa had the lowest total number of doses     #
## distributed from January 2021 till present, When compared to      #
## the United Kingdom, France, and Russia                            #
######################################################################



####################################### THE END ##############################################################

