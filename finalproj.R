#packages required for analysis
install.packages("broom")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("dplyr")


#load packages
library(broom)
library(ggpubr)
library(ggplot2)
library(dplyr)

#check that imported data has been read correctly 
summary(deathrate)

#change variables to names that better represent the data

#deathrate <- deathrate %>%   #make changes to var names permanent

#rename( dth_rate = V1,       #death rate per 100,000 residents                    -> dependent var
#        doc_avail = V2,      #doctor availability per 100,000 residents           -> independent var
#        hos_avail = V3,      #hospital availability per 100,000 residents         -> independent var
#        income = V4,         #annual per capita income in thousands of dollars    -> independent var
#        pop_dens = V5)       #population density people per square mile           -> independent var


    
summary(deathrate)

# I will be performing a linear regression using this dataset

# --------- Check if the four main assumptions for multiple linear regression are met  --------- 

# 1) Independence of observations -> test the correlation between independent variables and check to see that they are not 
#   highly correlated. 

#check doc_avail against other independent variables
cor(deathrate$doc_avail, deathrate$hos_avail) # -> 0.2956284      - low correlation (between 0.3-0.5)
cor(deathrate$doc_avail, deathrate$income)    # ->  0.433288      - low correlation
cor(deathrate$doc_avail, deathrate$pop_dens)  # -> -0.01993791    - weak negative correlation (< 0.3)

#check hos_avail against other independent variables
cor(deathrate$hos_avail, deathrate$income)    # -> 0.02750354     - weak correlation (< 0.3)
cor(deathrate$hos_avail, deathrate$pop_dens)  # -> 0.1866163      - weak correlation      (< 0.3)

#check income against other independent variable
cor(deathrate$income, deathrate$pop_dens)     # -> 0.1287437      - weak correlation



# 2) Normality -> Use a histogram to check if the dependent variable (dth_rate) follows a normal distribution

hist(deathrate$dth_rate, 
     main = "Distribution of the variable deathrate", 
     xlab = "dth_rate")

#distribution is bell-shaped, with only one peak and is roughly symmetric around the mean
# we can conclude that the death rate variable is normally distributed 

# 3) Linearity -> Use scatterplots to test linearity for independent variables against dependent variable

plot(dth_rate ~ income, data=deathrate)
plot(dth_rate ~ pop_dens, data=deathrate)
plot(dth_rate ~ doc_avail, data=deathrate)
plot(dth_rate ~ hos_avail, data=deathrate)

#The variable income appears to a clear linear form 




# ---------- Perform Linear Regression Analysis --------------------

#multiple regression
dth_rate.lm<-lm(dth_rate ~ income + pop_dens + doc_avail + hos_avail, data = deathrate)
summary(dth_rate.lm)

# Results of importance: 
# - Estimated effect of income on death rate is -0.33, so for every 1% increase in income
# (annual per capita income in thousands of dollars), there is a correlated 0.33% decrease in death rate
#- The other independent variables have much lower percentages: pop_dens: -0.00946, doc_avail: 0.0073916,
# and hos_avail: 0.0005837


# 4) Check for homoscedasticity -> check that there is not a large variation in the model error
par(mfrow=c(2,2))
plot(dth_rate.lm)
par(mfrow=c(1,1))
# result: mean of residuals (red line) are horizontal and centered around zero (no biases)
# so, the model fits the assumption of homoscedasticity

# ------------------------------------  Visualize the Linear Regression Model with Graphs ------------------------------------
# I will plot an example regression model to visualize the results 

#multiple regression
dth_rate2.lm<-lm(dth_rate ~ income + pop_dens , data = deathrate)
summary(dth_rate2.lm)

# ex) Plotting the relationship between income and death rate at different levels of population density 

plotting.data<-expand.grid(
  income = seq(min(deathrate$income), max(deathrate$income), length.out=30),
  pop_dens=c(min(deathrate$pop_dens), mean(deathrate$pop_dens), max(deathrate$pop_dens))) #3 levels of pop dense

plotting.data$predicted.y <- predict.lm(dth_rate2.lm, newdata=plotting.data) #predict values of death rate based on linear model

plotting.data$pop_dens <- round(plotting.data$pop_dens, digits = 2) #round to 2 decimals

plotting.data$pop_dens <- as.factor(plotting.data$pop_dens) #make pop_dens variable into a factor (to plot at each level)

#plot data
dthrt.plot <- ggplot(deathrate, aes(x=income, y=dth_rate)) +
  geom_point()

dthrt.plot

#add regression lines to the graph 
dthrt.plot <- dthrt.plot +
  geom_line(data=plotting.data, aes(x=income, y=predicted.y, color=pop_dens), linewidth=1.25)

dthrt.plot

dthrt.plot <-
  dthrt.plot +
  theme_bw() +
  labs(title = "Death rate (per 100,000 residents ) as a function of income \nand population density (people per square mile)",
       x = "Income (annual per capita income in thousands of dollars)",
       y = "Death rate (per 100,000 residents)",
       color = "Population Density (people per square mile)")

dthrt.plot




