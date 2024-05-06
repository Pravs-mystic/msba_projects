library(psych)
library(dplyr)
library(nnet)
library(stargazer)



rm(list=ls(all=TRUE))


#Import Data
df <- read.csv("psam_p56.csv", header=TRUE)  
cat("\nDataset dimensions\n")
dim(df)
#Removing NAs
df <- df[!is.na(df$WAGP), ]
cat("\nDataset dimensions after removing blank values in dependent variable\n")
dim(df)
#Continuos Variables
df$wage <- as.numeric(df$WAGP)
df$age <- as.numeric(df$AGEP)
df$workingHours <- as.numeric(df$WKHP)

df$educationalAttainment <- as.numeric(df$SCHL)

# categorical (1s and 0s)
df$asian <- df$RACASN
df$white <- df$RACWHT
df$black <- df$RACBLK
df$otherRace <- df$RACSOR

#Creating dummies
df$employed <- ifelse(df$ESR %in% c(1, 2, 4, 5,6), 1, 0) #civilian,armed forces
df$unemployed <- ifelse(df$ESR %in% c(3), 1, 0) #unemployed

df$male <- ifelse(df$SEX == 1, 1, 0)
df$female <- ifelse(df$SEX == 2, 1, 0)

df$married <- ifelse(df$MSP %in% c(1,2), 1, 0)
df$single <- ifelse(df$MSP %in% c(3,4,5,6), 1, 0)

df$disable <- ifelse(df$DIS == 1, 1, 0)
df$able <- ifelse(df$DIS == 2, 1, 0)

df$ScienceEngineering <- ifelse(df$OCCP %in% c("1005", "1006", "1007", "1010", "1021", "1022", "1031", "1032", "1050", "1065", "1105", "1108", "1200", "1220", "1240"), 1, 0)
df$HealthcareSocialservices <- ifelse(df$OCCP %in% c("2012", "2013", "2014", "2015", "2016", "2025", "2040", "2050", "2060", "2100", "2105", "2145", "2170", "2180", "2205", "2300", "2310", "2320", "2330", "2350", "2360", "2400", "2435", "2440", "2545", "2555", "2600", "2631", "2632", "2633", "2634", "2635", "2636", "2640", "2700", "2710", "2721", "2722", "2723", "2740", "2751", "2752", "2755", "2770", "2805", "2810", "2825", "2830", "2840", "902850", "2861", "2862", "2865", "2905", "2910", "2920", "3000", "3010", "3030", "3040", "3050", "3090", "3100", "3110", "3120", "3140", "3150", "3160", "3200", "3210", "3220", "3230", "3245", "3250", "3255", "3256", "3258", "3261", "3270", "3300", "3310", "3321", "3322", "3323", "3324", "3330", "3401", "3402", "3421", "3422", "3423", "3424", "3430", "3500", "3515", "3520", "3545", "3550"), 1, 0)
df$Others <- ifelse(df$ScienceEngineering == 0 & df$HealthcareSocialservices == 0, 1, 0)

#Exploratory Data Analysis

# Drop NA values for continuous variables
df <- df[complete.cases(df$age, df$workingHours, df$educationalAttainment), ]
cat("\nDataset dimensions after removing blank values in continuos variables and adding dummies\n")
dim(df)


# Drop wage=0 for performing logarithmic model
df <- df[df$wage != 0, ]
cat("\nDataset dimensions after removing wage=0 values\n")
dim(df)

# Drop NA values for dummy variables
dummy_vars <- c("asian", "white", "black","employed","unemployed", "otherRace", "male", "female", "married", "single", "disable", "able", "ScienceEngineering", "HealthcareSocialservices", "Others")
df <- df[complete.cases(df[dummy_vars]), ]
cat("\nDataset dimensions after removing blank values in dummy variables\n")
dim(df)

continuous_vars <- c("wage", "age", "workingHours", "educationalAttainment")


# Print summary statistics
summary <- sapply(df[c(dummy_vars,continuous_vars)], function(x) {
  c(mean = mean(x), sd = sd(x), median = median(x), min = min(x), max = max(x), q1 = quantile(x, 0.25), q3 = quantile(x, 0.75))
})
print("Summary Statistics:")
print(summary)

# Calculate correlation matrix for all variables
correlation <- cor(df[c(continuous_vars,dummy_vars)])
print("Correlation Matrix for All Variables:")
print(correlation)

# Find high correlations (> 0.7)
high_correlations <- which(correlation > 0.7 & correlation < 1, arr.ind = TRUE)
if (length(high_correlations) > 0) {
  print("High Correlations (> 0.7):")
  for (i in 1:nrow(high_correlations)) {
    print(paste(row.names(high_correlations)[i], colnames(high_correlations)[i], correlation[high_correlations[i]], sep=": "))
  }
} else {
  print("No high correlations (> 0.7) found.")
}


#Interactive terms
df$male.educationalAttainment <- df$male*df$educationalAttainment

# MODELS

# Taking log of wages as dependent variable
df$log_wage <- log(df$wage)

cat('\n\n...........................................................\n\n')
print ("LOGARITHMIC MODEL - RESTRICTED MODEL WITHOUT CONTROL VARIABLES")
model1 <- lm(log_wage ~ educationalAttainment + male + male.educationalAttainment, data = df)

# Summarize the model
summary(model1)


cat('\n\n...........................................................\n\n')
print ("LOGARITHMIC MODEL - UNRESTRICTED MODEL WITH ADDITIONAL CONTROL VARIABLES")
model2 <- lm(log_wage ~ age + workingHours + employed+ educationalAttainment + male + married + able + 
               ScienceEngineering + HealthcareSocialservices + asian + 
               white + black + male.educationalAttainment,
             data = df)

# Summarize the model
summary(model2)

cat('\n\n...........................................................\n\n')
print ("LOGARITHMIC MODEL - RESTRICTED MODEL WITH CONTROL VARIABLES (MAIN MODEL)")
model3 <- lm(log_wage ~ workingHours + employed + educationalAttainment + male + married +  
               ScienceEngineering + HealthcareSocialservices + male.educationalAttainment, 
             data = df)

# Summarize the model
summary(model3)

stargazer(attitude)
stargazer(model1,model2,model3,type='text',title='Regression Results',digits=2,star.cutoffs=c(0.05,0.01,0.001),align=TRUE)


##Plots

# Set smaller margins
par(mar = c(5, 5, 2, 2))  # c(bottom, left, top, right)

# Scatter plot between educational attainment and wage
plot(df$educationalAttainment, log(df$wage), 
     xlab = "Educational Attainment", ylab = "Log(Wage)", 
     main = "Scatter Plot of Educational Attainment vs. Log(Wage)")

cat("\nThe scatterplot for Educational Attainment vs. Wage shows exponential growth and hence we should consider a logarithmic model\n")


# Scatter plot between educational attainment and age
plot(df$age, df$educationalAttainment, 
     xlab = "Age", ylab = "Educational Attainment", 
     main = "Scatter Plot of Educational Attainment vs. Age")

hist(scale(df$wage), main = "Histogram of wage", xlab = "wage (in $k)", ylab = "Frequency", cex.main = 1, cex.lab = 1, border = "black")
boxplot(scale(wage) ~ age, data = df, main = "Box plot of wage by Age", xlab = "Age", ylab = "wage (in $k)", cex.main = 1, cex.lab = 1)
plot(scale(wage) ~ workingHours, data = df, main = "Scatter plot of wage against Working Hours", xlab = "Working Hours", ylab = "wage (in $k)", cex.main = 1, cex.lab = 1)

#Histograms for Quantitative Variables:

par(mfrow=c(2,1))
hist(df$age, main = "Histogram of Age", xlab = "Age", ylab = "Frequency", col = "skyblue", border = "black", ylim = c(0, 400))
axis(side = 2, at = seq(0, 300, by = 50)) # Adding additional frequency values
hist(df$workingHours, main = "Histogram of Working Hours", xlab = "Working Hours(Hours)", ylab = "Frequency", col = "lightgreen", border = "black", ylim = c(0, 400))
axis(side = 2, at = seq(0, 300, by = 50)) # Adding additional frequency values

#Bar Charts  for Qualitative Variables :

par(mar = c(5, 5, 2, 2))
barplot(table(df$employed), main = "Bar Chart of Employment Status", xlab = "Employed", ylab = "Frequency", col = "lightcoral")
barplot(table(df$educationalAttainment), main = "Bar Chart of Educational Attainment", xlab = "Educational Attainment", ylab = "Frequency", col = "lightyellow")

#Scatterplot:

scaled_wage <- scale(df$wage)
plot(df$age, scaled_wage, main = "Scatter Plot of wage against Age", xlab = "Age", ylab = "wage(in USD)", col = "blue")

# Cross-tabulation between employed and educationalAttainment
xt_emp_edu <- table(df$employed, df$educationalAttainment)
print(xt_emp_edu)

# Corr-Coeff
cor(df[c("age", "workingHours", "employed", "educationalAttainment", "wage")])
