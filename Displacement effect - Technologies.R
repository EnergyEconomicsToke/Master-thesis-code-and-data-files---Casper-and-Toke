rm(list=ls())
setwd("C:/Users/toket/OneDrive - University of Copenhagen/Master ENRE/Master - ENRE/Master Thesis/Datasets/R - Model")

 
install.packages("readxl")
library(readxl)

#install.packages("plm")
#install.packages("dplyr")
#install.packages("broom")
# Install necessary packages if not installed
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("fixest")
#install.packages("corrplot")

# Load libraries
library(plm)
library(dplyr)
library(broom)
library(ggplot2)
library(readxl)
library(dplyr)
library(fixest)
library(car)
library(corrplot)

library(plm)
library(lmtest)
library(sandwich)

#load dataset for electricity, heat and control variables (population, GDP, Urban, Industry...)

data1 <- read_xlsx("Eurostat Datasets - Gross Electricity and Gross Heat Production.xlsx", sheet = "Gross Electricity kWh")

data2 <- read_xlsx("Eurostat Datasets - Gross Electricity and Gross Heat Production.xlsx", sheet = "Gross Heat kWh")


#Create same Model 1 as in Richard York 2012. Define per capita variables. 

#just a check to see if data is loaded correctly

mean(data1$Wind, na.rm = TRUE)

# Assuming your dataset is in 'data1'
mean_total_denmark <- mean(data1$Total[data1$Country == "Denmark"], na.rm = TRUE)

# Print the result
print(mean_total_denmark)

summary(data1)

#Check correlations

numeric_data <- data1[, c("Wind", "Hydro","Nuclear_heat", "Bioenergy","Fossil_energy","Net_imports","Renewables_and_biofuels")]
cor(numeric_data)


#regression model check 

library(plm) 
library(sandwich)
library(lmtest)

#create variables electricity per capita, GDP^2 and GDP^3



model1_york <- plm(Fossil_energy ~ Wind + Hydro + Bioenergy + GDP_per_capita + Population, data=data1, index = c("Country", "TIME"))


# Print the summary of the model
summary(model1_york)


modelPrais_Winston <- feols(Fossil_energy ~ Wind + Hydro + Bioenergy + GDP_per_capita + Population + factor(TIME) | Country, 
                     data = data1, 
                     autocor = 1)  # Corrects for AR(1) autocorrelation

# View Results
summary(modelPrais_Winston)

length(modelPrais_Winston)

#### Create per capita variables


#Replace rows with NA values with 0
data1[is.na(data1)] <- 0

# Manually create per capita variables for all relevant variables

# Add new variables to the data1 dataset
data1$Fossil_energy_per_capita <- data1$Fossil_energy / data1$Population
data1$Solid_fossil_fuels_per_capita <- data1$Solid_fossil_fuels / data1$Population
data1$Oil_per_capita <- data1$Oil_and_petroleum_products / data1$Population
data1$natural_gas_per_capita <- data1$Natural_gas / data1$Population
data1$Hydro_per_capita <- data1$Hydro / data1$Population
data1$geothermal_per_capita <- data1$Geothermal / data1$Population
data1$Wind_per_capita <- data1$Wind / data1$Population
data1$Solar_thermal_per_capita <- data1$Solar_thermal / data1$Population
data1$Solar_photovoltaic_per_capita <- data1$Solar_photovoltaic / data1$Population
data1$Industrial_waste_non_renewable_per_capita <- data1$`Industrial_waste_(non-renewable)` / data1$Population
data1$Bioenergy_per_capita <- data1$Bioenergy / data1$Population
data1$Nuclear_heat_per_capita <- data1$Nuclear_heat / data1$Population
data1$Renewables_per_capita <- data1$Renewables_and_biofuels / data1$Population
data1$Netimports_per_capita <- data1$Net_imports / data1$Population

# Create GDP per capita squared and add to dataset
data1$GDP_per_capita_sq <- data1$GDP_per_capita^2

# Create GDP per capita cubed and add to dataset
data1$GDP_per_capita_cub <- data1$GDP_per_capita^3

# Create log GDP and add to dataset
data1$ln_GDP_per_capita <- log(data1$GDP_per_capita)

# Create LN GDP per capita squared and add to dataset
data1$ln_GDP_per_capita_sq <- log(data1$GDP_per_capita)^2

# Create GDP per capita cubed and add to dataset
data1$ln_GDP_per_capita_cub <- log(data1$GDP_per_capita)^3




# Create squared Urbanisation and add to dataset
data1$Urban_sq <- data1$Urban_Population^2

# Create variable for fossil fuels (oil + gas + coal) and add to dataset
data1$fossil_fuels_per_capita <- (data1$Solid_fossil_fuels + data1$Oil_and_petroleum_products + data1$Natural_gas) / data1$Population

# Create variable for fossil fuels (oil coal) and add to dataset
data1$oil_coal_per_capita <- (data1$Solid_fossil_fuels + data1$Oil_and_petroleum_products) / data1$Population

######COMBINED VARIABLES

data1$Wind_Solar_combined <- data1$Wind_per_capita + data1$Solar_photovoltaic_per_capita
data1$Wind_Solar_Hydro_combined <- data1$Wind_per_capita + data1$Solar_photovoltaic_per_capita + data1$Hydro_per_capita
data1$Non_hydro <- data1$Wind_per_capita + data1$Bioenergy_per_capita + data1$Solar_photovoltaic_per_capita

data1$Intermittent <- data1$Wind_per_capita + data1$Solar_photovoltaic_per_capita
data1$Dispatchable <- data1$Bioenergy_per_capita + data1$Hydro_per_capita



numeric_data <- data1[, c("Fossil_energy_per_capita","Renewables_per_capita","Netimports_per_capita","ln_GDP_per_capita","Industry","Age_dependency_ratio","Natural_gas_price","Coal_price","Electricity_kWh_per_capita")]
cor(numeric_data)




####correlation matrix ################################################

# All predictors (excluding the dependent variable Fossil_energy_per_capita)
predictors <- c("Fossil_energy_per_capita","Renewables_per_capita",
  "Hydro_per_capita",
  "Wind_per_capita", 
 "Solar_photovoltaic_per_capita", "Wind_Solar_combined",
 "Bioenergy_per_capita", 
  "Nuclear_heat_per_capita", 
 "Age_dependency_ratio", 
 #"HDD", 
 "ln_GDP_per_capita", 
 "GDP_per_capita",
 #"GDP_per_capita_sq",
 #"GDP_per_capita_sq",
 "Urban_Population",
 #"Urban_sq"
 "Manufacturing",
 "Industry",
"Net_imports"
#"Coal_price",
#"Natural_gas_price"
)

# Create subset data frame with all predictors
X_data <- data1[, predictors]

# Compute correlation matrix
cor_matrix <- cor(X_data, use = "pairwise.complete.obs")

# Plot correlation heatmap

# Plot upper triangle of the correlation matrix with enhancements
corrplot(cor_matrix, method = "color", 
         type = "upper",               # Only show upper triangle
         #order = "hclust",             # Cluster similar variables
         tl.cex = 0.8,                 # Label size
         number.cex = 0.7,             # Number size
         number.digits = 2,           # Round to 2 digits
         addCoef.col = "black",       # Add correlation numbers
         col = colorRampPalette(c("blue", "white", "red"))(200)) 

# Linear model with Fossil_energy_per_capita as dependent variable
vif_model <- lm(Fossil_energy_per_capita ~ ., data = data1[, c("Fossil_energy_per_capita", predictors)])

# Calculate and print VIF values
vif_values <- vif(vif_model)
print(vif_values)


cor(data1[, c("Fossil_energy_per_capita", predictors)], use = "pairwise.complete.obs")



# Exclude Denmark and Norway from the data
data_no_norway_denmark <- data1[data1$Country != "Denmark" & data1$Country != "Norway", ]

# Subset data for predictors
X_data_no_norway_denmark <- data_no_norway_denmark[, predictors]

# Compute correlation matrix excluding Denmark and Norway
cor_matrix_no_norway_denmark <- cor(X_data_no_norway_denmark, use = "pairwise.complete.obs")

# Plot correlation heatmap for the remaining countries
corrplot(cor_matrix_no_norway_denmark, method = "color", tl.cex = 0.8, number.cex = 0.7)

# Linear model with Fossil_energy_per_capita as dependent variable
vif_model_no_norway_denmark <- lm(Fossil_energy_per_capita ~ ., data = data_no_norway_denmark[, c("Fossil_energy_per_capita", predictors)])

# Calculate and print VIF values excluding Denmark and Norway
vif_values_no_norway_denmark <- vif(vif_model_no_norway_denmark)
print(vif_values_no_norway_denmark)


# All predictors (excluding the dependent variable Fossil_energy_per_capita)
predictors_no_hydro <- c(
  #"Hydro_per_capita",
  "Wind_per_capita", 
  "Solar_photovoltaic_per_capita", 
  "Bioenergy_per_capita", 
  "Nuclear_heat_per_capita", 
  "Age_dependency_ratio", 
  "HDD", 
  "ln_GDP_per_capita", 
  #"Urban_Population",
  #"Manufacturing",
  "Industry",
  "Net_imports"
)

# Create subset data frame with all predictors
X_data_no_hydro <- data1[, predictors_no_hydro]

# Compute correlation matrix
cor_matrix_no_hydro <- cor(X_data_no_hydro, use = "pairwise.complete.obs")

# Plot correlation heatmap
corrplot(cor_matrix_no_hydro, method = "color", tl.cex = 0.8, number.cex = 0.7)


# Linear model with Fossil_energy_per_capita as dependent variable
vif_model <- lm(Fossil_energy_per_capita ~ ., data = data1[, c("Fossil_energy_per_capita", predictors_no_hydro)])

# Calculate and print VIF values
vif_values <- vif(vif_model)
print(vif_values)



#More correlations...

predictors1 <- c(
  "Renewables_per_capita", "Age_dependency_ratio", 
  #"HDD", 
  #"ln_GDP_per_capita",
  "GDP_per_capita",
  #"Manufacturing", 
  "Industry", 
  #"Urban_Population",
  "Net_imports", "Electricity_kWh_per_capita"
)

# Create subset data frame with all predictors
X_data1 <- data1[, predictors1]

# Compute correlation matrix
cor_matrix1 <- cor(X_data1, use = "pairwise.complete.obs")

# Plot correlation heatmap
corrplot(cor_matrix1, method = "color", tl.cex = 0.8, number.cex = 0.7)


# Linear model with Fossil_energy_per_capita as dependent variable
vif_model1 <- lm(Fossil_energy_per_capita ~ ., data = data1[, c("Fossil_energy_per_capita", predictors1)])

# Calculate and print VIF values
vif_values1 <- vif(vif_model1)
print(vif_values1)






####add country and time specific effects ###############
##### YORK PAPER

model1_york <- plm(Fossil_energy_per_capita ~ GDP_per_capita+Renewables_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model1_york)

# Estimate the random effects model
model1_york_re <- plm(Fossil_energy_per_capita ~ GDP_per_capita + Renewables_per_capita, 
                      data = data1, index = c("Country", "TIME"), 
                      model = "random", effect = "twoways")

# Print summary
summary(model1_york_re)

# Perform Hausman test
hausman_test <- phtest(model1_york, model1_york_re)

# Print results
print(hausman_test)

#Final Choice: Fixed effects model is preferred. 


model1_york_Prais <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + GDP_per_capita+ factor(TIME),
                          data = data1, 
                          model = "within", 
                          effect = "individual", 
                          index = c("Country", "TIME"),
                          method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_york_Prais)


# Extract residuals from the model
residuals <- residuals(model1_york_Prais)


# Adjust plot margins before plotting
par(mar = c(5, 5, 2, 2))  # Adjust margins (bottom, left, top, right)
# Check for autocorrelation in the residuals
windows(width = 7, height = 7)  # This opens a new window with a set width and height for the plot

acf(residuals)
plot(acf(residuals))  # Explicitly plot the ACF

# Plot a histogram of residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "skyblue", border = "black")

shapiro_test <- shapiro.test(residuals)
shapiro_test

#p-value=0.6038 
#If the p-value from the Shapiro-Wilk test is less than 0.05, you would reject the null hypothesis and conclude that the residuals are not normally distributed.

#If the p-value is greater than 0.05, it suggests that the residuals are approximately normally distributed.

# Load the lmtest package
library(lmtest)

# Perform Durbin-Watson test on the residuals
dw_test <- dwtest(model1_york_Prais)
dw_test

bg_test <- bgtest(model1_york_Prais)
bg_test

#To assess stability, try running the regression on sub-samples of the data. If the coefficients vary significantly across these sub-samples, it likely indicates instability due to the small sample size.

# Check the length of the key variables
length(data1$TIME)
length(data1$Fossil_energy_per_capita)
length(data1$Population)
length(data1$Renewables_per_capita)

# Splitting data by 10-year intervals

data_subsample_1990_2000 <- subset(data1, TIME >= 1990 & TIME <= 2000)
data_subsample_2001_2010 <- subset(data1, TIME >= 2001 & TIME <= 2010)
data_subsample_2011_2020 <- subset(data1, TIME >= 2011 & TIME <= 2020)

# You can continue the same for subsequent decades if needed.

# Example: Running the model on each sub-sample
model_1990_2000 <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + GDP_per_capita + factor(TIME),
                       data = data_subsample_1990_2000, model = "within", effect = "individual", index = c("Country", "TIME"), method = "prais")

model_2001_2010 <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + GDP_per_capita + factor(TIME),
                       data = data_subsample_2001_2010, model = "within", effect = "individual", index = c("Country", "TIME"), method = "prais")

model_2011_2020 <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + GDP_per_capita + factor(TIME),
                       data = data_subsample_2011_2020, model = "within", effect = "individual", index = c("Country", "TIME"), method = "prais")
# Summary of each model
summary(model_1990_2000)
summary(model_2001_2010)
summary(model_2011_2020)


coeftest(model_1990_2000, vcov = vcovHC(model_1990_2000, type = "HC1"))
coeftest(model_2001_2010, vcov = vcovHC(model_2001_2010, type = "HC1"))

# Compare using a Wald test or F-test
waldtest(model1_york_Prais,model_2001_2010)  # Or use a custom test with the 'linearHypothesis()' function


library(car)

# Create an interaction model
data1$TIME <- as.numeric(as.character(data1$TIME))
data1$Period <- cut(data1$TIME, breaks = c(1990, 2000, 2010, 2023), labels = c("P1", "P2", "P3"))


# Test if coefficients are equal across periods

linearHypothesis(model_interaction, c("GDP_per_capita:PeriodP2 = GDP_per_capita:PeriodP3"))


#If you want to report this result, you would mention that the inclusion of interactions significantly improved the model, and that this suggests that the effects of GDP_per_capita and Renewables_per_capita differ across the periods in your dataset.





model2_york <- plm(Fossil_energy_per_capita ~ GDP_per_capita+Urban_Population+Manufacturing+Age_dependency_ratio+Renewables_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model2_york)

#using fossil fuels (natural gas + oil + coal) instead of all fossil energy


#plus add country and time specific effects 

model3_york <- plm(fossil_fuels_per_capita ~ GDP_per_capita+Urban_Population+Manufacturing+Age_dependency_ratio+Renewables_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model3_york)


######
######
######
#YORK's MODEL 1:Do alternative energy sources displace fossil fuels?
######
######
######


#model1_YORK <- plm(fossil_fuels_per_capita ~ GDP_per_capita+GDP_per_capita_sq+GDP_per_capita_cub+Renewables_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")
#Multicollinearity!!!

# Print the summary of the model
#summary(model1_YORK)

#Remove sq and cub GDP terms 
model1_YORK2 <- plm(fossil_fuels_per_capita ~ GDP_per_capita+Renewables_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")

# Print the summary of the model
summary(model1_YORK2)

#YORK's MODEL 2: adding Urbanisaiton, Manufacturing, Age dependency ratio
#Note without sq and cub GDP 
model2_YORK <- plm(fossil_fuels_per_capita ~ GDP_per_capita+Urban_Population+Urban_sq+Manufacturing+Age_dependency_ratio+Renewables_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model2_YORK)


#Create a non-hydro variable
non_hydro <- data1$Renewables_and_biofuels - data1$Hydro

#non_hydro in per capita terms
non_hydro_per_capita <- non_hydro/ data1$Population


#YORK's MODEL 5:#Yorks Model 5: Divide into nuclear, hydro and non-hydro
#Note without sq and cub GDP 
model5_YORK <- plm(fossil_fuels_per_capita ~ GDP_per_capita+Nuclear_heat_per_capita+Hydro_per_capita+non_hydro_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model5_YORK)

#YORK's MODEL 6:: Divide into nuclear, hydro and non-hydro
#Note without sq and cub GDP 
model6_YORK <- plm(fossil_fuels_per_capita ~ GDP_per_capita+Nuclear_heat_per_capita+Hydro_per_capita+non_hydro_per_capita+Urban_Population+Urban_sq+Manufacturing+Age_dependency_ratio+Renewables_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model6_YORK)

#YORK's MODEL 6.1: ln of GDP instead

model6_1_YORK <- plm(fossil_fuels_per_capita ~ ln_GDP_per_capita+Renewables_per_capita+Urban_Population+Urban_sq+Manufacturing+Age_dependency_ratio, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model6_1_YORK)




#Try variables as in Yihan's Master Thesis: 

model1_all_renew <- plm(fossil_fuels_per_capita ~ Netimports_per_capita+Renewables_per_capita+ Industry +Urban_Population+ ln_GDP_per_capita+Age_dependency_ratio+HDD, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")



modelX_all <- plm(fossil_fuels_per_capita ~ Netimports_per_capita+Nuclear_heat_per_capita+Renewables_per_capita+ln_GDP_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(modelX_all)

modelX_all_nonuclear <- plm(fossil_fuels_per_capita ~ Netimports_per_capita+Renewables_per_capita+ln_GDP_per_capita, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(modelX_all_nonuclear)

#adding more control variables 
modelX_all_york <- plm(fossil_fuels_per_capita ~ Netimports_per_capita+Renewables_per_capita+ ln_GDP_per_capita+Urban_Population+Manufacturing+Industry+Age_dependency_ratio, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(modelX_all_york)

#### remove manufacture

modelX_all_non_york1 <- plm(fossil_fuels_per_capita ~ Netimports_per_capita+Renewables_per_capita+ln_GDP_per_capita+Urban_Population+Industry+Age_dependency_ratio, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(modelX_all_non_york1)

modelX_all_non_york2 <- plm(fossil_fuels_per_capita ~ Netimports_per_capita+Renewables_per_capita+ln_GDP_per_capita+Urban_Population+Industry, data=data1, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(modelX_all_non_york2)


###########ALL RENEWABLES IN ONE######################################


model1_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita +Nuclear_heat_per_capita+ GDP_per_capita+Industry+Urban_Population+Netimports_per_capita,
                    data = data1, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_RENEW)

library(plm)

data1 <- na.omit(data1)

# Create pooled OLS model (no fixed effects)
pooled_model <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + Nuclear_heat_per_capita + GDP_per_capita ,
                    data = data1,
                    model = "pooling",
                    index = c("Country", "TIME"))

# Create one-way fixed effects model (individual effects only, without Prais-Winsten)
fixed_model <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + Nuclear_heat_per_capita + GDP_per_capita ,
                   data = data1,
                   model = "within",
                   effect = "individual",
                   index = c("Country", "TIME"))

summary(fixed_model)
summary(pooled_model)

pFtest_result <- pFtest(fixed_model, pooled_model)
summary(pFtest_result)
# Poolability test
pooltest_result <- pooltest(fixed_model, pooled_model)

summary(pooltest_result)





model2_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita +Nuclear_heat_per_capita+ ln_GDP_per_capita+Industry+Urban_Population+Netimports_per_capita,
                    data = data1, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model2_RENEW)




model3_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + Nuclear_heat_per_capita+ln_GDP_per_capita + Urban_Population +Industry+ Netimports_per_capita,
                    data = data1, 
                    model = "within", 
                    effect = "twoways",  # Change effect to "individual" if no time effects
                    index = c("Country","TIME"),  # Removed TIME
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)

summary(model3_RENEW)




model4_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita+ Nuclear_heat_per_capita + ln_GDP_per_capita+Industry+Netimports_per_capita,
                    data = data1, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model4_RENEW)


model5_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita +Nuclear_heat_per_capita+ ln_GDP_per_capita+Urban_Population+Netimports_per_capita,
                    data = data1, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model5_RENEW)

data1$Non_fossil_energy <- data1$Renewables_per_capita + data1$Nuclear_heat_per_capita


model6_RENEW <- plm(Fossil_energy_per_capita ~ Non_fossil_energy+ ln_GDP_per_capita+ Industry+Urban_Population+Netimports_per_capita,
                    data = data1, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model6_RENEW)



# Load stargazer
library(stargazer)

# Create a summary table
stargazer(model1_RENEW, model2_RENEW, model3_RENEW,
          model4_RENEW, model5_RENEW, model6_RENEW,
          type = "text",  # Change to "latex" or "html" as needed
          title = "Fixed Effects Models with Prais-Winsten Correction",
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
          model.numbers = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          no.space = TRUE)



# DIAGNOSTIC TESTS
# -------------------------------
library(lmtest)
library(plm)
library(lmtest)
library(stargazer)
library(tseries)  # for Jarque-Bera
model1_RENEW_RE <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita + Age_dependency_ratio + Industry + HDD + Netimports_per_capita,
                       data = data1, model = "random", index = c("Country", "TIME"), random.method = "walhus")
hausman_test1 <- phtest(model1_RENEW, model1_RENEW_RE)

model2_RENEW_RE <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita + Age_dependency_ratio + Industry + Netimports_per_capita,
                       data = data1, model = "random", index = c("Country", "TIME"), random.method = "walhus")
hausman_test2 <- phtest(model2_RENEW, model2_RENEW_RE)

model3_RENEW_RE <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita + Industry + Netimports_per_capita,
                       data = data1, model = "random", index = c("Country", "TIME"), random.method = "walhus")
hausman_test3 <- phtest(model3_RENEW, model3_RENEW_RE)

model4_RENEW_RE <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita + Netimports_per_capita,
                       data = data1, model = "random", index = c("Country", "TIME"), random.method = "walhus")
hausman_test4 <- phtest(model4_RENEW, model4_RENEW_RE)

model5_RENEW_RE <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + GDP_per_capita + Age_dependency_ratio + Industry + Netimports_per_capita,
                       data = data1, model = "random", index = c("Country", "TIME"), random.method = "walhus")
hausman_test5 <- phtest(model5_RENEW, model5_RENEW_RE)

#Breusch-Pagan Test (Heteroskedasticity)
bp_test1 <- bptest(model1_RENEW)
bp_test2 <- bptest(model2_RENEW)
bp_test3 <- bptest(model3_RENEW)
bp_test4 <- bptest(model4_RENEW)
bp_test5 <- bptest(model5_RENEW)

#Breusch-Godfrey Test (Serial Correlation)
bg_test1 <- bgtest(model1_RENEW)
bg_test2 <- bgtest(model2_RENEW)
bg_test3 <- bgtest(model3_RENEW)
bg_test4 <- bgtest(model4_RENEW)
bg_test5 <- bgtest(model5_RENEW)

#Jarque-Bera Test (Normality of Residuals)
jb_test1 <- jarque.bera.test(resid(model1_RENEW))
jb_test2 <- jarque.bera.test(resid(model2_RENEW))
jb_test3 <- jarque.bera.test(resid(model3_RENEW))
jb_test4 <- jarque.bera.test(resid(model4_RENEW))
jb_test5 <- jarque.bera.test(resid(model5_RENEW))


# R-squared (within)
# Calculate R-squared (within) for each model
rsq1 <- summary(model1_RENEW)$r.squared["rsq"]
rsq2 <- summary(model2_RENEW)$r.squared["rsq"]
rsq3 <- summary(model3_RENEW)$r.squared["rsq"]
rsq4 <- summary(model4_RENEW)$r.squared["rsq"]
rsq5 <- summary(model5_RENEW)$r.squared["rsq"]

# Residual sum of squares
rss1 <- sum(resid(model1_RENEW)^2)
rss2 <- sum(resid(model2_RENEW)^2)
rss3 <- sum(resid(model3_RENEW)^2)
rss4 <- sum(resid(model4_RENEW)^2)
rss5 <- sum(resid(model5_RENEW)^2)

# Number of observations
n1 <- nobs(model1_RENEW)
n2 <- nobs(model2_RENEW)
n3 <- nobs(model3_RENEW)
n4 <- nobs(model4_RENEW)
n5 <- nobs(model5_RENEW)

# Number of parameters (including the intercept)
k1 <- length(coef(model1_RENEW))
k2 <- length(coef(model2_RENEW))
k3 <- length(coef(model3_RENEW))
k4 <- length(coef(model4_RENEW))
k5 <- length(coef(model5_RENEW))

# Calculate AIC using the formula
aic1 <- n1 * log(rss1 / n1) + 2 * k1
aic2 <- n2 * log(rss2 / n2) + 2 * k2
aic3 <- n3 * log(rss3 / n3) + 2 * k3
aic4 <- n4 * log(rss4 / n4) + 2 * k4
aic5 <- n5 * log(rss5 / n5) + 2 * k5
library(stargazer)

# Format the test results for easy inclusion in the stargazer table
bp_results <- c(format.pval(bp_test1$p.value, digits = 3), format.pval(bp_test2$p.value, digits = 3), 
                format.pval(bp_test3$p.value, digits = 3), format.pval(bp_test4$p.value, digits = 3), 
                format.pval(bp_test5$p.value, digits = 3))

bg_results <- c(format.pval(bg_test1$p.value, digits = 3), format.pval(bg_test2$p.value, digits = 3), 
                format.pval(bg_test3$p.value, digits = 3), format.pval(bg_test4$p.value, digits = 3), 
                format.pval(bg_test5$p.value, digits = 3))

jb_results <- c(format.pval(jb_test1$p.value, digits = 3), format.pval(jb_test2$p.value, digits = 3), 
                format.pval(jb_test3$p.value, digits = 3), format.pval(jb_test4$p.value, digits = 3), 
                format.pval(jb_test5$p.value, digits = 3))

rsq_results <- c(rsq1, rsq2, rsq3, rsq4, rsq5)

# Number of observations for each model
observations <- c(n1, n2, n3, n4, n5)

# AIC values for each model
aic_results <- c(aic1, aic2, aic3, aic4, aic5)



# Extract p-values from each test
hausman_pvals <- c(hausman_test1$p.value, hausman_test2$p.value, hausman_test3$p.value, hausman_test4$p.value, hausman_test5$p.value)

bp_pvals <- c(bp_test1$p.value, bp_test2$p.value, bp_test3$p.value, bp_test4$p.value, bp_test5$p.value)

bg_pvals <- c(bg_test1$p.value, bg_test2$p.value, bg_test3$p.value, bg_test4$p.value, bg_test5$p.value)

jb_pvals <- c(jb_test1$p.value, jb_test2$p.value, jb_test3$p.value, jb_test4$p.value, jb_test5$p.value)



# Now use stargazer to display the results in a nice table format
# Create a data frame with the test results and other statistics
results_table <- data.frame(
  Model = c("Full Spec", "No HDD", "No HDD & Age", "Only Econ", "GDP instead of lnGDP"),
  
  # Add p-values for Hausman test
  Hausman_pval = format.pval(hausman_pvals, digits = 3),
  
  # Add p-values for Breusch-Pagan test (Heteroskedasticity)
  BP_pval = format.pval(bp_pvals, digits = 3),
  
  # Add p-values for Breusch-Godfrey test (Serial Correlation)
  BG_pval = format.pval(bg_pvals, digits = 3),
  
  # Add p-values for Jarque-Bera test (Normality of Residuals)
  JB_pval = format.pval(jb_pvals, digits = 3),
  
  # Add number of observations for each model
  Observations = c(n1, n2, n3, n4, n5),
  
  # Add R-squared values for each model
  R_squared = c(summary(model1_RENEW)$r.squared["rsq"], 
                summary(model2_RENEW)$r.squared["rsq"], 
                summary(model3_RENEW)$r.squared["rsq"], 
                summary(model4_RENEW)$r.squared["rsq"], 
                summary(model5_RENEW)$r.squared["rsq"]),
  
  # Add AIC values for each model
  AIC = c(aic1, aic2, aic3, aic4, aic5)
)

# Print the results table using knitr::kable for neat formatting
library(knitr)
kable(results_table, caption = "Model Statistics and Test Results", format = "markdown")


# Transpose the table
results_table_transposed <- t(results_table)

# Convert it to a data frame for better presentation
results_table_transposed <- as.data.frame(results_table_transposed)

# Set row names as column headers
colnames(results_table_transposed) <- results_table$Statistic

# Print the transposed results table using knitr::kable for neat formatting
library(knitr)
kable(results_table_transposed, caption = "Model Statistics and Test Results", format = "markdown")






#####################################################################################################





# Load necessary libraries
library(ggplot2)

# Plot for GDP per capita
ggplot(data1, aes(x = GDP_per_capita)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of GDP per Capita", x = "GDP per Capita", y = "Frequency") +
  theme_minimal()

# Plot for log(GDP per capita)
ggplot(data1, aes(x = log(GDP_per_capita))) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of log(GDP per Capita)", x = "log(GDP per Capita)", y = "Frequency") +
  theme_minimal()







# Load necessary libraries
library(plm)

# Fit both models (model3_RENEW and model4_RENEW are already fit in your case)
# model3_RENEW and model4_RENEW are already fit

# Perform the F-test to compare the models
f_test_result <- pFtest(model3_RENEW, model4_RENEW)

# Print the F-test result
print(f_test_result)

#Conclusion: Since the p-value is not significant (greater than 0.05), you fail to reject the null hypothesis. This means that adding the extra variables (like Industry) in model3_RENEW does not significantly improve the model compared to model4_RENEW.



# model4_RENEW and model5_RENEW are already fit

# Perform the F-test to compare the models
f_test_result <- pFtest(model5_RENEW, model3_RENEW)

# Print the F-test result
print(f_test_result)




model_interact4 <- plm(Fossil_energy_per_capita ~ Renewables_per_capita * Country + ln_GDP_per_capita + Industry + Netimports_per_capita,
                      data = data1,
                      model = "within",
                      effect = "twoways",
                      index = c("Country", "TIME"),
                      method = "prais")
summary(model_interact)


#These are deviations from the Denmark baseline for each country 

model_interact3 <- plm(Fossil_energy_per_capita ~ Renewables_per_capita * Country + ln_GDP_per_capita + Netimports_per_capita,
                      data = data1,
                      model = "within",
                      effect = "twoways",
                      index = c("Country", "TIME"),
                      method = "prais")
summary(model_interact3)


#############TIME SUBSAMPLE######################################################################################
#Make sure TIME is numeric
data1$TIME <- as.numeric(as.character(data1$TIME))

# Now create the time period variable
data1$Period <- cut(data1$TIME, 
                    breaks = c(1989, 2005, 2015, 2023), 
                    labels = c("Pre2005", "2005_2015", "Post2015"),
                    right = TRUE, include.lowest = TRUE)

model_time_interaction <- plm(
  Fossil_energy_per_capita ~ Renewables_per_capita * Period + Nuclear_heat_per_capita * Period+ ln_GDP_per_capita + Industry +Urban_Population+ Netimports_per_capita,
  data = data1,
  model = "within",
  effect = "twoways",
  index = c("Country", "TIME"),
  method = "prais"
)

summary(model_time_interaction)



# Load necessary packages
library(plm)
library(dplyr)
library(ggplot2)

# Get coefficients and variance-covariance matrix
coefs <- coef(model_time_interaction)  # your model name
vcov_mat <- vcov(model_time_interaction)

# Marginal effects
effects <- data.frame(
  Period = c("Pre2005", "2005_2015", "Post2015"),
  Coefficient = c(
    coefs["Renewables_per_capita"],
    coefs["Renewables_per_capita"] + coefs["Renewables_per_capita:Period2005_2015"],
    coefs["Renewables_per_capita"] + coefs["Renewables_per_capita:PeriodPost2015"]
  ),
  SE = c(
    sqrt(vcov_mat["Renewables_per_capita", "Renewables_per_capita"]),
    
    sqrt(
      vcov_mat["Renewables_per_capita", "Renewables_per_capita"] +
        vcov_mat["Renewables_per_capita:Period2005_2015", "Renewables_per_capita:Period2005_2015"] +
        2 * vcov_mat["Renewables_per_capita", "Renewables_per_capita:Period2005_2015"]
    ),
    
    sqrt(
      vcov_mat["Renewables_per_capita", "Renewables_per_capita"] +
        vcov_mat["Renewables_per_capita:PeriodPost2015", "Renewables_per_capita:PeriodPost2015"] +
        2 * vcov_mat["Renewables_per_capita", "Renewables_per_capita:PeriodPost2015"]
    )
  )
)

# Marginal effects for Nuclear_heat_per_capita
effects_nuclear_heat <- data.frame(
  Period = c("Pre2005", "2005_2015", "Post2015"),
  Coefficient = c(
    coefs["Nuclear_heat_per_capita"],
    coefs["Nuclear_heat_per_capita"] + coefs["Nuclear_heat_per_capita:Period2005_2015"],
    coefs["Nuclear_heat_per_capita"] + coefs["Nuclear_heat_per_capita:PeriodPost2015"]
  ),
  SE = c(
    sqrt(vcov_mat["Nuclear_heat_per_capita", "Nuclear_heat_per_capita"]),
    sqrt(
      vcov_mat["Nuclear_heat_per_capita", "Nuclear_heat_per_capita"] +
        vcov_mat["Nuclear_heat_per_capita:Period2005_2015", "Nuclear_heat_per_capita:Period2005_2015"] +
        2 * vcov_mat["Nuclear_heat_per_capita", "Nuclear_heat_per_capita:Period2005_2015"]
    ),
    sqrt(
      vcov_mat["Nuclear_heat_per_capita", "Nuclear_heat_per_capita"] +
        vcov_mat["Nuclear_heat_per_capita:PeriodPost2015", "Nuclear_heat_per_capita:PeriodPost2015"] +
        2 * vcov_mat["Nuclear_heat_per_capita", "Nuclear_heat_per_capita:PeriodPost2015"]
    )
  )
)


# Compute 95% confidence intervals
#effects <- effects %>%
#  mutate(
#    CI_lower = Coefficient - 1.96 * SE,
#    CI_upper = Coefficient + 1.96 * SE
#  )
#
#print(effects)


# Combine the effects for both Renewables and Nuclear Heat
effects <- bind_rows(
  effects_renewables %>% mutate(Variable = "Renewables_per_capita"),
  effects_nuclear_heat %>% mutate(Variable = "Nuclear_heat_per_capita")
)

# Compute 95% confidence intervals
effects <- effects %>%
  mutate(
    CI_lower = Coefficient - 1.96 * SE,
    CI_upper = Coefficient + 1.96 * SE
  )

# Print the results
print(effects)


# Define coefficients and standard errors
coefs <- c(
  Renewables_per_capita = -0.459933,
  Nuclear_heat_per_capita = -0.276268,
  ln_GDP_per_capita = 9620.004463,
  Industry = -32.860451,
  Urban_Population = 410.237349,
  Netimports_per_capita = -0.655109,
  Renewables_per_capita_Period2005_2015 = -0.012954,
  Renewables_per_capita_PeriodPost2015 = 0.010666,
  Nuclear_heat_per_capita_Period2005_2015 = -0.011149,
  Nuclear_heat_per_capita_PeriodPost2015 = -0.011214
)

# Define the standard errors
se <- c(
  Renewables_per_capita = 0.069034,
  Nuclear_heat_per_capita = 0.078523,
  ln_GDP_per_capita = 1572.132349,
  Industry = 23.044430,
  Urban_Population = 84.566648,
  Netimports_per_capita = 0.057481,
  Renewables_per_capita_Period2005_2015 = 0.014344,
  Renewables_per_capita_PeriodPost2015 = 0.025601,
  Nuclear_heat_per_capita_Period2005_2015 = 0.042336,
  Nuclear_heat_per_capita_PeriodPost2015 = 0.052106
)

# Calculate the marginal effects for Renewables and Nuclear heat
effects_renewables <- data.frame(
  Period = c("Pre2005", "2005_2015", "Post2015"),
  Coefficient = c(
    coefs["Renewables_per_capita"],  # Pre2005
    coefs["Renewables_per_capita"] + coefs["Renewables_per_capita_Period2005_2015"],  # 2005_2015
    coefs["Renewables_per_capita"] + coefs["Renewables_per_capita_PeriodPost2015"]  # Post2015
  ),
  SE = c(
    se["Renewables_per_capita"],  # Pre2005
    sqrt(se["Renewables_per_capita"]^2 + se["Renewables_per_capita_Period2005_2015"]^2 + 2 * se["Renewables_per_capita"] * se["Renewables_per_capita_Period2005_2015"]),  # 2005_2015
    sqrt(se["Renewables_per_capita"]^2 + se["Renewables_per_capita_PeriodPost2015"]^2 + 2 * se["Renewables_per_capita"] * se["Renewables_per_capita_PeriodPost2015"])  # Post2015
  )
)

effects_nuclear <- data.frame(
  Period = c("Pre2005", "2005_2015", "Post2015"),
  Coefficient = c(
    coefs["Nuclear_heat_per_capita"],  # Pre2005
    coefs["Nuclear_heat_per_capita"] + coefs["Nuclear_heat_per_capita_Period2005_2015"],  # 2005_2015
    coefs["Nuclear_heat_per_capita"] + coefs["Nuclear_heat_per_capita_PeriodPost2015"]  # Post2015
  ),
  SE = c(
    se["Nuclear_heat_per_capita"],  # Pre2005
    sqrt(se["Nuclear_heat_per_capita"]^2 + se["Nuclear_heat_per_capita_Period2005_2015"]^2 + 2 * se["Nuclear_heat_per_capita"] * se["Nuclear_heat_per_capita_Period2005_2015"]),  # 2005_2015
    sqrt(se["Nuclear_heat_per_capita"]^2 + se["Nuclear_heat_per_capita_PeriodPost2015"]^2 + 2 * se["Nuclear_heat_per_capita"] * se["Nuclear_heat_per_capita_PeriodPost2015"])  # Post2015
  )
)

# Compute 95% confidence intervals for both
effects_renewables <- effects_renewables %>%
  mutate(
    CI_lower = Coefficient - 1.96 * SE,
    CI_upper = Coefficient + 1.96 * SE
  )

effects_nuclear <- effects_nuclear %>%
  mutate(
    CI_lower = Coefficient - 1.96 * SE,
    CI_upper = Coefficient + 1.96 * SE
  )

# Print the results
print(effects_renewables)
print(effects_nuclear)




# Full model (with interactions)
model_full <- plm(
  Fossil_energy_per_capita ~ Renewables_per_capita * Period + Nuclear_heat_per_capita * Period + ln_GDP_per_capita + Industry + Urban_Population + Netimports_per_capita,
  data = data1,
  model = "within",
  effect = "twoways",
  index = c("Country", "TIME"),
  method = "prais"
)

# Restricted model (coefficient for periods are the same)
model_restricted <- plm(
  Fossil_energy_per_capita ~ Renewables_per_capita + Nuclear_heat_per_capita + ln_GDP_per_capita + Industry + Urban_Population + Netimports_per_capita,
  data = data1,
  model = "within",
  effect = "twoways",
  index = c("Country", "TIME"),
  method = "prais"
)

# Perform the Wald test
library(car)


wald_test <- linearHypothesis(model_full, 
                              c("Renewables_per_capita:Period2005_2015 = 0", 
                                "Renewables_per_capita:PeriodPost2015 = 0",
                                "Period2005_2015:Nuclear_heat_per_capita = 0", 
                                "PeriodPost2015:Nuclear_heat_per_capita = 0")
)
print(wald_test)


#The interaction terms do not significantly improve the model. There's no evidence that the effects of renewables or nuclear heat differ across periods.




#############################################################################
###### 2 periods

data1$Period <- cut(data1$TIME, 
                    breaks = c(1989, 2006, 2023),  # Split: 1990–2006 vs 2007–2023
                    labels = c("Pre2007", "Post2006"),
                    right = TRUE, include.lowest = TRUE)




model_time_interaction <- plm(
  Fossil_energy_per_capita ~ Renewables_per_capita * Period + 
    Nuclear_heat_per_capita * Period + 
    ln_GDP_per_capita + Industry + Urban_Population + Netimports_per_capita,
  data = data1,
  model = "within",
  effect = "twoways",
  index = c("Country", "TIME"),
  method = "prais"
)

summary(model_time_interaction)


coefs <- coef(model_time_interaction)
vcov_mat <- vcov(model_time_interaction)


effects_renewables <- data.frame(
  Period = c("Pre2007", "Post2006"),
  Coefficient = c(
    coefs["Renewables_per_capita"],
    coefs["Renewables_per_capita"] + coefs["Renewables_per_capita:PeriodPost2006"]
  ),
  SE = c(
    sqrt(vcov_mat["Renewables_per_capita", "Renewables_per_capita"]),
    sqrt(
      vcov_mat["Renewables_per_capita", "Renewables_per_capita"] +
        vcov_mat["Renewables_per_capita:PeriodPost2006", "Renewables_per_capita:PeriodPost2006"] +
        2 * vcov_mat["Renewables_per_capita", "Renewables_per_capita:PeriodPost2006"]
    )
  )
)

effects_nuclear <- data.frame(
  Period = c("Pre2007", "Post2006"),
  Coefficient = c(
    coefs["Nuclear_heat_per_capita"],
    coefs["Nuclear_heat_per_capita"] + coefs["Nuclear_heat_per_capita:PeriodPost2006"]
  ),
  SE = c(
    sqrt(vcov_mat["Nuclear_heat_per_capita", "Nuclear_heat_per_capita"]),
    sqrt(
      vcov_mat["Nuclear_heat_per_capita", "Nuclear_heat_per_capita"] +
        vcov_mat["Nuclear_heat_per_capita:PeriodPost2006", "Nuclear_heat_per_capita:PeriodPost2006"] +
        2 * vcov_mat["Nuclear_heat_per_capita", "Nuclear_heat_per_capita:PeriodPost2006"]
    )
  )
)

# Replace all NA values with 0 in the entire data frame
data1[is.na(data1)] <- 0
library(car)
# Update the hypothesis with correct interaction term names
wald_test <- linearHypothesis(model_time_interaction, 
                              c("Renewables_per_capita:PeriodPost2006 = 0", 
                                "PeriodPost2006:Nuclear_heat_per_capita  = 0"))

summary(wald_test)












# Ensure TIME is numeric
data1$TIME <- as.numeric(as.character(data1$TIME))

# Manually define breaks and labels for 10-year periods
breaks_10years <- c(1989, 1999, 2009, 2019, 2023)  # 1990-2000, 2001-2010, 2011-2020, 2021-2023
labels_10years <- c("1990-2000", "2001-2010", "2011-2020", "2021-2023")  # Corresponding labels

# Create period variable
data1$Period_10years <- cut(data1$TIME, 
                            breaks = breaks_10years, 
                            labels = labels_10years,
                            right = TRUE, include.lowest = TRUE)

# Run the model with interactions for the new periods
model_time_interaction_10years <- plm(
  Fossil_energy_per_capita ~ Renewables_per_capita * Period_10years + ln_GDP_per_capita + Industry + Netimports_per_capita,
  data = data1,
  model = "within",
  effect = "twoways",
  index = c("Country", "TIME"),
  method = "prais"
)

# Summarize the results
summary(model_time_interaction_10years)

# Ensure TIME is numeric
data1$TIME <- as.numeric(as.character(data1$TIME))

# Manually define breaks and labels for 2 equal periods
breaks_equal_periods <- c(1989, 2006, 2023)  # 1990-2006, 2007-2023
labels_equal_periods <- c("1990-2006", "2007-2023")  # Corresponding labels

# Create period variable
data1$Period_equal <- cut(data1$TIME, 
                          breaks = breaks_equal_periods, 
                          labels = labels_equal_periods,
                          right = TRUE, include.lowest = TRUE)

# Run the model with interactions for the new periods
model_time_interaction_two <- plm(
  Fossil_energy_per_capita ~ Renewables_per_capita * Period_equal + ln_GDP_per_capita + Industry + Netimports_per_capita,
  data = data1,
  model = "within",
  effect = "twoways",
  index = c("Country", "TIME"),
  method = "prais"
)

# Summarize the results
summary(model_time_interaction_two)


############################################################
# Ensure TIME is numeric
data1$TIME <- as.numeric(as.character(data1$TIME))

# Manually define breaks and labels for 3 even periods
breaks_three_periods <- c(1989, 2000, 2011, 2023)  # 1990-2000, 2001-2011, 2012-2023
labels_three_periods <- c("1990-2000", "2001-2011", "2012-2023")  # Corresponding labels

# Create period variable
data1$Period_three <- cut(data1$TIME, 
                         breaks = breaks_three_periods, 
                         labels = labels_three_periods,
                         right = TRUE, include.lowest = TRUE)

# Run the model with interactions for the new periods
model_time_interaction_three <- plm(
  Fossil_energy_per_capita ~ Renewables_per_capita * Period_three + ln_GDP_per_capita + Industry + Netimports_per_capita,
  data = data1,
  model = "within",
  effect = "twoways",
  index = c("Country", "TIME"),
  method = "prais"
)

# Summarize the results
summary(model_time_interaction_three)








###################SPLIT UP INTO TECHNOLOGIES############################################################################

# Select only the relevant variables
vars <- data1[, c(
  "Fossil_energy_per_capita","Nuclear_heat_per_capita", "Hydro_per_capita", "Wind_per_capita", "Bioenergy_per_capita", "Solar_photovoltaic_per_capita",
  "Electricity_kWh_per_capita", "ln_GDP_per_capita", "Age_dependency_ratio", "Industry", "HDD", "Netimports_per_capita"
)]

# Correlation matrix
cor(vars, use = "pairwise.complete.obs")


# First run an OLS version of your model
library(car)

model1_RENEW_single_ols <- lm(
  Fossil_energy_per_capita ~ Nuclear_heat_per_capita + Hydro_per_capita + Wind_per_capita + Bioenergy_per_capita + Solar_photovoltaic_per_capita +
    Electricity_kWh_per_capita + ln_GDP_per_capita + Age_dependency_ratio + Industry + HDD + Netimports_per_capita,
  data = data1
)

# Then calculate VIFs
vif(model1_RENEW_single_ols)

# First run an OLS version of your model
library(car)

model1_RENEW_single_ols <- lm(
  Fossil_energy_per_capita ~ Nuclear_heat_per_capita + Hydro_per_capita + Wind_per_capita + Bioenergy_per_capita + Solar_photovoltaic_per_capita +
    Electricity_kWh_per_capita + ln_GDP_per_capita + Age_dependency_ratio + Industry + HDD + Netimports_per_capita,
  data = data1
)

# Then calculate VIFs
vif(model1_RENEW_single_ols)




model1_RENEW_single <- plm(Fossil_energy_per_capita ~ Nuclear_heat_per_capita+Hydro_per_capita+Wind_per_capita+Bioenergy_per_capita+ Solar_photovoltaic_per_capita +ln_GDP_per_capita+Netimports_per_capita,
                    data = data1, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_RENEW_single)

# Step 1: Fit a pooled OLS model (ignores FE for now)
model_vif <- lm(Fossil_energy_per_capita ~ Nuclear_heat_per_capita + Hydro_per_capita + Wind_per_capita + 
                  Bioenergy_per_capita + Solar_photovoltaic_per_capita + ln_GDP_per_capita + 
                  #Age_dependency_ratio 
                  + Industry + Netimports_per_capita, 
                data = data1)

# Step 2: Check VIF
vif(model_vif)





# Model 1: Solar as the dependent variable, with Fossil_energy as an explanatory variable
model_solar <- plm(Solar_photovoltaic_per_capita ~ Wind_per_capita + Hydro_per_capita + Bioenergy_per_capita + Nuclear_heat_per_capita +
                     Fossil_energy_per_capita + ln_GDP_per_capita + Industry + Netimports_per_capita,
                   data = data1, 
                   model = "within", 
                   effect = "twoways", 
                   index = c("Country", "TIME"),
                   method = "prais")

# Model 2: Wind as the dependent variable, with Fossil_energy as an explanatory variable
model_wind <- plm(Wind_per_capita ~ Solar_photovoltaic_per_capita + Hydro_per_capita + Bioenergy_per_capita + Nuclear_heat_per_capita + 
                    Fossil_energy_per_capita + ln_GDP_per_capita + Industry + Netimports_per_capita,
                  data = data1, 
                  model = "within", 
                  effect = "twoways", 
                  index = c("Country", "TIME"),
                  method = "prais")

# Model 3: Hydro as the dependent variable, with Fossil_energy as an explanatory variable
model_hydro <- plm(Hydro_per_capita ~ Wind_per_capita + Solar_photovoltaic_per_capita + Bioenergy_per_capita + Nuclear_heat_per_capita + 
                     Fossil_energy_per_capita + ln_GDP_per_capita + Industry + Netimports_per_capita,
                   data = data1, 
                   model = "within", 
                   effect = "twoways", 
                   index = c("Country", "TIME"),
                   method = "prais")

# Model 4: Bioenergy as the dependent variable, with Fossil_energy as an explanatory variable
model_bioenergy <- plm(Bioenergy_per_capita ~ Wind_per_capita + Solar_photovoltaic_per_capita + Hydro_per_capita + Nuclear_heat_per_capita + 
                         Fossil_energy_per_capita + ln_GDP_per_capita + Industry + Netimports_per_capita,
                       data = data1, 
                       model = "within", 
                       effect = "twoways", 
                       index = c("Country", "TIME"),
                       method = "prais")

# Model 5: Nuclear as the dependent variable, with Fossil_energy as an explanatory variable
model_nuclear <- plm(Nuclear_heat_per_capita ~ Wind_per_capita + Solar_photovoltaic_per_capita + Hydro_per_capita + Bioenergy_per_capita + 
                       Fossil_energy_per_capita + ln_GDP_per_capita + Industry + Netimports_per_capita,
                     data = data1, 
                     model = "within", 
                     effect = "twoways", 
                     index = c("Country", "TIME"),
                     method = "prais")


# For Solar model
summary(model_solar)

# For Wind model
summary(model_wind)

# For Hydro model
summary(model_hydro)

# For Bioenergy model
summary(model_bioenergy)

# For Nuclear model
summary(model_nuclear)

#########################################################################################

# Model 1: Wind_Solar_combined as the dependent variable, with Fossil_energy and other explanatory variables
model_solar_wind1 <- plm(Wind_Solar_combined ~ Hydro_per_capita + Bioenergy_per_capita + Nuclear_heat_per_capita +
                     Fossil_energy_per_capita + ln_GDP_per_capita +Urban_Population+ Industry + Netimports_per_capita,
                   data = data1, 
                   model = "within", 
                   effect = "twoways", 
                   index = c("Country", "TIME"),
                   method = "prais")


# Model 2: Hydro as the dependent variable, with Wind_Solar_combined and Fossil_energy as explanatory variables
model_hydro1 <- plm(Hydro_per_capita ~ Wind_Solar_combined + Bioenergy_per_capita + Nuclear_heat_per_capita + Fossil_energy_per_capita + 
                     ln_GDP_per_capita +Urban_Population+ Industry + Netimports_per_capita,
                   data = data1, 
                   model = "within", 
                   effect = "twoways", 
                   index = c("Country", "TIME"),
                   method = "prais")

# Model 3: Bioenergy as the dependent variable, with Wind_Solar_combined and Fossil_energy as explanatory variables
model_bioenergy1 <- plm(Bioenergy_per_capita ~ Wind_Solar_combined + Hydro_per_capita + Nuclear_heat_per_capita + Fossil_energy_per_capita + 
                         ln_GDP_per_capita +Urban_Population+ Industry + Netimports_per_capita,
                       data = data1, 
                       model = "within", 
                       effect = "twoways", 
                       index = c("Country", "TIME"),
                       method = "prais")

# Model 4: Nuclear as the dependent variable, with Wind_Solar_combined and Fossil_energy as explanatory variables
model_nuclear1 <- plm(Nuclear_heat_per_capita ~ Wind_Solar_combined + Hydro_per_capita + Bioenergy_per_capita + Fossil_energy_per_capita + 
                       ln_GDP_per_capita +Urban_Population+ Industry + Netimports_per_capita,
                     data = data1, 
                     model = "within", 
                     effect = "twoways", 
                     index = c("Country", "TIME"),
                     method = "prais")


# Model 5: Wind_Solar_combined as the dependent variable, with Fossil_energy and other explanatory variables
model_solar_wind1_no_hydro <- plm(Wind_Solar_combined ~ Bioenergy_per_capita + Nuclear_heat_per_capita +
                           Fossil_energy_per_capita + ln_GDP_per_capita +Urban_Population+ Industry + Netimports_per_capita,
                         data = data1, 
                         model = "within", 
                         effect = "twoways", 
                         index = c("Country", "TIME"),
                         method = "prais")


# Model 6: Nuclear as the dependent variable, with Wind_Solar_combined and Fossil_energy as explanatory variables
model_bio1_no_hydro <- plm(Bioenergy_per_capita ~ Wind_Solar_combined  + Nuclear_heat_per_capita+ Fossil_energy_per_capita + 
                        ln_GDP_per_capita +Urban_Population+ Industry + Netimports_per_capita,
                      data = data1, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")





#The result of the linear hypothesis test shows that you cannot reject the null hypothesis that the coefficient for the combined renewable sources (Wind_Solar_Hydro_combined) is zero.


model1__single_RENEW <- plm(Fossil_energy_per_capita ~ Hydro_per_capita+Wind_per_capita+Bioenergy_per_capita+Nuclear_heat_per_capita+Solar_photovoltaic_per_capita +ln_GDP_per_capita+#Age_dependency_ratio
                              +Industry+Netimports_per_capita,
                      data = data1, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1__single_RENEW)




############################################################################################

# FOR OUR RESULTS
#Run models with single technologies, one by one. 



model1_all <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined++ Hydro_per_capita+ Nuclear_heat_per_capita + Bioenergy_per_capita+ ln_GDP_per_capita
                        
                        +Industry+Netimports_per_capita+Urban_Population,
                        data = data1, 
                        model = "within", 
                  effect = "twoways", 
                  index = c("Country","TIME"),
                        method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_all)


model1_non_hydro <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined+ Nuclear_heat_per_capita + Bioenergy_per_capita+ ln_GDP_per_capita
                      
                        +Industry+Netimports_per_capita+Urban_Population,
                      data = data1, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country","TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_non_hydro)

model1_non_hydro_urban <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined+ Nuclear_heat_per_capita + Bioenergy_per_capita+ ln_GDP_per_capita
                        
                        +Industry+Netimports_per_capita,
                        data = data1, 
                        model = "within", 
                        effect = "twoways", 
                        index = c("Country","TIME"),
                        method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_non_hydro_urban)

model1_non_hydro_non_bio <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined+ Nuclear_heat_per_capita + ln_GDP_per_capita
                              
                              +Industry+Netimports_per_capita+Urban_Population,
                              data = data1, 
                              model = "within", 
                              effect = "twoways", 
                              index = c("Country","TIME"),
                              method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_non_hydro_non_bio)


model1_bio <- plm(Fossil_energy_per_capita ~ Bioenergy_per_capita + ln_GDP_per_capita
                                
                                +Industry
                                + Urban_Population
                  +Netimports_per_capita,
                                data = data1, 
                                model = "within", 
                  effect = "twoways", 
                  index = c("Country","TIME"),
                                method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_bio)



model1_singlehydro <- plm(Fossil_energy_per_capita ~ Hydro_per_capita + ln_GDP_per_capita
                  
                 
                  + Urban_Population
                  +Netimports_per_capita+Urban_Population,
                  data = data1, 
                  model = "within", 
                  effect = "twoways", 
                  index = c("Country","TIME"),
                  method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_singlehydro)




# Extract coefficients and standard errors
coef1 <- coef(summary(model1_all))["Hydro_per_capita", "Estimate"]
se1 <- coef(summary(model1_all))["Hydro_per_capita", "Std. Error"]

coef5 <- coef(summary(model1_singlehydro))["Hydro_per_capita", "Estimate"]
se5 <- coef(summary(model1_singlehydro))["Hydro_per_capita", "Std. Error"]

# Calculate z-score for difference
z <- (coef1 - coef5) / sqrt(se1^2 + se5^2)

# p-value
p_value <- 2 * (1 - pnorm(abs(z)))

# Output
z
p_value


# Extract coefficients and standard errors for Model 1 and Model 2
coef1 <- coef(summary(model1_all))["Wind_Solar_combined", "Estimate"]
se1 <- coef(summary(model1_all))["Wind_Solar_combined", "Std. Error"]

coef2 <- coef(summary(model1_non_hydro))["Wind_Solar_combined", "Estimate"]
se2 <- coef(summary(model1_non_hydro))["Wind_Solar_combined", "Std. Error"]

# Z-test: Model 1 vs Model 2
z_12 <- (coef1 - coef2) / sqrt(se1^2 + se2^2)
p_12 <- 2 * (1 - pnorm(abs(z_12)))

# Extract coefficients and standard errors for Model 2 and Model 3
coef3 <- coef(summary(model1_non_hydro_urban))["Wind_Solar_combined", "Estimate"]
se3 <- coef(summary(model1_non_hydro_urban))["Wind_Solar_combined", "Std. Error"]

# Z-test: Model 2 vs Model 3
z_23 <- (coef2 - coef3) / sqrt(se2^2 + se3^2)
p_23 <- 2 * (1 - pnorm(abs(z_23)))

# Output
z_12
p_12
z_23
p_23


# Extract coefficients and standard errors for Model 3 and Model 4
coef4 <- coef(summary(model1_non_hydro_non_bio))["Wind_Solar_combined", "Estimate"]
se4 <- coef(summary(model1_non_hydro_non_bio))["Wind_Solar_combined", "Std. Error"]

# Z-test: Model 3 vs Model 4
z_34 <- (coef3 - coef4) / sqrt(se3^2 + se4^2)
p_34 <- 2 * (1 - pnorm(abs(z_34)))

# Output
z_34
p_34


##############################################################################################################
# Filter the dataset Hydro for Sweden, Norway and Finland only
data_sw_fi_hydro <- subset(data1, Country %in% c("Sweden", "Finland","Norway"))

# Run the fixed effects (within) model with Prais-Winsten correction
library(plm)

model1_hydro_subset <- plm(Fossil_energy_per_capita ~ Hydro_per_capita + ln_GDP_per_capita
                   +Urban_Population + Industry + Netimports_per_capita,
                    data = data_sw_fi_hydro, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Note: "prais" is available in some extensions

# Show model summary
#summary(model1_hydro_subset)

dk_se <- vcovSCC(model1_hydro_subset, type = "HC0", maxlag = 2)
coeftest(model1_hydro_subset, vcov = dk_se)




# Filter the dataset Hydro for Sweden, Norway and Finland only
data_sw_fi_hydro <- subset(data1, Country %in% c("Sweden", "Finland"))

# Run the fixed effects (within) model with Prais-Winsten correction
library(plm)

model1_hydro_subset1 <- plm(Fossil_energy_per_capita ~ Hydro_per_capita + ln_GDP_per_capita
                           +Urban_Population + Industry + Netimports_per_capita,
                           data = data_sw_fi_hydro, 
                           model = "within", 
                           effect = "twoways", 
                           index = c("Country", "TIME"),
                           method = "prais")  # Note: "prais" is available in some extensions

# Show model summary
#summary(model1_hydro_subset1)


dk_se <- vcovSCC(model1_hydro_subset1, type = "HC0", maxlag = 2)
coeftest(model1_hydro_subset1, vcov = dk_se)





# Filter the dataset Hydro for Sweden, Norway and Finland only
data_sw_fi_hydro <- subset(data1, Country %in% c("Sweden", "Finland"))


model1_hydro_subset2 <- plm(Fossil_energy_per_capita ~ Hydro_per_capita + ln_GDP_per_capita
                            +Urban_Population + Industry + Netimports_per_capita,
                            data = data_sw_fi_hydro, 
                            model = "within", 
                            effect = "individual", 
                            index = c("Country"),
                            method = "prais")  # Note: "prais" is available in some extensions

# Show model summary
#summary(model1_hydro_subset2)


robust_se <- vcovHC(model1_hydro_subset2, type = "HC1")
coeftest(model1_hydro_subset2, vcov = robust_se)



model1_hydro_subset3 <- plm(Fossil_energy_per_capita ~ Hydro_per_capita + ln_GDP_per_capita
                            +Urban_Population + Industry + Netimports_per_capita,
                            data = data_sw_fi_hydro, 
                            model = "within", 
                            effect = "individual", 
                            index = c("TIME"),
                            method = "prais")  # Note: "prais" is available in some extensions

# Show model summary
#summary(model1_hydro_subset3)


robust_se <- vcovHC(model1_hydro_subset3, type = "HC1")
coeftest(model1_hydro_subset3, vcov = robust_se)







# Filter the dataset Bioenergy for Sweden, Norway and Finland only
data_sw_fi_bio <- subset(data1, Country %in% c("Sweden", "Finland","Denmark"))

# Run the fixed effects (within) model with Prais-Winsten correction
library(plm)

model1_bioenergy_subset <- plm(Fossil_energy_per_capita ~ Bioenergy_per_capita + ln_GDP_per_capita
                               +Urban_Population+ Industry + Netimports_per_capita,
                    data = data_sw_fi_bio, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Note: "prais" is available in some extensions

# Show model summary
#summary(model1_bioenergy_subset)


dk_se <- vcovSCC(model1_bioenergy_subset, type = "HC0", maxlag = 2)
coeftest(model1_bioenergy_subset, vcov = dk_se)





data_sw_fi_nuclear <- subset(data1, Country %in% c("Sweden", "Finland"))

# Run the fixed effects (within) model with Prais-Winsten correction
library(plm)

model1_nuclear_subset <- plm(Fossil_energy_per_capita ~ Nuclear_heat_per_capita + ln_GDP_per_capita
                             +Urban_Population + Industry + Netimports_per_capita,
                               data = data_sw_fi_nuclear, 
                               model = "within", 
                               effect = "twoways", 
                               index = c("Country", "TIME"),
                               method = "prais")  # Note: "prais" is available in some extensions

# Show model summary
#summary(model1_nuclear_subset)


dk_se <- vcovSCC(model1_nuclear_subset, type = "HC0", maxlag = 2)
coeftest(model1_nuclear_subset, vcov = dk_se)


## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# Unload stargazer if loaded
detach("package:stargazer",unload=T)
# Delete it
remove.packages("stargazer")
# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
# Save back
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
install.packages("stargazer", repos = NULL, type="source")


library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

# --- Get robust or DK standard errors for each model and extract coefficients + SEs

# Model 1: Hydro (SE/NO/FI, twoway)
dk_se1 <- vcovSCC(model1_hydro_subset, type = "HC0", maxlag = 2)
coefs1 <- coeftest(model1_hydro_subset, vcov = dk_se1)

# Model 2: Hydro (SE/FI, twoway)
dk_se2 <- vcovSCC(model1_hydro_subset1, type = "HC0", maxlag = 2)
coefs2 <- coeftest(model1_hydro_subset1, vcov = dk_se2)

# Model 3: Hydro (SE/FI, individual FE)
robust_se3 <- vcovHC(model1_hydro_subset2, type = "HC1")
coefs3 <- coeftest(model1_hydro_subset2, vcov = robust_se3)

# Model 4: Hydro (SE/FI, individual FE, TIME as index — less standard, keep if needed)
robust_se4 <- vcovHC(model1_hydro_subset3, type = "HC1")
coefs4 <- coeftest(model1_hydro_subset3, vcov = robust_se4)

# Model 5: Bioenergy (SE/FI/DK)
dk_se5 <- vcovSCC(model1_bioenergy_subset, type = "HC0", maxlag = 2)
coefs5 <- coeftest(model1_bioenergy_subset, vcov = dk_se5)

# Model 6: Nuclear (SE/FI)
dk_se6 <- vcovSCC(model1_nuclear_subset, type = "HC0", maxlag = 2)
coefs6 <- coeftest(model1_nuclear_subset, vcov = dk_se6)


# --- Create Stargazer output
stargazer(model1_hydro_subset, model1_hydro_subset1, model1_hydro_subset2,
          model1_hydro_subset3, model1_bioenergy_subset, model1_nuclear_subset,
          type = "text",  # Change to "latex" or "html" if desired
          se = list(
            coefs1[, "Std. Error"],
            coefs2[, "Std. Error"],
            coefs3[, "Std. Error"],
            coefs4[, "Std. Error"],
            coefs5[, "Std. Error"],
            coefs6[, "Std. Error"]
          ),
          column.labels = c("Hydro: SE/FI/NO", "Hydro: SE/FI", "Hydro ind. FE", 
                            "Hydro ind. TIME", "Bioenergy", "Nuclear"),
          dep.var.caption = "DV: Fossil Energy per Capita",
          title = "Panel Models with Robust/Driscoll-Kraay Standard Errors",
          model.names = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"))





model1_hydro_urban <- plm(Fossil_energy_per_capita ~ Hydro_per_capita+ ln_GDP_per_capita
                    
                    +Urban_Population+Netimports_per_capita,
                    data = data1, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_hydro_urban)


##### Individual technologies 

model1__single <- plm(Fossil_energy_per_capita ~Hydro_per_capita+ ln_GDP_per_capita
                      +#Age_dependency_ratio
                        Industry+Netimports_per_capita,
                           data = data1, 
                           model = "within", 
                           effect = "twoways", 
                           index = c("Country", "TIME"),
                           method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1__single)


model2__single <- plm(Fossil_energy_per_capita ~ Wind_per_capita+ln_GDP_per_capita+#Age_dependency_ratio
                        Industry+Netimports_per_capita,
                           data = data1, 
                           model = "within", 
                           effect = "twoways", 
                           index = c("Country", "TIME"),
                           method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model2__single)



cor(data1$Industry,data1$Urban_Population)


model3__single <- plm(Fossil_energy_per_capita ~ Bioenergy_per_capita +ln_GDP_per_capita+#Age_dependency_ratio
                        Industry#+Netimports_per_capita
                      ,
                           data = data1, 
                           model = "within", 
                           effect = "twoways", 
                           index = c("Country", "TIME"),
                           method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model3__single)

data1$TIME1 <- integer(data1$TIME)

model4__single <- plm(Fossil_energy_per_capita ~ Nuclear_heat_per_capita + ln_GDP_per_capita+#Age_dependency_ratio
                        Industry+Netimports_per_capita+TIME,
                           data = data1, 
                           model = "within", 
                           effect = "individual", 
                           index = c("Country"),
                           method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model4__single)



model5__single <- plm(Fossil_energy_per_capita ~  Solar_photovoltaic_per_capita + ln_GDP_per_capita+#Age_dependency_ratio
                        +Industry+Netimports_per_capita,
                           data = data1, 
                           model = "within", 
                           effect = "twoways", 
                           index = c("Country", "TIME"),
                           method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model5__single)

model6__single <- plm(Fossil_energy_per_capita ~  Wind_Solar_combined + ln_GDP_per_capita+#Age_dependency_ratio
                        +Industry+Netimports_per_capita,
                      data = data1, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model6__single) 









































model1_all <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined++ Hydro_per_capita+ Nuclear_heat_per_capita + Bioenergy_per_capita+ ln_GDP_per_capita
                  
                  +Industry+Netimports_per_capita+Urban_Population,
                  data = data1, 
                  model = "within", 
                  effect = "twoways", 
                  index = c("Country","TIME"),
                  method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_all)


model1_non_hydro <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined+ Nuclear_heat_per_capita + Bioenergy_per_capita+ ln_GDP_per_capita
                        
                        +Industry+Netimports_per_capita+Urban_Population,
                        data = data1, 
                        model = "within", 
                        effect = "twoways", 
                        index = c("Country","TIME"),
                        method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_non_hydro)

model1_non_hydro_urban <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined+ Nuclear_heat_per_capita + Bioenergy_per_capita+ ln_GDP_per_capita
                              
                              +Industry+Netimports_per_capita,
                              data = data1, 
                              model = "within", 
                              effect = "twoways", 
                              index = c("Country","TIME"),
                              method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_non_hydro_urban)

model1_non_hydro_non_bio <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined+ Nuclear_heat_per_capita + ln_GDP_per_capita
                                
                                +Industry+Netimports_per_capita+Urban_Population,
                                data = data1, 
                                model = "within", 
                                effect = "twoways", 
                                index = c("Country","TIME"),
                                method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_non_hydro_non_bio)


model1_bio <- plm(Fossil_energy_per_capita ~ Bioenergy_per_capita + ln_GDP_per_capita
                  
                  +Industry
                  + Urban_Population
                  +Netimports_per_capita,
                  data = data1, 
                  model = "within", 
                  effect = "twoways", 
                  index = c("Country","TIME"),
                  method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_bio)



model1_singlehydro <- plm(Fossil_energy_per_capita ~ Hydro_per_capita + ln_GDP_per_capita
                          
                          
                          + Urban_Population
                          +Netimports_per_capita+Urban_Population,
                          data = data1, 
                          model = "within", 
                          effect = "twoways", 
                          index = c("Country","TIME"),
                          method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_singlehydro)




### POOOLED

model1_all <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined + Hydro_per_capita + Nuclear_heat_per_capita + Bioenergy_per_capita + ln_GDP_per_capita
                  
                  + Industry + Netimports_per_capita + Urban_Population,
                  data = data1, 
                  model = "pooling",   # pooled OLS instead of fixed effects
                  index = c("Country", "TIME"),
                  method = "prais")  # Prais-Winsten correction


summary(model1_all)


model1_non_hydro <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined + Nuclear_heat_per_capita + Bioenergy_per_capita + ln_GDP_per_capita
                        
                        + Industry + Netimports_per_capita + Urban_Population,
                        data = data1, 
                        model = "pooling",
                        index = c("Country", "TIME"),
                        method = "prais")


summary(model1_non_hydro)


model1_non_hydro_urban <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined + Nuclear_heat_per_capita + Bioenergy_per_capita + ln_GDP_per_capita
                              
                              + Industry + Netimports_per_capita,
                              data = data1, 
                              model = "pooling",
                              index = c("Country", "TIME"),
                              method = "prais")


summary(model1_non_hydro_urban)


model1_non_hydro_non_bio <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined + Nuclear_heat_per_capita + ln_GDP_per_capita
                                
                                + Industry + Netimports_per_capita + Urban_Population,
                                data = data1, 
                                model = "pooling",
                                index = c("Country", "TIME"),
                                method = "prais")


summary(model1_non_hydro_non_bio)


model1_bio <- plm(Fossil_energy_per_capita ~ Bioenergy_per_capita + ln_GDP_per_capita
                  
                  + Industry + Urban_Population + Netimports_per_capita,
                  data = data1, 
                  model = "pooling",
                  index = c("Country", "TIME"),
                  method = "prais")


summary(model1_bio)


model1_singlehydro <- plm(Fossil_energy_per_capita ~ Hydro_per_capita + ln_GDP_per_capita
                          
                          + Urban_Population + Netimports_per_capita,
                          data = data1, 
                          model = "pooling",
                          index = c("Country", "TIME"),
                          method = "prais")


summary(model1_singlehydro)



#RANDOM 

library(plm)

# Model 1: all variables
model1_all_re <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined + Hydro_per_capita + Nuclear_heat_per_capita + Bioenergy_per_capita + ln_GDP_per_capita
                     + Industry + Netimports_per_capita + Urban_Population,
                     data = data1, 
                     model = "random",
                     index = c("Country", "TIME"),
                     method = "prais")

summary(model1_all_re)


# Model 2: no hydro
model1_non_hydro_re <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined + Nuclear_heat_per_capita + Bioenergy_per_capita + ln_GDP_per_capita
                           + Industry + Netimports_per_capita + Urban_Population,
                           data = data1, 
                           model = "random",
                           index = c("Country", "TIME"),
                           method = "prais")

summary(model1_non_hydro_re)


# Model 3: no hydro, no urban population
model1_non_hydro_urban_re <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined + Nuclear_heat_per_capita + Bioenergy_per_capita + ln_GDP_per_capita
                                 + Industry + Netimports_per_capita,
                                 data = data1, 
                                 model = "random",
                                 index = c("Country", "TIME"),
                                 method = "prais")

summary(model1_non_hydro_urban_re)


# Model 4: no hydro, no bioenergy
model1_non_hydro_non_bio_re <- plm(Fossil_energy_per_capita ~ Wind_Solar_combined + Nuclear_heat_per_capita + ln_GDP_per_capita
                                   + Industry + Netimports_per_capita + Urban_Population,
                                   data = data1, 
                                   model = "random",
                                   index = c("Country", "TIME"),
                                   method = "prais")

summary(model1_non_hydro_non_bio_re)


# Model 5: bioenergy only plus controls
model1_bio_re <- plm(Fossil_energy_per_capita ~ Bioenergy_per_capita + ln_GDP_per_capita
                     + Industry + Urban_Population + Netimports_per_capita,
                     data = data1, 
                     model = "random",
                     index = c("Country", "TIME"),
                     method = "prais")

summary(model1_bio_re)


# Model 6: hydro only plus controls
model1_singlehydro_re <- plm(Fossil_energy_per_capita ~ Hydro_per_capita + ln_GDP_per_capita
                             + Urban_Population + Netimports_per_capita,
                             data = data1, 
                             model = "random",
                             index = c("Country", "TIME"),
                             method = "prais")

summary(model1_singlehydro_re)










