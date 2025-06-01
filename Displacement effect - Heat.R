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

# Load libraries
library(plm)
library(dplyr)
library(broom)
library(ggplot2)
library(readxl)
library(dplyr)
library(plm) 
library(sandwich)
library(lmtest)
library(car)


#load dataset for electricity, heat and control variables (population, GDP, Urban, Industry...)

data2 <- read_xlsx("Eurostat Datasets - Gross Electricity and Gross Heat Production.xlsx", sheet = "Gross Heat kWh")


summary(data2)

#Replace rows with NA values with 0
data2[is.na(data2)] <- 0





#regression model check 

model1_york <- plm(Fossil_energy ~ Wind + Hydro + Bioenergy + GDP_per_capita + Population, data=data2, index = c("Country", "TIME"))


# Print the summary of the model
summary(model1_york)



# Create per capita variables for all relevant variables

# Manually create per capita variables for all relevant variables

# Add new variables to the data1 dataset
data2$Fossil_energy_per_capita <- data2$Fossil_energy / data2$Population
data2$Electricity_per_capita <- data2$Electricity / data2$Population
data2$Solid_fossil_fuels_per_capita <- data2$Solid_fossil_fuels / data2$Population
data2$Oil_per_capita <- data2$Oil_and_petroleum_products / data2$Population
data2$natural_gas_per_capita <- data2$Natural_gas / data2$Population
data2$Hydro_per_capita <- data2$Hydro / data2$Population
data2$geothermal_per_capita <- data2$Geothermal / data2$Population
data2$Wind_per_capita <- data2$Wind / data2$Population
data2$Solar_thermal_per_capita <- data2$Solar_thermal / data2$Population
data2$Solar_photovoltaic_per_capita <- data2$Solar_photovoltaic / data2$Population
data2$Industrial_waste_non_renewable_per_capita <- data2$`Industrial_waste_(non-renewable)` / data2$Population
data2$Renewable_waste_per_capita <- data2$`Renewable_municipal_waste` / data2$Population
data2$Heat_pumps_per_capita <- data2$New_heatpumps / data2$Population
data2$Bioenergy_per_capita <- data2$Bioenergy / data2$Population
data2$Nuclear_heat_per_capita <- data2$Nuclear_heat / data2$Population
data2$Renewables_per_capita <- data2$Renewables_and_biofuels / data2$Population
data2$Solid_biofuels_per_capita <- data2$Primary_solid_biofuels /data2$Population



data2$Renewable_waste_only_per_capita <- data2$`Renewable_municipal_waste` / data2$Population
data2$Bioenergy_without_waste_per_capita <- data2$Bioenergy_per_capita - data2$Renewable_waste_per_capita

data2$Solid_biofuels_per_capita <- data2$`Primary_solid_biofuels` / data2$Population
data2$Bioenergy_other <- data2$Bioenergy_per_capita - data2$Solid_biofuels_per_capita



# Create GDP per capita squared and add to dataset
data2$GDP_per_capita_sq <- data2$GDP_per_capita^2

# Create GDP per capita cubed and add to dataset
data2$GDP_per_capita_cub <- data2$GDP_per_capita^3

# Create log GDP and add to dataset
data2$ln_GDP_per_capita <- log(data2$GDP_per_capita)

# Create LN GDP per capita squared and add to dataset
data2$ln_GDP_per_capita_sq <- log(data2$GDP_per_capita)^2

# Create GDP per capita cubed and add to dataset
data2$ln_GDP_per_capita_cub <- log(data2$GDP_per_capita)^3




# Create squared Urbanisation and add to dataset
data2$Urban_sq <- data2$Urban_Population^2

# Create variable for fossil fuels (oil + gas + coal) and add to dataset
data2$fossil_fuels_per_capita <- (data2$Solid_fossil_fuels + data2$Oil_and_petroleum_products + data2$Natural_gas) / data2$Population

# Create variable for fossil fuels (oil coal) and add to dataset
data2$oil_coal_per_capita <- (data2$Solid_fossil_fuels + data2$Oil_and_petroleum_products) / data2$Population


# Create a lagged variable (1-year lag)
data2$Renewables_per_capita_lag1 <- plm::lag(data2$Renewables_per_capita, k = 1)

# Create a lagged variable (2-year lag)
data2$Renewables_per_capita_lag2 <- plm::lag(data2$Renewables_per_capita, k = 2)

# Create a lagged variable (5-year lag)
data2$Renewables_per_capita_lag5 <- plm::lag(data2$Renewables_per_capita, k = 5)


######COMBINED VARIABLES

#data1$Wind_Solar_combined <- data1$Wind_per_capita + data1$Solar_photovoltaic_per_capita
#data1$Wind_Solar_Hydro_combined <- data1$Wind_per_capita + data1$Solar_photovoltaic_per_capita + data1$Hydro_per_capita
#data1$Non_hydro <- data1$Wind_per_capita + data1$Bioenergy_per_capita + data1$Solar_photovoltaic_per_capita

#data1$Intermittent <- data1$Wind_per_capita + data1$Solar_photovoltaic_per_capita
#data1$Dispatchable <- data1$Bioenergy_per_capita + data1$Hydro_per_capita



####correlation matrix ################################################

# All predictors (excluding the dependent variable Fossil_energy_per_capita)
predictors <- c("Fossil_energy_per_capita","Renewables_per_capita",
                #"Hydro_per_capita",
                #"Wind_per_capita", 
                #"Solar_photovoltaic_per_capita", 
                "Bioenergy_per_capita", 
                #"Nuclear_heat_per_capita",
                "Heat_pumps_per_capita",
                "Age_dependency_ratio", 
                "HDD", 
                "ln_GDP_per_capita", 
                "GDP_per_capita",
                #"GDP_per_capita_sq",
                #"GDP_per_capita_sq",
                "Urban_Population",
                #"Urban_sq"
                "Manufacturing",
                "Industry",
                #"Net_imports"
                "Coal_price",
                "Natural_gas_price"
)

# Create subset data frame with all predictors
X_data <- data2[, predictors]

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


#York model 1 

model1_york <- plm(Fossil_energy_per_capita ~ GDP_per_capita+Renewables_per_capita, data=data2, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model1_york)

#new model


model1 <- plm(Fossil_energy_per_capita ~ GDP_per_capita+Peat_per_capita+solar_thermal_per_capita+Heatpumps_per_capita+Industrial_waste_non_renewable_per_capita+non_renewable_per_capita+Bioenergy_per_capita+Industry+HDD, data=data2, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model1)


model2 <- plm(Fossil_energy_per_capita ~ GDP_per_capita+Peat_per_capita+Heatpumps_per_capita+Industrial_waste_non_renewable_per_capita+non_renewable_per_capita+Bioenergy_per_capita+Industry+HDD, data=data2, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model2)

#Robustness tests
bptest(model2)

#white test
coeftest(model2,vcov=vcovHC(model2,method="white1", type="HC4"))

#serial correlation test
pbgtest(model2)



#Try to compare with random effects:
#re_model2 <- plm(Fossil_energy_per_capita ~ GDP_per_capita+Peat_per_capita+Heatpumps_per_capita+Industrial_waste_non_renewable_per_capita+non_renewable_per_capita+Bioenergy_per_capita+Industry+HDD, data=data2, model = "random", effect = "twoways")
#summary(re_model2)

#Compare with pooled OLS 


model3 <- plm(Fossil_energy_per_capita ~ ln_GDP_per_capita+Bio_waste_per_capita+Renewable_waste_per_capita+Urban_Population+Industry+Age_dependency_ratio, data=data2, index = c("Country", "TIME"), model = "within", effect = "twoways")


# Print the summary of the model
summary(model3)




###########ALL RENEWABLES IN ONE######################################
### For results 

model1_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita+Industry+Urban_Population+HDD,
                    data = data2, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_RENEW)

model1_RENEW_ols <- lm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita+Industry+Urban_Population+HDD,
                    data = data2,)  # Specify Prais-Winsten correction for AR(1)


summary(model1_RENEW_ols)


library(car)
vif(model1_RENEW_ols)


### Fjerne Urban, da korreleret

model2_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita +ln_GDP_per_capita+Industry+HDD
                    ,
                    data = data2, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model2_RENEW)

model1_RENEW_ols1 <- lm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita+Industry+HDD,
                       data = data2,)  # Specify Prais-Winsten correction for AR(1)


summary(model1_RENEW_ols1)


library(car)
vif(model1_RENEW_ols1)


### fjerne Urban

model3_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita+Urban_Population+HDD,
                    data = data2, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model3_RENEW)

model1_RENEW_ols2 <- lm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita+Urban_Population+HDD,
                        data = data2,)  # Specify Prais-Winsten correction for AR(1)


summary(model1_RENEW_ols2)


library(car)
vif(model1_RENEW_ols2)


### fjerne Industry

model4_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita+HDD,
                    data = data2, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model4_RENEW)



### Prøve fjerne HDD

model5_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita,
                    data = data2, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model5_RENEW)

### no HDD plus Industry

model6_RENEW <- plm(Fossil_energy_per_capita ~ Renewables_per_capita + ln_GDP_per_capita+Industry,
                    data = data2, 
                    model = "within", 
                    effect = "twoways", 
                    index = c("Country", "TIME"),
                    method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model6_RENEW)


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





###################################################################################################################
#SPLIT UP INTO TECHNOLOGIES 
library(sandwich)


model1_single <- plm(Bioenergy_per_capita ~
                         
                         Heat_pumps_per_capita+Fossil_energy_per_capita +ln_GDP_per_capita+
                    + Industry
                   
                        +HDD+ Natural_gas_price,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_single, vcov = function(x) vcovSCC(x, type = "HC0", maxlag = 2))




model2_single <- plm(Heat_pumps_per_capita ~Bioenergy_per_capita +Fossil_energy_per_capita + ln_GDP_per_capita
                      + Industry
                        +HDD+Natural_gas_price,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)



summary(model2_single, vcov = function(x) vcovSCC(x, type = "HC0", maxlag = 2))

model3_single <- plm(Bioenergy_per_capita ~ Heat_pumps_per_capita+Fossil_energy_per_capita+ ln_GDP_per_capita
                      
                        +HDD+Natural_gas_price,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)



summary(model3_single, vcov = function(x) vcovSCC(x, type = "HC0", maxlag = 2))




model4_single <- plm(Heat_pumps_per_capita ~Bioenergy_per_capita 
                      + Fossil_energy_per_capita+ ln_GDP_per_capita
                      +#Age_dependency_ratio
                       +HDD+Natural_gas_price,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)



summary(model4_single, vcov = function(x) vcovSCC(x, type = "HC0", maxlag = 2))



model5_single <- plm(Bioenergy_per_capita ~
                      
                        
                      + Heat_pumps_per_capita+ Fossil_energy_per_capita
                      + ln_GDP_per_capita
                      #+Industry
                      +Natural_gas_price
                      
                      ,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)



summary(model5_single, vcov = function(x) vcovSCC(x, type = "HC0", maxlag = 2))


model6_single <- plm(Heat_pumps_per_capita ~
                        +
                        Bioenergy_per_capita 
                      + Fossil_energy_per_capita
                      + ln_GDP_per_capita
                      #+Industry
                      +Natural_gas_price
                      
                      ,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)



summary(model6_single, vcov = function(x) vcovSCC(x, type = "HC0", maxlag = 2))


###########################SPLIT INTO BIOFUELS AND BIOENERGY


model1_solid_bio <- plm(Fossil_energy_per_capita ~
                        
                      Bioenergy_other
                        + Solid_biofuels_per_capita
                      + Heat_pumps_per_capita
                      + ln_GDP_per_capita
                      + Industry
                      +HDD
                      ,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_solid_bio)


####HIGH CORRELATION BETWEEN WASTE AND HDD, try to remove HDD


model2__single_waste <- plm(Fossil_energy_per_capita ~Bioenergy_without_waste_per_capita + Renewable_waste_per_capita  + Heat_pumps_per_capita+ ln_GDP_per_capita
                     ,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model2__single_waste)

ols_model2__single <- lm(Fossil_energy_per_capita ~
                           Bioenergy_without_waste_per_capita + Renewable_waste_per_capita  
                         + Heat_pumps_per_capita+ ln_GDP_per_capita,
                         data = data2,) 


summary(ols_model2__single)
vif(ols_model2__single)




model3__single <- plm(Fossil_energy_per_capita ~Bioenergy_without_waste_per_capita + Renewable_waste_per_capita + Heat_pumps_per_capita+Natural_gas_price+ ln_GDP_per_capita+Surface_temperature,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model3__single)


#### surface temperature gives the same 


model4__single <- plm(Fossil_energy_per_capita ~Renewable_waste_per_capita + 
                        +Natural_gas_price
                      + ln_GDP_per_capita+Urban_Population+Manufacturing
                      +Heat_kWh_per_capita
                      ,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model4__single)


#### surface temperature gives the same 


ols_model4__single <- lm(Fossil_energy_per_capita ~Renewable_waste_per_capita + 
                           +Natural_gas_price
                         + ln_GDP_per_capita+Urban_Population+#Manufacturing
                         +Heat_kWh_per_capita,
                         data = data2,) 


summary(ols_model4__single)
vif(ols_model4__single)


###################Bioenergy single minus waste

model1_indi <- plm(Fossil_energy_per_capita ~Bioenergy_per_capita + 
                        
                      + ln_GDP_per_capita+Natural_gas_price+HDD
                      ,
                      data = data2, 
                      model = "within", 
                      effect = "twoways", 
                      index = c("Country", "TIME"),
                      method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model1_indi)

model2_indi <- plm(Fossil_energy_per_capita ~Bioenergy_without_waste_per_capita + 
                     
                   + ln_GDP_per_capita+Natural_gas_price+HDD
                   
                   ,
                   data = data2, 
                   model = "within", 
                   effect = "twoways", 
                   index = c("Country", "TIME"),
                   method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model2_indi)

model3_indi <- plm(Fossil_energy_per_capita ~Heat_pumps_per_capita + 
                     
                   + ln_GDP_per_capita+HDD+Natural_gas_price
                   ,
                   data = data2, 
                   model = "within", 
                   effect = "twoways", 
                   index = c("Country", "TIME"),
                   method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model3_indi)

model4_indi <- plm(Fossil_energy_per_capita ~Solid_biofuels_per_capita + 
                     
                     + ln_GDP_per_capita+HDD
                   #+Heat_kWh_per_capita 
                   
                   ,
                   data = data2, 
                   model = "within", 
                   effect = "twoways", 
                   index = c("Country", "TIME"),
                   method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model4_indi)



model5_indi <- plm(Fossil_energy_per_capita ~Renewable_waste_per_capita + 
                     
                   + ln_GDP_per_capita+HDD
                   #+Heat_kWh_per_capita 
                   
                   ,
                   data = data2, 
                   model = "within", 
                   effect = "twoways", 
                   index = c("Country", "TIME"),
                   method = "prais")  # Specify Prais-Winsten correction for AR(1)


summary(model5_indi)

model5_indi_ols <- lm(Fossil_energy_per_capita ~Renewable_waste_per_capita + 
                     
                     + ln_GDP_per_capita+HDD
                   #+Heat_kWh_per_capita 
                   
                   ,
                   data = data2, 


summary(model6_indi_ols)

vif(model6_indi_ols)








#####just to test AIC 

model_lm_gdp <- lm(Fossil_energy_per_capita ~ GDP_per_capita + Bioenergy_without_waste_per_capita + 
                     +Natural_gas_price+Industry, data = data2)
model_lm_lngdp <- lm(Fossil_energy_per_capita ~ ln_GDP_per_capita + Bioenergy_without_waste_per_capita + 
                       +Natural_gas_price+Industry, data = data2)

AIC(model_lm_gdp)
AIC(model_lm_lngdp)


model_lm_gdp1 <- lm(Fossil_energy_per_capita ~ ln_GDP_per_capita +Renewable_waste_per_capita+ Bioenergy_without_waste_per_capita + Heat_pumps_per_capita+
                      Natural_gas_price++Urban_Population, data = data2)
model_lm_lngdp1 <- lm(Fossil_energy_per_capita ~ ln_GDP_per_capita+ln_GDP_per_capita_sq +Renewable_waste_per_capita+ Bioenergy_without_waste_per_capita +Heat_pumps_per_capita+ 
                       Natural_gas_price+Urban_Population, data = data2)

AIC(model_lm_gdp1)
AIC(model_lm_lngdp1)

#ln GDP sq lower AIC, Urban Population lower AIC than Industry, even lower when removing natural, lowest with all renewable variable
