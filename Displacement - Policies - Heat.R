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
#install.packages("marginaleffects")

# Load libraries
library(plm)
library(dplyr)
library(broom)
library(ggplot2)
library(readxl)
library(dplyr)
library(fixest)
library(car)
library(marginaleffects)
#load dataset with policies 

data1 <- read_xlsx("EEA_Policies.xlsx", sheet = "R-data Heat")


# Vector of relevant prefixes
policy_prefixes <- c("BAN", "GRA", "INF", "POL", "PUB", 
                     "REG", "SUB", "TAX", "TEN", "TRA", "VOL","NON")

# Initialize an empty list to store counts
policy_counts <- list()

# Loop through each prefix and count how many columns start with it
for (prefix in policy_prefixes) {
  matching_cols <- grep(paste0("^", prefix), names(data1), value = TRUE)
  policy_counts[[prefix]] <- length(matching_cols)
}

# Display the counts
policy_counts

#Now add IPCC categories together. Ban = regulation, Grant = Subsidies and other incentives, 

# Define mapping from prefixes to IPCC categories
ipcc_mapping <- list(
  "Regulations and Standards" = c("BAN","PUB", "REG"),
  "Subsidies and Other Incentives" = c("GRA", "SUB", "TEN"),
  "Taxes and Charges" = c("TAX"),
  "Tradable Permits" = c("TRA"),
  "Voluntary Agreements and Information" = c("VOL", "INF", "POL")
)

# Sum the counts per IPCC category
ipcc_counts <- sapply(ipcc_mapping, function(prefixes) {
  sum(unlist(policy_counts[prefixes]), na.rm = TRUE)
})

# Show the results
ipcc_counts

# Total number of policies (excluding NON)
total_policies <- sum(ipcc_counts)
cat("\nTotal number of policies (excluding 'NON'):", total_policies, "\n")


# 1. Create new variables by grouping columns based on their prefix
data1$Regulations_and_Standards <- rowSums(data1[, grep("^BAN", colnames(data1))], na.rm = TRUE) + 
  rowSums(data1[, grep("^PUB", colnames(data1))], na.rm = TRUE) +
  rowSums(data1[, grep("^REG", colnames(data1))], na.rm = TRUE)

data1$Subsidies_and_Other_Incentives <- rowSums(data1[, grep("^GRA", colnames(data1))], na.rm = TRUE) + 
  rowSums(data1[, grep("^SUB", colnames(data1))], na.rm = TRUE) +
  rowSums(data1[, grep("^TEN", colnames(data1))], na.rm = TRUE)

data1$Taxes_and_Charges <- rowSums(data1[, grep("^TAX", colnames(data1))], na.rm = TRUE)

data1$Tradable_Permits <- rowSums(data1[, grep("^TRA", colnames(data1))], na.rm = TRUE)

data1$Voluntary_Agreements_and_Information <- rowSums(data1[, grep("^VOL", colnames(data1))], na.rm = TRUE) + 
  rowSums(data1[, grep("^INF", colnames(data1))], na.rm = TRUE) +
  rowSums(data1[, grep("^POL", colnames(data1))], na.rm = TRUE)



data1$Regulations_and_Standards <- 
  rowSums(data1[, grep("^BAN", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^PUB", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^REG", names(data1)), drop = FALSE], na.rm = TRUE)

data1$Subsidies_and_Other_Incentives <- 
  rowSums(data1[, grep("^GRA", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^SUB", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^TEN", names(data1)), drop = FALSE], na.rm = TRUE)

data1$Taxes_and_Charges <- 
  rowSums(data1[, grep("^TAX", names(data1)), drop = FALSE], na.rm = TRUE)

data1$Tradable_Permits <- 
  rowSums(data1[, grep("^TRA", names(data1)), drop = FALSE], na.rm = TRUE)

data1$Voluntary_Agreements_and_Information <- 
  rowSums(data1[, grep("^VOL", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^INF", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^POL", names(data1)), drop = FALSE], na.rm = TRUE)

data1[, c("Regulations_and_Standards", 
          "Subsidies_and_Other_Incentives", 
          "Taxes_and_Charges", 
          "Tradable_Permits", 
          "Voluntary_Agreements_and_Information")]

# Convert policy groups and controls to numeric (if not already)
data1$Regulations_and_Standards <- as.numeric(data1$Regulations_and_Standards)
data1$Subsidies_and_Other_Incentives <- as.numeric(data1$Subsidies_and_Other_Incentives)
data1$Taxes_and_Charges <- as.numeric(data1$Taxes_and_Charges)
data1$Tradable_Permits <- as.numeric(data1$Tradable_Permits)
data1$Voluntary_Agreements_and_Information <- as.numeric(data1$Voluntary_Agreements_and_Information)

# Manually create per capita variables for all relevant variables

# Add new variables to the data1 dataset
data1$Solid_fossil_fuels_per_capita <- data1$Solid_fossil_fuels / data1$Population
data1$Oil_per_capita <- data1$Oil_and_petroleum_products / data1$Population
data1$natural_gas_per_capita <- data1$Natural_gas / data1$Population
data1$Hydro_per_capita <- data1$Hydro / data1$Population
data1$Wind_per_capita <- data1$Wind / data1$Population
data1$Solar_photovoltaic_per_capita <- data1$Solar_photovoltaic / data1$Population
#data1$Industrial_waste_non_renewable_per_capita <- data1$`Industrial_waste_(non-renewable)` / data1$Population
data1$Bioenergy_per_capita <- data1$Bioenergy / data1$Population
#data1$Nuclear_heat_per_capita <- data1$Nuclear_heat / data1$Population
data1$Renewables_per_capita <- data1$Renewables_and_biofuels / data1$Population
data1$Netimports_per_capita <- data1$Net_imports / data1$Population
data1$Total_per_capita <- data1$Total/data1$Population

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

#GDP per capita growth 
data1$GDP_growth_per_capita <- ave(data1$GDP_per_capita, data1$Country, FUN = function(x) c(NA, diff(x) / head(x, -1) * 100))




# Create squared Urbanisation and add to dataset
data1$Urban_sq <- data1$Urban_Population^2

# Create variable for fossil fuels (oil + gas + coal) and add to dataset
data1$Fossil_energy_per_capita <- data1$Fossil_energy

# Create variable for fossil fuels (oil coal) and add to dataset
data1$oil_coal_per_capita <- (data1$Solid_fossil_fuels + data1$Oil_and_petroleum_products) / data1$Population

data1$Fossil_share <- (data1$Fossil_energy / data1$Total)


# Create a total policy count by summing the five categories
data1$Total_Policies <- data1$Regulations_and_Standards +
  data1$Subsidies_and_Other_Incentives +
  data1$Taxes_and_Charges +
  data1$Tradable_Permits +
  data1$Voluntary_Agreements_and_Information

head(data1$Total_Policies)
summary(data1$Total_Policies)




pdata <- pdata.frame(data1, index = c("Country", "TIME"))



# All predictors (excluding the dependent variable Fossil_energy_per_capita)
predictors <- c("Fossil_energy_per_capita","Fossil_share","Renewables_per_capita","Total_Policies", "Subsidies_and_Other_Incentives","Taxes_and_Charges",
                "Regulations_and_Standards", 
                "Tradable_Permits", 
                "Voluntary_Agreements_and_Information",
                #"Hydro_per_capita",
                #"Wind_per_capita", 
                #"Solar_photovoltaic_per_capita", 
                #"Bioenergy_per_capita", 
                #"Nuclear_heat_per_capita",
                #"Heat_pumps_per_capita",
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
X_data <- data1[, predictors]

# Compute correlation matrix
cor_matrix <- cor(X_data, use = "pairwise.complete.obs")

# Plot correlation heatmap

# Plot upper triangle of the correlation matrix with enhancements
corrplot(cor_matrix, method = "color", 
         type = "upper",               # Only show upper triangle
         #order = "hclust",             # Cluster similar variables
         tl.cex = 0.6,                 # Label size
         number.cex = 0.65,             # Number size
         number.digits = 2,           # Round to 2 digits
         addCoef.col = "black",       # Add correlation numbers
         col = colorRampPalette(c("blue", "white", "red"))(200)) 




# Select the relevant variables used in your model
selected_vars <- data1[, c("Fossil_share","Total_Policies", "Subsidies_and_Other_Incentives","Taxes_and_Charges",
                           "Regulations_and_Standards", 
                           "Tradable_Permits", 
                           "Voluntary_Agreements_and_Information",#"Subsidies_and_Tender","Grants",
                           "GDP_per_capita", "ln_GDP_per_capita",
                           "Industry", "Urban_Population",
                           "Netimports_per_capita","HDD","Natural_gas_price","Heat_kWh_per_capita")]

# Remove rows with NA values to avoid errors in correlation calculation
selected_vars <- na.omit(selected_vars)

# Compute the correlation matrix
cor_matrix <- cor(selected_vars, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)





model1 <- plm(Fossil_share ~ 
                Regulations_and_Standards + 
                Subsidies_and_Other_Incentives + 
                Taxes_and_Charges + 
                Tradable_Permits + 
                Voluntary_Agreements_and_Information + 
                GDP_per_capita + 
                Industry + 
                HDD,
              data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")  # Specify Prais-Winsten correction for AR(1)

summary(model1)


model2 <- plm(Fossil_share ~ GDP_per_capita+Regulations_and_Standards + 
                Subsidies_and_Other_Incentives + 
                Taxes_and_Charges + 
                Tradable_Permits + 
                #Voluntary_Agreements_and_Information +Industry
                +Urban_Population+Manufacturing +HDD+Natural_gas_price, data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")


# Print the summary of the model
summary(model2)


model3 <- plm(Fossil_share ~ ln_GDP_per_capita+Regulations_and_Standards + 
                Subsidies_and_Other_Incentives + 
                Taxes_and_Charges + 
                Voluntary_Agreements_and_Information+Netimports_per_capita+Industry+Urban_Population, data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")


# Print the summary of the model
summary(model3)

model3_1 <- plm(Fossil_share ~ GDP_per_capita+Regulations_and_Standards + 
                  Subsidies_and_Other_Incentives + 
                  Taxes_and_Charges + 
                  Voluntary_Agreements_and_Information+Netimports_per_capita+Industry+Urban_Population, data = data1, 
                model = "within", 
                effect = "twoways", 
                index = c("Country", "TIME"),
                method = "prais")


# Print the summary of the model
summary(model3_1)


model4 <- plm(Fossil_share ~ ln_GDP_per_capita+Regulations_and_Standards + 
                Taxes_and_Charges + 
                Voluntary_Agreements_and_Information+Netimports_per_capita+Industry+Urban_Population, data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")


# Print the summary of the model
summary(model4)


model5 <- plm(Fossil_share ~ ln_GDP_per_capita+Regulations_and_Standards + 
                Taxes_and_Charges + 
                Voluntary_Agreements_and_Information+Netimports_per_capita+Industry+Urban_Population+Manufacturing+Industry+Natural_gas_price+Coal_price, data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")

# Print the summary of the model
summary(model5)

model6 <- plm(Fossil_share ~ ln_GDP_per_capita+ Regulations_and_Standards + Subsidies_and_Other_Incentives+#Voluntary_Agreements_and_Information+
                Taxes_and_Charges +Netimports_per_capita+Industry+Natural_gas_price, data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")


# Print the summary of the model
summary(model6)


model7 <- plm(Fossil_share ~ GDP_per_capita+ Total_Policies +Industry+Manufacturing+Coal_price+Natural_gas_price+HDD, data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")


# Print the summary of the model
summary(model7)


model8 <- plm(Fossil_share ~ GDP_per_capita+ Total_Policies +Netimports_per_capita+Urban_Population+Industry+Natural_gas_price, data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")

# Print the summary of the model
summary(model8)

#install.packages("tseries")
library(tseries)
library(stargazer)
library(plm)
library(lmtest)
library(sandwich)


# Breusch-Pagan test for heteroscedasticity (model7 and model8)
bp_test_model7 <- bptest(model7)
bp_test_model8 <- bptest(model8)

# Breusch-Godfrey test for autocorrelation (model7 and model8)
bg_test_model7 <- pbgtest(model7)
bg_test_model8 <- pbgtest(model8)

# Shapiro-Wilk test for normality (model7 and model8)
shapiro_test_model7 <- shapiro.test(residuals(model7))
shapiro_test_model8 <- shapiro.test(residuals(model8))



model9 <- plm(Fossil_share ~ ln_GDP_per_capita+Regulations_and_Standards + 
                Subsidies_and_Other_Incentives + 
                Taxes_and_Charges + 
                Tradable_Permits + 
                Voluntary_Agreements_and_Information+Netimports_per_capita+Industry+Urban_Population+Natural_gas_price+Coal_price, data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"),
              method = "prais")


# Print the summary of the model
summary(model9)

model10 <- plm(Fossil_share ~ GDP_per_capita+Regulations_and_Standards + 
                 Subsidies_and_Other_Incentives + 
                 Taxes_and_Charges + 
                 Tradable_Permits + 
                 Voluntary_Agreements_and_Information+Netimports_per_capita+Industry+Urban_Population+Natural_gas_price+Coal_price, data = data1, 
               model = "within", 
               effect = "twoways", 
               index = c("Country", "TIME"),
               method = "prais")


# Print the summary of the model
summary(model10)

data1$Market_Instrument <- data1$Subsidies_and_Other_Incentives + data1$Taxes_and_Charges

data1$Market_Instrument_plus_permits <- data1$Subsidies_and_Other_Incentives + data1$Taxes_and_Charges + data1$Tradable_Permits


model11 <- plm(Fossil_share ~ ln_GDP_per_capita+Regulations_and_Standards + 
                 Market_Instrument+
                 Tradable_Permits + 
                 Voluntary_Agreements_and_Information+
                 Netimports_per_capita+Industry+Natural_gas_price+Coal_price, data = data1, 
               model = "within", 
               effect = "twoways", 
               index = c("Country", "TIME"),
               method = "prais")


# Print the summary of the model
summary(model11)

# Run a standard linear model without fixed effects to compute VIF
lm_vif_model <- lm(Fossil_share ~ ln_GDP_per_capita + Regulations_and_Standards+Market_Instrument + 
                     Tradable_Permits +Voluntary_Agreements_and_Information+ Netimports_per_capita + 
                     Industry + Natural_gas_price, 
                   data = data1)

# Calculate VIF
library(car)
vif(lm_vif_model)


#######################################################################################################
#FRACTIONAL LOGIT

by(data1$Total_Policies, data1$Country, sd)



library(ggplot2)
ggplot(data1, aes(x = Total_Policies, y = Fossil_share)) +
  geom_point() +
  facet_wrap(~Country) +
  geom_smooth(method = "loess", se = FALSE, color = "blue")



#####TOTAL POLICIES ##################################################################################


model1_fraclogit <- feglm(Fossil_share ~ Total_Policies 
                          + ln_GDP_per_capita 
                          + HDD
                          + Industry 
                          #+ Urban_Population 
                          + Natural_gas_price
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME")) 

#summary(model1_fraclogit)
summary(model1_fraclogit, vcov = DK(2))


# Extract deviance from the model
deviance_value <- model1_fraclogit$deviance

# Get the number of parameters
num_params <- length(coef(model1_fraclogit))

# Calculate AIC manually
AIC_manual <- 2 * num_params - 2 * deviance_value

# Print the manually calculated AIC
print(AIC_manual)







# Fit the full model
full_model <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + HDD + Industry + Urban_Population + Natural_gas_price
                    | Country + TIME,
                    data = data1,
                    family = quasibinomial("logit"),
                    panel.id = c("Country", "TIME"))

# Fit the reduced model (without Urban_Population)
reduced_model <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + HDD + Industry + Natural_gas_price
                       | Country + TIME,
                       data = data1,
                       family = quasibinomial("logit"),
                       panel.id = c("Country", "TIME"))

# Compare AIC and BIC
AIC(full_model, reduced_model)
BIC(full_model, reduced_model)









#############################################################################################
# ALL POLICES AT ONCE, individually


# Fit fractional logit model with two-way fixed effects
model1.1_fraclogit <- feglm(Fossil_share ~ Regulations_and_Standards
                            #+Subsidies_and_Other_Incentives
                            +Taxes_and_Charges+Tradable_Permits#
                            +Voluntary_Agreements_and_Information
                          #+ Nuclear_heat_per_capita
                          + ln_GDP_per_capita 
                          #+ HDD
                          + Industry 
                          #+ Urban_Population 
                          + Natural_gas_price
                          +Heat_kWh_per_capita
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"))

# Print model summary
#summary(model2_fraclogit)
summary(model1.1_fraclogit, vcov = "standard")
summary(model1.1_fraclogit, vcov = ~ Country)


model1.1_fraclogit_ols <- lm(Fossil_share ~ Regulations_and_Standards+Subsidies_and_Other_Incentives
                               +Taxes_and_Charges+Tradable_Permits+Voluntary_Agreements_and_Information
                            #+ Nuclear_heat_per_capita
                            + ln_GDP_per_capita 
                            + HDD
                            + Industry 
                            + Urban_Population 
                            + Natural_gas_price
                            #+Heat_kWh_per_capita
                            ,
                            data = data1,)

# Print model summary
#summary(model2_fraclogit)
summary(model1.1_fraclogit_ols, vcov = "standard")



vif(model1.1_fraclogit_ols)

### HIGH CORRELATION, RUN INDIVIDUALLY 


######################################################################################
#Individual policies

# Fit fractional logit model with two-way fixed effects
model2.1_fraclogit <- feglm(Fossil_share ~ Regulations_and_Standards 
                          #+ Nuclear_heat_per_capita
                          + ln_GDP_per_capita 
                          + HDD
                          + Industry 
                          #+ Urban_Population 
                          + Natural_gas_price
                          +Heat_kWh_per_capita
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.1_fraclogit, vcov = DK(2))

model2.2_fraclogit <- feglm(Fossil_share ~ Taxes_and_Charges
                          
                          + ln_GDP_per_capita 
                          + HDD
                          + Industry 
                          #+ Urban_Population 
                          + Natural_gas_price
                          +Heat_kWh_per_capita
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.2_fraclogit, vcov = DK(2))



model2.3_fraclogit <- feglm(Fossil_share ~ Subsidies_and_Other_Incentives
                          
                          + ln_GDP_per_capita 
                          + HDD
                          + Industry 
                          #+ Urban_Population 
                          + Natural_gas_price
                          +Heat_kWh_per_capita
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

summary(model2.3_fraclogit, vcov = DK(2))

model2.4_fraclogit <- feglm(Fossil_share ~ Tradable_Permits
                            
                            + ln_GDP_per_capita 
                            + HDD
                            + Industry 
                            #+ Urban_Population 
                            + Natural_gas_price
                            +Heat_kWh_per_capita
                            | Country + TIME,
                            data = data1,
                            family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.4_fraclogit, vcov = DK(2))



model2.5_fraclogit <- feglm(Fossil_share ~ Voluntary_Agreements_and_Information
                            
                            + ln_GDP_per_capita 
                            + HDD
                            + Industry 
                            #+ Urban_Population 
                            + Natural_gas_price
                            +Heat_kWh_per_capita
                            | Country + TIME,
                            data = data1,
                            family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.5_fraclogit, vcov = DK(2))



# Function to compute APE for a given model and data
calculate_ape_for_model <- function(model, data) {
  # Get predicted probabilities (fitted values)
  phat <- predict(model, type = "response")
  
  # Get the coefficients
  betas <- coef(model)
  
  # Compute APE for each variable
  APE_values <- sapply(names(betas), function(var) {
    if (var != "(Intercept)") {
      betas[var] * mean(phat * (1 - phat))
    } else {
      NA
    }
  })
  
  return(APE_values)
}

# Calculate APE for each model
ape_model1 <- calculate_ape_for_model(model2_fraclogit, data1)
ape_model2 <- calculate_ape_for_model(model2.2_fraclogit, data1)
ape_model3 <- calculate_ape_for_model(model2.3_fraclogit, data1)
ape_model4 <- calculate_ape_for_model(model2.4_fraclogit, data1)
ape_model5 <- calculate_ape_for_model(model2.5_fraclogit, data1)

# Print the APE for each model
print(ape_model1)
print(ape_model2)
print(ape_model3)
print(ape_model4)
print(ape_model5)


# Create a data frame for the APE results
ape_results <- data.frame(
  Variable = c("Regulations_and_Standards", "ln_GDP_per_capita", "HDD", "Heat_kWh_per_capita"),
  Model_1_Regulations = c(ape_model1["Regulations_and_Standards.Regulations_and_Standards"], 
                          ape_model1["ln_GDP_per_capita.ln_GDP_per_capita"], 
                          ape_model1["HDD.HDD"], 
                          ape_model1["Heat_kWh_per_capita.Heat_kWh_per_capita"]),
  
  Model_2_Taxes = c(ape_model2["Taxes_and_Charges.Taxes_and_Charges"], 
                    ape_model2["ln_GDP_per_capita.ln_GDP_per_capita"], 
                    ape_model2["HDD.HDD"], 
                    ape_model2["Heat_kWh_per_capita.Heat_kWh_per_capita"]),
  
  Model_3_Subsidies = c(ape_model3["Subsidies_and_Other_Incentives.Subsidies_and_Other_Incentives"], 
                        ape_model3["ln_GDP_per_capita.ln_GDP_per_capita"], 
                        ape_model3["HDD.HDD"], 
                        ape_model3["Heat_kWh_per_capita.Heat_kWh_per_capita"]),
  
  Model_4_Tradable = c(ape_model4["Tradable_Permits.Tradable_Permits"], 
                       ape_model4["ln_GDP_per_capita.ln_GDP_per_capita"], 
                       ape_model4["HDD.HDD"], 
                       ape_model4["Heat_kWh_per_capita.Heat_kWh_per_capita"]),
  
  Model_5_Voluntary = c(ape_model5["Voluntary_Agreements_and_Information.Voluntary_Agreements_and_Information"], 
                        ape_model5["ln_GDP_per_capita.ln_GDP_per_capita"], 
                        ape_model5["HDD.HDD"], 
                        ape_model5["Heat_kWh_per_capita.Heat_kWh_per_capita"])
)

# Print the data frame to inspect
print(ape_results)

# Now, use stargazer to print the results
library(stargazer)

stargazer(ape_results, type = "text", summary = FALSE, rownames = TRUE, 
          column.labels = c("Model 1: Regulations", "Model 2: Taxes", "Model 3: Subsidies", 
                            "Model 4: Tradable", "Model 5: Voluntary"))







# 1. Get predicted values (fitted probabilities)
data1$phat <- predict(model1_fraclogit, type = "response")

# 2. Get coefficients (excluding fixed effects)
betas <- coef(model1_fraclogit)

# 3. Manually compute marginal effects for each observation
# Formula: beta_j * phat * (1 - phat)
data1$APE_Total_Policies <- betas["Total_Policies"] * data1$phat * (1 - data1$phat)
data1$APE_ln_GDP_per_capita <- betas["ln_GDP_per_capita"] * data1$phat * (1 - data1$phat)
data1$APE_Industry <- betas["Industry"] * data1$phat * (1 - data1$phat)
data1$APE_Urban_Population <- betas["Urban_Population"] * data1$phat * (1 - data1$phat)
# If you include other variables, just add more lines similarly

# 4. Average marginal effects (APE)
APE <- colMeans(data1[, grep("^APE_", names(data1))])
print(APE)

# Compute APEs in a compact way
phat <- predict(model1_fraclogit, type = "response")
betas <- coef(model1_fraclogit)

APE <- sapply(names(betas), function(var) {
  betas[var] * mean(phat * (1 - phat))
})

round(APE, 4)





# Predicted values (on response scale)
p_hat <- predict(model1_fraclogit, type = "response")

# Derivative of logistic function
lambda_prime <- p_hat * (1 - p_hat)

# Model matrix (excluding fixed effects)
X <- model.matrix(~ Total_Policies + ln_GDP_per_capita + Industry + 
                    Urban_Population + Heat_kWh_per_capita, data = data1)

# Coefficients
beta <- coef(model1_fraclogit)

# Multiply each column of X by lambda_prime, then by beta
AMEs <- colMeans(sweep(X, 1, lambda_prime, "*") * beta)

# Display rounded results
round(AMEs, 5)







# 2. Define a function to extract APEs
compute_ape <- function(model) {
  phat <- predict(model, type = "response")
  betas <- coef(model)
  ape <- sapply(names(betas), function(var) {
    betas[var] * mean(phat * (1 - phat))
  })
  return(ape)
}

# 3. Get point estimates
APE_point <- compute_ape(model1_fraclogit)

# 4. Bootstrap to get standard errors
set.seed(123)
n_boot <- 200  # increase to 1000+ for publication-quality
boot_APEs <- replicate(n_boot, {
  idx <- sample(nrow(data1), replace = TRUE)
  boot_data <- data1[idx, ]
  tryCatch({
    boot_model <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita +
                          Industry + Urban_Population |
                          Country + TIME,
                        data = boot_data, family = quasibinomial("logit"))
    compute_ape(boot_model)
  }, error = function(e) rep(NA, length(APE_point)))
})

# 5. Calculate SEs, CIs, and p-values
boot_APEs <- t(boot_APEs)
boot_APEs <- boot_APEs[complete.cases(boot_APEs), ]  # remove failed iterations

APE_se <- apply(boot_APEs, 2, sd)
p_values <- 2 * pnorm(-abs(APE_point / APE_se))


# 6. Combine results
APE_table <- data.frame(
  APE = round(APE_point, 4),
  SE = round(APE_se, 4),
 
  p_value = round(p_values, 4)
)

print(APE_table)




# Fit fractional logit model with only Country Fixed Effects and a TIME trend
model2.1_fraclogit <- feglm(Fossil_share ~ Regulations_and_Standards 
                            #+ Nuclear_heat_per_capita
                            + ln_GDP_per_capita 
                            + HDD
                            + Industry 
                            #+ Urban_Population 
                            + Natural_gas_price
                            + Heat_kWh_per_capita
                              # Add TIME trend
                            | TIME,  # Only Country FE
                            data = data1,
                            family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.1_fraclogit, vcov = DK(2))

# Fit fractional logit model with Taxes and Charges and a TIME trend
model2.2_fraclogit <- feglm(Fossil_share ~ Taxes_and_Charges
                            + ln_GDP_per_capita 
                            + HDD
                            + Industry 
                            #+ Urban_Population 
                            + Natural_gas_price
                            + Heat_kWh_per_capita
                              # Add TIME trend
                            | TIME,  # Only Country FE
                            data = data1,
                            family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.2_fraclogit, vcov = DK(2))

# Fit fractional logit model with Subsidies and Other Incentives and a TIME trend
model2.3_fraclogit <- feglm(Fossil_share ~ Subsidies_and_Other_Incentives
                            + ln_GDP_per_capita 
                            + HDD
                            + Industry 
                            #+ Urban_Population 
                            + Natural_gas_price
                            + Heat_kWh_per_capita
                              # Add TIME trend
                            | TIME,  # Only Country FE
                            data = data1,
                            family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.3_fraclogit, vcov = DK(2))

# Fit fractional logit model with Tradable Permits and a TIME trend
model2.4_fraclogit <- feglm(Fossil_share ~ Tradable_Permits
                            + ln_GDP_per_capita 
                            + HDD
                            + Industry 
                            #+ Urban_Population 
                            + Natural_gas_price
                            + Heat_kWh_per_capita
                              # Add TIME trend
                            | TIME,  # Only Country FE
                            data = data1,
                            family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.4_fraclogit, vcov = DK(2))

# Fit fractional logit model with Voluntary Agreements and Information and a TIME trend
model2.5_fraclogit <- feglm(Fossil_share ~ Voluntary_Agreements_and_Information
                            + ln_GDP_per_capita 
                            + HDD
                            + Industry 
                            #+ Urban_Population 
                            + Natural_gas_price
                            + Heat_kWh_per_capita
                              # Add TIME trend
                            | TIME,  # Only Country FE
                            data = data1,
                            family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.5_fraclogit, vcov = DK(2))


#POOLED OLS

library(fixest)

# Model 1: Regulations_and_Standards, pooled fractional logit
model2.1_pooled <- feglm(Fossil_share ~ Regulations_and_Standards
                         + ln_GDP_per_capita 
                         + HDD
                         + Industry
                         + Natural_gas_price
                         + Heat_kWh_per_capita,
                         data = data1,
                         family = quasibinomial("logit"),
                         panel.id = c("Country", "TIME"))

summary(model2.1_pooled, vcov = DK(2))


# Model 2: Taxes_and_Charges, pooled fractional logit
model2.2_pooled <- feglm(Fossil_share ~ Taxes_and_Charges
                         + ln_GDP_per_capita 
                         + HDD
                         + Industry
                         + Natural_gas_price
                         + Heat_kWh_per_capita,
                         data = data1,
                         family = quasibinomial("logit"),
                         panel.id = c("Country", "TIME"))

summary(model2.2_pooled, vcov = DK(2))


# Model 3: Subsidies_and_Other_Incentives, pooled fractional logit
model2.3_pooled <- feglm(Fossil_share ~ Subsidies_and_Other_Incentives
                         + ln_GDP_per_capita 
                         + HDD
                         + Industry
                         + Natural_gas_price
                         + Heat_kWh_per_capita,
                         data = data1,
                         family = quasibinomial("logit"),
                         panel.id = c("Country", "TIME"))

summary(model2.3_pooled, vcov = DK(2))


# Model 4: Tradable_Permits, pooled fractional logit
model2.4_pooled <- feglm(Fossil_share ~ Tradable_Permits
                         + ln_GDP_per_capita 
                         + HDD
                         + Industry
                         + Natural_gas_price
                         + Heat_kWh_per_capita,
                         data = data1,
                         family = quasibinomial("logit"),
                         panel.id = c("Country", "TIME"))

summary(model2.4_pooled, vcov = DK(2))


# Model 5: Voluntary_Agreements_and_Information, pooled fractional logit
model2.5_pooled <- feglm(Fossil_share ~ Voluntary_Agreements_and_Information
                         + ln_GDP_per_capita 
                         + HDD
                         + Industry
                         + Natural_gas_price
                         + Heat_kWh_per_capita,
                         data = data1,
                         family = quasibinomial("logit"),
                         panel.id = c("Country", "TIME"))

summary(model2.5_pooled, vcov = DK(2))


