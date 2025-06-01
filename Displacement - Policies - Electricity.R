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

# Load libraries
library(plm)
library(dplyr)
library(broom)
library(ggplot2)
library(readxl)
library(dplyr)
library(fixest)
library(car)

#load dataset with policies 

data1 <- read_xlsx("EEA_Policies.xlsx", sheet = "R-data")


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

data1$Subsidies_and_Tender <- 
  rowSums(data1[, grep("^SUB", colnames(data1))], na.rm = TRUE) +
  rowSums(data1[, grep("^TEN", colnames(data1))], na.rm = TRUE)


data1$Taxes_and_Charges <- rowSums(data1[, grep("^TAX", colnames(data1))], na.rm = TRUE)

data1$Tradable_Permits <- rowSums(data1[, grep("^TRA", colnames(data1))], na.rm = TRUE)

data1$Voluntary_Agreements_and_Information <- rowSums(data1[, grep("^VOL", colnames(data1))], na.rm = TRUE) + 
  rowSums(data1[, grep("^INF", colnames(data1))], na.rm = TRUE) +
  rowSums(data1[, grep("^POL", colnames(data1))], na.rm = TRUE)


data1$Grants <- rowSums(data1[, grep("^GRA", colnames(data1))], na.rm = TRUE) 




data1$Regulations_and_Standards <- 
  rowSums(data1[, grep("^BAN", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^PUB", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^REG", names(data1)), drop = FALSE], na.rm = TRUE)

data1$Subsidies_and_Other_Incentives <- 
  rowSums(data1[, grep("^GRA", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^SUB", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^TEN", names(data1)), drop = FALSE], na.rm = TRUE)

data1$Subsidies_Tender <- 
  
  rowSums(data1[, grep("^SUB", names(data1)), drop = FALSE], na.rm = TRUE) +
  rowSums(data1[, grep("^TEN", names(data1)), drop = FALSE], na.rm = TRUE)


data1$Grants <- 
  rowSums(data1[, grep("^GRA", names(data1)), drop = FALSE], na.rm = TRUE)
 

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
          "Voluntary_Agreements_and_Information", "Grants")]

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


data1$Total_Policies_minus_permits <- data1$Regulations_and_Standards +
  data1$Subsidies_and_Other_Incentives +
  data1$Taxes_and_Charges +
  data1$Voluntary_Agreements_and_Information

head(data1$Total_Policies)
summary(data1$Total_Policies)



pdata <- pdata.frame(data1, index = c("Country", "TIME"))


#Regulations and Standards       Subsidies and Other Incentives                    Taxes and Charges 
#105                                  105                                   51 
#Tradable Permits Voluntary Agreements and Information 
#8                                   40 




# Select the relevant variables used in your model
selected_vars <- data1[, c("Fossil_share","Total_Policies", "Subsidies_and_Other_Incentives","Taxes_and_Charges",
                           "Regulations_and_Standards", 
                           "Tradable_Permits", 
                           "Voluntary_Agreements_and_Information","Subsidies_and_Tender","Grants","Nuclear_heat",
                           "GDP_per_capita", "ln_GDP_per_capita",
                           "Industry", "Urban_Population","Electricity_kWh_per_capita","Natural_gas_price",
                           "Netimports_per_capita")]

# Remove rows with NA values to avoid errors in correlation calculation
selected_vars <- na.omit(selected_vars)



library(corrplot)
# All predictors (excluding the dependent variable Fossil_energy_per_capita)
predictors <- c("Fossil_energy_per_capita","Fossil_share","Renewables_per_capita","Total_Policies", "Subsidies_and_Other_Incentives","Taxes_and_Charges",
                "Regulations_and_Standards", 
                "Tradable_Permits", 
                "Voluntary_Agreements_and_Information",
                
                "ln_GDP_per_capita", 
                "GDP_per_capita",
                #"GDP_per_capita_sq",
                #"GDP_per_capita_sq",
                "Urban_Population",
                #"Urban_sq"
                "Manufacturing",
                "Industry",
                "Net_imports", "Electricity_kWh_per_capita"
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
         tl.cex = 0.6,                 # Label size
         number.cex = 0.6,             # Number size
         number.digits = 2,           # Round to 2 digits
         addCoef.col = "black",       # Add correlation numbers
         col = colorRampPalette(c("blue", "white", "red"))(200)) 








# Compute the correlation matrix
cor_matrix <- cor(selected_vars, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

selected_vars1 <- data1[, c("Fossil_share", "Subsidies_and_Tender","Grants","Taxes_and_Charges",
                           "Regulations_and_Standards", 
                           "Tradable_Permits", 
                           "Voluntary_Agreements_and_Information", 
                           "GDP_per_capita", 
                           "Industry", 
                           "Netimports_per_capita")]

# Remove rows with NA values to avoid errors in correlation calculation
selected_vars1 <- na.omit(selected_vars1)

# Compute the correlation matrix
cor_matrix1 <- cor(selected_vars1, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix1)

#HIGH CORRELATION BETWEEN TAXES AND SUBSIDIES
#HIGH CORRELATION BETWEEN TRADEABLE PERMITS AND VOLANTARY AGREEMENTS
#########################################################################

# Load required packages
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
# Model 1.1
model1.1 <- plm(Fossil_share ~ 
                  Total_Policies + 
                  ln_GDP_per_capita + 
                  Electricity_kWh_per_capita + 
                  Industry + 
                  Urban_Population + 
                  Netimports_per_capita, 
                data = data1, 
                model = "within", 
                effect = "twoways", 
                index = c("Country", "TIME"), 
                method = "prais")  # Prais-Winsten correction for AR(1)

# Model 1.2
model1.2 <- plm(Fossil_share ~ 
                  Total_Policies + 
                  ln_GDP_per_capita + 
                  Electricity_kWh_per_capita + 
                  Industry + 
                  Urban_Population, 
                data = data1, 
                model = "within", 
                effect = "twoways", 
                index = c("Country", "TIME"), 
                method = "prais")

# Model 1.3
model1.3 <- plm(Fossil_share ~ 
                  Total_Policies + 
                  ln_GDP_per_capita + 
                  Electricity_kWh_per_capita + 
                  Urban_Population + 
                  Netimports_per_capita, 
                data = data1, 
                model = "within", 
                effect = "twoways", 
                index = c("Country", "TIME"), 
                method = "prais")

# Model 1.4
model1.4 <- plm(Fossil_share ~ 
                  Total_Policies + 
                  ln_GDP_per_capita + 
                  Electricity_kWh_per_capita + 
                  Industry + 
                  Netimports_per_capita, 
                data = data1, 
                model = "within", 
                effect = "twoways", 
                index = c("Country", "TIME"), 
                method = "prais")

# Model 1.5
model1.5 <- plm(Fossil_share ~ 
                  Total_Policies + 
                  ln_GDP_per_capita + 
                  Electricity_kWh_per_capita + 
                  Netimports_per_capita, 
                data = data1, 
                model = "within", 
                effect = "twoways", 
                index = c("Country", "TIME"), 
                method = "prais")

# Model 1.6
model1.6 <- plm(Fossil_share ~ 
                  Total_Policies + 
                  ln_GDP_per_capita + 
                  Electricity_kWh_per_capita + 
                  Industry + 
                  Netimports_per_capita, 
                data = data1, 
                model = "within", 
                effect = "twoways", 
                index = c("Country", "TIME"), 
                method = "prais")


# Driscoll-Kraay (PCSE) function
se_type <- function(model) vcovSCC(model, type = "HC0", maxlag = 2)

# Coefficient tests with DK standard errors
summary1 <- coeftest(model1.1, vcov. = se_type(model1.1))
summary2 <- coeftest(model1.2, vcov. = se_type(model1.2))
summary3 <- coeftest(model1.3, vcov. = se_type(model1.3))
summary4 <- coeftest(model1.4, vcov. = se_type(model1.4))
summary5 <- coeftest(model1.5, vcov. = se_type(model1.5))
summary6 <- coeftest(model1.6, vcov. = se_type(model1.6))

# Coefficient tests with DK standard errors
summary1 
summary2 
summary3 
summary4 
summary5 
summary6 


# Predictions from each model
pred1 <- predict(model1.1)
pred2 <- predict(model1.2)
pred3 <- predict(model1.3)
pred4 <- predict(model1.4)
pred5 <- predict(model1.5)
pred6 <- predict(model1.6)

# Check for values outside [0,1] range
range_check <- function(pred) {
  out_of_bounds <- sum(pred < 0 | pred > 1)
  cat("Number of predictions outside [0, 1]:", out_of_bounds, "\n")
  cat("Min prediction:", min(pred), "\n")
  cat("Max prediction:", max(pred), "\n\n")
}

range_check(pred1)
range_check(pred2)
range_check(pred3)
range_check(pred4)
range_check(pred5)
range_check(pred6)


# Model 1: Regulations and Standards
model1 <- plm(Fossil_share ~ Regulations_and_Standards +
                ln_GDP_per_capita + Industry + Electricity_kWh_per_capita + 
                Netimports_per_capita,
              data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"), 
              method = "prais")

# Model 2: Subsidies and Other Incentives
model2 <- plm(Fossil_share ~ Subsidies_and_Other_Incentives +
                ln_GDP_per_capita + Industry + Electricity_kWh_per_capita + 
                Netimports_per_capita,
              data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"), 
              method = "prais")

# Model 3: Taxes and Charges
model3 <- plm(Fossil_share ~ Taxes_and_Charges +
                ln_GDP_per_capita + Industry + Electricity_kWh_per_capita + 
                Netimports_per_capita,
              data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"), 
              method = "prais")

# Model 4: Tradable Permits
model4 <- plm(Fossil_share ~ Tradable_Permits +
                ln_GDP_per_capita + Industry + Electricity_kWh_per_capita + 
                Netimports_per_capita,
              data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"), 
              method = "prais")

# Model 5: Voluntary Agreements and Information
model5 <- plm(Fossil_share ~ Voluntary_Agreements_and_Information +
                ln_GDP_per_capita + Industry + Electricity_kWh_per_capita + 
                Netimports_per_capita,
              data = data1, 
              model = "within", 
              effect = "twoways", 
              index = c("Country", "TIME"), 
              method = "prais")

# Summaries with DK standard errors
summary1 <- coeftest(model1, vcov. = se_type(model1))
summary2 <- coeftest(model2, vcov. = se_type(model2))
summary3 <- coeftest(model3, vcov. = se_type(model3))
summary4 <- coeftest(model4, vcov. = se_type(model4))
summary5 <- coeftest(model5, vcov. = se_type(model5))

# View results
summary1
summary2
summary3
summary4
summary5

pred1.1 <- predict(model1)
pred2.2 <- predict(model2)
pred3.3 <- predict(model3)
pred4.4 <- predict(model4)
pred5.5 <- predict(model5)

# Check for values outside [0,1] range
range_check <- function(pred) {
  out_of_bounds <- sum(pred < 0 | pred > 1)
  cat("Number of predictions outside [0, 1]:", out_of_bounds, "\n")
  cat("Min prediction:", min(pred), "\n")
  cat("Max prediction:", max(pred), "\n\n")
}

range_check(pred1.1)
range_check(pred2.2)
range_check(pred3.3)
range_check(pred4.4)
range_check(pred5.5)


















install.packages("writexl")

library(writexl)

# Save the dataframe to an Excel file
write_xlsx(data1, "C:/Users/toket/OneDrive - University of Copenhagen/Master ENRE/Master - ENRE/Master Thesis/Datasets/R - Model/Test.xlsx")


###################################################################################
#SHift to logit!



#####TOTAL POLICIES 


# Fit fractional logit model with two-way fixed effects
# Model 1: All controls 
model1_fraclogit <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita +
                            + Industry + Urban_Population  +Electricity_kWh_per_capita+ Netimports_per_capita |
                            Country + TIME,
                          data = data1, family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

# Model 2: No imports
model2_fraclogit <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Electricity_kWh_per_capita+
                            Industry + Urban_Population |
                            Country + TIME,
                          data = data1, family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

# Model 3: No Industry
model3_fraclogit <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Electricity_kWh_per_capita +
                           + Urban_Population + #Nuclear_heat
                          +Netimports_per_capita |
                            Country + TIME,
                          data = data1, family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

# Model 4: No Urban
model4_fraclogit <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Electricity_kWh_per_capita +
                             + Industry + #Nuclear_heat
                          +Netimports_per_capita |
                            Country + TIME,
                          data = data1, family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

# Model 5: Only Total_Policies, lnGDP, and Netimports
model5_fraclogit <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita +Electricity_kWh_per_capita +
                            Netimports_per_capita|
                            Country + TIME,
                          data = data1, family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

# Model 6: Only Total_Policies, lnGDP, and Industry
model6_fraclogit <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Electricity_kWh_per_capita +
                            Industry+Netimports_per_capita| #Nuclear_heat
                            
                            Country + TIME,
                          data = data1, family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))
#summary(model1_fraclogit, vcov ="HC1")
#summary(model2_fraclogit, vcov ="HC1")
#summary(model3_fraclogit, vcov ="HC1")
#summary(model4_fraclogit, vcov ="HC1")
#summary(model5_fraclogit, vcov ="HC1")
#summary(model6_fraclogit, vcov ="HC1")

summary(model1_fraclogit, vcov =DK(2))
summary(model2_fraclogit, vcov =DK(2))
summary(model3_fraclogit, vcov =DK(2))
summary(model4_fraclogit, vcov =DK(2))
summary(model5_fraclogit, vcov =DK(2))
summary(model6_fraclogit, vcov =DK(2))

####
library(fixest)

dk_se <- function(model) vcov(model, vcov = DK(2))
# Create a named list of models
models <- list(
  "Model 1" = model1_fraclogit,
  "Model 2" = model2_fraclogit,
  "Model 3" = model3_fraclogit,
  "Model 4" = model4_fraclogit,
  "Model 5" = model5_fraclogit,
  "Model 6" = model6_fraclogit
)


library(marginaleffects)

# Store models in a named list
models <- list(
  "Model 1" = model1_fraclogit,
  "Model 2" = model2_fraclogit,
  "Model 3" = model3_fraclogit,
  "Model 4" = model4_fraclogit,
  "Model 5" = model5_fraclogit,
  "Model 6" = model6_fraclogit
)

# Function to compute and print APE for Total_Policies
check_ape <- function(model_list) {
  for (i in names(model_list)) {
    cat("\n", strrep("-", 50), "\n")
    cat(i, "\n")
    ape <- slopes(model_list[[i]], variables = "Total_Policies", average = TRUE)
    print(summary(ape))
  }
}

# Run the check
check_ape(models)

library(knitr)

# Calculate Average Partial Effects with robust SEs (HC1) for all models
ape_model1 <- avg_slopes(model1_fraclogit, type = "response", vcov = "HC1")
ape_model2 <- avg_slopes(model2_fraclogit, type = "response", vcov = "HC1")
ape_model3 <- avg_slopes(model3_fraclogit, type = "response", vcov = "HC1")
ape_model4 <- avg_slopes(model4_fraclogit, type = "response", vcov = "HC1")
ape_model5 <- avg_slopes(model5_fraclogit, type = "response", vcov = "HC1")
ape_model6 <- avg_slopes(model6_fraclogit, type = "response", vcov = "HC1")

ape_table <- list(
  Model_1 = ape_model1,
  Model_2 = ape_model2,
  Model_3 = ape_model3,
  Model_4 = ape_model4,
  Model_5 = ape_model5,
  Model_6 = ape_model6
) %>%
  purrr::map_df(~ select(., term, estimate), .id = "Model") %>%
  tidyr::pivot_wider(names_from = Model, values_from = estimate)

kable(ape_table, digits = 5, caption = "Average Partial Effects by Model (HC1 SEs)")







#### TEST FOR VIF
library(car)  # for vif()

# Model 1
lm1 <- lm(Fossil_share ~ Total_Policies + ln_GDP_per_capita +
            + Industry + Urban_Population + Natural_gas_price +Electricity_kWh_per_capita+ Netimports_per_capita,
          data = data1)
vif(lm1)

# Model 2
lm2 <- lm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Electricity_kWh_per_capita+
            Industry + Urban_Population + Natural_gas_price,
          data = data1)
vif(lm2)

# Model 3
lm3 <- lm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Electricity_kWh_per_capita +
            + Urban_Population + Natural_gas_price+Netimports_per_capita,
          data = data1)
vif(lm3)

# Model 4
lm4 <- lm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Electricity_kWh_per_capita +
            + Industry + Natural_gas_price+Netimports_per_capita,
          data = data1)
vif(lm4)

# Model 5
lm5 <- lm(Fossil_share ~ Total_Policies + ln_GDP_per_capita +Electricity_kWh_per_capita +
            Netimports_per_capita,
          data = data1)
vif(lm5)

# Model 6
lm6 <- lm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Electricity_kWh_per_capita +
            Industry+Netimports_per_capita,
          data = data1)
vif(lm6)




# ALL POLICES AT ONCE, individually

# Fit fractional logit model with two-way fixed effects
model1.1_fraclogit <- feglm(Fossil_share ~ Regulations_and_Standards
                            +Subsidies_and_Other_Incentives
                            +Taxes_and_Charges+Tradable_Permits
                            +Voluntary_Agreements_and_Information
                            #+ Nuclear_heat_per_capita
                            + ln_GDP_per_capita 
                            
                            + Industry 
                            #+ Urban_Population 
                            #+ Natural_gas_price
                            +Electricity_kWh_per_capita
                            +Netimports_per_capita
                            | Country + TIME,
                            data = data1, family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model1.1_fraclogit, vcov = DK(2))



# Compute Driscoll-Kraay VCOV matrix
vcov_dk2 <- vcov(model1.1_fraclogit, vcov = "HC1")

# Now compute APEs using the matrix
ape_model1 <- avg_slopes(model1.1_fraclogit, type = "response", vcov = "HC1")

ape_table <- list(
  Model_1 = ape_model1
) %>%
  purrr::map_df(~ select(., term, estimate), .id = "Model") %>%
  tidyr::pivot_wider(names_from = Model, values_from = estimate)

kable(ape_table, digits = 5, caption = "Average Partial Effects by Model (HC1 SEs)")


# Compute Driscoll-Kraay VCOV matrix
vcov_dk2 <- vcov(model1.1_fraclogit, vcov = DK(2))

# Now compute APEs using the matrix
ape_model1 <- avg_slopes(model1.1_fraclogit, type = "response", vcov = vcov_dk2)

ape_table <- list(
  Model_1 = ape_model1
) %>%
  purrr::map_df(~ select(., term, estimate), .id = "Model") %>%
  tidyr::pivot_wider(names_from = Model, values_from = estimate)

kable(ape_table, digits = 5, caption = "Average Partial Effects by Model (HC1 SEs)")








######################################################################################
#Individual policies

# Fit fractional logit model with two-way fixed effects
model2.1_fraclogit <- feglm(Fossil_share ~ Regulations_and_Standards
                          #+Subsidies_and_Other_Incentives
                          #+Taxes_and_Charges
                          #+Tradable_Permits
                          #+Voluntary_Agreements_and_Information
                          #+ Nuclear_heat
                          + ln_GDP_per_capita 
                          
                          + Industry 
                          #+ Urban_Population 
                          #+ Natural_gas_price
                          +Electricity_kWh_per_capita
                          +Netimports_per_capita
                          | Country + TIME,
                          data = data1, family = quasibinomial("logit"),
                          panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.1_fraclogit, vcov = DK(2))


model2.2_fraclogit <- feglm(Fossil_share ~ 
                              #Regulations_and_Standards
                            +Subsidies_and_Other_Incentives
                            #+Taxes_and_Charges+Tradable_Permits
                            #+Voluntary_Agreements_and_Information
                            #+ Nuclear_heat_per_capita
                            + ln_GDP_per_capita 
                            
                            + Industry 
                            #+ Urban_Population 
                            #+ Natural_gas_price
                            +Electricity_kWh_per_capita+Netimports_per_capita
                            | Country + TIME,
                            data = data1, family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.2_fraclogit, vcov = DK(2))

model2.3_fraclogit <- feglm(Fossil_share ~ 
                              #Regulations_and_Standards
                            #+Subsidies_and_Other_Incentives
                            +Taxes_and_Charges
                            #+Tradable_Permits
                            #+Voluntary_Agreements_and_Information
                            #+ Nuclear_heat_per_capita
                            + ln_GDP_per_capita 
                            
                            + Industry 
                            #+ Urban_Population 
                            #+ Natural_gas_price
                            +Electricity_kWh_per_capita+Netimports_per_capita
                            | Country + TIME,
                            data = data1, family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.3_fraclogit, vcov = DK(2))

model2.4_fraclogit <- feglm(Fossil_share ~ 
                              #Regulations_and_Standards
                            #+Subsidies_and_Other_Incentives
                            #+Taxes_and_Charges
                            +Tradable_Permits
                            #+Voluntary_Agreements_and_Information
                            #+ Nuclear_heat_per_capita
                            + ln_GDP_per_capita 
                            
                            + Industry 
                            #+ Urban_Population 
                            #+ Natural_gas_price
                            +Electricity_kWh_per_capita
                            +Netimports_per_capita
                            | Country + TIME,
                            data = data1, family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.4_fraclogit, vcov = DK(2))



model2.5_fraclogit <- feglm(Fossil_share ~ #Regulations_and_Standards
                            #+Subsidies_and_Other_Incentives
                            #+Taxes_and_Charges+Tradable_Permits
                            +Voluntary_Agreements_and_Information
                            #+ Nuclear_heat_per_capita
                            + ln_GDP_per_capita 
                            
                            + Industry 
                            #+ Urban_Population 
                            #+ Natural_gas_price
                            +Electricity_kWh_per_capita+Netimports_per_capita
                            | Country + TIME,
                            data = data1, family = quasibinomial("logit"),
                            panel.id = c("Country", "TIME"))

# Print model summary
summary(model2.5_fraclogit, vcov = DK(2))




library(knitr)

# Calculate Average Partial Effects with robust SEs (HC1) for all models
ape_model1 <- avg_slopes(model2.1_fraclogit, type = "response", vcov = "HC1")
ape_model2 <- avg_slopes(model2.2_fraclogit, type = "response", vcov = "HC1")
ape_model3 <- avg_slopes(model2.3_fraclogit, type = "response", vcov = "HC1")
ape_model4 <- avg_slopes(model2.4_fraclogit, type = "response", vcov = "HC1")
ape_model5 <- avg_slopes(model2.5_fraclogit, type = "response", vcov = "HC1")

ape_table <- list(
  Model_1 = ape_model1,
  Model_2 = ape_model2,
  Model_3 = ape_model3,
  Model_4 = ape_model4,
  Model_5 = ape_model5
) %>%
  purrr::map_df(~ select(., term, estimate), .id = "Model") %>%
  tidyr::pivot_wider(names_from = Model, values_from = estimate)

kable(ape_table, digits = 5, caption = "Average Partial Effects by Model (HC1 SEs)")













library(fixest)

# Fit fractional logit model with two-way fixed effects
model1_fraclogit <- feglm(Fossil_share ~ Total_Policies 
                          #+ Nuclear_heat_per_capita
                          + ln_GDP_per_capita + Netimports_per_capita 
                          + Industry 
                          + Urban_Population 
                          + Natural_gas_price
                          #+Electricity_kWh_per_capita
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"))

# Print model summary
summary(model1_fraclogit)
summary(model1_fraclogit, vcov = "standard")

# AIC and BIC
AIC(model1_fraclogit)
BIC(model1_fraclogit)


# Required packages
library(sandwich)
library(lmtest)

# Finland Model
model_finland <- glm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Netimports_per_capita + 
                       Industry + Urban_Population + Natural_gas_price,
                     data = subset(data1, Country == "Finland"),
                     family = quasibinomial("logit"))

cat("\n===== Finland Model Summary =====\n")
#summary(model_finland)
print(coeftest(model_finland, vcov = vcovHC(model_finland, type = "HC0")))

# Denmark Model
model_denmark <- glm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Netimports_per_capita + 
                       Industry + Urban_Population + Natural_gas_price,
                     data = subset(data1, Country == "Denmark"),
                     family = quasibinomial("logit"))

cat("\n===== Denmark Model Summary =====\n")
#summary(model_denmark)
print(coeftest(model_denmark, vcov = vcovHC(model_denmark, type = "HC0")))

# Sweden Model
model_sweden <- glm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Netimports_per_capita + 
                      Industry + Urban_Population + Natural_gas_price,
                    data = subset(data1, Country == "Sweden"),
                    family = quasibinomial("logit"))

cat("\n===== Sweden Model Summary =====\n")
#summary(model_sweden)
print(coeftest(model_sweden, vcov = vcovHC(model_sweden, type = "HC0")))

# Norway Model
model_norway <- glm(Fossil_share ~ Total_Policies + ln_GDP_per_capita + Netimports_per_capita + 
                      Industry + Urban_Population + Natural_gas_price,
                    data = subset(data1, Country == "Norway"),
                    family = quasibinomial("logit"))

cat("\n===== Norway Model Summary =====\n")
#summary(model_norway)
print(coeftest(model_norway, vcov = vcovHC(model_norway, type = "HC0")))




# Fit fractional logit model with two-way fixed effects
model1_fraclogit_subset <- feglm(Fossil_share ~ Total_Policies 
                          #+ Nuclear_heat
                          + ln_GDP_per_capita 
                          #+ Netimports_per_capita 
                          + Industry 
                          + Urban_Population 
                          #+ Natural_gas_price
                          #+Electricity_kWh_per_capita
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"))

# Print model summary
summary(model1_fraclogit_subset)
summary(model1_fraclogit_subset, vcov = "standard")









#####################################################################################

# TAKE NORWAY OUT

# Subset the data for Finland, Denmark, and Sweden
model_fraclogit_all <- feglm(Fossil_share ~ Total_Policies + ln_GDP_per_capita 
                             + Netimports_per_capita 
                             + Industry 
                             + Urban_Population 
                             + Natural_gas_price
                             | TIME,
                             data = subset(data1, Country %in% c("Finland", "Denmark", "Sweden")),
                             family = quasibinomial("logit"))

# Print the summary for the model
summary(model_fraclogit_all)
summary(model_fraclogit_all, vcov = "standard")



model2_fraclogit <- feglm(Fossil_share ~ Total_Policies 
                          #+ Nuclear_heat_per_capita
                          + ln_GDP_per_capita + Netimports_per_capita 
                          + Industry 
                          + Urban_Population 
                          + Natural_gas_price+Electricity_kWh_per_capita
                          |TIME,
                          data = data1,
                          family = quasibinomial("logit"))

# Print model summary
summary(model2_fraclogit)
summary(model2_fraclogit, vcov = "standard")

# AIC and BIC
AIC(model2_fraclogit)
BIC(model2_fraclogit)


model3_fraclogit <- feglm(Fossil_share ~ Total_Policies 
                          #+ Nuclear_heat_per_capita
                          + ln_GDP_per_capita + Netimports_per_capita 
                          + Industry 
                          + Urban_Population 
                          + Natural_gas_price+Electricity_kWh_per_capita
                          | Country,
                          data = data1,
                          family = quasibinomial("logit"))

# Print model summary
summary(model3_fraclogit)
summary(model3_fraclogit, vcov = "standard")

# AIC and BIC
AIC(model3_fraclogit)
BIC(model3_fraclogit)







by(data1$Total_Policies, data1$Country, sd)


#####Plot Total policies and fossil share

library(ggplot2)
ggplot(data1, aes(x = Total_Policies, y = Fossil_share)) +
  geom_point() +
  facet_wrap(~Country) +
  geom_smooth(method = "loess", se = FALSE, color = "blue")


##### Only time and country FE's, seperately


# Fractional Logit with only time (TIME)
model_fe_time_fraclogit <- feglm(
  Fossil_share ~ Total_Policies + ln_GDP_per_capita + Netimports_per_capita + 
    Industry + Natural_gas_price | TIME, 
  data = data1, 
  family = "quasibinomial"
)

# Print summary of the model
summary(model_fe_time_fraclogit)


# Fractional Logit with only country (Country)
model_fe_country_fraclogit <- feglm(
  Fossil_share ~ Total_Policies + ln_GDP_per_capita + Netimports_per_capita + 
    Industry + Natural_gas_price | Country, 
  data = data1, 
  family = "quasibinomial"
)

# Print summary of the model
summary(model_fe_country_fraclogit)



#####TOTAL POLICIES ################################################################


# Fit fractional logit model with two-way fixed effects
model1_fraclogit <- feglm(Fossil_share ~ Total_Policies
                          + Nuclear_heat
                          + ln_GDP_per_capita 
                          + Netimports_per_capita 
                          #+ HDD
                          + Industry 
                          + Urban_Population 
                          + Natural_gas_price
                          #+Electricity_kWh_per_capita
                          
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"))

# Print model summary
#summary(model1_fraclogit)
#summary(model1_fraclogit, vcov = "standard")
#summary(model1_fraclogit, vcov = "hetero") 

#summary(model1_fraclogit, vcov = ~ Country)

summary(model1_fraclogit, vcov = "hc1")



#############################################################################################
# ALL POLICES AT ONCE, individually


# Fit fractional logit model with two-way fixed effects
model1.1_fraclogit <- feglm(Fossil_share ~ Regulations_and_Standards
                            #+Subsidies_and_Other_Incentives
                            +Taxes_and_Charges+Tradable_Permits
                            +Voluntary_Agreements_and_Information
                            + Nuclear_heat
                            + ln_GDP_per_capita 
                            + Netimports_per_capita
                            #+ HDD
                            + Industry 
                            + Urban_Population 
                            #+ Natural_gas_price
                            #+Electricity_kWh_per_capita
                            | Country + TIME,
                            data = data1,
                            family = quasibinomial("logit"))

# Print model summary
#summary(model2_fraclogit)
summary(model1.1_fraclogit, vcov = "standard")


model1.1_fraclogit_ols <- lm(Fossil_share ~ Fossil_share ~ Regulations_and_Standards
                             #+Subsidies_and_Other_Incentives
                             +Taxes_and_Charges+Tradable_Permits
                             +Voluntary_Agreements_and_Information
                             + Nuclear_heat
                             + ln_GDP_per_capita 
                             + Netimports_per_capita
                             
                             #+ Industry 
                             + Urban_Population 
                             #+ Natural_gas_price
                             #+Electricity_kWh_per_capita
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
model2_fraclogit <- feglm(Fossil_share ~ Regulations_and_Standards 
                          + Nuclear_heat
                          + ln_GDP_per_capita 
                          + Netimports_per_capita
                          #+ HDD
                          + Industry 
                          + Urban_Population 
                          #+ Natural_gas_price
                          #+Electricity_kWh_per_capita
                          | Country + TIME,
                          data = data1,
                          family = quasibinomial("logit"))

# Print model summary
#summary(model2_fraclogit)
summary(model2_fraclogit, vcov = "standard")
summary(model2_fraclogit, vcov = "hetero")

summary(model2_fraclogit, vcov = ~ Country)


model2.2_fraclogit <- feglm(Fossil_share ~ Taxes_and_Charges
                            +Nuclear_heat
                            + ln_GDP_per_capita 
                            + Netimports_per_capita
                            #+HDD
                            + Industry 
                            + Urban_Population 
                            #+ Natural_gas_price
                            #+Electricity_kWh_per_capita
                            | Country + TIME,
                            data = data1,
                            family = quasibinomial("logit"))

# Print model summary
#summary(model2.2_fraclogit)
summary(model2.2_fraclogit, vcov = "standard")
summary(model2.2_fraclogit, vcov = "hetero")



model2.3_fraclogit <- feglm(Fossil_share ~ Subsidies_and_Other_Incentives
                            +Nuclear_heat
                            + ln_GDP_per_capita 
                            + Netimports_per_capita
                            #+ HDD
                            + Industry 
                            + Urban_Population 
                            #+ Natural_gas_price
                            #+Electricity_kWh_per_capita
                            | Country + TIME,
                            data = data1,
                            family = quasibinomial("logit"))

# Print model summary
#summary(model2.3_fraclogit)
summary(model2.3_fraclogit, vcov = "standard")
summary(model2.3_fraclogit, vcov = "hetero")



model2.4_fraclogit <- feglm(Fossil_share ~ Tradable_Permits
                            +Nuclear_heat
                            + ln_GDP_per_capita 
                            + Netimports_per_capita
                            #+ HDD
                            + Industry 
                            + Urban_Population 
                            #+ Natural_gas_price
                            #+Electricity_kWh_per_capita
                            | Country + TIME,
                            data = data1,
                            family = quasibinomial("logit"))

# Print model summary
#summary(model2.4_fraclogit)
summary(model2.4_fraclogit, vcov = "standard")
summary(model2.4_fraclogit, vcov = "hetero")


model2.5_fraclogit <- feglm(Fossil_share ~ Voluntary_Agreements_and_Information
                            +Nuclear_heat
                            + ln_GDP_per_capita 
                            + Netimports_per_capita
                            #+ HDD
                            + Industry 
                            + Urban_Population 
                            #+ Natural_gas_price
                            #+Electricity_kWh_per_capita
                            | Country + TIME,
                            data = data1,
                            family = quasibinomial("logit"))

# Print model summary
#summary(model2.5_fraclogit)
summary(model2.5_fraclogit, vcov = "standard")
summary(model2.5_fraclogit, vcov = "hetero")

