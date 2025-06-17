# Create a demo dataset for the EDAhelper Shiny app
# This script generates a sample dataset with various types of variables
# and saves it as a CSV file in the package's extdata directory

# Set seed for reproducibility
set.seed(123)

# Sample size
n <- 500

# Generate numeric variables
age <- round(rnorm(n, mean = 40, sd = 15))
income <- round(exp(rnorm(n, mean = 10, sd = 0.7)) / 100) * 100  # Log-normal distribution
height <- round(rnorm(n, mean = 170, sd = 10), 1)
weight <- round(rnorm(n, mean = 70, sd = 15), 1)
blood_pressure <- round(rnorm(n, mean = 120, sd = 15))

# Generate categorical variables
gender <- sample(c("Male", "Female", "Non-binary"), n, replace = TRUE, prob = c(0.48, 0.48, 0.04))
education <- sample(c("High School", "Bachelor's", "Master's", "PhD"), n, replace = TRUE)
region <- sample(c("North", "South", "East", "West", "Central"), n, replace = TRUE)
smoking_status <- sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.6, 0.2, 0.2))

# Generate date variables
start_date <- as.Date("2020-01-01")
registration_date <- start_date + sample(0:730, n, replace = TRUE)

# Generate some missing values
missing_indices <- sample(1:n, n * 0.1)  # 10% missing
age[missing_indices[1:30]] <- NA
income[missing_indices[31:60]] <- NA
education[missing_indices[61:90]] <- NA
blood_pressure[missing_indices[91:120]] <- NA

# Generate some outliers
outlier_indices <- sample(setdiff(1:n, missing_indices), 10)
income[outlier_indices[1:5]] <- income[outlier_indices[1:5]] * 5
weight[outlier_indices[6:10]] <- weight[outlier_indices[6:10]] * 1.5

# Calculate BMI (creates correlation with height and weight)
bmi <- weight / ((height / 100) ^ 2)

# Create binary health risk indicator (creates correlation with age, BMI, smoking)
health_risk_score <- 0.02 * age + 0.1 * bmi + 
                    ifelse(smoking_status == "Current", 2, 
                          ifelse(smoking_status == "Former", 1, 0)) +
                    rnorm(n, mean = 0, sd = 1)

health_risk <- ifelse(health_risk_score > quantile(health_risk_score, 0.7, na.rm = TRUE), 
                     "High", "Low")

# Create a data frame
demo_data <- data.frame(
  Age = age,
  Income = income,
  Height = height,
  Weight = weight,
  BMI = bmi,
  BloodPressure = blood_pressure,
  Gender = gender,
  Education = education,
  Region = region,
  SmokingStatus = smoking_status,
  RegistrationDate = registration_date,
  HealthRisk = health_risk
)

# Save the dataset
file_path <- file.path("inst", "extdata", "demo_data.csv")
write.csv(demo_data, file = file_path, row.names = FALSE)

# This file can be run directly to regenerate the demo data
# The data can be accessed from the package with:
# system.file("extdata", "demo_data.csv", package = "EDAhelper")