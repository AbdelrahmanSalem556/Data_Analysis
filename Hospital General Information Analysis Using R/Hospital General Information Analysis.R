#Dataset link:https://www.kaggle.com/datasets/cms/hospital-general-information

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Read the data
data <- read.csv("D:/Data Analysis/Hospital General Information Analysis Using R/HospInfo.csv")

# Clean the data
data <- na.omit(data)  # Remove rows with missing values

# Analyze the geographical distribution of hospitals by state
state_distribution <- data %>%
  group_by(State) %>%
  summarise(hospital_count = n(), .groups = 'drop')

ggplot(state_distribution, aes(x = reorder(State, -hospital_count), y = hospital_count)) +
  geom_bar(stat = "identity", fill = "Red") +
  labs(title = "Distribution of Hospitals by State", x = "State", y = "Number of Hospitals") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Analyze hospital type and ownership
ownership_type_distribution <- data %>%
  group_by(Hospital.Ownership, Hospital.Type) %>%
  summarise(hospital_count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Hospital.Type, values_from = hospital_count, values_fill = 0)

# Plot the distribution by hospital type and ownership
ggplot(data, aes(x = Hospital.Ownership, fill = Hospital.Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Hospital Types by Ownership", x = "Hospital Ownership", y = "Number of Hospitals") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the number of TRUE and FALSE values in the Emergency.Services column
emergency_services_counts <- table(data$Emergency.Services)

# Convert the data to a data frame for easier plotting
emergency_services_df <- as.data.frame(emergency_services_counts)
colnames(emergency_services_df) <- c("Emergency.Services", "Count")

# Plot the distribution using ggplot2
ggplot(emergency_services_df, aes(x = Emergency.Services, y = Count, fill = Emergency.Services)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Emergency Services", x = "Emergency Services Available", y = "Count") +
  scale_fill_manual(values = c("TRUE" = "skyblue", "FALSE" = "coral")) +
  theme_minimal()

# Select only the hospital name and phone number columns
hospital_contact_info <- data %>%
  select(Hospital.Name, Phone.Number)

# Select the hospital name and emergency services availability columns
hospital_Emergency_Type <- data %>%
  select(Hospital.Name, Emergency.Services)

# Select the hospital name, address, and location columns
hospital_details <- data %>%
  select(Hospital.Name, Address, Location)

# Calculate the frequency of different values in the specified columns
mortality_comparison_counts <- table(data$Mortality.national.comparison)
mortality_comparison_footnote_counts <- table(data$Mortality.national.comparison.footnote)
safety_comparison_counts <- table(data$Safety.of.care.national.comparison)
safety_comparison_footnote_counts <- table(data$Safety.of.care.national.comparison.footnote)
readmission_comparison_counts <- table(data$Readmission.national.comparison)
readmission_comparison_footnote_counts <- table(data$Readmission.national.comparison.footnote)
patient_experience_comparison_counts <- table(data$Patient.experience.national.comparison)
patient_experience_comparison_footnote_counts <- table(data$Patient.experience.national.comparison.footnote)
effectiveness_comparison_counts <- table(data$Effectiveness.of.care.national.comparison)
effectiveness_comparison_footnote_counts <- table(data$Effectiveness.of.care.national.comparison.footnote)
timeliness_comparison_counts <- table(data$Timeliness.of.care.national.comparison)
timeliness_comparison_footnote_counts <- table(data$Timeliness.of.care.national.comparison.footnote)
efficient_imaging_comparison_counts <- table(data$Efficient.use.of.medical.imaging.national.comparison)
efficient_imaging_comparison_footnote_counts <- table(data$Efficient.use.of.medical.imaging.national.comparison.footnote)

# Convert the data to data frames
mortality_comparison_df <- as.data.frame(mortality_comparison_counts)
mortality_comparison_footnote_df <- as.data.frame(mortality_comparison_footnote_counts)
safety_comparison_df <- as.data.frame(safety_comparison_counts)
safety_comparison_footnote_df <- as.data.frame(safety_comparison_footnote_counts)
readmission_comparison_df <- as.data.frame(readmission_comparison_counts)
readmission_comparison_footnote_df <- as.data.frame(readmission_comparison_footnote_counts)
patient_experience_comparison_df <- as.data.frame(patient_experience_comparison_counts)
patient_experience_comparison_footnote_df <- as.data.frame(patient_experience_comparison_footnote_counts)
effectiveness_comparison_df <- as.data.frame(effectiveness_comparison_counts)
effectiveness_comparison_footnote_df <- as.data.frame(effectiveness_comparison_footnote_counts)
timeliness_comparison_df <- as.data.frame(timeliness_comparison_counts)
timeliness_comparison_footnote_df <- as.data.frame(timeliness_comparison_footnote_counts)
efficient_imaging_comparison_df <- as.data.frame(efficient_imaging_comparison_counts)
efficient_imaging_comparison_footnote_df <- as.data.frame(efficient_imaging_comparison_footnote_counts)

# Set column names
colnames(mortality_comparison_df) <- c("Metric", "Count")
colnames(mortality_comparison_footnote_df) <- c("Metric", "Count")
colnames(safety_comparison_df) <- c("Metric", "Count")
colnames(safety_comparison_footnote_df) <- c("Metric", "Count")
colnames(readmission_comparison_df) <- c("Metric", "Count")
colnames(readmission_comparison_footnote_df) <- c("Metric", "Count")
colnames(patient_experience_comparison_df) <- c("Metric", "Count")
colnames(patient_experience_comparison_footnote_df) <- c("Metric", "Count")
colnames(effectiveness_comparison_df) <- c("Metric", "Count")
colnames(effectiveness_comparison_footnote_df) <- c("Metric", "Count")
colnames(timeliness_comparison_df) <- c("Metric", "Count")
colnames(timeliness_comparison_footnote_df) <- c("Metric", "Count")
colnames(efficient_imaging_comparison_df) <- c("Metric", "Count")
colnames(efficient_imaging_comparison_footnote_df) <- c("Metric", "Count")

# Combine data into one table
combined_df <- bind_rows(
  mutate(mortality_comparison_df, Category = "Mortality Comparison"),
  mutate(mortality_comparison_footnote_df, Category = "Mortality Comparison Footnote"),
  mutate(safety_comparison_df, Category = "Safety of Care Comparison"),
  mutate(safety_comparison_footnote_df, Category = "Safety of Care Comparison Footnote"),
  mutate(readmission_comparison_df, Category = "Readmission Comparison"),
  mutate(readmission_comparison_footnote_df, Category = "Readmission Comparison Footnote"),
  mutate(patient_experience_comparison_df, Category = "Patient Experience Comparison"),
  mutate(patient_experience_comparison_footnote_df, Category = "Patient Experience Comparison Footnote"),
  mutate(effectiveness_comparison_df, Category = "Effectiveness of Care Comparison"),
  mutate(effectiveness_comparison_footnote_df, Category = "Effectiveness of Care Comparison Footnote"),
  mutate(timeliness_comparison_df, Category = "Timeliness of Care Comparison"),
  mutate(timeliness_comparison_footnote_df, Category = "Timeliness of Care Comparison Footnote"),
  mutate(efficient_imaging_comparison_df, Category = "Efficient Use of Medical Imaging Comparison"),
  mutate(efficient_imaging_comparison_footnote_df, Category = "Efficient Use of Medical Imaging Comparison Footnote")
)

# Display selected columns
selected_columns <- data %>%
  select(
    Mortality.national.comparison,
    Mortality.national.comparison.footnote,
    Safety.of.care.national.comparison,
    Safety.of.care.national.comparison.footnote,
    Readmission.national.comparison,
    Readmission.national.comparison.footnote,
    Patient.experience.national.comparison,
    Patient.experience.national.comparison.footnote,
    Effectiveness.of.care.national.comparison,
    Effectiveness.of.care.national.comparison.footnote,
    Timeliness.of.care.national.comparison,
    Timeliness.of.care.national.comparison.footnote,
    Efficient.use.of.medical.imaging.national.comparison,
    Efficient.use.of.medical.imaging.national.comparison.footnote
  )
