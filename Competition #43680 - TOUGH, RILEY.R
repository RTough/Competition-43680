##### Author Information #####
# Leaders in Training Program - Data Science Stream (P3) - 43680 Assessment
# R Version: 4.4.2
# Date Created: February 18-2025
# By: Riley H. Tough
# Contact: toughr@myumanitoba.ca

##### Library Control #####
cran_libraries = c("remotes","tidyverse", "data.table", "ggplot2", "dplyr", "stringr", "tm", "SnowballC") # Most common ones I use as a default
install.packages("data.table")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("tm")
install.packages("SnowballC")
install.packages("stringdist")
install.packages("lubridate")

library(tm)
library(SnowballC)
library(lubridate)



#Function to check for and install 
install_missing_cran_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(missing_packages)) {
    install.packages(missing_packages, dependencies = TRUE)
  }
}
install_missing_cran_packages(cran_libraries)

# Load the required libraries
lapply(cran_libraries, library, character.only = TRUE)

# Create a lockfile to ensure that package versions remain constant
##renv::snapshot() # Do not reset this value unless you are certain you know what you are doing

##### Loading in the Data #####
Inspections_20250218 <- read.csv("~/Professional/Applications/Provincial/DataIntern/Assessment/Data/Building_and_Safety_Inspections_20250218.csv")
Permit_Information_20250218 <- read.csv("~/Professional/Applications/Provincial/DataIntern/Assessment/Data/Building_and_Safety_Permit_Information_Old_20250218.csv")

#Remove the periods from the column names
colnames(Permit_Information_20250218)
colnames(Inspections_20250218)

colnames(Permit_Information_20250218) = gsub("\\.", "", colnames(Permit_Information_20250218))
colnames(Inspections_20250218) = gsub("\\.", "", colnames(Inspections_20250218))

# Standard Data Cleaning and Pre-Processing
colnames(Inspections_20250218)
Inspections = Inspections_20250218 %>%
  dplyr::select(c("PERMIT", "PermitStatus", "InspectionType", "InspectionDate", "InspectionResult","ADDRESS"))
sum(is.na(Inspections$PERMIT)) # There are no missing permit IDs

colnames(Permit_Information_20250218) # Only need specific columns for addressing this question
Permits = Permit_Information_20250218 %>%
  dplyr::mutate(PermitAddress = paste(AddressStart, StreetDirection, StreetName, StreetSuffix)) #Including the address to double check permit is for the right location
sum(is.na(Permits$PCISPermit)) # There are no missing permit IDs

# Are the permits in the same format?
head(Permits$PCISPermit)
head(Inspections$PERMIT) #Permits are not in the same format

# Modify the permit format to be "-" separated and merge Permit information with Inspection information
Permits$PCISPermit = gsub(" ", "-", Permits$PCISPermit)
Permit_Inspection = merge(Permits, by.x="PCISPermit", Inspections, by.y = "PERMIT")


Permit_Inspection %>% count(InspectionResult, sort = TRUE) # 51 different types of result including 241075 blank
Permit_Inspection %>% count(InspectionType, sort = TRUE) # 155 different types of result including 241075 blank
length(unique(Permit_Inspection$PermitStatus)) # 16 unique Permit Statuses

# While a Bag of Words model might be useful here, the risk of not understanding technical jargon is just too high
# While tedious, it's much better to just categorize the technical terms manually

###### Definitions ####
### Categories ###
### Permit Completed ###
# Permit Closed: The permit was issued and the work may or may not have been completed. The permit is no longer active.
# Permit Finaled: The work associated with the issued permit and all the required inspections were completed.
## Newly Issued##
# Issued: The permit application has been issued and is referred to as an "issued permit".
### Active or In Progress ###
# CofO in Progress: Some building permits require a Certificate of Occupancy (CofO) to be issued before the building can be occupied. This status indicates that the required CofO is in the process of being issued but has not been issued yet
# Insp Scheduled: No information ~ Pending.
# No Progress: No information ~ Pending.
# Ready to Issue: The permit application is ready to be issued once the permit issuance fees are paid. The permit has not been issued.
#	CofO Issued: Some building permits require a Certificate of Occupancy (CofO) to be issued before the building can be occupied. This status indicates that the required CofO has been issued.
# CofO Corrected: No information ~ Pending.
# Refund in Progress: The refund process associated with a refund request for an issued permit has begun. No inspection activity can occur once the refund process has started.
# Intent to Revoke: LADBS has started the process of revoking the permit. No inspection activity can occur once the revocation process has started.
### Failed/Revoked/Withdrawn ###
# CofO Revoked: No information ~ Failed.
# Permit Expired: No work was done and the issued permit has been expired. The issued permit is no longer active.
# Permit Withdrawn: For some reason, the issued permit was withdrawn by the applicant. The issued permit is no longer active.
### Refunded ###
# Refund Completed: The refund process associated with a refund request for an issued permit has been completed. The issued permit is no longer active.

####### Resuming Analysis ####
length(unique(Permit_Inspection$PermitStatus))
Permit_Inspection <- Permit_Inspection %>%
  mutate(PermitSummary = case_when(
    PermitStatus %in% c("Permit Finaled", "Permit Closed") ~ "Permit Completed",
    PermitStatus == "Issued" ~ "Newly Issued",
    PermitStatus %in% c("CofO in Progress", "Insp Scheduled", "No Progress", "Ready to Issue", "CofO Issued", "CofO Corrected", "Refund in Progress", "Intent to Revoke") ~ "Active or in Progress",
    PermitStatus %in% c("CofO Revoked", "Permit Expired", "Permit Withdrawn") ~ "Failed/Revoked/Withdrawn",
    PermitStatus == "Refund Completed" ~ "Refunded",
    PermitStatus == "" ~ "Remove")
  )

Permit_Inspection <- Permit_Inspection %>%
  mutate(InspectionResultCategory = case_when(
    InspectionResult %in% c("Approved", "Permit Finaled", "Completed", "CofO Issued", "SGSOV Approved", "Completed (special insp)", "") ~ "Approved/Completed",
    InspectionResult %in% c("CofO in Progress", "Pending", "Pending Review", "Insp Scheduled", "Not Ready for Inspection", "Corrections Issued", "No Progress", "Partial Approval", 
                            "CofO on Hold") ~ "Pending/Under Review",
    InspectionResult %in% c("Conditional Approval", "OK for CofO", "OK to Issue CofO", "OK for TCO") ~ "Temporary Approval",
    InspectionResult %in% c("Insp Cancelled", "Cancelled", "Permit Withdrawn", "Permit Closed-Status Void") ~ "Cancelled/Withdrawn",
    InspectionResult %in% c("Order to Comply Issued", "No Access for Inspection", "SGSOV Not Ready", "SGSOV Not Required", "SGSOV No Gas") ~ "Violations",
    InspectionResult %in% c("Permit Expired", "OK to Expire Permit", "Partial Inspection", "Permit Closed") ~ "Expired/Inactive",
    InspectionResult %in% c("Off-Hour Fees Due", "SGSOV Gas Company", "CofO Corrected") ~ "Other",
    InspectionResult == "Not Applicable" ~ "Not Applicable")
  )


##### Question 1 #####
# Make a table and a visualization showing an interesting characteristic of the permit and inspection dataset. #
# What might be interesting about the permit and inspections datasets?

## Important Questions to ask ##
## How many permits are issues each year?
## How many inspections are conducted each year and what are trends in inspection results over time?

### Question: What is the overall status of permits over the past 10 years? ###
Q1_Data = Permit_Inspection
Q1_Data = Q1_Data %>%
  dplyr::filter(!(PermitSummary %in% "Remove"))
Q1_Data$PermitSummary = factor(Q1_Data$PermitSummary,
                                             levels = c("Permit Completed","Newly Issued", "Active or in Progress", "Failed/Revoked/Withdrawn", "Refunded", "Remove"))
Q1_Permit_Status_Summary <- Q1_Data %>%
  dplyr::count(PermitSummary, sort = TRUE) %>%
  dplyr::mutate( Percent = n / sum(n) * 100
  )
print(Q1_Permit_Status_Summary)

# Graphing the Result
ggplot(Q1_Permit_Status_Summary, aes(x = PermitSummary, y = n, fill = PermitSummary)) +
  geom_bar(stat = "identity") +
  labs(title = "Summary of Permit Status Between 2013 and 2023",
       x = "Permit Status",
       y = "Number of Inspections") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

### Question: How many inspections are conducted per year? ###
InspectionsPerYear <- Q1_Data %>%
  dplyr::select(c("InspectionDate", "InspectionResult", "InspectionResultCategory")) %>%
  mutate(InspectionYear = year(mdy(InspectionDate))) %>%
  filter(!is.na(InspectionYear))  # Exclude missing InspectionDate

# Count the number of permits per year and status category
InspectionsPerYear <- InspectionsPerYear %>%
  dplyr::group_by(InspectionYear, InspectionResultCategory) %>%
  dplyr::summarise(Count = n(), .groups = "drop") %>%
  dplyr::filter(!(is.na(InspectionResultCategory )))

# Plot separate line charts for each permit status
ggplot(InspectionsPerYear, aes(x = InspectionYear, y = Count, color = InspectionResultCategory, group = InspectionResultCategory)) +
  geom_line(size = 1) +
  labs(title = "Yearly Inspection Trends",
       x = "Year",
       y = "Number of Inspections") +
  theme_minimal()
# The number of approved/pending inspections seem to have dramatically decreased whereas the number of violations has not

##### Question 2 #####
# Make a table and a visualization showing the number of inspections by geography. In a sentence or two describe any patterns you observe.
# Necessary packages
# Statefiles
library(sf)

# Step One: Calculate Inspections by ZIP Code
# Count inspections by ZIP Code & Council District
Inspection_Summary <- Permit_Inspection %>%
  filter(!(ZipCode == "0")) %>%
  group_by(ZipCode, CouncilDistrict) %>%
  summarise(Num_Inspections = n(), .groups = "drop")

# Print summary table
print(Inspection_Summary)

# Creating a map of Los Angeles
#  Separate the latitude and longitude
Permit_Inspection$Latitude = gsub("\\(","", Permit_Inspection$LatitudeLongitude)
Permit_Inspection$Latitude = as.numeric(gsub(",.*","", Permit_Inspection$Latitude))
Permit_Inspection = Permit_Inspection %>%
  mutate(Longitude = as.numeric(sub(".*,(.*)\\)", "\\1", LatitudeLongitude))) # Extract longitude

# Remove missing values
Permit_Inspection <- Permit_Inspection %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Plot inspections on a simple map
# What are the different Zip Codes in LA? 
ggplot(Permit_Inspection, aes(x = Longitude, y = Latitude)) +
  borders("state", region = "california") +  # Adjust for relevant region
  geom_point(aes(color = as.factor(ZipCode)), alpha = 0.6) +
  scale_color_viridis_d() +
  coord_cartesian(xlim = c(-118.7, -118.1), ylim = c(33.7, 34.4)) +  # The latitude/longitude of LA
  labs(title = "Number of Inspections by Zip Code in LA",
       x = "Longitude",
       y = "Latitude") +
  guides(fill="none")+
  theme_bw()


Permit_Inspection <- Permit_Inspection %>%
  mutate(InspectionResultCategory = case_when(
    InspectionResult %in% c("Approved", "Permit Finaled", "Completed", "CofO Issued", "SGSOV Approved", "Completed (special insp)", "") ~ "Approved/Completed",
    InspectionResult %in% c("CofO in Progress", "Pending", "Pending Review", "Insp Scheduled", "Not Ready for Inspection", "Corrections Issued", "No Progress", "Partial Approval", 
                            "CofO on Hold") ~ "Pending/Under Review",
    InspectionResult %in% c("Conditional Approval", "OK for CofO", "OK to Issue CofO", "OK for TCO") ~ "Temporary Approval",
    InspectionResult %in% c("Insp Cancelled", "Cancelled", "Permit Withdrawn", "Permit Closed-Status Void") ~ "Cancelled/Withdrawn",
    InspectionResult %in% c("Order to Comply Issued", "No Access for Inspection", "SGSOV Not Ready", "SGSOV Not Required", "SGSOV No Gas") ~ "Violations",
    InspectionResult %in% c("Permit Expired", "OK to Expire Permit", "Partial Inspection", "Permit Closed") ~ "Expired/Inactive",
    InspectionResult %in% c("Off-Hour Fees Due", "SGSOV Gas Company", "CofO Corrected") ~ "Other",
    InspectionResult == "Not Applicable" ~ "Not Applicable")
  )

Permit_Inspection$InspectionResultCategory = factor(Permit_Inspection$InspectionResultCategory,
                                                    levels = c("Pending/Under Review","Approved/Completed",  "Temporary Approval", "Cancelled/Withdrawn",
                                                               "Expired/Inactive", "Violations", "Other", "Not Applicable"))

Q2_Data = Permit_Inspection
Q2_Data = Q2_Data %>%
  dplyr::filter(!is.na(InspectionResultCategory)) %>%
  dplyr::filter(!(InspectionResultCategory  %in% c("Other", "Not Applicable")))

ggplot(Permit_Inspection, aes(x = Longitude, y = Latitude)) +
  borders("state", region = "california") +  # Adjust for relevant region
  geom_point(aes(color = as.factor(InspectionResultCategory)), alpha = 0.6) +
  scale_color_viridis_d() +
  coord_cartesian(xlim = c(-118.7, -118.1), ylim = c(33.7, 34.4)) +  # The latitude/longitude of LA
  labs(title = "Number of Inspections by Zip Code in LA",
       x = "Longitude",
       y = "Latitude") +
  theme_bw()

###### Make a table and a visualization showing the results of inspections across geographies. In a sentence or two describe any patterns you observe.######
# Interestingly, there are

###### Were there any permits that did not get an inspection? #####
# Notably, there are a number of permits that have blank inspection dates and no result. 
# This could represent ongoing work, or simply that when the final data was logged, that there were no results which ended up being
# record as a blank value.

##### Question 3 #####
#Your manager is convinced ‘out of town’ contractors are not as invested in the success of their projects and so are the main culprits when it comes to violations. You are asked to complete an analysis to test this hypothesis.
#Produce a model that quantifies the relationship between a contractor’s place of origin and their inspection outcome history. Investigate any other relevant factors as necessary.
#Interpret your results and produce a clear response for your manager.


# Hypothesis: Contractors from "out of town" are more likely to have permit violations

# ContractorState and InspectionResultCategory are most important for this
#I apologize that I may have to sudo code the following details

# Perform data cleaning to remove NA values
# Set ContractorState as the factor

#Run a logistic regression comparing ContractorState and InspectionResultCategory
## Assess the potential for covariates including type of inspection to determine whether out of town contractors are more likely to commit certain violations
# It would also be really interesting to compare the evaluation and type of project

# This would use a glm() model in R. 

