# Clear the environment
rm(list = ls())

## Load Packages

library(tidyverse)
library(dplyr)
library(tidyr)
library(labelled)
library(ggplot2)
library(readr)
library(readxl)
library(gtsummary)
library(DataExplorer)
library(survey)


# Set the working directory to your data file path
Folder <- file.path("C:/Users/saidul.haq/OneDrive - World Food Programme/Desktop/Workspace/Asthma/Final Analysis/R Analysis")
setwd(Folder)


# Load the dataset
Risk_data <- read.csv("Risk Factor.csv")
asthma_data <- read.csv("Asthma_data.csv")
weight_data <- readxl::read_excel("Final post weight_CAS.xlsx", sheet="sheet1")


## Merge Asthma data with Weigth Data

data <- merge(asthma_data, weight_data, by.x = "Psucode", by.y = "psu", all = FALSE, sort = TRUE)

## Data Profiling and See columns names

#create_report(data)

column_names <- colnames(data)
print(column_names)


write.csv(data, "my_data.csv", row.names = FALSE)

## data recoding create a new variable for child Age group where 5-9 is 1 and 10-17 is o


design <- svydesign(id=~1, strata=~PSU, weights = ~wgt, data = data)
summary(design)




#-------------------------------------Soci-economic weithting -----------------------
#
#
#------------------------------------------------------------------------------------------------------------

# Respondent gender

res_gen <- svytable(~RespondedGender, design = design)
res_df <- as.data.frame(res_gen)
res_gen_table <- data.frame(Age = rownames(res_df), Number = res_df$Freq)
print(res_gen_table)

# Respondent Age group
res_age <- svytable(~Age.Respondent..group., design = design)
res_age_df <- as.data.frame(res_age)
res_age_df_table <- data.frame(Age = rownames(res_age_df), Number = res_age_df$Freq)
print(res_age_df_table)

# Place of Residence 

residence <- svytable(~Area..group., design = design)
residence_df <- as.data.frame(residence)
residence_df_table <- data.frame(Age = rownames(residence_df), Number = residence_df$Freq)
print(residence_df_table )


#  Divison

divison <- svytable(~Division, design = design)
divison_df <- as.data.frame(divison)
divison_df_table <- data.frame(Age = rownames(divison_df), Number = divison_df$Freq)
print(divison_df_table )

####Table 0.2 : Socio-demographic characteristics of study population (5-17 years old)

#  Child Genger

child_gender <- svytable(~ChildGender, design = design)
child_gender_df <- as.data.frame(child_gender)
child_gender_df_table <- data.frame(Age = rownames(child_gender_df), Number = child_gender_df$Freq)
print(child_gender_df_table )
print(child_gender)

#  Child Age

child_age <- svytable(~ChildAge..group., design = design)
child_age_df <- as.data.frame(child_age)
child_age_df_table <- data.frame(Age = rownames(child_age_df), Number = child_age_df$Freq)
print(child_age_df_table )
print(child_age)

#  Child Education

child_edu <- svytable(~Child.Education..group., design = design)
child_edu_df <- as.data.frame(child_edu)
child_edu_df_table <- data.frame(Age = rownames(child_edu_df), Number = child_edu_df$Freq)
print(child_edu_df_table )
print(child_edu)

#  Father Education

father_edu <- svytable(~Father.Edu..group., design = design)
father_edu_df <- as.data.frame(father_edu)
father_edu_df_table <- data.frame(Age = rownames(father_edu_df), Number = father_edu_df$Freq)
print(father_edu_df_table )
print(father_edu)

#  Father Ocupation

father_ocup <- svytable(~Father.Ocupation..group., design = design)
father_ocup_df <- as.data.frame(father_ocup)
father_ocup_df_table <- data.frame(Age = rownames(father_ocup_df), Number = father_ocup_df$Freq)
print(father_ocup_df_table )
print(father_ocup)

table(asthma_data$Father.Ocupation..group.)
table(asthma_data$Religion)

#  Mother Education

mother_edu <- svytable(~Mother.Edu..group., design = design)
mother_edu_df <- as.data.frame(mother_edu)
mother_edu_df_table <- data.frame(Age = rownames(mother_edu_df), Number = mother_edu_df$Freq)
print(mother_edu_df_table )
print(mother_edu)

#  Mother Ocupation

mother_ocup <- svytable(~Mother.Ocupation..group., design = design)
mother_ocup_df <- as.data.frame(mother_ocup)
mother_ocup_df_table <- data.frame(Age = rownames(mother_ocup_df), Number = mother_ocup_df$Freq)
print(mother_ocup_df_table )
print(mother_ocup)

## Table 0.3 : Household characteristics of study population
# Family Size
family_size <- svytable(~FamilySize..group., design = design)
print(family_size)

# Total 5-17 Year Age of
total_5_17_child <- svytable(~Total.Child.5.17, design = design)
print(total_5_17_child)

# religion
religion <- svytable(~Religion, design = design)
print(religion)

# Number of Room Used for sleeping
room_sleeping <- svytable(~Noofroom, design = design)
print(room_sleeping)

# Material used in HH construction

floor_mat <- svytable(~Materialdwellingfloorcat, design = design) # Type of floor material
print(floor_mat)

roof_mat <- svytable(~Materialroofcat, design = design) # Type of material in roof
print(roof_mat)

ext_wall <- svytable(~Materialexteriorwallgroup, design = design) # Type of material in the exterior wall
print(ext_wall)

# type of coke stove
cook_stove <- svytable(~TypeOfStove, design = design) # Type of floor material
print(cook_stove)

# type of fuel
fuel_type <- svytable(~TypeOfFuelGroup, design = design) # Type of floor material
print(fuel_type)

# Others single household assest variable

#------------ In this section as all the household variable is started with have so I take startwith funciton 
# Get variable names that start with "have"
have_variables <- grep("^have", names(data), value = TRUE)

# Loop through each "have" variable
for (variable in have_variables) {
  cat("Analyzing variable:", variable, "\n")
  # Create the svytable for the current variable
  result <- svytable(as.formula(paste("~", variable)), design = design)
  
  # Print the result
  print(result)
}
#--------------------------------


# BMI of Children
bmi <- svytable(~BMI.Cat, design = design) # BMI of children to see nutritional status
print(bmi)

# see the age distirbution 
age1 <- svytable(~ChildAge.1, design = design)
print(age1)



#---------------------- Cross tabulation with Asthma-----------------

#--------------------------------------------------------------------

gender_asthma <- svytable(~ ChildGender+ Active.asthma, design = design) # Prevalence of Asthma by Gender
print(gender_asthma)

age_group <- svytable(~ChildAge..group.+Active.asthma, design = design) # Prevalence by Age group
print(age_group)

place_res <- svytable(~Area..group.+ Active.asthma, design = design) # Prevalence by Place of Residence 
print(place_res)

division <- svytable(~Division+ Active.asthma, design = design) # Prevalence by Division
print(division)


table_active_asthma_age <- svytable(~ ChildAge.1+ Active.asthma, design = design) # Prevalence by Age not group
print(table_active_asthma_age)


# row_percentages <- prop.table(table_active_asthma, margin = 2) * 100    #* WE can use this code to find row percentage 


#---------------------- Cross tabulation cough, Wheez, others----------------

Have.Recurent.Cough, Have.Cough, Have.Persistant.Cough, Cough.Dry, Sytmops.Trigger.Cough..group.
#--------------------------------------------------------------------

recurent_cough <- svytable(~ ChildAge..group.+ Have.Recurent.Cough, design = design) # Recurent cough
print(recurent_cough)

per_cough <- svytable(~ ChildAge..group.+ Have.Persistant.Cough, design = design) # Persistent cough
print(per_cough)

dry_cough <- svytable(~ ChildAge..group.+ Cough.Dry, design = design) # dry cough
print(dry_cough)

night_wake <- svytable(~ ChildAge..group.+ Wakeup.Night, design = design) # sleep disturbance at night due to cough
print(night_wake)

cold <- svytable(~ ChildAge..group.+ Dry.Cough.History, design = design) # having dry cough without fever or cold
print(cold)


sytmptomtig_cough <- svytable(~ ChildAge..group.+ Sytmops.Trigger.Cough..group., design = design) # Symtomps triggering cough
print(sytmptomtig_cough)


physcial_cough <- svytable(~ ChildAge..group.+ cough.relation.Physical, design = design) # Cough relation to physical activities
print(physical_cough)

########################### Prevalence of wheez ######################################################

current_wheez <- svytable(~ ChildAge..group.+ Have.Wheez, design = design) # Current Wheez
print(current_wheez)

ever_wheez <- svytable(~ ChildAge..group.+ Wheez.History, design = design) # Ever Wheez
print(ever_wheez)

month12_wheez <- svytable(~ ChildAge..group.+ Wheez.12Month, design = design) # Wheez pas 12 months
print(month12_wheez)

respiratory_distress <- svytable(~ ChildAge..group.+ Wheez.12Month, design = design) # Suffered respiratory distress
print(respiratory_distress)

wheez_diffculties <- svytable(~ ChildAge..group.+ Wheez.12Month.Severe, design = design) # Wheez related difficulties of Spech
print(wheez_diffculties)

attack_wheez_12month <- svytable(~ ChildAge..group.+ Wheez.Attack.Times..group., design = design) # Attack of Wheez in the past 12months
print(attack_wheez_12month)

wheez_trigger <- svytable(~ ChildAge..group.+ Wheez.Session.Time, design = design) # Triggering factor of Wheez (Sessonal attack)
print(wheez_trigger)

wheez_time_day <- svytable(~ ChildAge..group.+ wheez.partOfDay, design = design) # Wheez Specific time of the day
print(wheez_time_day)

###-----------------------------------------------table 3 atopy allerge-------------------------------------------------------------------------

atopy <- svytable(~ ChildAge..group.+ F30have.atopy.allergy, design = design) # Wheez Specific time of the day
print(atopy)


#-------------Doctor visit

doctor_visit <- svytable(~ ChildAge..group.+ Visit.Doctor..group., design = design) # Wheez Specific time of the day
print(doctor_visit)


relivSymptom_visit <- svytable(~ ChildAge..group.+ Relieve.Symptoms, design = design) # Wheez Specific time of the day
print(relivSymptomt)

school_miss <- svytable(~ ChildAge..group.+ School.miss, design = design) # Wheez Specific time of the day
print(school_miss1)


##----------Emergency Department visit--------------------
emer_visit <- svytable(~ ChildAge..group.+ Urgent.Emergency..group., design = design) # Wheez Specific time of the day
print(emer_visit)

school_miss1 <- svytable(~ ChildAge..group.+ School.miss..group., design = design) # Wheez Specific time of the day
print(school_miss1)

###########################    not use ######################################################
table_socio <- svytable(~ Age.Respondent..group. + Area..group. + Division + RespondedGender, design = design)
result_df <- as.data.frame(table_socio)
print(result_df)

# Create a data frame with the desired format
final_table <- data.frame(
  Age = rep(rownames(result_df), ncol(result_df$Freq)),
  Variable = rep(colnames(result_df$Freq), each = nrow(result_df)),
  Number = c(result_df$Freq)
)

print(final_table)


########################-------------------------------------#########################################













#-------------------------------------variable recoding and new variable generate here-----------------------
#
#
#------------------------------------------------------------------------------------------------------------

## Create new Age group and Area to fit the varaibles for regression analysis 
data <- data %>%
  mutate(
    ChildAgeGroup = ifelse(ChildAge..group.=="1",0,1),
    AreaGroup = ifelse(Area..group.=="Urban", 1,0)
    
    )

#------------------------------------end-----------------


#------------------ tabulate the variables and see variales type-------------------------------------------

table(data$ChildAge..group.)
table(data$ChildAgeGroup)
table(data$AreaGroup)
#--------------------------------------end----------------

#-------------------------------------Regression Analysis--------------------------------------------------
#                 To develop model for Odd ratio, Confidence interval, and P value
#
#------------------------------------------------------------------------------------------------------------

## Generate different models for unadjusted and adjusted odd ratios and confidence intervals (Age group, Area)
model_results <- list()
model_results$model_age = glm(Active.asthma~ChildAgeGroup, data=data, family = "binomial")
model_results$model_area = glm(Active.asthma~AreaGroup, data=data, family = "binomial")
model_results$model_age_adj = glm(Active.asthma~ ChildAgeGroup+AreaGroup, data=data, family = "binomial")
model_results$model_area_adj = glm(Active.asthma~ AreaGroup+ ChildAgeGroup, data=data, family = "binomial")
lapply(model_results, summary)

##
model_names <- names(model_results)

# Loop through the list of models

for(name in model_names) {
  model <- model_results[[name]]
  
  # Extract odd ratios and confidance intervals and P-value
  
  odd_ratios <- exp(coef(model))
  conf_interval <- exp(confint(model, level=0.95))
  p_value <- summary(model)$coefficients[, "Pr(>|z|)"] # Extracting P-value
  # Display results
  cat("Model:", name, "\n")
  cat("Odd Ratios: \n")
  print(odd_ratios)
  
  cat("\n 95% Confidence Intervals: \n")
  print(conf_interval)
  
  cat("\nP-values: \n")
  print(p_value)
  cat("\n")
}




#-------------------------------------------------Example code not need use ------------------


#--------------------------------------------------------------------------------------------
## Regression Analysis

model <- glm(Active.asthma~ChildAgeGroup, data=data, family = "binomial")
summary(model)

# Extracting odd ratios and their confidence interval
odd_ratio <- exp(coef(model))
conf_int <- exp(confint(model))
# Show results
cat("Odd ratio: \n")
print(odd_ratio)


## Chi squar test and Odd ratio 
age_tab = table(data$ChildAgeGroup, data$Active.asthma)
chisq.test(age_tab)
fisher.test(age_tab)

#-------------------------end of the section----------------------------------------------------







# Re-code Gender

data$gender_child <-NA

data$gender_child [data$HI7Genderofchild_name=='Female']<-0
data$gender_child [data$HI7Genderofchild_name=='Male']<-1
data$gender_child[data$HI7Genderofchild_name=='Third gender'] <-2
# define value label
gender_labels <- c("Female", "Male", "Third Gender")
levels(data$gender_child)<- gender_labels

# calcuate the categoires of the houshld size 

data <- data %>% 
  mutate(
    familysize_cat = case_when(
      family_size %in% 1:3 ~ "Small",
      family_size %in% 4:6 ~ "Medium",
      family_size %in% 7:9 ~"Large",
      TRUE~ 'Extra large'
    ), 
    familysize_cat = factor(familysize_cat, levels = c("Small", "Medium", "Large", "Extra large"))
    
  )

summary(data$familysize_cat)


# Produce table

tab4 <- data %>% 
  select(gender_respondent, have_cough, Division) %>%
  tbl_summary(missing = "no",
              by = Division,
              statistic= all_continuous()~  c("{mean}", "{min}", "{max}"),
              label= list(
                gender_respondent ~ "Gender",
                have_cough ~ "Have Cough"
              )
  )
tab4
