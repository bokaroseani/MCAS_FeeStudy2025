library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
library(tidyverse)



# 2019
file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  licenses19 <- read.csv(file_path, fileEncoding = "Windows-1252")
}

# 2020
file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  licenses20 <- read.csv(file_path, fileEncoding = "Windows-1252")
}

# 2021
file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  licenses21 <- read.csv(file_path, fileEncoding = "Windows-1252")
}

# 2022
file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  licenses22 <- read.csv(file_path, fileEncoding = "Windows-1252")
}

# 2023
file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  licenses23 <- read.csv(file_path, fileEncoding = "Windows-1252")
}

# 2024
file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  licenses24 <- read.csv(file_path, fileEncoding = "Windows-1252")
}

# 2025
file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  licenses25 <- read.csv(file_path, fileEncoding = "Windows-1252")
}

### Merge all the files
all <- rbind(licenses19, licenses20, licenses21, licenses22, licenses23, licenses24, licenses25)
all <- all %>%
  mutate(
    # Step 1: Convert character string to a proper date object 🗓️
    Issue.Date = mdy(Issue.Date),
    
    # Step 2: Calculate the fiscal year number
    FY_year = if_else(month(Issue.Date) >= 7, year(Issue.Date) + 1, year(Issue.Date)),
    
    # Step 3: Format it as "FY" followed by the last two digits
    FY = paste0("FY", FY_year), 
    License.Fee1 = str_remove_all(License.Fee, "[\\$,]") %>% as.numeric()
  ) %>%
  
  # Optional: Remove the intermediate helper column
  select(-FY_year)

#save(all, file = "Z:/My Drive/Animal Services/MCAS Fee Study/2025 Animal Services Financial Data/ShelterBuddyData/all6yearsLicense.RData")

###########################
#### START FROM HERE ######
###########################

load("Z:/My Drive/Animal Services/MCAS Fee Study/2025 Animal Services Financial Data/ShelterBuddyData/all6yearsLicense.RData")


all6<- subset(all, all$FY %in% c("FY2020", "FY2021", "FY2022", "FY2023", "FY2024", "FY2025"))

ggplot(data = all6, aes(x = License.Fee1)) +
  geom_histogram()


temp <- all6 |> 
  group_by(FY, License.Type) |> 
  count()

temp1 <- as.data.frame(table(all6$License.Type, all6$License.Fee1, all6$FY))

temp1_reordered <- temp1 %>%
  mutate(Var1 = fct_reorder(Var1, Freq, .fun = sum, .desc = TRUE))

ggplot(temp1, aes(x = Var3, y = Freq)) +
  geom_bar(aes(fill = Var2), stat = "identity", position = "stack") +
  facet_wrap(~ Var1) +
  labs(title = "Stacked Bar Chart of Freq by Var1, Var2, and Var3",
       x = "Variable 1",
       y = "Frequency",
       fill = "Variable 2") +
  theme_minimal()



# For each level of License.Type, find the number of licenses issued each year and make a bar chart
temp2 <- as.data.frame(table(all6$License.Type, all6$FY))

temp2Dog <- subset(temp2, grepl("Dog", temp2$Var1))

ggplot(temp2Dog, aes(x = Var2, y = Freq)) +
  geom_bar(stat = "identity", position = "stack", fill = "skyblue") +
  geom_text(aes(label = Freq), hjust = -0.25, size = 2.5, color = "black") +
  facet_wrap(~ Var1, ncol = 2) +
  labs(title = "Number of Dog licenses processed in the past 6 years",
       x = "Fiscal Year",
       y = "Number of licenses") +
  coord_flip()

temp2Cat <- subset(temp2, grepl("Cat", temp2$Var1))

ggplot(temp2Cat, aes(x = Var2, y = Freq)) +
  geom_bar(stat = "identity", position = "stack", fill = "skyblue") +
  geom_text(aes(label = Freq), hjust = -0.25, size = 2.5, color = "black") +
  facet_wrap(~ Var1, ncol = 2) +
  labs(title = "Number of Cat licenses processed in the past 6 years",
       x = "Fiscal Year",
       y = "Number of licenses") +
  coord_flip()


temp2PDD <- subset(temp2, grepl("PDD", temp2$Var1))

ggplot(temp2PDD, aes(x = Var2, y = Freq)) +
  geom_bar(stat = "identity", position = "stack", fill = "skyblue") +
  geom_text(aes(label = Freq), hjust = -0.25, size = 2.5, color = "black") +
  facet_wrap(~ Var1, ncol = 2) +
  labs(title = "Number of Potentially Dangerous Dog licenses processed in the past 6 years",
       x = "Fiscal Year",
       y = "Number of licenses") +
  coord_flip()

# For each License.Type, find the total amount of fees collected each year and make a bar chart
temp3 <- all6 |> 
  group_by(FY, License.Type) |> 
  summarise(TotalFees = sum(License.Fee1))

temp3Dog <- subset(temp3, grepl("Dog", temp3$License.Type))
sum(temp3Dog$TotalFees[temp3Dog$FY == "FY2025"])

ggplot(temp3Dog, aes(x = FY, y = TotalFees)) +
  geom_bar(stat = "identity", position = "stack", fill = "darkgreen") +
  geom_text(aes(label = TotalFees), hjust = -0.25, size = 2.5, color = "black") +
  facet_wrap(~ License.Type) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Annual revenue from Dog licenses for the past 6 years",
       x = "Fiscal Year",
       y = "Total amount") +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

temp3Cat <- subset(temp3, grepl("Cat", temp3$License.Type))

ggplot(temp3Cat, aes(x = FY, y = TotalFees)) +
  geom_bar(stat = "identity", position = "stack", fill = "darkgreen") +
  geom_text(aes(label = TotalFees), hjust = -0.25, size = 2.5, color = "black") +
  facet_wrap(~ License.Type) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Annual revenue from Cat licenses for the past 6 years",
       x = "Fiscal Year",
       y = "Total amount") +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

temp3PDD <- subset(temp3, grepl("PDD", temp3$License.Type))

ggplot(temp3PDD, aes(x = FY, y = TotalFees)) +
  geom_bar(stat = "identity", position = "stack", fill = "darkgreen") +
  geom_text(aes(label = TotalFees), hjust = -0.25, size = 2.5, color = "black") +
  facet_wrap(~ License.Type) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Annual revenue from Potentially Dangerous\nDog (PDD) licenses for the past 6 years",
       x = "Fiscal Year",
       y = "Total amount") +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

###################################################################################################
##################################   ADOPTIONS   ##################################################
###################################################################################################
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
library(tidyverse)


file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  Adoptions <- read.csv(file_path, fileEncoding = "Windows-1252")
}


names(Adoptions)
Adoptions$Adoption.Date
summary(as.factor(Adoptions$Adoption.Amount))
Adoptions$Adoption.Amount[Adoptions$Adoption.Amount == ""] <- NA


Adoptions <- Adoptions |> 
  mutate(
    Adoption.Date = as.POSIXct(Adoption.Date, format = "%m/%d/%Y %H:%M"),
    FY_year = if_else(month(Adoption.Date) >= 7, year(Adoption.Date) + 1, year(Adoption.Date)),
    FY = paste0("FY ", FY_year),
    SpayNeuter.Status = `Spay...Neuter.Status` %>%
      str_to_title() %>%
      as.factor(),
    Adoption.Amount1 = str_remove_all(as.character(Adoptions$Adoption.Amount), "[\\$,]") %>% as.numeric(),
    Receipt.Total1 = str_remove_all(as.character(Adoptions$Receipt.Total), "[\\$,]") %>% as.numeric()
  ) %>%
  select(-FY_year) |> 
  select(-Spay...Neuter.Status)
    

names(Adoptions)

selection <- c("Person.DOB", 
               "Spay...Neuter.Status", 
               "Type",  
               "Animal.Gender", 
               "Adoption.Date", 
               "Current.Status", 
               "Current.Source", 
               "Animal.DOB", 
               "Age",  
               "Region", 
               "Adoption.Summary", 
               "Incoming.Date", 
               "Outgoing.SubStatus", 
               "Adoption.Fee.Description", 
               "Size", 
               "Microchip", 
               "Secondary.Microchip",
               "Breed",
               "Breed.Secondary", 
               "Adoption.Counselors", 
               "Return.Date", 
               "Alt..Placement", 
               "FY", 
               "SpayNeuter.Status", 
               "Adoption.Amount1", 
               "Receipt.Total1")    

Adoptions1 <- Adoptions[, selection]


AdoptionFeePerYear <- Adoptions |> 
  group_by(FY, Type) |> 
  summarise(TotalFees = sum(Receipt.Total1, na.rm = TRUE))

AdoptionFeePerYear <- subset(AdoptionFeePerYear, AdoptionFeePerYear$Type %in% c("Dog", "Cat", "Puppy", "Kitten"))
AdoptionFeePerYear <- subset(AdoptionFeePerYear, !AdoptionFeePerYear$FY %in% c("FY2020"))


ggplot(AdoptionFeePerYear, aes(x=FY, y=TotalFees)) +
  geom_bar(aes(fill = Type), stat = "identity") + 
  geom_text(
    aes(label = dollar(TotalFees)), # Using dollar() for consistent formatting
    # Adjust vjust to a value close to 1 to move the label to the top
    position = position_stack(vjust = 0.095), 
    size = 2.5,
    color = "black"
  ) +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Adoptions Fees by Fiscal Year", 
       x = "Fiscal Year", 
       y = "Revenue from Adoption Fees")

#########################################################################
############## Range of Dog adoption fees ###############################
#########################################################################

# Install and load the necessary packages

library(tidyverse)
file_path <- file.choose()

if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  data <- read.csv(file_path, fileEncoding = "Windows-1252")
}


data$Puppy.Adoption.Fees[data$Puppy.Adoption.Fees == "" | data$Puppy.Adoption.Fees == "n/a"] <- NA
data$Cat.Adoption.Fees[data$Cat.Adoption.Fees == "" | data$Cat.Adoption.Fees == "n/a"] <- NA
data$Kitten.Adoption.Fees[data$Kitten.Adoption.Fees == "" | data$Kitten.Adoption.Fees == "n/a"] <- NA

data <- data %>%
  mutate(across(everything(), ~str_replace_all(., "\\$", "")))
data$Shelter.Name[data$Shelter.Name == "Multnomah County Animal Services"] <- "**Multnomah County Animal Services**"

##### Dog Adoption Fee range

data <- data %>%
  mutate(
    # Check if the cell contains a range (" - ")
    is_range = str_detect(Dog.Adoption.Fees, fixed(" - ")),
    
    # Create dog.min: if it's a range, grab the first number; otherwise, NA
    dog.min = if_else(is_range,
                      as.numeric(str_extract(Dog.Adoption.Fees, "^[0-9]+\\.?[0-9]*")),
                      NA_real_
    ),
    
    # Create dog.max: if it's a range, grab the last number; otherwise, NA
    dog.max = if_else(is_range,
                      as.numeric(str_extract(Dog.Adoption.Fees, "[0-9]+\\.?[0-9]*$")),
                      NA_real_
    ),
    
    # Create dog.mid: if it's a range, calculate the midpoint; otherwise, use the value
    dog.mid = if_else(is_range,
                      (dog.min + dog.max) / 2,
                      as.numeric(Dog.Adoption.Fees)
    )
  ) %>%
  select(-is_range) # This removes the temporary helper column



plot_data <- data %>%
  # Sort Shelter factor levels by the dog.mid value
  mutate(Shelter = fct_reorder(Shelter.Name, dog.mid)) %>%
  # Reshape data from wide to long format
  pivot_longer(
    cols = c(dog.min, dog.max, dog.mid),
    names_to = "fee_type",
    values_to = "fee_value"
  ) %>%
  # Remove any rows where the fee_value is NA (for single-number shelters)
  filter(!is.na(fee_value))

# 3. CREATE THE PLOT
ggplot(plot_data, aes(x = fee_value, y = Shelter, color = fee_type)) +
  geom_point(size = 4) + # Add the points
  scale_color_manual( # Manually set the colors
    name = "Fee Type", # Legend title
    labels = c("dog.max" = "Max", "dog.mid" = "Midpoint", "dog.min" = "Min"),
    values = c("dog.max" = "red", "dog.mid" = "yellow", "dog.min" = "green")
  ) +
  labs( # Add titles and labels
    title = "Dog Adoption Fee Range by Shelter",
    subtitle = "Sorted by midpoint adoption fee",
    x = "Adoption Fee (USD)",
    y = "Shelter"
  ) +
  geom_line(aes(group = Shelter), color = "gray")+
  theme_minimal() + # Use a clean theme
  theme(panel.grid.major.y = element_blank()) # Remove horizontal grid lines



##### Cat Adoption Fee range

data <- data %>%
  mutate(
    # Check if the cell contains a range (" - ")
    is_range = str_detect(Cat.Adoption.Fees, fixed(" - ")),
    
    # Create Cat.min: if it's a range, grab the first number; otherwise, NA
    Cat.min = if_else(is_range,
                      as.numeric(str_extract(Cat.Adoption.Fees, "^[0-9]+\\.?[0-9]*")),
                      NA_real_
    ),
    
    # Create Cat.max: if it's a range, grab the last number; otherwise, NA
    Cat.max = if_else(is_range,
                      as.numeric(str_extract(Cat.Adoption.Fees, "[0-9]+\\.?[0-9]*$")),
                      NA_real_
    ),
    
    # Create Cat.mid: if it's a range, calculate the midpoint; otherwise, use the value
    Cat.mid = if_else(is_range,
                      (Cat.min + Cat.max) / 2,
                      as.numeric(Cat.Adoption.Fees)
    )
  ) %>%
  select(-is_range) # This removes the temporary helper column

data <- subset(data, !is.na(data$Cat.mid))

plot_data <- data %>%
  # Sort Shelter factor levels by the Cat.mid value
  mutate(Shelter = fct_reorder(Shelter.Name, Cat.mid)) %>%
  # Reshape data from wide to long format
  pivot_longer(
    cols = c(Cat.min, Cat.max, Cat.mid),
    names_to = "fee_type",
    values_to = "fee_value"
  ) %>%
  # Remove any rows where the fee_value is NA (for single-number shelters)
  filter(!is.na(fee_value))

# 3. CREATE THE PLOT
ggplot(plot_data, aes(x = fee_value, y = Shelter, color = fee_type)) +
  geom_point(size = 4) + # Add the points
  scale_color_manual( # Manually set the colors
    name = "Fee Type", # Legend title
    labels = c("Cat.max" = "Max", "Cat.mid" = "Midpoint", "Cat.min" = "Min"),
    values = c("Cat.max" = "red", "Cat.mid" = "yellow", "Cat.min" = "green")
  ) +
  labs( # Add titles and labels
    title = "Cat Adoption Fee Range by Shelter",
    subtitle = "Sorted by midpoint adoption fee",
    x = "Adoption Fee (USD)",
    y = "Shelter"
  ) +
  geom_line(aes(group = Shelter), color = "gray")+
  theme_minimal() + # Use a clean theme
  theme(panel.grid.major.y = element_blank()) # Remove horizontal grid lines


