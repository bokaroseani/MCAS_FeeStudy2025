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

#save(all, file = "Z:/My Drive/Animal Services/MCAS Fee Study/ShelterBuddyData/all6yearsLicense.RData")

###########################
#### START FROM HERE ######
###########################

load("Z:/My Drive/Animal Services/MCAS Fee Study/ShelterBuddyData/all6yearsLicense.RData")

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
  facet_wrap(~ Var1) +
  labs(title = "Number of Dog licenses processed in the past 6 years",
       x = "Fiscal Year",
       y = "Number of licenses") +
  coord_flip()

temp2Cat <- subset(temp2, grepl("Cat", temp2$Var1))

ggplot(temp2Cat, aes(x = Var2, y = Freq)) +
  geom_bar(stat = "identity", position = "stack", fill = "skyblue") +
  geom_text(aes(label = Freq), hjust = -0.25, size = 2.5, color = "black") +
  facet_wrap(~ Var1) +
  labs(title = "Number of Cat licenses processed in the past 6 years",
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