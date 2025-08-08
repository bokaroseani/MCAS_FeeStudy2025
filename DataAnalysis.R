library(dplyr)
library(ggplot2)
library(scales)

# Choose a file using a dialog box
file_path <- file.choose()

# Print the selected file path (for verification)
print(file_path)

# Now you can use file_path with your data loading function
# For example, if it's a CSV file:
if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
  feesCollected <- read.csv(file_path, fileEncoding = "Windows-1252")
  print("CSV file loaded successfully!")
  head(feesCollected)
} else if (grepl("\\.txt$", file_path, ignore.case = TRUE)) {
  feesCollected <- read.table(file_path, header = TRUE) # Example for text file with header
  print("Text file loaded successfully!")
  head(feesCollected)
} else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
  # You'll need the 'readxl' package for Excel files
  # install.packages("readxl") # Uncomment and run if you don't have it
  library(readxl)
  feesCollected <- read_excel(file_path)
  print("Excel file loaded successfully!")
  head(feesCollected)
} else {
  print("Unsupported file type or no file selected.")
}


feesCollected <- feesCollected[, !sapply(feesCollected, function(x) all(is.na(x)))]
feesCollected <- feesCollected[, !grepl("Tririga", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Department", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Division", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Fund", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Business.Unit", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Ledger.Account.1", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Cost.Center.1", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Operational.Transaction.1", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Journal.Source.1", names(feesCollected))]
feesCollected <- feesCollected[, !grepl("Primary", names(feesCollected))]



# 1. Remove commas from the entire column
feesCollected$Amount <- gsub(",", "", feesCollected$Amount)

# 2. Replace the parenthesis pattern "(...)" with "-..."
# The pattern ^\\((.*)\\)$ finds strings that start and end with parentheses
# and replaces them with a hyphen followed by the content inside the parentheses.
feesCollected$Amount <- gsub("^\\((.*)\\)$", "-\\1", feesCollected$Amount)

# 3. Convert the cleaned character column to numeric and change the sign.
feesCollected$Amount <- -1*as.numeric(feesCollected$Amount)

### Remove the $25000 that was yearly paid out as "50220:Licenses & Fees" with the Header Memo saying "Revenue Transfer per Resolution 2010-098".
### $25,000 is a portion of the license fee revenue retained in the the restricted accounts, per County Resolution 2010-098. 
feesCollected <- subset(feesCollected, feesCollected$Amount != -25000)

names(feesCollected)

temp <- feesCollected %>%
  group_by(Revenue.Category) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Annual.Average.Revenue = round(Total.Amount/5,0),
  )

temp <- subset(temp, temp$Revenue.Category!="02200005 - Licenses & Fees, General")
s <- sum(temp$Total.Amount)
temp$percentOfTotal <- round(temp$Total.Amount*100/s, 3)


ggplot(temp, aes(x=reorder(Revenue.Category, Annual.Average.Revenue), y=Annual.Average.Revenue)) +
  geom_col() +
  geom_text(aes(label = paste0("$", Annual.Average.Revenue), hjust = -0.25), size = 3) +
  coord_flip() +
  scale_y_continuous(labels = label_dollar()) + 
  labs(
    title = "Annual average revenue from various sources",
    subtitle = "Average calculated based on revenue from FY21 to FY25",
    x = "Revenue Source",
    y = "Annual average revenue",
    caption = "Source: Workday Ledger report"
  )

# How many board fees have been collected? Only 6 occassions in the last 5 years have we collected Board Fees totalling $1375. 
# The highest Board Fees collected was $900 in FY24. That was the last time we collected Board Fees. 
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200035 - DCS Board Fees")


### Select the top 6 fee categories

selected <- c("02200025 - DCS Dog Licenses", 
              "02200020 - DCS Cat Licenses", 
              "02200050 - DCS Adoption Fees", 
              "02200030 - DCS Facility Licenses",
              "04400015 - DCS Fines From NOI's - Issued By Field",
              "04400010 - DCS Fines From NOI's - Issued By Client", 
              "02200070 - DCS Euthanasia Fees, Disposal Fees"
#              "02200015 - DCS Owner Surrender Fees"
              )

temp <- feesCollected
temp$Revenue.Category[!temp$Revenue.Category %in% selected] <- "a) Other revenue sources" # Others include:
                                                                                          # DCS Owner Surrender Fees
                                                                                          # Vet Fees
                                                                                          # Appeal Fees, Appeal Board Fees, Court Board Fees
                                                                                          # Spay and Save Fees
                                                                                          # Impound Fees
                                                                                          # Dolly's Fund Donations
                                                                                          # Board Fees
                                                                                          # Potentially Dangerous Dog Classification
                                                                                          # Adoption Outreach Donations
                                                                                    
temp$Revenue.Category[temp$Revenue.Category == "02200025 - DCS Dog Licenses"] <- "h) Dog Licenses"
temp$Revenue.Category[temp$Revenue.Category == "02200020 - DCS Cat Licenses"] <- "g) Cat Licenses"
temp$Revenue.Category[temp$Revenue.Category == "02200050 - DCS Adoption Fees"] <- "f) Adoption Fees"
temp$Revenue.Category[temp$Revenue.Category == "02200030 - DCS Facility Licenses"] <- "e) Facility Lincenses"
temp$Revenue.Category[temp$Revenue.Category == "04400015 - DCS Fines From NOI's - Issued By Field"] <- "d) Fines from NOI Issued by Field"
temp$Revenue.Category[temp$Revenue.Category == "04400010 - DCS Fines From NOI's - Issued By Client"] <- "c) Fines from NOI Issued by Client"
temp$Revenue.Category[temp$Revenue.Category == "02200070 - DCS Euthanasia Fees, Disposal Fees"] <- "b) Euthanasia Fees, Disposal Fees"


#library(RColorBrewer)
#display.brewer.all()



yearly_summary <- temp %>%
  group_by(Fiscal.Year, Revenue.Category) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE)
  )

yearly_summary$Revenue.Category <- as.factor(yearly_summary$Revenue.Category)


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col(aes(fill = Revenue.Category)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Total Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 


### Questions for Erin and team:
### 1) What share of animals (dogs and cats only) that have been reunited with their owners paid for Boarding and what share paid other penalties?
### 2) What share of animals (dogs and cats only) that have been reunited with their owners 




temp <- feesCollected %>%
  group_by(Ledger.Account) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE)
  )



# What are the Revenue.Categories under Ledger.Account == "50235:Charges for Services"? They are only in FY21, and FY22.
serviceCharges <- subset(feesCollected, feesCollected$Ledger.Account == "50235:Charges for Services")
summary(as.factor(serviceCharges$Revenue.Category)) 

donations <- subset(feesCollected, feesCollected$Ledger.Account == "50300:Donations, Restricted, Operating") # These are only in FY21, 22 and 23.
summary(as.factor(donations$Revenue.Category))

punitive <- subset(feesCollected, feesCollected$Ledger.Account == "50280:Fines and Forfeitures") # Fines issued by Field Officers and Clients.
summary(as.factor(punitive$Revenue.Category))



###############################################################################
####### Analysis by different Revenue Categories ##############################
###############################################################################

# Dog Licenses
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200025 - DCS Dog Licenses")
summary(-temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount)))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Dog license fee collected across 5 years")

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Dog license fee collected across 5 years\n(Zooming in on distribution between -$100 and $500)") +
  coord_cartesian(xlim = c(-100, 500)) 

yearly_summary <- temp %>%
  group_by(Fiscal.Year) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Fees Collected from Dog Licenses by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 


# Cat Licenses
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200020 - DCS Cat Licenses")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount)))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Dog license fee collected across 5 years")

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of cat license fee collected across 5 years\n(Zooming in on distribution between -$100 and $500)") +
  coord_cartesian(xlim = c(-100, 500)) 

yearly_summary <- temp %>%
  group_by(Fiscal.Year) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Fees Collected from Cat Licenses by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 





# Adoption Fees
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200050 - DCS Adoption Fees")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Adoption fees collected across 5 years")

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Adoption fees collected across 5 years\n(Zooming in on distribution between -$100 and $500)") +
  coord_cartesian(xlim = c(-100, 500)) 

yearly_summary <- temp %>%
  group_by(Fiscal.Year) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Adoption Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 



# Facility Licenses
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200030 - DCS Facility Licenses")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Facility License fees collected across 5 years")

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Facility License fees collected across 5 years\n(Zooming in on distribution between -$100 and $500)") +
  coord_cartesian(xlim = c(-100, 500)) 

yearly_summary <- temp %>%
  group_by(Fiscal.Year) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Facility License Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 
















temp <- feesCollected |> 
  group_by(Primary.Cost.Object, Revenue.Category, Fiscal.Year) |> 
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE)
  )


summary(as.factor(feesCollected$`Cost Center ID`))
attributes(feesCollected)


feesCollected <- feesCollected %>%
  mutate(
    # Convert 'Department' and 'Division' to factors
    Department = as.factor(Department),
    Division = as.factor(Division),
    
    # Convert 'Amount' to numeric
    Amount = as.numeric(Amount),
    
    # Convert 'Header Memo' and 'Line Memo' to character
    `Header Memo` = as.character(`Header Memo`),
    `Line Memo` = as.character(`Line Memo`)
  )

# Display the structure of the data frame and the type of each column
str(feesCollected)


# Make sure you have the necessary packages installed and loaded
# install.packages("dplyr")
# install.packages("lubridate")
library(dplyr)
library(lubridate)

yearly_project_summary <- feesCollected %>%
  # First, ensure the 'Accounting Date' is a Date type, then create a 'Year' column
  mutate(
    `Accounting Date` = as.Date(`Accounting Date`), # This step might vary based on your date format
    Year = year(`Accounting Date`)
  ) %>%
  
  # Group by the new 'Year' column and the 'Project' column
  group_by(Year, `Primary Cost Object`) %>%
  
  # Calculate the total amount for each group
  summarise(
    TotalAmount = sum(Amount, na.rm = TRUE)
  ) %>%
  
  # It's good practice to ungroup after summarizing
  ungroup()

# View the resulting dataframe
print(yearly_project_summary)


