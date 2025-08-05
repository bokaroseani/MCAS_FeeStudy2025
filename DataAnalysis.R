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
  feesCollected <- read.csv(file_path)
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

# 3. Convert the cleaned character column to numeric
feesCollected$Amount <- as.numeric(feesCollected$Amount)

### Remove the $25000 that was yearly paid out as "50220:Licenses & Fees" with the Header Memo saying "Revenue Transfer per Resolution 2010-098".
feesCollected <- subset(feesCollected, feesCollected$Amount != 25000)

names(feesCollected)

temp <- feesCollected %>%
  group_by(Revenue.Category) %>%
  summarise(
    Total.Amount = -1*sum(Amount, na.rm = TRUE)
  )
temp$Revenue.Category <- as.factor(temp$Revenue.Category)
levels(temp$Revenue.Category)[1] <- "05200015 - DCS Dollys Fund Donations"


ggplot(temp, aes(x=reorder(Revenue.Category, Total.Amount), y=Total.Amount)) +
  geom_col() +
  coord_flip()

### Select the top 6 fee categories

selected <- c("02200025 - DCS Dog Licenses", 
              "02200020 - DCS Cat Licenses", 
              "02200050 - DCS Adoption Fees", 
              "02200030 - DCS Facility Licenses",
              "04400015 - DCS Fines From NOI's - Issued By Field",
              "04400010 - DCS Fines From NOI's - Issued By Client", 
              "02200070 - DCS Euthanasia Fees, Disposal Fees",
              "02200015 - DCS Owner Surrender Fees")

temp <- subset(feesCollected, feesCollected$Revenue.Category %in% selected)

temp$Revenue.Category[temp$Revenue.Category == "02200025 - DCS Dog Licenses"] <- "h) Dog Licenses"
temp$Revenue.Category[temp$Revenue.Category == "02200020 - DCS Cat Licenses"] <- "g) Cat Licenses"
temp$Revenue.Category[temp$Revenue.Category == "02200050 - DCS Adoption Fees"] <- "f) Adoption Fees"
temp$Revenue.Category[temp$Revenue.Category == "02200030 - DCS Facility Licenses"] <- "e) Facility Lincenses"
temp$Revenue.Category[temp$Revenue.Category == "04400015 - DCS Fines From NOI's - Issued By Field"] <- "d) Fines from NOI Issued by Field"
temp$Revenue.Category[temp$Revenue.Category == "04400010 - DCS Fines From NOI's - Issued By Client"] <- "c) Fines from NOI Issued by Client"
temp$Revenue.Category[temp$Revenue.Category == "02200070 - DCS Euthanasia Fees, Disposal Fees"] <- "b) Euthanasia Fees, Disposal Fees"
temp$Revenue.Category[temp$Revenue.Category == "02200015 - DCS Owner Surrender Fees"] <- "a) Owner Surrender Fees"




yearly_summary <- temp %>%
  group_by(Fiscal.Year, Revenue.Category) %>%
  summarise(
    Total.Amount = -1*sum(Amount, na.rm = TRUE)
  )

yearly_summary$Revenue.Category <- as.factor(yearly_summary$Revenue.Category)


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col(aes(fill = Revenue.Category)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Total Fees Collected by Fiscal Year",
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


