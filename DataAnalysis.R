library(dplyr)
library(ggplot2)
library(scales)
library(readxl)
library(forcats)

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
summary(as.factor(feesCollected$Revenue.Category))

feesCollected$Revenue.Category[feesCollected$Revenue.Category == "02200055 - DCS Appeal Fees, Appeal Board Fees, Court Board Fees"] <- "02200055 - DCS Appeal Fees"

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

levels(as.factor(feesCollected$Revenue.Category))

feesCollected$Revenue.Category1 <- as.character(feesCollected$Revenue.Category)

# Use sub() to replace the pattern with an empty string ""
feesCollected$Revenue.Category1 <- sub(
  pattern = "^[0-9]+\\s*-\\s*(DCS\\s)?", 
  replacement = "", 
  x = feesCollected$Revenue.Category1
)
feesCollected$Revenue.Category1 <- sub(
  pattern = " - Issued By Client", 
  replacement = "", 
  x = feesCollected$Revenue.Category1
)
feesCollected$Revenue.Category1 <- sub(
  pattern = " - Issued By Field", 
  replacement = "", 
  x = feesCollected$Revenue.Category1
)
levels(as.factor(feesCollected$Revenue.Category1))

feesCollected <- feesCollected %>%
  mutate(Fiscal.Year = fct_relabel(Fiscal.Year, ~ sub("FY", "FY 20", .x)))

#### Remove Donations
feesCollected <- subset(feesCollected, !grepl("Adoption Outreach Donations", feesCollected$Revenue.Category1))
feesCollected <- subset(feesCollected, !grepl("Dolly’s Fund Donations", feesCollected$Revenue.Category1))


yearlyfee <- feesCollected |> 
  group_by(Fiscal.Year) |> 
  summarise(Total = sum(Amount, na.rm = TRUE))

yearlyfee$TotalPlus <- yearlyfee$Total + 25000
yearlyfee$TotalMinus <- yearlyfee$Total - 25000

### Remove the $25000 that was yearly paid out as "50220:Licenses & Fees" with the Header Memo saying "Revenue Transfer per Resolution 2010-098".
### $25,000 is a portion of the license fee revenue retained in the the restricted accounts, per County Resolution 2010-098. 
feesCollected <- subset(feesCollected, feesCollected$Amount != -25000)

onlyFees <- c("Adoption Fees",
              "Appeal Fees", 
              "Board Fees", 
              "Cat Licenses", 
              "Dog Licenses", 
              "Euthanasia Fees, Disposal Fees",
              "Facility Licenses",
              "Fines From NOI's",
              "Impound Fees",
              "Owner Surrender Fees",
              "Potentially Dangerous Dog Classification",
              "Vet Fees")

FeesAndFines <- subset(feesCollected, feesCollected$Revenue.Category1 %in% onlyFees)

yearlyfee <- feesCollected |> 
  group_by(Fiscal.Year) |> 
  summarise(Total = sum(Amount, na.rm = TRUE))

yearlyfee <- FeesAndFines |> 
  group_by(Fiscal.Year) |> 
  summarise(Total = sum(Amount, na.rm = TRUE))

names(feesCollected)

temp <- feesCollected %>%
  group_by(Revenue.Category1) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Annual.Average.Revenue = round(Total.Amount/5,0),
  )

summary(as.factor(temp$Revenue.Category1))

  

temp <- subset(temp, temp$Revenue.Category1!="Licenses & Fees, General")

### Adoption Fee share of the total fee

temp$Total.Amount[temp$Revenue.Category1 == "Adoption Fees"] / sum(temp$Total.Amount)

s <- sum(temp$Total.Amount)
temp$percentOfTotal <- round(temp$Total.Amount*100/s, 3)


ggplot(temp, aes(x=reorder(Revenue.Category1, Annual.Average.Revenue), y=Annual.Average.Revenue)) +
  geom_col(fill = "#008080") +
  geom_text(aes(label = paste0("$", Annual.Average.Revenue), hjust = -0.25), size = 3) +
  coord_flip() +
  scale_y_continuous(labels = label_dollar()) + 
  labs(
    title = "Annual average revenue from various sources",
    subtitle = "Average calculated based on revenue from FY 2021 to FY 2025",
    x = "Revenue Source",
    y = "Annual average revenue",
    caption = "Source: Workday Ledger report"
  )

max_revenue <- max(temp$Annual.Average.Revenue)

ggplot(temp, aes(x=reorder(Revenue.Category1, Annual.Average.Revenue), y=Annual.Average.Revenue)) +
  geom_col(fill = "#008080") +
  geom_text(aes(label = paste0("$", format(Annual.Average.Revenue, big.mark = ",")),
                hjust = ifelse(Annual.Average.Revenue == max_revenue, 1.1, -0.25),
                colour = ifelse(Annual.Average.Revenue == max_revenue, 'white', 'black')),
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = label_dollar()) +
  scale_colour_identity() +
  labs(
    title = "Annual average revenue from various sources",
    subtitle = "Average calculated based on revenue from FY 2021 to FY 2025",
    x = "Revenue Source",
    y = "Annual average revenue",
    caption = "Source: Workday Ledger report"
  )




# How many board fees have been collected? Only 6 occassions in the last 5 years have we collected Board Fees totalling $1375. 
# The highest Board Fees collected was $900 in FY24. That was the last time we collected Board Fees. 
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200035 - DCS Board Fees")


### Select the top 6 fee categories

selected <- c("Dog Licenses", 
              "Cat Licenses", 
              "Adoption Fees"
              )

temp <- feesCollected
temp$Revenue.Category1[!temp$Revenue.Category1 %in% selected] <- "a) Other Fees and Fines" # Others include:
                                                                                          # Owner Surrender Fees
                                                                                          # Vet Fees
                                                                                          # Appeal Fees, Appeal Board Fees, Court Board Fees
                                                                                          # Spay and Save Fees
                                                                                          # Impound Fees
                                                                                          # Dolly's Fund Donations
                                                                                          # Board Fees
                                                                                          # Potentially Dangerous Dog Classification
                                                                                          # Adoption Outreach Donations
                                                                                          # Euthanasia Fees, Disposal Fees
                                                                                    
temp$Revenue.Category1[temp$Revenue.Category1 == "Dog Licenses"] <- "d) Dog Licenses"
temp$Revenue.Category1[temp$Revenue.Category1 == "Cat Licenses"] <- "c) Cat Licenses"
temp$Revenue.Category1[temp$Revenue.Category1 == "Adoption Fees"] <- "b) Adoption Fees"

levels(as.factor(temp$Revenue.Category1))

#library(RColorBrewer)
#display.brewer.all()



yearly_summary <- temp %>%
  group_by(Fiscal.Year, Revenue.Category1) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE)
  )

yearly_summary$Revenue.Category1 <- as.factor(yearly_summary$Revenue.Category1)
yearly_tot <- yearly_summary |> 
  group_by(Fiscal.Year) |> 
  summarise(total = sum(Total.Amount))

AverageTotal<- mean(yearly_tot$total)
yearly_totals <- yearly_summary %>%
  group_by(Fiscal.Year) %>%
  summarize(Total.Amount = sum(Total.Amount))

ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col(aes(fill = Revenue.Category1)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "MCAS Fee Revenue by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) +
  geom_text(data = yearly_totals, aes(label = dollar(Total.Amount), y = Total.Amount), vjust = -0.5, size = 3.5)



############################################################################
yearly_percentages <- temp %>%
  # First, find the total for each category within each year
  group_by(Fiscal.Year, Revenue.Category1) %>%
  summarise(
    Category.Amount = sum(Amount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Now, group by only the year to calculate percentages
  group_by(Fiscal.Year) %>%
  mutate(
    # Calculate the percentage each category contributes to the yearly total
    Percentage = Category.Amount / sum(Category.Amount)
  ) %>%
  # It's good practice to ungroup after calculations
  ungroup()


# 3. Build the 100% stacked bar plot
ggplot(yearly_percentages, aes(x = as.factor(Fiscal.Year), y = Percentage, fill = Revenue.Category1)) +
  # Layer 1: The 100% stacked columns
  geom_col() +
  
  # Layer 2: Text labels inside each segment
  # We use position_stack to place labels in the middle of each segment
  geom_text(
    aes(label = percent(Percentage, accuracy = 1)), # Format label as a percentage
    position = position_stack(vjust = 0.5), # Center the label vertically in the segment
    color = "white", # Use white text for better contrast
    size = 3.5
  ) +
  
  # --- Formatting and Labels ---
  
  # Use a color palette for the bar fills
  scale_fill_brewer(palette = "Paired") +
  
  # Format the Y-axis to show percentage labels (0%, 25%, etc.)
  scale_y_continuous(labels = label_percent()) +
  
  # Add titles and axis labels
  labs(
    title = "MCAS Fee Revenue Share by Fiscal Year",
    x = "Fiscal Year",
    y = "Share of Total Revenue",
    fill = "Revenue Category"
  ) +
  
  # Apply a clean theme
  theme_minimal() +
  
  # Optional: Remove grid lines for a cleaner look
  theme(panel.grid = element_blank())
###################################################################################

### Questions for Erin and team:
### 1) What share of animals (dogs and cats only) that have been reunited with their owners paid for Boarding and what share paid other penalties?
### 2) What share of animals (dogs and cats only) that have been reunited with their owners did not have a valid license when found? 




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
  group_by(Fiscal.Year, Cost.Center) %>%
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
  group_by(Fiscal.Year, Cost.Center) %>%
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
  group_by(Fiscal.Year, Cost.Center) %>%
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
  group_by(Fiscal.Year, Cost.Center) %>%
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



# Fines From NOI's - Issued By Field
temp <- subset(feesCollected, feesCollected$Revenue.Category == "04400015 - DCS Fines From NOI's - Issued By Field")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Fines from NOI's (Field) collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Fines from NOI's (Field) Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 




# 04400010 - DCS Fines From NOI's - Issued By Client
temp <- subset(feesCollected, feesCollected$Revenue.Category == "04400010 - DCS Fines From NOI's - Issued By Client")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Fines from NOI's (Client) collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Fines from NOI's (Client) Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 




# 	02200070 - DCS Euthanasia Fees, Disposal Fees
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200070 - DCS Euthanasia Fees, Disposal Fees")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Euthanasia and Disposal Fees collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Euthanasia and Disposal Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 


# 02200015 - DCS Owner Surrender Fees
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200015 - DCS Owner Surrender Fees")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Owner Surrender Fees collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Owner Surrender Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 

# 02200065 - DCS Vet Fees
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200065 - DCS Vet Fees")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Vet Fees collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Vet Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 

# 02200055 - DCS Appeal Fees, Appeal Board Fees, Court Board Fees
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200055 - DCS Appeal Fees, Appeal Board Fees, Court Board Fees")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Appeal Fees, Appeal Board, Court Board Fees collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )


ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Appeal Fees, Appeal Board, Court Board Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 

# 03000025 - DCS Spay and Save Fees
temp <- subset(feesCollected, feesCollected$Revenue.Category == "03000025 - DCS Spay and Save Fees")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Spay and Save Fees collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )

ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Spay and Save Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 

# 02200040 - DCS Impound Fees
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200040 - DCS Impound Fees")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Impound Fees collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Case.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )

ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Impound Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 

# 05200015 - DCS Dolly’s Fund Donations
temp <- subset(feesCollected, feesCollected$Revenue.Category == "05200015 - DCS Dolly’s Fund Donations")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Dolly's Fund Donations collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )

ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Dolly's Fund Donations Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 

# 02200035 - DCS Board Fees
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200035 - DCS Board Fees")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Board Fees collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )

ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Board Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 

# 02200060 - DCS Potentially Dangerous Dog Classification
temp <- subset(feesCollected, feesCollected$Revenue.Category == "02200060 - DCS Potentially Dangerous Dog Classification")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Potentially Dangerous Dog Classification Fees collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )

ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Potentially Dangerous Dog Classification Fees Collected by Fiscal Year",
    x = "Fiscal Year",
    y = "Total Amount Collected ($)"
  ) +
  scale_y_continuous(labels = label_dollar()) 

# 05200020 - DCS Adoption Outreach Donations
temp <- subset(feesCollected, feesCollected$Revenue.Category == "05200020 - DCS Adoption Outreach Donations")
summary(temp$Amount)
temp1 <- as.data.frame(summary(as.factor(temp$Amount[temp$Fiscal.Year == "FY25"])))

top_20_amounts <- temp |>
  filter(Fiscal.Year == "FY25") |> 
  count(Amount, sort = TRUE) |> # Count occurrences for each amount and sort descending
  slice_head(n = 20)

ggplot(temp, aes(Amount)) +
  geom_histogram(aes(fill = Fiscal.Year), binwidth = 20) +
  labs(title = "Distribution of Dollar Amount of Adoption Outreach Donations collected across 5 years")

yearly_summary <- temp %>%
  group_by(Fiscal.Year, Cost.Center) %>%
  summarise(
    Total.Amount = sum(Amount, na.rm = TRUE),
    Count = n(), 
    Amount_per_transaction = Total.Amount/Count
  )

ggplot(data = yearly_summary, aes(x = as.factor(Fiscal.Year), y = Total.Amount)) +
  geom_col() +
  geom_text(aes(label = paste0("N = ", Count), vjust = -0.5)) +
  labs(
    title = "Total Adoption Outreach Donations Collected by Fiscal Year",
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


