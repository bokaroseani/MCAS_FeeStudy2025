library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)



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

all6<- subset(all, all$FY %in% c("FY2020", "FY2021", "FY2022", "FY2023", "FY2024", "FY2025"))

ggplot(data = all5, aes(x = License.Fee1)) +
  geom_histogram()


temp <- all6 |> 
  group_by(FY, License.Type) |> 
  count()

temp1 <- as.data.frame(table(all5$License.Type, all5$License.Fee1, all5$FY))

# For each level of License.Type, find the number of licenses issued each year and make a bar chart

