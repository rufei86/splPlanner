
# splPlanner

<!-- badges: start -->
<!-- badges: end -->

The goal of splPlanner is to ...

## Installation

You can install the development version of splPlanner like so:

``` r
# Install the development version from GitHub:
remotes::install_github("rufei86/splPlanner")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(splPlanner)
## Use case example code

# Declare directory
setwd("/Users/rufeilu/OMRF Dropbox/Rufei Lu/Lu Lab/Animal Related Works/Sample Collection/SampleCollectionPlanner")
here::i_am("Mouse_Experiment_Longitudinal_Scheduler.R")

# Read in the mouse manifest for the LGLPS longitudinal data
manifest_mouse <-
  readxl::read_excel(
    path = here::here("Sample Collection", "Manifest", "LGLPS_Manifest.xlsx"),
    sheet = 1
  ) |>
  dplyr::select(-Age)

# Read in the current collected database for the LGLPS
database <-
  read.csv(file = here::here("Sample Collection", "Database", "LGLPS_Dataset.csv"), check.name = FALSE) |>
  dplyr::mutate(Collection_Date = as.Date(`Collection Date`, format = "%Y-%m-%d")) |>
  dplyr::mutate(`Physical Tag` = as.character(`Physical Tag`))

# Update the database with newly collected sheets
dir <- here::here("Sample Collection", "Daily Collection", "Collected")
database <- 
  update_collection(dir = dir, database = database, id_name_alt = "Physical ID")
write.csv(x = database, file = here::here("Sample Collection", "Database", "LGLPS_Dataset.csv"))

# Generate plan for LGLPS
plan <- generate_collection_plan(
  manifest = manifest_mouse,
  collected = database,
  collection = c(
    "Weight",
    "Urine",
    "Blood",
    "Urine_Tube",
    "Plasma_Tube",
    "WBC_Tube"
  ),
  start_age_wk = 6,
  interval_wk = 4,
  recurrence = 10
) |>
  dplyr::select(-`End Date`)

# Declare the folder for the sample collection for daily collection
dir_sheet <- here::here("Sample Collection", "Daily Collection")

# LGLPS Monitoring
make_individual_worksheet(plan = plan, directory = dir_sheet, project_name = "LGLPS_Monitor")

# Automatically update the LGLPS on click-up website
clickrup::cu_set_pat("pk_114015593_V007PRIPTS0URVE7JQ4JCLBW7WZTLOWB") # Set the Click-Up Token API
make_ClickUp_tasks(plan = plan, project_name = "LGLPS", message = "Weigh and collect urine/blood: ", folder_name = "Mouse Works", list_name = "LGLPS")

```

