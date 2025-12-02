
#' Generate collection dates for recurring sample intervals.
#'
#' This function computes scheduled collection dates,
#' starting from the later of the mouse's initial eligible collection date (`start_age_wk` after DOB)
#' or the current system date, then proceeds at a set interval for a fixed number of recurrences.
#'
#' @param dob Date of birth as character, in "%m-%d-%Y" format.
#' @param start_age_wk Integer; age in weeks to start collecting.
#' @param interval_wk Integer; weeks between collections.
#' @param recurrence Integer; number of planned collections.
#' @return Character vector of future collection dates in "%m-%d-%Y" format.
#' @export
add_collect_date <- function(dob, start_age_wk, interval_wk, recurrence) {
  dob <- as.Date(dob, format = "%m-%d-%Y")
  # Determine first possible collection date
  # init_date <- dplyr::if_else(dob + start_age_wk * 7 < Sys.Date(),
  #                             Sys.Date(),
  #                             dob + start_age_wk * 7)
  init_date <- dob + start_age_wk * 7 
  # Generate all collection dates
  dates <- seq.Date(from = init_date,
                    by = paste0(interval_wk, " week"),
                    length.out = recurrence) |>
    format("%Y-%m-%d")
  return(dates)
}

#' Adjust Dates to Weekdays
#'
#' Adjusts a given date so that any date falling on
#' a weekend or Friday is moved to the closest workday:
#'
#' - Sunday moves to Monday (+1)
#' - Saturday moves to Monday (+2)
#' - Friday moves to Thursday (-1)
#' - All other days remain unchanged
#'
#' @param date A vector of class /code{Date} (base R).
#' @return A /code{Date} vector the same length as /code{date}, adjusted so that
#'   weekends and Fridays are shifted to the appropriate weekday.
#' @examples
#' adjust_to_weekday(as.Date(c("2023-10-14", "2023-10-15", "2023-10-13"))) # Sat, Sun, Fri
#' @export
adjust_to_weekday <- function(date) {
  wday <- as.POSIXlt(date)$wday # 0 = Sunday ... 6 = Saturday (base R)
  date_new <- ifelse(
    wday == 0, date + 1,                 # Sunday: move to Monday
    ifelse(wday == 6, date + 2,          # Saturday: move to Monday
           ifelse(wday == 5, date - 1, date)  # Friday: move to Thursday
    )
  )
  as.Date(date_new, origin = "1970-01-01")
}


#' Generate a master sample collection plan
#'
#' This function processes a manifest data.frame and a collection history,
#' generating scheduled sample collection dates and assigning tube codes per date.
#' Only collections not already completed are returned, according to each individual's
#' latest collected date.
#'
#' @param manifest DataFrame or tibble: animal manifest with at least ID and DOB columns.
#' @param collected DataFrame or tibble: history of previously collected samples (with ID and Collection_Date).
#' @param collection Character vector: sample types to generate (e.g., tube/non-tube assay names).
#' @param id_name Character: manifest column name for unique animal ID.
#' @param dob_name Character: manifest column name for date of birth.
#' @param start_age_wk Integer: age (weeks) at first collection.
#' @param interval_wk Integer: interval (weeks) between collections.
#' @param recurrence Integer: number of recurrence cycles (planned sample events).
#'
#' @return Tibble with rows for each pending (not yet collected) sample event by individual and date,
#'         with generated collection IDs where applicable.
#'
#' @importFrom dplyr filter group_by across all_of mutate ungroup summarise left_join select if_else
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @export
generate_collection_plan <- function(
    manifest,
    collected,
    collection,
    id_name = "Physical Tag",
    dob_name = "Date of Birth",
    start_age_wk = 6,
    interval_wk = 4,
    recurrence = 10
) {
  # Core pipeline adapted to function
  plan_master <- 
    manifest |>
    dplyr::filter(is.na(.data[["End Date"]])) |> # Check to see if the animal is still alive
    dplyr::group_by(.data[[id_name]], .drop = FALSE) |> # Group by the animal ID
    dplyr::mutate(
      Collection_Date = purrr::map(
        .data[[dob_name]],
        ~ add_collect_date(.x, start_age_wk, interval_wk, recurrence) 
      )
    ) |>
    tidyr::unnest(Collection_Date) |>
    dplyr::mutate(Collection_Date = as.Date(Collection_Date, format = "%Y-%m-%d")) 
  
  # Getting the columns that needs to have tube taken and the columns that just need data collection
  col_tube <- grep("_Tube", collection, value = TRUE)
  col_norm <- collection[!collection %in% col_tube]

  # Fill in NA for non-tube collections
  for (i in col_norm) {
    plan_master <- plan_master |>
      dplyr::mutate(!!i := NA)
  }

  # Generate codes for tubes
  for (i in col_tube) {
    plan_master <- plan_master |>
      dplyr::mutate(!!i := paste0(.data[[id_name]], "_", Collection_Date, "_", substr(i, 1, 1)))
  }
  
  # Collate most recent completed date per ID (if any)
  manifest_collected <- 
    collected |>
    dplyr::mutate(Collection_Date = as.Date(Collection_Date)) |>
    dplyr::group_by(.data[[id_name]]) |>
    dplyr::summarise(Latest_Date = max(Collection_Date), .groups = 'drop')
  
  # Return pending (future) collections only
  plan_master_curated <-
    plan_master |>
    dplyr::ungroup() |>
    #dplyr::filter(.data[[id_name]] %in% manifest_collected[[id_name]]) |>
    dplyr::left_join(manifest_collected, by = id_name) |>
    dplyr::filter(Collection_Date > Latest_Date | is.na(Latest_Date)) |>    # Keep only Collection Dates that are after the last collection
    dplyr::filter(Collection_Date > Sys.Date()) |>                          # Finally, keep only the ones that are in the future
    dplyr::mutate(Collection_Date = adjust_to_weekday(Collection_Date)) |>  # Change Friday, Saturday, and Sunday to nearest working Monday through Thursday
    dplyr::select(-Latest_Date)
  plan_master_curated <- remake_sch(plan_master_curated)

  return(plan_master_curated)
}

#' Export Worksheets by Collection Date
#'
#' Writes individual Excel files for each unique collection date found in the 'plan' data frame,
#' saving them to a specified directory and using the project name in the file naming convention.
#'
#' @param plan A data.frame or tibble containing at least a "Collection_Date" column.  
#' @param directory The target directory for the exported Excel files (character string).  
#' @param project_name The name to be included in each Excel file's prefix (character string).
#'
#' @importFrom dplyr filter
#' @importFrom openxlsx write.xlsx
#'
#' @return Invisibly returns the vector of file paths that were written.
#' @export
#'
#' @examples
#' # Example usage:
#' # make_individual_worksheet(plan = my_plan, directory = "output", project_name = "projectA")
make_individual_worksheet <- function(plan, directory, project_name){
  # Check that required packages are available
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed.")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required but not installed.")
  }
  # Check input validity
  if (!"Collection_Date" %in% colnames(plan)) {
    stop("The input 'plan' must contain a column named 'Collection_Date'.")
  }
  if (!dir.exists(directory)) {
    stop("Specified directory does not exist: ", directory)
  }
  # Vector to store output file paths
  written_files <- character(0)
  for (d in unique(plan$Collection_Date)){
    data_export <-
      dplyr::filter(plan, Collection_Date == d)
    # Construct output path
    export_path <- file.path(
      directory,
      paste0(as.Date(d), "_", project_name, ".xlsx")
    )
    openxlsx::write.xlsx(x = data_export, file = export_path)
    written_files <- c(written_files, export_path)
  }
  invisible(written_files)
}

#' Match Data Frame Columns to Template
#'
#' Ensures a data frame has all columns specified in a template; 
#' missing columns are added with NA values and columns are reordered to match the template sequence.
#'
#' @param df A data frame to modify.
#' @param template_names A character vector of column names specifying the template.
#'
#' @return A data frame with columns reordered and missing columns filled as NA.
#' @examples
#' df <- data.frame(a = 1, b = 2)
#' template_names <- c("a", "b", "c")
#' match_columns(df, template_names)
match_columns <- function(df, template_names) {
  missing_cols <- setdiff(template_names, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  df <- df[, template_names, drop = FALSE]
  return(df)
}

#' Update a Database Collection by Adding New Files
#'
#' Reads in all Excel files from a directory, optionally renaming an ID column, matches columns to a template, and combines them with an existing database.
#'
#' @param dir Directory path containing Excel files.
#' @param database The existing database data frame serving as a template for columns.
#' @param id_name_alt Optional alternative column name to be renamed to 'Physical Tag'. Default is NA (no renaming).
#'
#' @return A data frame combining the original database and new rows from the directory.
#' @export
#' @examples
#' # update_collection("data", database, id_name_alt = "OtherTag")
update_collection <- function(dir, database, id_name_alt = NA) {
  # Read files and optionally rename ID column
  files <- if (is.na(id_name_alt)) {
    lapply(base::dir(dir), FUN = function(i) 
      readxl::read_xlsx(here::here(dir, i), sheet = 1) |>
      dplyr::mutate(`Date of Birth` = as.Date(`Date of Birth`, format = "%m-%d-%Y"))
    )
  } else {
    lapply(base::dir(dir), FUN = function(i) 
      readxl::read_xlsx(here::here(dir, i), sheet = 1) |>
        dplyr::rename(`Physical Tag` = dplyr::any_of(id_name_alt)) |>
        dplyr::mutate(`Date of Birth` = as.Date(`Date of Birth`, format = "%m-%d-%Y"))
    )
  }
  
  # Match columns to template names
  template_names <- base::names(database)
  dfs_matched <- lapply(files, match_columns, template_names = template_names)
  collected <- do.call(base::rbind, dfs_matched)
  
  # Bind new data with existing database
  result <- 
    dplyr::bind_rows(database, collected) |>
    dplyr::distinct()
  return(result)
}

#' Remake Schedule by Collection Groups
#'
#' Organize collection dates into groups based on sample count and day difference.
#'
#' @param plan_master_curated A data.frame or tibble with at least 'Physical Tag' and 'Collection_Date' columns.
#'
#' @return A modified data.frame or tibble with rescheduled collection dates.
#'
#' @export
remake_sch <- function(plan_master_curated) {
  
  # Prepare the initial summary schedule grouped by Collection_Date
  remake_schedule <- plan_master_curated |>
    dplyr::select(`Physical Tag`, Collection_Date) |>
    dplyr::group_by(Collection_Date) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(Collection_Date) |>
    dplyr::mutate(
      day_diff = as.numeric(Collection_Date - dplyr::lag(Collection_Date)),
      group = 0,
      sum_n = n
    )
  
  # Handle first row explicitly for day_diff
  remake_schedule[1, "day_diff"] <- 0
  
  group <- 0
  
  i <- 1
  while (i < nrow(remake_schedule)) {
    # Criteria to combine into the same group
    if (remake_schedule[i, "sum_n"] < 8 && remake_schedule[i + 1, "day_diff"] < 7) {
      remake_schedule[c(i, i + 1), "group"] <- group
      remake_schedule[i + 1, "sum_n"] <- sum(remake_schedule[c(i, i + 1), "sum_n"])
      remake_schedule[i + 1, "day_diff"] <- sum(remake_schedule[c(i, i + 1), "day_diff"])
    } else {
      if (i == nrow(remake_schedule) - 1) {
        remake_schedule[i + 1, "group"] <- group
        group <- group + 1
      }
      remake_schedule[i, "group"] <- group
      group <- group + 1
    }
    i <- i + 1
  }
  
  # Compute max date for each group
  remake_schedule <- remake_schedule |>
    dplyr::group_by(group) |>
    dplyr::mutate(New_Date = median(Collection_Date)) |>
    dplyr::ungroup()
  
  # Perform left join and reassign dates as requested
  plan_master_mod <-
    dplyr::left_join(
      plan_master_curated,
      dplyr::select(remake_schedule, Collection_Date, New_Date),
      by = "Collection_Date"
    ) |>
    dplyr::mutate(Collection_Date = New_Date) |>
    dplyr::select(-New_Date)
  
  return(plan_master_mod)
}
