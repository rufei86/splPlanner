
#' Create ClickUp Tasks from a Plan
#'
#' Creates tasks for each unique collection date in a plan data frame using the ClickUp API.
#' Tasks are assigned a name, description, high priority, set assignees, and a due date one day after the collection date.
#'
#' @param plan A data frame with at least 'Collection_Date' and 'Physical ID' columns.
#' @param folder_name Name of the ClickUp folder containing the desired list.
#' @param list_name Name of the ClickUp list within the folder.
#' @param project_name Project name for the ClickUp task (default "Sac and Collect").
#' @param message Message to use in the task description (default "Please sac and collect these animals: ").
#'
#' @return None. Tasks are created in ClickUp via clickrup API.
#' @examples
#' # make_ClickUp_tasks(plan, "MyFolder", "MyList")
make_ClickUp_tasks <- function(plan, folder_name, list_name, project_name, message, id_name = "Physical Tag") {
  # Get available workspaces (teams)
  Teams <- clickrup::cu_get_teams()

  # Get spaces within a workspace
  team_id <- Teams$teams[[1]]$id
  Teams$teams[[1]]$members[[17]]
  Spaces <- clickrup::cu_get_spaces(team_id)

  # Get folders and lists
  space_id <- Spaces$spaces[[1]]$id
  Folders <- clickrup::cu_get_folders(space_id)
  
  # Find the folder name for the list
  names_folder <- sapply(
    X = seq_along(Folders$folders),
    FUN = function(i) Folders$folders[[i]]$name
  )
  folder_index <- which(names_folder == folder_name)
  folder_id <- Folders$folders[[folder_index]]$id
  
  # Get the list for the folder
  Lists <- clickrup::cu_get_lists(folder_id)
  names_list <- sapply(
    X = seq_along(Lists$lists),
    FUN = function(i) Lists$lists[[i]]$name
  )
  list_index <- which(names_list == list_name)
  
  for (d in unique(plan$Collection_Date)) {
    ids <- plan |>
      dplyr::filter(Collection_Date == d) |>
      dplyr::pull(dplyr::any_of(id_name))
      
    
    clickrup::cu_create_task(
      list_id = Lists$lists[[list_index]]$id,
      name = paste0(as.character(as.Date(d)), " ", project_name, "(N = ", length(ids), ")"),
      description = paste0(message, paste0(ids, collapse = ", ")),
      priority = 2,  # High priority
      assignees = I(c(81340160, 75576274)),
      due_date = clickrup::cu_date_to(as.Date(d) + 1)
    )
  }
}