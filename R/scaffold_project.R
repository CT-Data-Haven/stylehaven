#' @title Setup directories for a typical DataHaven project
#' @description This sets up a typical (opinionated) project directory structure that we use for many projects at DataHaven. It will write directories at the specified path, but it will NOT overwrite any directories that already exist. You'll have the option to cancel before anything is written.
#' @param dir String: path to directory in which new files will be written. Default: '.'
#' @param input_data Create a directory `input_data`. Default: TRUE.
#'
#'  Standard use: data from some outside source to be analyzed in this project.
#' @param output_data Create a directory `output_data`. Default: TRUE.
#'
#'  Standard use: data written after analysis done in this project, generally in formats that can still be used for analysis and visualization (csv, rds) rather than formats for distribution (I usually add a folder `to_distro`) or to pass on to a client (xlsx). Nice spreadsheet outputs should go in `format_tables` or some other distribution-centered folder.
#' @param fetch_data Create a directory `fetch_data`. Default: TRUE.
#'
#' Standard use: a place to dump data as it comes in from API calls, queries, batch file downloads, etc.
#' @param analysis Create a directory `analysis`. Default: TRUE
#'
#' Standard use: main analysis scripts, both notebooks and .R scripts.
#' @param prep_scripts Create a directory `prep_scripts`. Default: FALSE
#'
#' Standard use: scripts use to prep or reshape data or documents, e.g. creating formatted spreadsheets for a client, making metadata, prepping to post to data.world, formatting for a website, bulk rendering parameterized Rmarkdown documents.
#' @param plots Create a directory `plots`. Default: FALSE
#'
#' Standard use: plots, either for in-house use or outside distribution.
#' @param format_tables Create a directory `format_tables`. Default: FALSE
#'
#' Standard use: spreadsheets--probably written by a script in `prep_scripts`--to be shared with clients or collaborators. Think of these as being files appropriate for presentation or addenda to a report, not for doing further analysis.
#' @param drafts Create a directory `drafts`. Default: FALSE
#'
#' Standard use: separating the more EDA-centered notebooks from notebooks used for drafting writing. Also a good place to keep files that have been edited in outside software (.docx, etc).
#' @param utils Create a directory `utils`. Default: TRUE
#'
#' Standard use: utility scripts and miscellaneous files, e.g. logo images, snippets of data, lists of colors to use.
#' @param addl A string vector of any additional directories to create. Default: NULL
#' @param gitblank Logical: whether to write a blank placeholder file in each new directory to force git tracking, even without yet having folder contents. Default: TRUE. If FALSE, empty directories will *not* be tracked by git.
#' @param dryrun Logical: whether to just do a dry run without actually writing any directories or files. Defaults FALSE.
#' @return Returns nothing, but prints paths to newly created directories.
#' @importFrom utils menu
#' @examples
#' # create default folders--good for small analysis projects
#' scaffold_project(dryrun = TRUE)
#' 
#' # create all available folders--good for larger print projects
#' scaffold_project(prep_scripts = TRUE, 
#'                  plots = TRUE,
#'                  format_tables = TRUE,
#'                  drafts = TRUE,
#'                  dryrun = TRUE)
#' 
#' @export
#' @keywords function
#' @rdname scaffold_project
scaffold_project <- function(dir = ".",
                        input_data = TRUE,
                        output_data = TRUE,
                        fetch_data = TRUE,
                        analysis = TRUE,
                        prep_scripts = FALSE,
                        plots = FALSE,
                        format_tables = FALSE,
                        drafts = FALSE,
                        utils = TRUE,
                        addl = NULL,
                        gitblank = TRUE,
                        dryrun = FALSE) {
  all_dirs <- list(input_data = input_data, 
                   output_data = output_data, 
                   fetch_data = fetch_data, 
                   analysis = analysis, 
                   prep_scripts = prep_scripts, 
                   plots = plots, 
                   format_tables = format_tables, 
                   drafts = drafts, 
                   "_utils" = utils)
  dirs <- purrr::set_names(c(names(all_dirs[unlist(all_dirs)]), purrr::compact(addl)))
  dirs <- sort(dirs)
  # if (utils) names(dirs)[names(dirs) == "utils"] <- "_utils"
  does_dir_exist <- purrr::map_chr(dirs, ~file.path(dir, .))
  does_dir_exist <- purrr::map_lgl(does_dir_exist, dir.exists)
  n_exist <- sum(does_dir_exist)
  n_create <- sum(!does_dir_exist)
  
  if (dryrun) {
    cli::cli_alert_info("Note that this is just a dry run. You'll see the normal printouts but no files will actually be written.\n")
  }
  
  # don't overwrite
  if (n_exist > 0) {
    cli::cli_alert_info("The following directories already exist and will NOT be overwritten:")
    cli::cli_ul(sprintf("{.file %s}", dirs[does_dir_exist]))
  }
  
  # check that okay to write new dirs
  if (n_create > 0) {
    cli::cli_alert_info("The following new directories will be created:")
    cli::cli_ul(sprintf("{.file %s}", dirs[!does_dir_exist]))
    
    cli::cli_rule()
    
    if (dryrun || !rlang::is_interactive()) {
      ok <- 1L
    } else {
      ok <- menu(c("Yes, write directories", "No, cancel"), title = "Okay to proceed?")
    }
    
    if (ok == 1) {
      purrr::walk(dirs[!does_dir_exist], function(d) {
        path <- file.path(dir, d)
        if (!dryrun) {
          dir.create(path)
        }
        cli::cli_alert("Writing {.file {path}}")
        if (gitblank) {
          g_path <- file.path(path, ".gitblank")
          if (!dryrun) {
            file.create(g_path)
          }
        }
      })
    } else {
      cli::cli_alert_danger("Aborting; nothing new will be written.")
    }
  } else {
    cli::cli_alert_info("No new directories need to be written.")
  }
  
}
