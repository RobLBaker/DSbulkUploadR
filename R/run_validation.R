
#' Runs all validation checks and generates a summary statement
#'
#' @param filename String. Name of the file for data validation.
#' @param path String. Path to the file. Defaults to the current working directory.
#' @param max_file_upload Integer. Maximum number of files that can be uploaded. Defaults to 500.
#' @param max_data_upload Integer. Maximum amount of data that can be uploaded, in GB. Defaults to 100.
#' @param dev Logical. Should functions target the development server? Defaults to FALSE.
#'
#' @returns Vector. A list of named numbers (number of errors, number of warnings)
#' @export
#'
#' @examples
#' \dontrun{
#' run_input_validation(filename = "test_file.txt")}
run_input_validation <- function(filename,
                                 path = getwd(),
                                 max_file_upload = 500,
                                 max_data_upload = 100,
                                 dev = FALSE){
  err_count <- 0
  warn_count <- 0

  msg1 <- "Validating input file data..."
  cli::cli_h1(msg1)

  tryCatch(check_ref_type(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(  check_ref_type_supported(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_files_exist(filename = filename, path = path),
            error = function(e) {
              err_count <<- err_count + 1
              cli::cli_bullets(c(e$message, e$body))
            },
            warning = function(w) {
              warn_count <<- warn_count + 1
              cli::cli_bullets(c(w$message, w$body))
            }
  )

  tryCatch(check_file_number(filename = filename,
                              path = path,
                              file_number_error = max_file_upload),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_file_size(filename = filename,
                              path = path,
                              file_size_error = max_data_upload),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_508_format(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_unique_title(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )


  tryCatch(check_start_date(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_end_date(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_end_after_start(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_dates_past(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_author_email(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_authors_orcid(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_orcid_format(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_license_type(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_prod_units(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  tryCatch(check_content_units(filename = filename, path = path),
           error = function(e) {
             err_count <<- err_count + 1
             cli::cli_bullets(c(e$message, e$body))
           },
           warning = function(w) {
             warn_count <<- warn_count + 1
             cli::cli_bullets(c(w$message, w$body))
           }
  )

  cli::cli_h2("Summary")
  if (err_count > 0) {
    cli::cli_alert_danger("{err_count} errors to address")
  }
  if (warn_count > 0) {
    cli::cli_alert_warning("{warn_count} warnings to look into")
  }
  if (warn_count + err_count == 0) {
    cli::cli_alert_success("Success! All data validation checks passed.")
  }
  return(invisible(c("errors" = err_count, "warnings" = warn_count)))
}
