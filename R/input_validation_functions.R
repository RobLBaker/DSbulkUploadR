# Helper functions ----
check_start_date <- function(filename, path = getwd()) {
  upload_data <- read.delim(file=paste0(path, "/", filename))
  start_dates <- upload_data$content_begin_date
  #valid dates are "TRUE"
  valid_dates <- !is.na(suppressWarnings(lubridate::ymd(start_dates)))
  if (sum(valid_dates) < nrow(upload_data)) {
    msg <- paste0("Some content begin dates are not in ISO 8601 format ",
                  "(yyyy-mm-dd). Please supply all dates in ISO 8601 format.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All content begin dates are supplied ",
                  "in the correct format.")
    cli::cli_inform(c("v" = msg))
  }
}

check_end_date <- function(filename, path = getwd()) {
  upload_data <- read.delim(file=paste0(path, "/", filename))
  end_dates <- upload_data$content_end_date
  valid_dates <- !is.na(suppressWarnings(lubridate::ymd(end_dates)))
  if (sum(valid_dates) < nrow(upload_data)) {
    msg <- paste0("Some content end dates are not in ISO 8601 format ",
                  "(yyyy-mm-dd). Please supply all dates in ISO 8601 format.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All content end dates are supplied ",
                  "in the correct format.")
    cli::cli_inform(c("v" = msg))
  }
}

check_end_after_start <- function(filename, path = getwd){
  upload_data <- read.delim(file=paste0(path, "/", filename))
  end_dates <- lubridate::ymd(upload_data$content_end_date)
  start_dates <- lubridate::ymd(upload_data$content_begin_date)
  time_dif <- lubridate::interval(start_dates, end_dates)
  if (any(time_dif < 0)) {
    msg <- paste0("Some content end dates predate content start dates. ",
                  "Please make sure end dates are after start dates.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All end dates occur after start dates.")
    cli::cli_inform(c("v" = msg))
  }
}

check_dates_past <- function(filename, pathe = getwd){
  upload_data <- read.delim(file=paste0(path, "/", filename))
  end_dates <- lubridate::ymd(upload_data$content_end_date)
  start_dates <- lubridate::ymd(upload_data$content_begin_date)
  today <- Sys.Date()

  check_start <- lubridate::interval(start_dates, today)
  check_end <- lubridate::interval(end_dates, today)

  if (any(check_start < 0) | any(check_end < 0)) {
    msg <- paste0("Some of your content start or end dates occur in the ",
                  "future. Please make sure all dates occur in the past.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All content start and end dates occur in the past.")
    cli::cli_inform(c("v" = msg))
  }
}


#' Checks for valid content and producing unit codes
#'
#' Loads an input file (.txt) and checks two coluns of data (producing_units and content_units) against the NPS Units Service API to see if they are valid units. Accepts a list of comma-separated (or individual) unit codes. If invalid producing units are encountered, the function will abort with a list of invalid producing units. If all producing units are valid, the function continues on to check content units. If any invalid content units are encountered, the function will abort with an error message listing the invalid content units. If all units are valid, the function returns TRUE.
#'
#' @param filename input file name
#' @param path input file path. Defaults to the current working directory
#'
#' @returns TRUE or NULL
#' @export
#'
#' @examples
#' \dontrun{
#' check_units("example_input_file.txt")
#' }
check_prod_units <- function(filename, path = getwd()){
  upload_data <- read.delim(file=paste0(path, "/", filename))

  #get list of all units:
  f <- file.path(tempdir(), "irmadownload.xml")
  if (!file.exists(f)) {
    # access all park codes from NPS xml file
    curl::curl_download("https://irmaservices.nps.gov/v2/rest/unit/", f)
  }
  result <- XML::xmlParse(file = f)
  dat <- XML::xmlToDataFrame(result)

  #check for bad producing units:
  producing_units <- NULL
  for (i in 1:nrow(upload_data)) {
    producing_units <-
      append(producing_units,
             unlist(stringr::str_split(upload_data$producing_units[i], ", ")))
  }
  producing_units <- unique(producing_units)
  bad_prod_units <- producing_units[!(producing_units %in% dat$UnitCode)]

  if (length(bad_prod_units > 0)) {
    msg <- paste0("The following producing units are invalid. ",
                  "Please provide valid producing units: {bad_prod_units}.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All producing units are valid.")
    cli::cli_inform(c("v" = msg))
  }
}

check_content_units <- function(filename, path = getwd()) {
  upload_data <- read.delim(file=paste0(path, "/", filename))

  #get list of all units:
  f <- file.path(tempdir(), "irmadownload.xml")
  if (!file.exists(f)) {
    # access all park codes from NPS xml file
    curl::curl_download("https://irmaservices.nps.gov/v2/rest/unit/", f)
  }
  result <- XML::xmlParse(file = f)
  dat <- XML::xmlToDataFrame(result)

  #check for bad conten units:
  content_units <- NULL
  for (i in 1:nrow(upload_data)) {
    content_units <-
      append(content_units,
             unlist(stringr::str_split(upload_data$content_units[i], ", ")))
  }
  content_units <- unique(content_units)
  bad_content_units <- content_units[!(content_units %in% dat$UnitCode)]

  if (length(bad_content_units > 0)) {
    msg <- paste0("Please provide valid content units. The following ",
                  "content units are invalid: {bad_content_units}.")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All content units are valid.")
    cli::cli_inform(c("v" = msg))
  }
}

check_files_exist <- function(filename, path = getwd()){
  upload_data <- read.delim(file=paste0(path, "/", filename))
  bad_files <- NULL
  for (i in 1:nrow(upload_data)) {
    if (!file.exists(path = upload_data$file_path[i])) {
      bad_files <- append(bad_files, upload_data$title[i])
    }
  }
  if (!is.null(bad_files)) {
    msg <- paste0("All references must have files to upload. ",
                  "The following references have a bad file path ",
                  "or no files associated with them:\n {bad_files}")
    cli::cli_abort(c("x" = msg))
  } else {
    msg <- paste0("All file paths contain file for upload.")
    cli::cli_inform(c("v" = msg))
  }
}

#error if > file_number_error; warn if > 50% of file_number_error
check_file_number <- function(filename,
                              path = getwd(),
                              file_number_error = 500) {
  upload_data <- read.delim(file=paste0(path, "/", filename))
  file_num <- 0
  for (i in 1:nrow(upload_data)) {
    files_per_ref <- length(list.files(upload_data$file_path[i]))
    file_num <- (file_num + files_per_ref)
  }
  if (file_num > file_number_error) {
    msg <- paste0("You are attempting to upload {file_num} files, ",
                  "which exceeds the maximum allowable number, ",
                  "({error} files). Please either adjust the maximum number ",
                  "of files or attempt to upload fewer files.")
    cli::cli_abort(c("x" = msg))
  } else if (file_num > (file_number_error * 0.5)) {
    msg <- paste0("You are attempting to upload {file_num} files, ",
                  "which has triggered a warning. Please make sure you ",
                  "want to upload this many files.")
    cli::cli_warn(c("!" = msg))
  } else {
    msg <- paste0("The files to upload does not exceed the maximum number.")
    cli::cli_inform(c("v" = msg))
  }
}

#WARN AND ERROR GIVIN IN GB
#error if > file error size; warn if > 50% error size
check_file_size <- function(filename,
                            path = getwd(),
                            file_size_error = 100) {
  #convert gb to bytes:
  error_bytes <- file_size_error * 1073741824
  warn_bytes <- error_bytes * 0.5
  upload_data <- read.delim(file=paste0(path, "/", filename))
  file_size <- 0
  for (i in 1:nrow(upload_data)) {
    file_size <- file_size +
      sum(file.info(list.files(upload_data$file_path[i],
                               full.names = TRUE))$size)
  }
  file_gb <- file_size/1073741824
  if (file_size > error_bytes) {
    msg <- paste0("You are attempting to upload {file_gb} GB of data, ",
                  "which exceeds the maximum allowable number, ",
                  "({error} GB) per bulk upload. Please either adjust ",
                  "the maximum data upload amount, or attempt to ",
                  "upload less data.")
    cli::cli_abort(c("x" = msg))
  } else if (file_size > warn_bytes) {
    msg <- paste0("You are attempting to upload {file_gb} GB of data, ",
                  "which has triggered a warning. Please make sure you ",
                  "want to upload this much data.")
    cli::cli_inform(c("!" = msg))
  } else
    msg <- paste0("The cumulative file size to upload does ",
                  "not exeed the maximum allowable.")
  cli::cli_inform(c("v" = msg))
}

check_ref_type <- function(filename, path = getwd()) {
  upload_data <- read.delim(file=paste0(path, "/", filename))
  refs <- upload_data$reference_type

  url <- paste0(.ds_secure_api(), "FixedList/ReferenceTypes")
  #API call to look for an existing reference:
  req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
  status_code <- httr::stop_for_status(req)$status_code

  #if API call fails, alert user and remind them to log on to VPN:
  if(!status_code == 200){
    stop("DataStore connection failed.")
  }

  refs_json <- httr::content(req, "text")
  refs_rjson <- jsonlite::fromJSON(refs_json)

  refs <- unique(refs)
  bad_refs <- refs[!(refs %in% refs_rjson$key)]

  if (length(bad_refs > 0)) {
    msg <- paste0("Please use valid reference types. The following ",
                  "reference types are invalid: {bad_refs}.")
    cli::cli_abort(c("x" = msg))
  }
  cli::cli_inform(c("v" = "All reference types are valid."))
}

check_license_type <- function(filename, path = getwd()) {
  upload_data <- read.delim(file=paste0(path, "/", filename))
  valid_license <- sum(upload_data$license_code != (1 | 2 |3 | 4 | 5))
  if (valid_license > 0) {
    msg <- paste0("You have supplied an invalid license code. ",
                  "Please supply a valid license code.")
    cli::cli_abort(c("x" = msg))
  } else {
    cli::cli_inform(c("v" = "All license codes are valid."))
  }
  return(TRUE)
}

#uses API to verify user email addresses
check_author_email <- function(filename, path = getwd()) {
  upload_data <- read.delim(file=paste0(path, "/", filename))

  usr_email <- NULL
  for (i in 1:nrow(upload_data)) {
    usr_email <-
      append(usr_email,
             unlist(stringr::str_split(upload_data$author_email_list[i], ",")))
  }
  usr_email <- stringr::str_trim(usr_email)
  usr_email <- unique(usr_email)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
  bdy <- usr_email
  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = rjson::toJSON(bdy))
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    cli::cli_abort(c("x" = "ERROR: Datastore connection failed."))
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  if (all(rjson$found)) {
    cli::cli_inform(c("v" = "All author emails are valid."))
  } else {
    bad_usr_emails <- dplyr::filter(rjson, found==FALSE)$searchTerm
    msg <- paste0("Please supply valid emails. The following emails ",
                  "are invalid: {bad_usr_emails}.")
    cli::cli_abort(c("x" = msg))
  }
}

#checks to make sure all users have orcids and that orcids are correctly formatted. Does not check whether orcids are valid/associated with the correct username.
check_authors_orcid <- function(filename, path = getwd()) {
  upload_data <- read.delim(file=paste0(path, "/", filename))

  usr_email <- NULL
  for (i in 1:nrow(upload_data)) {
    usr_email <-
      append(usr_email,
             unlist(stringr::str_split(upload_data$author_email_list[i], ",")))
  }
  usr_email <- stringr::str_trim(usr_email)
  usr_email <- unique(usr_email)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
  bdy <- usr_email
  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = rjson::toJSON(bdy))
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed.")
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  # test whether orcids exist in active directory:
  if(sum(is.na(rjson$extensionAttribute2)) == 0) {
    cli::cli_inform(c("v" = "All users have ORCiDs."))
  } else {
    no_orcid <- dplyr::filter(rjson, is.na(rjson$extensionAttribute2))$searchTerm
    msg <- paste0("All authors must have ORCiDs. Please have the ",
                  "following authors add ORCiDs to Active Directory ",
                  "prior to proceeding with reference creation: {no_orcid}.")
    cli::cli_abort(c("x" = msg))
  }
}

check_orcid_format <- function(filename, path = getwd()){
  upload_data <- read.delim(file=paste0(path, "/", filename))

  usr_email <- NULL
  for (i in 1:nrow(upload_data)) {
    usr_email <-
      append(usr_email,
             unlist(stringr::str_split(upload_data$author_email_list[i], ",")))
  }
  usr_email <- stringr::str_trim(usr_email)
  usr_email <- unique(usr_email)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
  bdy <- usr_email
  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = rjson::toJSON(bdy))
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed.")
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  # test whether orcids are valid:
  rjson$orcid_format <- grepl('[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[A-Za-z0-9]{1}', rjson$extensionAttribute2)
  if (sum(rjson$orcid_format) == nrow(rjson)) {
    cli::cli_inform(c("v" = "All ORCiDs have a valid format"))
  } else {
    rjson2 <- rjson %>% dplyr::filter(rjson$orcid_format == FALSE)
    msg <- paste0("All authors must have ORCiDs with valid formats. ",
                  "The following authors have ORCiDs with invalid formats ",
                  "(which may include lack of any ORCiD) listed in Active ",
                  "Dirctory: {rjson2$searchTerm}.")
    cli::cli_abort(c("x" = msg))
  }
}

#uses AD to verify users upn. Will not work after transition to entraID.
check_users_AD <- function(filename, path = getwd()){

  upload_data <- read.delim(file=paste0(path, "/", filename))
  #check for bad usernames:
  usr_name <- NULL
  for (i in 1:nrow(upload_data)) {
    usr_name <-
      append(usr_name,
             unlist(stringr::str_split(upload_data$author_upn_list[i], ", ")))
  }
  usr_name <- unique(usr_name)

  bad_usr <- NULL
  invalid_orcid <- NULL
  for (i in 1:length(usr_name)) {
    powershell_command <- paste0('([adsisearcher]\\"(samaccountname=',
                                 usr_name[i],
                                 ')\\").FindAll().Properties')

    AD_output <- system2("powershell",
                         args = c("-Command",
                                  powershell_command),
                         stdout = TRUE)

    if (length(AD_output) == 0 ) {
      bad_usr <- append(bad_usr, usr_name)
    }
    #check_orcid - doesn't check formatting just presence/absence and nchar.
    orcid <- AD_output[
      which(AD_output %>% stringr::str_detect("extensionattribute2\\b"))]
    #get orcid from output
    orcid <- stringr::str_extract(orcid, "\\{.*?\\}")
    # remove curly braces:
    orcid <- stringr::str_remove_all(orcid, "[{}]")
    if (length(orcid == 0)) {
      invalid_orcid <- append(invalid_orcid, usr_name)
    }
    if (!grep('[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[A-Za-z0-9]{1}', orcid)) {
      invalid_orcid <- append(invalid_orcid, usr_name)
    }
  }
  if (!is.null(bad_usr)) {
    cli::abort("The following usernames are invalid. Please provide valid usernames: {bad_usr}.")
    return(FALSE)
  }
  if (!is.null(invalid_oricd)) {
    cli::abort("The following usernames do not have ORCIDs (or have invalid ORCIDs) associated with them. Plase have the users add valid ORCIDs (https://orcid.org) to their Active Directory account at https://myaccount.nps.gov/: {invalid_orcid}.")
    return(FALSE)
  }
  cli::inform("The file upload spreadsheet has been checked for errors.")
  return(TRUE)
}

check_508_format <- function(filename, path = getwd()){
  upload_data <- read.delim(file=paste0(path, "/", filename))
  is_508 <- tolower(upload_data$files_508_compliant) != "yes" &
    tolower(upload_data$files_508_compliant) != "no"
  if (sum(is_508) > 0) {
    msg <- paste0('All files must have a 508 compiance status of "yes" ',
                  'or "no". Please provide valid 508 compliance for ',
                  'each reference.')
    cli::cli_abort(c("x" = msg))
  } else {
    cli::cli_inform(
      c("v" = "All files have a valid 508 compliance designation"))
  }
}
