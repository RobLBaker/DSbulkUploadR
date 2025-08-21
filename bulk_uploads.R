#setup/helper functions:
#initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent=emptyenv())

#data_store API base URL:
assign("ds_api",
       "https://irmaservices.nps.gov/datastore/v7/rest/",
       envir=.pkgglobalenv)

#data_store secure API base URL:
assign("ds_secure_api",
       "https://irmaservices.nps.gov/datastore-secure/v7/rest/",
       envir=.pkgglobalenv)

#data_store dev api (requires secure)
assign("ds_dev_api",
       "https://irmadevservices.nps.gov/datastore-secure/v7/rest/",
       envir = .pkgglobalenv)

.ds_api <- function(x){
  get("ds_api", envir = .pkgglobalenv)
}

.ds_secure_api <- function(x){
  get("ds_secure_api", envir = .pkgglobalenv)
}

.ds_dev_api <- function(x){
  get("ds_dev_api", envir = .pkgglobalenv)
}


# Helper functions ----

#' Checks whether dates are in ISO 8601 format
#'
#' This function can accept a single date or vector of dates, evaluates each one and return a vector of TRUE and FALSE values where TRUE indicates the date is in ISO 8601 format (yyyymmdd) and FALSE indicates it is not.
#'
#' @param mydate date or list of dates
#'
#' @returns vector of TRUE/FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' check_iso_date_format(12/12/2025)
#' }
check_iso_date_format <- function(mydate) {
  tryCatch(!is.na(as.Date(mydate, date.format = "yyyymmdd")),
           error = function(err) {FALSE})
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
check_units <- function(filename, path = getwd()){
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
    cli::cli_abort("msg")
  }

  #check for bad content units:
  content_units <- NULL
  for (i in 1:nrow(upload_data)) {
    content_units <-
      append(content_units,
             unlist(stringr::str_split(upload_data$content_units[i], ", ")))

    content_units <- unique(content_units)
    bad_content_units <- content_units[!(content_units %in% dat$UnitCode)]
  }

  if (length(bad_content_units > 0)) {
    msg <- paste0("The following content units are invalid. ",
                  "Please provide valid content units: {bad_content_units}.")
    cli::cli_abort(msg)
  }
  cli::cli_inform("All producing and content units are valid.")
  return(TRUE)
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
      cli::cli_abort("msg")
    }
  return(TRUE)
}

#error if > file_number_error; warn if > 50% of file_number_error
check_file_number <- function(filename,
                              path = getwd(),
                              file_number_error = 500) {
  upload_data <- read.delim(file=paste0(path, "/", filename))
  file_num <- NULL
  for (i in 1:nrow(upload_data)) {
    files_per_ref <- length(list.files(upoad_data$file_path[i]))
    file_num <- file_num + files_per_ref
  }
  if (file_num > error) {
    msg <- paste0("You are attempting to upload {file_num} files, ",
                  "which exceeds the maximum allowable number, ",
                  "({error} files). Please either adjust the maximum number ",
                  "of files or attempt to upload fewer files.")
    cli::cli_abort(msg)
  }
  if (file_num > (error * 0.5)) {
    msg <- paste0("You are attempting to upload {file_num} files, ",
                  "which has triggered a warning. Please make sure you ",
                  "want to upload this many files.")
    cli::cli_inform(msg)
  }
  return(TRUE)
}

#WARN AND ERROR GIVIN IN GB
#error if > file error size; warn if > 50% error size
check_file_size <- function(filename,
                            path = getwd(),
                            file_size_error = 100) {
  #convert gb to bytes:
  error_bytes <- error * 1073741824
  warn_bytes <- error_bytes * 0.5
  upload_data <- read.delim(file=paste0(path, "/", filename))
  file_size <- NULL
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
                  "the maximum number data upload amount, or attempt to ",
                  "upload less data.")
    cli::cli_abort(msg)
  }
  if (file_num > warn_bytes) {
    msg <- paste0("You are attempting to upload {file_gb} GB of data, ",
                  "which has triggered a warning. Please make sure you ",
                  "want to upload this much data.")
    cli::cli_inform(msg)
  }
  return(TRUE)
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
    cli::cli_abort(msg)
  }
  cli::cli_inform("All reference types are valid.")
  return(TRUE)
}

check_license_type <- function(filename, path = getwd()) {
  upload_data <- read.delim(file=paste0(path, "/", filename))
  valid_license <- sum(upload_data$license_code != (1 | 2 |3 | 4 | 5))
  if (valid_license > 0) {
    msg <- paste0("You have supplied an invalid license code. ",
                  "Please supply a valid license code.")
    cli::cli_abort(msg)
  }
  return(TRUE)
}

check_users_email <- function(filename, path = getwd()) {
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

  if (all(rjson$found)) {
    return(TRUE)
  } else {
    bad_usr_emails <- dplyr::filter(rjson, found==FALSE)$searchTerm
    msg <- paste0("Please supply valid emails. The following emails ",
                  "are invalid: {bad_usr_emails}.")
    cli::cli_abort(msg)
  }
}

check_user_orcid <- function(filename, path = getwd()) {
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

  # test whether orcids are valid:
  rjson$orcid_format <- grepl('[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[A-Za-z0-9]{1}', rjson$extensionAttribute2)
  if (sum(rjson$orcid_format) == nrow(rjson$orcid_format)) {
    cli::cli_inform(c("v" = "All ORCiDs have a valid format"))
  } else {
    rjson2 <- rjson %>% dplyr::filter(rjson$orcid_format == FALSE)
    msg <- paste0("All authors must have ORCiDs with valid formats. ",
                  "The following authors have ORCiDs with invalid formats",
                  "listed in Active Dirctory: {rjson2$searchTerm}.")
    cli::cli_abort(c("x" = msg))
  }
  return(TRUE)
}





}

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
  if (is_508 > 0) {
    msg <- paste0('All files must have a 508 compiance status of "yes" ',
                  'or "no". Please provide valid 508 compliance for ',
                  'each reference.')
    cli::cli_abort(msg)
  }
  return(TRUE)
}

#file_size_error in GB
check_input_file <- function(filename, path = getwd(),
                             file_size_error = 100,
                             file_number_error = 500) {

  #check reference type:
  refs <- check_ref_type(filename = filename, path = path)
  if (refs != TRUE){
    return(FALSE)
  }

  files <- check_files_exist(filename = filename, path = path)
  if (files != TRUE) {
    return(FALSE)
  }

  file_number <- check_file_number(filename,
                                   path = path,
                                   file_number_error = file_number_error)
  if (file_number !=TRUE) {
    return(FALSE)
  }

  file_size <- check_file_size(filename,
                               path = path,
                               file_size_error = file_size_error)
  if (file_size != TRUE) {
    return(FALSE)
  }

  is508 <- check_508_format(filename = filename, path = path)
  if (is508 != TRUE) {
    return(FALSE)
  }

  #check that dates are is ISO format:
  begin_iso <- !check_iso_date_format(upload_data$content_begin_date)
  if (sum(begin_iso > 0)) {
    msg <- paste0("Some dates in the begin_content_date column are not ",
                  "in ISO 8601 format (yyyymmdd). Please correct this error.")
    cli::cli_abort(msg)
  }

  end_iso <- !check_iso_date_format(upload_data$content_end_date)
  if ( sum(end_iso > 0)) {
    msg <- paste0("Some dates in the end_content_date column are not ",
                  "in ISO 8601 format (yyyymmdd). Please correct this error.")
    cli::cli_abort(msg)
  }

  #check that producing units and content units are valid units:
  units_valid <- check_units(filename = filename, path = path)
  if (units_valid != TRUE){
    return(FALSE)
  }

  #check that user names are valid and have valid orcids:
  users_valid <- check_users(filename = filename, path = path)
  if (users_valid != TRUE){
    return(FALSE)
  }
  msg <- paste0("Your file for bulk uploads has passed all data ",
                "validation steps.")
  cli::cli_inform(msg)
  return(TRUE)
}

#creates a draft reference; returns the new reference code:
create_draft_reference <- function(draft_title = "Temp Title",
                                   ref_type,
                                   dev = FALSE) {

  #generate draft title:
  dynamic_title <- draft_title
  #generate json body for rest api call:
  mylist <- list(referenceTypeId = ref_type,
                 title = dynamic_title,
                 location = "",
                 issuedDate = list(year = 0,
                                   month = 0,
                                   day = 0,
                                   precision = ""))
  bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)
  #Create empty draft reference:
  if(dev == TRUE){
    post_url <- paste0(.ds_dev_api(), "Reference/CreateDraft")
  } else {
    post_url <- paste0(.ds_secure_api(), "Reference/CreateDraft")
  }
  req <- httr::POST(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type'='application/json'),
                    body = bdy)
  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

  #get draft reference code:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  ds_ref <- rjson$referenceCode

  refturn(ds_ref)
}

#largely borrowed from Sarah's NPSdatastore package:
upload_files <- function(filename,
                         path,
                         reference_id,
                         is508 = FALSE,
                         chunk_size_mb = 1,
                         retry = 1,
                         dev = FALSE) {

  file_name <- paste0(path, "/", filename)

  # Get a token, which we need for a multi-chunk upload
  if(dev == TRUE){
    post_url <- paste0(.ds_dev_api())
  } else {
    post_url <- paste0(.ds_secure_api())
  }
  req <- httr2::req_options(httr2::request(post_url),
                            httpauth = 4L,
                            userpwd = ":::")

  upload_token <- req |>
    httr2::req_url_path_append("Reference",
                               reference_id,
                               "UploadFile",
                               "TokenRequest") |>
    httr2::req_body_json(list(Name = file_name,
                              Is508Compliant = is_508),
                         type = "application/json") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  .validate_resp(upload_token,
                 nice_msg_400 = "Could not retrieve upload token. This usually happens if you don't have permissions to edit the reference or if you are not connected to the DOI/NPS network."
  )

  upload_url <- upload_token$headers$Location

  # Get file size, set chunk size, determine number of chunks
  file_size_bytes <- file.size(file_name)
  chunk_size_bytes <- round(chunk_size_mb * 1024 * 1024)
  n_chunks <- ceiling(file_size_bytes/chunk_size_bytes)

  # Open file connection in binary mode
  file_con <- file(file_path, "rb")

  # Initialize variables and progress bar to track upload progress
  status <- NA
  total_bytes <- 0
  cli::cli_progress_bar("Uploading file", total = n_chunks)

  # Upload one chunk at a time
  for (i in 0:(n_chunks - 1)) {

    # Starting byte and ending byte for this chunk
    start <- i * chunk_size_bytes
    end <- start + chunk_size_bytes - 1

    # If we've exceeded the file size, reset ending byte
    if (end >= file_size_bytes) {
      end <- file_size_bytes - 1
    }

    n_bytes <- length(start:end)  # this should be chunk_size_bytes except on the last iteration
    total_bytes <- total_bytes + n_bytes  # total bytes uploaded so far

    # Reset the number of retries for each new chunk
    n_retries <- retry

    # Upload a single chunk. Potentially try again if it fails (retry > 0)
    while (n_retries >= 0) {
      upload_resp <- httr2::request(upload_url) |>
        httr2::req_method("PUT") |>
        httr2::req_headers(`Content-Length` = n_bytes,
                           `Content-Range` = glue::glue("bytes {start}-{end}/{file_size_bytes}")) |>
        httr2::req_body_raw(readBin(file_con, raw(), n = n_bytes)) |>
        httr2::req_options(httpauth = 4L, userpwd = ":::") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()

      if (!httr2::resp_is_error(upload_resp) || httr2::resp_status(upload_resp) == 410) {
        # If upload is successful, or if error is due to token problem, don't retry
        n_retries <- -1
      } else {
        # Decrement retries remaining
        n_retries <- n_retries - 1
      }
    }

    # Throw an error if the chunk ultimately fails
    if (httr2::resp_status(upload_resp) == 410) {
      err_msg <- "Your upload token is invalid or has expired. Please try again. If the problem persists, contact the package maintainer or the DataStore helpdesk."
    } else {
      err_msg <- "File upload was unsuccessful. Please try again. If the problem persists, contact the package maintainer or the DataStore helpdesk."
    }
    .validate_resp(upload_resp,
                   nice_msg_400 = err_msg)


    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  close(file_con)

  # TODO: Return details on uploaded file
  file_info <- list(url = upload_resp$headers$Location,
                    file_id = httr2::resp_body_json(upload_resp))

  return(file_info)
}


# BULK FILE GENERATION ----
bulk_reference_geneation <- function(filename,
                                     path = getwd(),
                                     dev = FALSE) {

  upload_data <- read.delim(file=paste0(path, "/", filename))

  #check upload file validity:
  validation <- check_input_file(upload_data = upload_data)
  if (validation != TRUE) {
    msg <- paste0("Please ensure you have supplied valid upload ",
                  "information and have addressed all the issues ",
                  "identified before proceeding with the bulk upload.")
    cli::cli_abort(msg)
    return()
  }

  #ask to proceed; verify number of refs to create:
  ref_count <- nrow(upload_data)
  msg <- paste0("Would you like to upload all of your files and create ",
                "{ref_count} ",
                "new references on DataStore?")
  cli::cli_inform(msg)
  var1 <- readline(prompt = "1: Yes\n2: No\n")
  if (var1 == 2) {
    cli::cli_inform("Exiting the function.")
    return()
  }

  #get system date
  today <- Sys.Date()

  for (i in 1:nrow(upload_data)) {
    # create draft reference ----
    ref_code <- create_draft_reference(draft_title = upload_data$title[i],
                                       ref_type,
                                       dev = dev)
    cli::cli_inform("Creating draft reference {ref_code}.")
    cli::cli_inform("Populating draft reference {ref_code}.")

    # populate draft reference bibliography ----
    begin_date <- upload_data$content_begin_date[i]
    end_date <- upload_data$content_end_date[i]

    #create author list ====
    usr_name <- unlist(stringr::str_split(upload_data$author_upn_list[i],
                                          ", "))
    # access active directory for each user name ####
    contacts <- NULL
    for (i in 1:length(usr_name)) {
      powershell_command <- paste0('([adsisearcher]\\"(samaccountname=',
                                   usr_name[i],
                                   ')\\").FindAll().Properties')

      AD_output <- system2("powershell",
                           args = c("-Command",
                                    powershell_command),
                           stdout = TRUE)

      # get orcid ####
      orcid <- AD_output[which(AD_output %>%
                                 stringr::str_detect("extensionattribute2\\b"))]
      # extract orcid
      orcid <- stringr::str_extract(orcid, "\\{.*?\\}")
      # remove curly braces:
      orcid <- stringr::str_remove_all(orcid, "[{}]")

      #get surname ####
      surname <- AD_output[which(AD_output %>%
                                   stringr::str_detect("sn\\b"))]
      surname <- stringr::str_extract(surname, "\\{.*?\\}")
      surname <- stringr::str_remove_all(surname, "[{}]")

      #get given name ####
      given <- AD_output[which(AD_output %>%
                                   stringr::str_detect("given"))]
      given <- stringr::str_extract(given, "\\{.*?\\}")
      given <- stringr::str_remove_all(given, "[{}]")

      #create author list ####
      author <- list(title = "",
                      primaryName = surname,
                      firstName = given,
                      middleName = "",
                      suffix = "",
                      affiliation = "",
                      isCorporate = FALSE,
                      ORCID = orcid)
      #add to contacts
      contacts <- append(contacts, author)
    }

    # dynamically set publisher because not all ref types have publishers ====
    pub_place <- NULL
    if (upload_data$reference_type[i] == "audioRecording") {
      pub_place <- ""
    } else {
      pub_place <- "Fort Collins, CO"
    }

    # generate json body for rest api call ====
    mylist <- list(title = upload_data$title[i],
                   issuedDate = list(year = lubridate::year(today),
                                     month = lubridate::month(today),
                                     day = lubridate::day(today),
                                     precision = ""),
                   contentBeginDate = list(year = lubridate::year(begin_date),
                                           month = lubridate::month(begin_date),
                                           day = lubridate::day(begin_date),
                                           precision = ""),
                   contentEndDate = list(year = lubridate::year(end_date),
                                         month = lubridate::month(end_date),
                                         day = lubridate::day(end_date),
                                         precision = ""),
                   location = "",
                   miscellaneousCode = "",
                   volume = "",
                   issue = "",
                   pageRange = "",
                   edition = "",
                   dateRange = "",
                   meetingPlace = "",
                   abstract = upload_data$description[i],
                   notes = upload_data$notes[i],
                   purpose = upload_data$purpose[i],
                   tableOfContents = "",
                   publisher = pub_place,
                   size1 = upload_data$length_of_recording[i],
                   contacts1 = list(contacts),
                   metadataStandardID = "",
                   licenseTypeID = upload_data$license[i])

    #for testing purposes and to look at the json sent:
    #x <- rjson::toJSON(mylist)
    #jsonlite::prettify(x)

    # make request to populate reference ====
    if (dev == TRUE) {
      api_url <- paste0(.ds_dev_api(),
                        "Reference/",  ref_code, "/Bibliography")
    } else {
      api_url <- paste0(.ds_secure_api(),
                        "Reference/",  ref_code , "/Bibliography")
    }

    req <- httr::PUT(
      url = api_url,
      httr::add_headers('Content-Type' = 'application/json'),
      httr::authenticate(":", "", "ntlm"),
      body = rjson::toJSON(mylist))

    # upload files to reference ----

    #translate 508compliance:
    complaint <- NULL
    if (upload_data$file_508_compliant[i] == "yes") {
      compliant <- TRUE
    } else {
      compliant <- FALSE
    }

    file_list <- list.files(path = path, full.names = TRUE)

    for (j in 1:length(list.files)) {

    upload_files(filename,
                 path = file_list[j],
                 reference_id = ref_code,
                 is508 = compliant,
                 chunk_size_mb = 1,
                 retry = 1,
                 dev = FALSE)
    }
  #add reference id column to dataframe to make it easier to find them all
  upload_data$reference_id[i] <- ref_code
  }
  return(upload_data)
}
