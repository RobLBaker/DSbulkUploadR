#' Creates a blank draft reference on DataStore
#'
#' @param draft_title String. The title for the reference.
#' @param ref_type String. The reference type to create.
#' @param dev Logical. Should the reference be created on the development server or the production server? Defaults to FALSE.
#'
#' @returns String. The DataStore reference number.
#' @export
#'
#' @examples
#' \dontrun{
#' create_draft_reference(draft_title = "DRAFT TITLE",
#'                         ref_type = "AudioRecording")}
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

  return(ds_ref)
}

#' Uploads files of indeterminant size to a DataStore reference
#'
#' This file uploader is largely borrowed from the R package NPSdatastore. It will take a file of any size and upload it to a given reference. This involves chunking the file into smaller pieces in order to upload larger files.
#'
#' @param filename String. Name of the file to be uploaded.
#' @param path String. Location of the file to be uploaded.
#' @param reference_id String. The reference number for the DataStore reference the file will be uploaded to.
#' @param is508 Logical. Is the file in question 508 compliant? TRUE = Yes, FALSE = No. Defaults to FALSE.
#' @param chunk_size_mb Integer. Size of file chunks to be uploaded, in MB
#' @param retry Integer. Number of times to retry uploading file chunks if a given chunk fails.
#' @param dev Logical. Defaults to FALSE. FALSE means files will be uploaded to the production server. TRUE means files will be uploaded to the development server. Use Dev = TRUE when testing the function.
#'
#' @returns List. Of information about the uploaded file.
#' @export
#'
#' @examples
#' \dontrun{upload_files(filename = "example_file.wav", path = getwd(), reference_id = 1234567, is508 = FALSE, dev = TRUE)}
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

  upload_url <- upload_token$headers$Location

  # Get file size, set chunk size, determine number of chunks
  file_size_bytes <- file.size(file_name)
  chunk_size_bytes <- round(chunk_size_mb * 1024 * 1024)
  n_chunks <- ceiling(file_size_bytes/chunk_size_bytes)

  # Open file connection in binary mode
  file_con <- file(file_name, "rb")

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
    #.validate_resp(upload_resp,
    #               nice_msg_400 = err_msg)


    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  close(file_con)

  # TODO: Return details on uploaded file
  file_info <- list(url = upload_resp$headers$Location,
                    file_id = httr2::resp_body_json(upload_resp))

  return(file_info)
}



add_keywords <- function(reference_id,
                         keywords,
                         dev = FALSE) {

  #make json for keywords list:

  bdy <- jsonlite::toJSON(keywords, pretty = TRUE, auto_unbox = TRUE)

  if(dev == TRUE){
    post_url <- paste0(.ds_dev_api(),
                       "Reference/",
                       reference_id,
                       "/Keywords")
  } else {
    post_url <- paste0(.ds_secure_api(),
                       "Reference/",
                       reference_id,
                       "/Keywords")
  }

  req <- httr::PUT(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type'='application/json'),
                    body = bdy)
  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

}


#' Launch bulk reference creation and file upload tool
#'
#' The function will first run `run_input_validation` on the supplied .txt of information and enforce data validation. All errors must be resolved before the function will proceed.
#'
#' The function will inform the user of the total number of references to be created, the number of files to be uploaded, and the total volume of data to be uploaded (in GB) and ask the user if they are sure they want to proceed.
#'
#' The function then creates a draft reference on DataStore for each line in the input .txt and uses the information provided in the .txt to populate the Reference. Finally, all files in the given path for a reference in the .txt will be uploaded to the appropriate reference.
#'
#' The original dataframe generated from the .txt is returned to the user with a single column added: the DataStore reference ID for each newly created reference.
#'
#'
#' @param filename String. The name of the file with information on what will be uploaded.
#' @param path String. Path to the file.
#' @param max_file_upload Integer. The maximum allowable number of files to upload. Defaults to 500.
#' @param max_data_upload Integer. The maximum allowable amount of data to upload (in GB). Defaults to 100.
#' @param dev Logical. Whether the reference creation/file uploads will occur on the development server (TRUE) or the production server (FALSE). Defaults to FALSE
#'
#' @returns Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' bulk_reference_generation(filename = "test_file.txt")}
bulk_reference_generation <- function(filename,
                                     path = getwd(),
                                     max_file_upload = 500,
                                     max_data_upload = 10,
                                     dev = FALSE) {

  #check upload file validity:
  validation <- run_input_validation(filename = filename,
                                     path = path,
                                     max_file_upload = max_file_upload,
                                     max_data_upload = max_data_upload,
                                     dev = dev)

  #force user to fix all errors:
  if (validation[1] > 0) {
    msg <- paste0("Please ensure you have supplied valid upload ",
                  "information and have addressed all the errors ",
                  "identified before proceeding with the bulk upload.")
    cli::cli_abort(msg)
    return()
  } else if (validation[2] > 0) {
    msg <- cat("The data validation process has identified",
                  "warnings. Are you sure you want to proceed without",
                  "addressing these warnings?","\n",
                  "1: Yes", "\n","2: No","\n")
    cli::cli_inform(c("!" = msg))
    var1 <- readline(prompt= "")
    if (var1 ==2) {
      cat("Exiting the function.")
      return()
    }
  }

  #get info about bulk upload creation:
  upload_data <- read.delim(file=paste0(path, "/", filename))

  #calculate number of files to upload:
  file_num <- 0
  for (i in 1:nrow(upload_data)) {
    files_per_ref <- length(list.files(upload_data$file_path[i]))
    file_num <- (file_num + files_per_ref)
  }

  #calculate total file size to upload:
  file_size <- 0
  for (i in 1:nrow(upload_data)) {
    file_size <- file_size +
      sum(file.info(list.files(upload_data$file_path[i],
                               full.names = TRUE))$size)
  }
  file_gb <- file_size/1073741824

  #ask to proceed; verify number of refs to create, files to upload, and total upload size:
  ref_count <- nrow(upload_data)
  msg <- paste0("Would you like to upload all of your files and create ",
                "{ref_count} new references on DataStore? This will ",
                "involve uploading {file_num} files and ",
                "{round(file_gb, 3)} GB of data.")
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
                                       ref_type = upload_data$reference_type[i],
                                       dev = dev)
    cli::cli_inform("Creating draft reference {ref_code}.")
    cli::cli_inform("Populating draft reference {ref_code}.")

    # populate draft reference bibliography ----
    begin_date <- upload_data$content_begin_date[i]
    end_date <- upload_data$content_end_date[i]

    #create author list ====
    usr_email <- unlist(stringr::str_split(upload_data$author_email_list[i],
                                          ", "))
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

    contacts <- NULL
    for (j in 1:length(usr_email)) {
      author <- list(title = "",
                     primaryName = rjson$sn[j],
                     firstName = rjson$givenName[j],
                     middleName = "",
                     suffix = "",
                     affiliation = "",
                     isCorporate = FALSE,
                     ORCID = rjson$extensionAttribute2[j])
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
    if (upload_data$files_508_compliant[i] == "yes") {
      compliant <- TRUE
    } else {
      compliant <- FALSE
    }

    file_list <- list.files(path = upload_data$file_path[i],
                            full.names = TRUE)

    for (j in 1:length(file_list)) {
      msg <- "Uploading file {j} of {length(file_list)} to reference {ref_code}."
      cli::cli_inform(msg)
      suppressWarnings(upload_files(filename = list.files(upload_data$file_path[i])[j],
                   path = upload_data$file_path[i],
                   reference_id = ref_code,
                   is508 = compliant,
                   chunk_size_mb = 1,
                   retry = 1,
                   dev = dev))
    }
  #add reference id column to dataframe to make it easier to find them all
  upload_data$reference_id[i] <- ref_code
  }
  return(upload_data)
}
