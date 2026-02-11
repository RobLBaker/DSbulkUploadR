#' Creates a blank draft reference on DataStore
#'
#' @param draft_title String. The title for the reference.
#' @param ref_type String. The reference type to create.
#' @param dev Logical. Should the reference be created on the development server or the production server? Defaults to TRUE
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
                                   dev = TRUE) {

  #Treat "FieldNotes" as "GenericDocument" as per management request.
  #FieldNotes will be uploaded as "GenericDocuments" because FieldNotes is
  #not a real reference type and does not exist in DataStore. GenericDocuments
  #that originated as FieldNotes will have the keyword "FieldNotes" added
  #to aid in later triage. Enjoy, people who will end up doing the triage!
  if (ref_type == "FieldNotes") {
    ref_type <- "GenericDocument"
  }


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

#' Uploads files of indeterminate size to a DataStore reference
#'
#' This file uploader is largely borrowed from the R package NPSdatastore. It will take a file of any size and upload it to a given reference. This involves chunking the file into smaller pieces in order to upload larger files.
#'
#' @param filename String. Name of the file to be uploaded.
#' @param path String. Location of the file to be uploaded.
#' @param reference_id String. The reference number for the DataStore reference the file will be uploaded to.
#' @param is_508 Logical. Is the file in question 508 compliant? TRUE = Yes, FALSE = No. Defaults to FALSE.
#' @param chunk_size_mb Integer. Size of file chunks to be uploaded, in MB
#' @param retry Integer. Number of times to retry uploading file chunks if a given chunk fails.
#' @param dev Logical. Defaults to TRUE. FALSE means files will be uploaded to the production server. TRUE means files will be uploaded to the development server. Use Dev = TRUE when testing the function.
#'
#' @returns List. Of information about the uploaded file.
#' @export
#'
#' @examples
#' \dontrun{upload_files(filename = "example_file.wav",
#'                                  path = getwd(),
#'                                  reference_id = 1234567,
#'                                  is508 = FALSE,
#'                                  dev = TRUE)}
upload_files <- function(filename,
                         path,
                         reference_id,
                         is_508 = FALSE,
                         chunk_size_mb = 1,
                         retry = 1,
                         dev = TRUE) {

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
        # Decrements retries remaining
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

#' Replaces Keywords from a DataStore reference with one or more supplied keywords
#'
#' @param reference_id Integer. The seven-digit DataStore ID for the reference
#' @param keywords String or Vector. The keywords to be added to the DataStore reference.
#' @param dev Logical. Defaults to TRUE. FALSE means files will be uploaded to the production server. TRUE means files will be uploaded to the development server. Use Dev = TRUE when testing the function.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' replace_keywords(reference_id = 1234567, keywords = c("test", "testing"))}
replace_keywords <- function(reference_id,
                         keywords,
                         dev = TRUE) {

  if (length(keywords < 2)) {
    bdy <- jsonlite::toJSON(keywords, pretty = TRUE, auto_unbox = FALSE)
  } else {
    bdy <- jsonlite::toJSON(keywords, pretty = TRUE, auto_unbox = TRUE)
  }

  # construct request URL
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
  #submit PUT request
  req <- httr::PUT(post_url,
                   httr::authenticate(":", "", "ntlm"),
                   httr::add_headers('Content-Type'='application/json'),
                   body = bdy)

  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  return(invisible(NULL))
}

#' Adds a product reference to one or more project references
#'
#' @param reference_id String (or Integer). The seven-digit DataStore reference ID of the product to be added to one or more Project References on DataStore. For instance this could be a Data Package, Web Site, etc.
#' @param project_id Integer, String, or Vector. One or more DataStore Project Reference IDs to which the Product reference will be added.
#' @param dev Logical. Whether or not the actions will occur on the development server. Defaults to TRUE.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' add_ref_to_projects(reference_id = 1234567, project_id = 7654321)
#' add_ref_to_projects(reference_id = 1234567, project_id = c(7654321,
#'                                                            9876543))}
add_ref_to_projects <- function(reference_id,
                                project_id,
                                dev = TRUE) {

  bdy <- jsonlite::toJSON(reference_id, pretty = TRUE)

  for (i in 1:length(project_id)) {

    # construct request URL
    if(dev == TRUE){
      post_url <- paste0(.ds_dev_api(),
                         "Reference/",
                         project_id[i],
                         "/ProductReference")
    } else {
      post_url <- paste0(.ds_secure_api(),
                         "Reference/",
                         project_id[i],
                         "/ProductReference")
    }

    req <- httr::POST(post_url,
                      httr::authenticate(":", "", "ntlm"),
                      httr::add_headers('Content-Type'='application/json'),
                      body = bdy)
  }
  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  return(invisible(NULL))
}

#' Set content unit links and bounding boxes
#'
#' This function takes one or more content unit codes and adds them, along with their bounding boxes, as content unit links to the specified reference on DataStore.
#'
#' @param reference_id String. Integer. The seven-digit DataStore ID for the reference that content unit links will be added to.
#' @param content_units String. Vector. One or more NPS park unit codes.
#' @param dev Logical. Whether or not the operation will be performed on the development server. Defaults to TRUE.
#' @param add_link Logical. Should content unit links be added? Defaults to TRUE
#' @param add_boundingbox Logical. Should a bounding box be added? Defaults to TRUE
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' set_content_units(reference_id = 1234567,
#'                   content_units = "ROMO")
#' set_content_units(reference_id = 1234567,
#'                   content_units = c("ROMO", "YELL",
#'                   dev = FALSE))}
set_content_units <- function(reference_id,
                              content_units,
                              dev = TRUE,
                              add_link = TRUE,
                              add_boundingbox = TRUE) {
  #generate body of API call:
  bdy <- NULL
  for (i in 1:length(content_units)) {
    cont_units <- list("unitCode" = content_units[i],
                       "andLinkedUnits" = add_link,
                       "andBoundingBox" = add_boundingbox)
    bdy <- append(bdy, list(cont_units))
  }
  bdy <- jsonlite::toJSON(bdy, pretty = TRUE, auto_unbox = TRUE)

  # construct request URL
  if(dev == TRUE){
    post_url <- paste0(.ds_dev_api(),
                       "Reference/",
                       reference_id,
                       "/Units")
  } else {
    post_url <- paste0(.ds_secure_api(),
                       "Reference/",
                       reference_id,
                       "/Units")
  }

  req <- httr::POST(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type'='application/json'),
                    body = bdy)
  status_code <- httr::stop_for_status(req)$status_code
  #if(!status_code == 200){
  #  stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  #}
  return(invisible(NULL))
}

#' Add one or more editors to a DataStore reference
#'
#' Accepts a comma separated list of valid nps (or nps partner) email addresses and adds the people specified via email address as an editors (formerly "owners") of the specified DataStore reference. Use the email address not the upn/username, e.g. first_last@nps.gov not FMLast@npsg.gov.
#'
#' @param reference_id String or Integer. The seven-digit DataStore ID for the reference that owners will be added to
#' @param editor_list String or Vector. The owner(s) that will be added to the reference
#' @param dev Logical. Whether or not the operations will be performed on the development server. Defaults to TRUE.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' add_editors(reference_id = 1234567,
#'            editor_list = "john_doe@nps.gov")
#'            }
add_editors <- function(reference_id,
                       editor_list,
                       dev = TRUE) {
  # get user info from email list:
  bdy <- editor_list
  bdy <- jsonlite::toJSON(bdy, pretty = TRUE, auto_unbox = FALSE)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = bdy)
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    cli::cli_abort(c("x" = "ERROR: Active Directory connection failed."))
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  bdy <- NULL
  for (i in 1:nrow(rjson)) {
    editors <- list("userCode" = rjson$userPrincipalName[i],
                   "lastName" = rjson$sn[i],
                   "fistName" = rjson$givenName[i],
                   "email" = rjson$mail[i])
      bdy <- append(bdy, list(editors))
    }
  bdy <- jsonlite::toJSON(bdy, pretty = TRUE, auto_unbox = TRUE)

  # construct request URL
  if(dev == TRUE){
    post_url <- paste0(.ds_dev_api(),
                       "Reference/",
                       reference_id,
                       "/Owners")
  } else {
    post_url <- paste0(.ds_secure_api(),
                       "Reference/",
                       reference_id,
                       "/Owners")
  }

  req <- httr::POST(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = bdy)
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    cli::cli_abort(c("x" = "ERROR: DataStore connection failed."))
  }
  return(invisible(NULL))
}

#' Remove editors (owners) from a DataStore reference
#'
#' You must be an owner/editor of the reference to remove owners/editors from the reference. You cannot remove all editors from a reference; all references must have at least one owner/editor. For a given reference ID, you can supply one or more owners/editors to be removed from that reference. Owners/editors to be removed should be supplied as a comma-separated list of email addresses (e.g. john_doe@nps.gov), not upns or AD usernames (e.g. not jdoe@nps.gov).
#'
#' @param reference_id String. Contains one 7-digit DataStore reference IDs
#' @param editor_list String or list. Contains one or more owner/editor email addresses to be removed from the specified reference.
#' @param dev Logical. Defaults to TRUE. Determines whether the operation will be executed on the development server (TRUE) or the production server (FALSE).
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'    remove_editors(reference_id = 1234567,
#'                  editor_list = c("john_doe@nps.gov",
#'                                 "jane_doe@partner.nps.gov"))
#'    }
remove_editors <- function(reference_id,
                          editor_list,
                          dev = TRUE) {

  #get upn for each email in a list of emails:
  bdy <- jsonlite::toJSON(editor_list, pretty = TRUE, auto_unbox = FALSE)
  req_url <- paste0("https://irmadevservices.nps.gov/",
                    "adverification/v1/rest/lookup/email")
  req <- httr::POST(req_url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = bdy)
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    cli::cli_abort(c("x" = "ERROR: Active Directory connection failed."))
  }
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  # store upn for email supplied:
  editors <- NULL
  for (i in 1:nrow(rjson)) {
    upns <- rjson$userPrincipalName[i]
    editors <- append(editors, upns)
  }
  #remove editors/owners one at a time:
  for (i in 1:length(seq_along(editors))) {

  # construct request URL
    if(dev == TRUE){
      delete_url <- paste0(.ds_dev_api(),
                         "Reference/",
                         reference_id,
                         "/Owners?userCode=",
                         editors[i])
    } else {
      delete_url <- paste0(.ds_secure_api(),
                         "Reference/",
                         reference_id,
                         "/Owners?userCode=",
                         editors[i])
    }

    req2 <- httr::DELETE(delete_url,
                      httr::authenticate(":", "", "ntlm"),
                      httr::add_headers('Content-Type' = 'application/json'))
    status_code <- httr::stop_for_status(req2)$status_code
    if (!status_code == 200) {
      cli::cli_abort(c("x" = "ERROR: DataStore connection failed."))
    }
  }
  return(invisible(NULL))
}


#' Remove a specific unit code from one or more references
#'
#' @param reference_id String. Integer. Vector. One or more of the seven-digit DataStore reference IDs for the reference(s) that content unit link will be removed.
#' @param unit_to_remove String. A single NPS park unit code.
#' @param dev Logical. Whether or not the operation will be performed on the development server. Defaults to TRUE.
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' remove_content_unit(2315542, "ROMO")
#' remove_content_unit(c(2315542, 2315551), "ROMO")}
remove_content_unit <- function(reference_id,
                                unit_to_remove,
                                dev = TRUE) {


  for (i in 1:length(reference_id)) {
    if(dev == TRUE){
      delete_url <- paste0(.ds_dev_api(),
                         "Reference/",
                         reference_id[i],
                         "/Units/",
                         unit_to_remove)
    } else {
      delete_url <- paste0(.ds_secure_api(),
                         "Reference/",
                         reference_id[i],
                         "/Units/",
                         unit_to_remove)
    }

    req <- httr::DELETE(url = delete_url,
                      httr::authenticate(":", "", "ntlm"),
                      httr::add_headers('Content-Type' = 'application/json'))
    status_code <- httr::stop_for_status(req)$status_code
    if (!status_code == 200) {
      cli::cli_abort(c("x" = "ERROR: DataStore connection failed."))
    }

    print(reference_id[i])

  }
  return(invisible(NULL))
}

#' Adds a keyword to a list of existing keywords for a DataStore reference
#'
#' add_another_keyword differs from add_keywords in that it does not replace the existing set of keywords but instead just adds one (or more - haven't tested multiple keywords yet) to the reference.
#'
#' @param reference_id String. Integer. A DataStore reference ID to have keywords added to.
#' @param keyword String. The keyword to be added
#' @param dev Logical. Whether the operation occur on the development or production server. Defaults to TRUE (development server)
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' add_keyword(1234567, "Ants")}
add_keyword <- function(reference_id,
                                keyword,
                                dev = TRUE){
  if (length(keyword < 2)) {
    bdy <- jsonlite::toJSON(keyword, pretty = TRUE, auto_unbox = FALSE)
  } else {
    bdy <- jsonlite::toJSON(keyword, pretty = TRUE, auto_unbox = TRUE)
  }

  # construct request URL
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
  #submit PUT request
  req <- httr::POST(post_url,
                   httr::authenticate(":", "", "ntlm"),
                   httr::add_headers('Content-Type'='application/json'),
                   body = bdy)

  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  return(invisible(NULL))
}

