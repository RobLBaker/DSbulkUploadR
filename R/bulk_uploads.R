






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
