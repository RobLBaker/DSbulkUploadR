write_core_bibliography <- function(reference_id,
                                    file_name,
                                    row_num,
                                    path = getwd(),
                                    dev = TRUE) {

  upload_data <- read.delim(file=paste0(path, "/", file_name))

  #get system date
  today <- Sys.Date()

  # populate draft reference bibliography ----
  begin_date <- upload_data$content_begin_date[row_num]
  end_date <- upload_data$content_end_date[row_num]

  #create author list ====
  usr_email <- unlist(stringr::str_split(
    upload_data$author_email_list[row_num],
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
    contacts <- append(contacts, list(author))
  }

  # generate json body for rest api call ====
  #AudioRecordings lack publisher element:
  if (upload_data$reference_type[row_num] == "AudioRecording") {
    mylist <- list(title = upload_data$title[row_num],
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
                   abstract = upload_data$description[row_num],
                   notes = upload_data$notes[row_num],
                   purpose = upload_data$purpose[row_num],
                   tableOfContents = "",
                   publisher = "Fort Collins, CO",
                   size1 = upload_data$length_of_recording[row_num],
                   contacts1 = contacts,
                   metadataStandardID = "",
                   licenseTypeID = upload_data$license[row_num])

  } else if (upload_data$reference_type[row_num] == "GenericDocument") {
    mylist <- list(title = upload_data$title[row_num],
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
                   abstract = upload_data$description[row_num],
                   notes = upload_data$notes[row_num],
                   purpose = upload_data$purpose[row_num],
                   tableOfContents = "",
                   publisher = "National Park Service",
                   publisher = "Fort Collins, CO",
                   contacts1 = contacts,
                   #metadataStandardID = "",
                   licenseTypeID = upload_data$license[row_num])
  }

  #for testing purposes and to look at the json sent:
  x <- rjson::toJSON(mylist)
  jsonlite::prettify(x)

  # make request to populate reference ====
  if (dev == TRUE) {
    api_url <- paste0(.ds_dev_api(),
                      "Reference/",  reference_id, "/Bibliography")
  } else {
    api_url <- paste0(.ds_secure_api(),
                      "Reference/",  reference_id , "/Bibliography")
  }

  req <- httr::PUT(
    url = api_url,
    httr::add_headers('Content-Type' = 'application/json'),
    httr::authenticate(":", "", "ntlm"),
    body = jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE))

}
