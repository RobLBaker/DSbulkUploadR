#setup/helper functions:
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


##### Helper functions:

#check for iso dates:
#returns a vector of TRUE/FALSE values where TRUE is iso and FALSE is not.
check_iso_date_format <- function(mydate, date.format = "yyyymmdd") {
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}

check_input_file <- function(upload_data) {

  #loop through the uploaded dataframe:

  #check that dates are is ISO format:
  begin_iso <- !check_iso_date_format(upload_data$content_begin_date)
  if (sum(begin_iso > 0)) {
    cli::cli_abort("Some dates in the begin_content_date column are not in ISO 8601 format (yyyymmdd). Please correct this error.")
  }

  end_iso <- !check_iso_date_format(upload_data$content_end_date)
  if ( sum(end_iso > 0)) {
    cli::cli_abort("Some dates in the end_content_date column are not in ISO 8601 format (yyyymmdd). Please correct this error.")
  }

  #check that all producing and content units are valid units:

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
    cli::cli_abort("The following producing units are invalid. Please provide valid producing units: {bad_prod_units}.")
    return()
  }

  #check for bad content units:
  content_units <- NULL
  for (i in 1:nrow(upload_data)) {
    content_units <-
      append(content_units,
             unlist(stringr::str_split(upload_data$content_units[i], ", ")))

    if (length(bad_content_units > 0)) {
      cli::cli_abort("The following content units are invalid. Please provide valid content units: {bad_content_units}.")
      return()
    }
  }

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
    if (nchar(orcid) != 37) {
      invalid_orcid <- append(invalid_orcid, usr_name)
    }
  }
  if (!is.null(bad_usr)) {
    cli::abort("The following usernames are invalid. Please provide valid usernames: {bad_usr}.")
    return()
  }
  if (!is.null(invalid_oricd)) {
    cli::inform("The following usernames do not have ORCIDs (or have invalid ORCIDs) associated with them. Plase have the users add valid ORCIDs (https://orcid.org) to their Active Directory account at https://myaccount.nps.gov/: {invalid_orcid}.")
  }
  cli::inform("The file upload spreadsheet has been checked for errors.")
}

#creates a draft reference; returns the new reference code:
create_draft_reference <- function(draft_title = "Temp Title",
                                   ref_type,
                                   dev = FALSE+) {
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


bulk_reference_creation <- function(filename, path = getwd(), dev = FALSE) {
  upload_data <- read.delim(file=paste0(path, "/", filename))

  check_input_file(upload_data = upload_data)

  today <- Sys.Date()

  for (i in 1:nrow(upload_data)) {
    create_draft_reference(draft_title = upload_data$title[i],
                           ref_type, dev = dev)

    #create a draft reference and return the draft reference code:
    ref_code <- create_draft_reference(upload_data$title[i],
                                       upload_data$reference_type[i])

    #populate reference using bibliography API:

    begin_date <- upload_data$content_begin_date[i]
    end_date <- upload_data$content_end_date[i]

    #create author list:
    usr_name <- unlist(stringr::str_split(upload_data$author_upn_list[i],
                                          ", "))
    #access active directory for each user name:
    contacts <- NULL
    for (i in 1:length(usr_name)) {
      powershell_command <- paste0('([adsisearcher]\\"(samaccountname=',
                                   usr_name[i],
                                   ')\\").FindAll().Properties')

      AD_output <- system2("powershell",
                           args = c("-Command",
                                    powershell_command),
                           stdout = TRUE)

      #get orcid:
      orcid <- AD_output[which(AD_output %>%
                                 stringr::str_detect("extensionattribute2\\b"))]
      # extract orcid
      orcid <- stringr::str_extract(orcid, "\\{.*?\\}")
      # remove curly braces:
      orcid <- stringr::str_remove_all(orcid, "[{}]")

      #get surname:
      surname <- AD_output[which(AD_output %>%
                                   stringr::str_detect("sn\\b"))]
      surname <- stringr::str_extract(surname, "\\{.*?\\}")
      surname <- stringr::str_remove_all(surname, "[{}]")

      #get given name:
      given <- AD_output[which(AD_output %>%
                                   stringr::str_detect("given"))]
      given <- stringr::str_extract(given, "\\{.*?\\}")
      given <- stringr::str_remove_all(given, "[{}]")

      #create author list:
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

    #somehow build author lists?

    #generate json body for rest api call:
    mylist <- list(title = upload_data$title[i],
                   title = dynamic_title,
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
                   publisher = "Fort Collins, CO",
                   sizes = list(index = 1,
                                label = "Length of Recording",
                                value = upload_data$length_of_recording[i]),
                   abstract = upload_data$description[i],
                   contacts1 = contacts,
                   metadataStandard = "",
                   licenseType = upload_data$license[i])



  }


}
