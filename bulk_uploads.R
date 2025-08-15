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


#creates a draft reference; returns the new reference code:
create_draft_reference <- function(draft_title = "Temp Title", ref_type) {
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

test_valid_dates <- function()

bulk_reference_creation <- function(filename, path = getwd(), dev = FALSE) {
  #check that file exists
  #check that file is a .csv

  #check that file has the appropriate columns

  #load file:
  upload_data <- read.delim(file=paste0(path, "/", filename))

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

  producing_units <- NULL
  for (i in 1:nrow(upload_data)) {
    producing_units <- append(producing_units,
           unlist(stringr::str_split(upload_data$producing_units[2], ", ")))
  }
  producing_units <- unique(producing_units)









  for (i in 1:nrow(upload_data)) {
    prod_units <- upload_data$producing_units[i]

  }




  }

  today <- Sys.Date()

  for(i in 1:nrow(upload_data)) {

    #create a draft reference and return the draft reference code:
    ref_code <- create_draft_reference(upload_data$title[i],
                                       upload_data$reference_type[i])

    #populate reference using bibliography API:

    #prework and data checking/wrangling:



    begin_date <- upload_data$content_begin_date[i]

    IsDate <- function(begin_date, date.format = "yyyymmdd") {
      tryCatch(!is.na(as.Date(begin_date, date.format)),
               error = function(err) {FALSE})
    }

    end_date <- upload_data$content_end_date[i]

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





                                                             ))


  }


}
