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

#creates a draft reference; returns the new reference code:
create_draft_reference <- function(draft_title = "Temp Title", ref_type) {
  #generate draft title:
  dynamic_title <- paste0("[DRAFT]: ", draft_title)
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

bulk_reference_creation <- function(path = getwd(),ilename) {
  #check that file exists
  #check that file is a .csv

  #check that file has the appropriate columns

  #load file:
  upload_data <- read.csv(file=paste0(path, "/", filename))

  ref_code <- create_draft_reference("reference_title_from_file")




}
