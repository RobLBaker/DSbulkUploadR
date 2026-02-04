#setup/helper functions:
#initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent=emptyenv())

#data_store API base URL:
assign("ds_api",
       "https://irmaservices.nps.gov/datastore/v8/rest/",
       envir=.pkgglobalenv)

#data_store secure API base URL:
assign("ds_secure_api",
       "https://irmaservices.nps.gov/datastore-secure/v8/rest/",
       envir=.pkgglobalenv)

#data_store dev api (requires secure)
assign("ds_dev_api",
       "https://irmadevservices.nps.gov/datastore-secure/v8/rest/",
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

#this gets rid of the "no visible binding for global variable 'x'" error in build checks:
globalVariables(c("found"))
