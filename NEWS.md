# DSbulkUploadR (development version)

##2025-09-23
  * Add function `add_ref_to_projects()` to address bug in external function
  * Lots of testing and bug fixes to enable WebSite reference type
  * Updates to DSbulkUploadR_input.xlsx template

##2025-09-22
  * Add the capability to add newly recreated DataStore references to an existing DataStore Project using the supplied project_id from the input.xlsx file.
  * Add a project_id column to all sheets on the input.xlsx template file
  * Add WebSite reference type column to the input.xlsx template file

##2025-09-19
  * Deprecate `make_input_template()` (.txt) in favor of `write_input_template()` (.xlsx)

##2025-09-18
  * Update functions and documentation as well as articles to take an .xlsx input file and add ability to specify the sheet within the .xlsx input file to use.

##2025-09-16
  * Fix bugs in setting core bibliography for AudioRecordings and GenericDocuments
  
##2025-09-03
  * Add support for AudioRecordings and GenericDocuments
