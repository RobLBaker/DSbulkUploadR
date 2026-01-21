# DSbulkUploadR (development version)
## 2026-01-20
  * Add support for the "reference" type "FieldNotes". Note that "FieldNotes" is not a real reference type on DataStore. Information entered into the "FieldNotes" sheet on the DSbulkUploadR_input.xlsx input file will be treated as a GenericDocument on DataStore and will have the keyword "FieldNotes" added so that this flavor of GenericDocument can be triaged at a later date.
  
## 2025-12-05
  * Add support for the Project reference type. Documentation not yet completed.
  * Add a new data validation function to test that all reference types on a given sheet in the input .xlsx are identical.
  
## 2025-12-02
  * Add support for Generic Dataset reference type
  
## 2025-11-28
  * Finish first pass of remove_editors function; add function documentation
  
## 2025-11-17
  * Update readme to include full list of currently supported reference types.

## 2025-11-17
  * Update documentation to specify correct input file format (.xlsx rather than .txt)
  
## 2025-11-14
  * Caught some typos in documentation!
  
## 2025-10-01
  * Update keyword function names to better reflect their actual properties (add_keywords -> `replace_keywords()`; add_another_keyword -> `add_keyword()`). Did not bother deprecating, etc as there has not been a formal release yet.
    * Add `check_CUI_license_match()` function; add this function to the list of functions run during input data validation
    
## 2025-09-30
  * Add function `add_another_keyword()` and `remove_content_unit()`. Bug fixes and upgrades to other functions.
  * Add `check_CUI_label_valid()` function; add function to list of functions run when validating an input file
  * Fix list of valid CUI Label codes in articles

## 2025-09-29
  * Add `add_owners()` function and build the ability to add additional reference owners into the `bulk_reference_generation()` function.

## 2025-09-25
  * Add ability to use `bulk_reference_generation()` to create references but NOT upload files to them.

## 2025-09-25
  * Add `set_content_units()` function; add ability to set content units (and bounding boxes) to the `bulk_reference_generation()` function
  * Add CUI labels, CUI contact name, and CUI contact address fields to input.xlsx template
  * Update documentation to reflect support for WebSite reference type and CUI fields in input.xlsx

## 2025-09-24
  * Move functions from bulk_uploads.R to set_ref_properties.R for better function/file management.

## 2025-09-23
  * Add function `add_ref_to_projects()` to address bug in external function
  * Lots of testing and bug fixes to enable WebSite reference type
  * Updates to DSbulkUploadR_input.xlsx template

## 2025-09-22
  * Add the capability to add newly recreated DataStore references to an existing DataStore Project using the supplied project_id from the input.xlsx file.
  * Add a project_id column to all sheets on the input.xlsx template file
  * Add WebSite reference type column to the input.xlsx template file

## 2025-09-19
  * Deprecate `make_input_template()` (.txt) in favor of `write_input_template()` (.xlsx)

## 2025-09-18
  * Update functions and documentation as well as articles to take an .xlsx input file and add ability to specify the sheet within the .xlsx input file to use.

## 2025-09-16
  * Fix bugs in setting core bibliography for AudioRecordings and GenericDocuments
  
## 2025-09-03
  * Add support for AudioRecordings and GenericDocuments
