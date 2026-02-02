#' Write a template input file
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Writes a template input file (e.g. "AutioRecording_input.xlsx") to the specified path. The template input file can then be edited as needed, saved, and passed to the `generate_references` function to create the specified references.
#'
#' The currently supported reference_types are: AudioRecording.
#'
#' @param reference_type String. Case-sensitive DataStore reference type you would like to create an input template for. Defaults to "AudioRecording". Note that the reference_type is a single word and follows the DataStore backend naming conventions, not necessarily the names displayed on the DataStore website (compare AudioRecording with Audio Recording).
#' @param write_path String. The path where the file will be written. Defaults to the current working directory.
#'
#' @returns dataframe (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' make_input_template()
#' }
make_input_template <- function(reference_type = "AudioRecording",
                                write_path = getwd()) {

  lifecycle::deprecate_warn("0.0.1",
                            "make_input_template()",
                            "write_input_template()")


  template_file <- paste0(reference_type, "_input.txt")
  dat <- read.delim(system.file("extdata",
                                template_file,
                                package = "DSbulkUploadR"))
  write_file <- paste0(write_path, "/", template_file)
  cli::cli_inform("Writing: {.file {write_file}}.")

  utils::write.table(
    dat,
    write_file,
    sep = "\t",
    row.names = F,
    quote = F,
    fileEncoding = "UTF-8"
  )
  return(invisible(dat))
}

#' Write a template input file
#'
#' Writes a template .xlsx file ("DSbulkUploadR_input.xlsx") to the specified location.
#'
#' @param write_path String. The path where the input.xlsx file should be written. Defaults to the current working directory
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' write_input_template()
#' }
write_input_template <- function(write_path = getwd()) {
  template_file <- "DSbulkUploadR_input.xlsx"
  destination_path <- write_path
  cli::cli_inform("Writing: {.file {template_file}} to {destination_path}.")
  file.copy(system.file("extdata",
                        template_file,
                        package = "DSbulkUploadR"), destination_path)
  return(invisible(NULL))

}
