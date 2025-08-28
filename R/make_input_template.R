#' Write a template input file
#'
#' Writes a template input file (e.g. "AutioRecording_input.txt") to the specified path. The template input file can then be edited as needed, saved, and passed to the `bulk_reference_generation` function to create the specified references.
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
  template_file <- paste0(reference_type, "_input.txt")
  dat <- read.delim(system.file("extdata",
                                template_file,
                                package = "DSbulkUploadR"))
  write_file <- paste0(write_path, "/", template_file)
  cli::cli_inform("Writing: {.file {write_file}}.")

  readr::write_delim(x = dat,
                     file = write_file)

  return(invisible(dat))

}
