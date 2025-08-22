#file_size_error in GB
check_input_file <- function(filename, path = getwd(),
                             file_size_error = 100,
                             file_number_error = 500) {

  #check reference type:
  check_ref_type(filename = filename, path = path)


  check_files_exist(filename = filename, path = path)


  check_file_number(filename,
                    path = path,
                    file_number_error = file_number_error)

  check_file_size(filename,
                  path = path,
                  file_size_error = file_size_error)


  check_508_format(filename = filename, path = path)

  #check that dates are is ISO format:
  check_start_date(filename = filename, path = path)
  check_end_date(filename = filename, path = path)

  #check dates occur at logical times:
  check_end_after_start(filename = filename, path = path)
  check_dates_past(filename = filename, path = path)


  #check that producing units are valid units:
  check_prod_units(filename = filename, path = path)

  #check that content units are valid:
  check_content_units(filename = filename, path = path)




}
