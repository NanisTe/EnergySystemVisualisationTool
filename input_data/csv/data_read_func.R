data_read_func <- function(file) {
  
  pacman::p_load(data.table)
  
  data <- data.table::fread(file, header = T,encoding = "UTF-8")
  
  return(data)
}