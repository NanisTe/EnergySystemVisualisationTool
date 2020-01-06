data_read_func <- function(file) {
  data <- read_excel(file,sheet = "Zeitreihen0h15")
  return(data)
}