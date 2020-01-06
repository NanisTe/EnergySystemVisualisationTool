data_read_func <- function(file) {
  get(load(file,.GlobalEnv)) #!! if many variables are stored in the Rdata file this may not work.
}