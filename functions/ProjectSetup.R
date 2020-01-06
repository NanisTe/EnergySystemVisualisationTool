### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### -- Project Setup File -- 
#
# Authors: Martin Ruedisueli (ruma), Sinan Teske (tes)
# Last update: 31-08-2018 (ruma)
# (c) Empa, 2018
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# TODO: check if R version is the needed version.


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# RESTORE WORKSPACE (last saved ---> .Rdata) ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
if (file.exists("./.Rdata")) { load("./.Rdata") }


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# OPERATORS ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
'%ni%' <- Negate('%in%')


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# PROJECT FUNCTIONS ----
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Meta data ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.metaData <- function(data_frame, var_desc = NULL, var_type = NULL, var_units = NULL, 
                              main_desc = "no description", type = "munged", ...) {
  # Source 1: https://cran.r-project.org/web/packages/dataMeta/vignettes/dataMeta_Vignette.html
  # Source 2: https://www.r-bloggers.com/adding-metadata-to-variables/
  #
  # EXAMPLE:
  # my_data <- .project.metaData(data_frame = my_data,
  #                              var_desc = c("Date when report was published",
  #                                           "Regional location",
  #                                           "Description of regional location",
  #                                           "Type of case",
  #                                           "A specific code for each data field",
  #                                           "The time period of each week",
  #                                           "The type of time period",
  #                                           "The number of cases per data field type",
  #                                           "The unit in which cases are reported"),
  #                              var_type = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  #                              var_units = c("cm", "cm", "cm", "cm", "cm", "cm", "cm", "cm", "cm"),
  #                              main_desc = "This data set portrays Zika infection related cases as reported by USVI.",
  #                              source = "BFE",
  #                              source_time = "01.02.2018",
  #                              location_Name = "Bern",
  #                              location_LV03 = "600000, 300000",
  #                              keyword = "A, B, C, D",
  #                              copied_from = "BAFU_Strompreise",
  #                              type = "munged")
  
  
  
  
  p_load(dataMeta)
  p_load(knitr)
  p_load(stringr)
  
  # number of variables/columns
  ncol_df <- ncol(data_frame)
  
  if (is.null(var_desc)) var_desc <- rep("-", ncol_df)
  if (is.null(var_type)) var_type <- rep(0, ncol_df)
  if (is.null(var_units)) var_units <- rep(NA, ncol_df)

    # create variable description
    # desc_data_frame <- data_frame[1,]
    # desc_data_frame <- rbind(desc_data_frame, var_desc, var_type)
    # desc_data_frame[1,] <- NULL
    # 
    # # edit variable description
    # data.entry(desc_data_frame)
    # 
    # # write description and type of varibales
    # var_desc <- desc_data_frame[1,]
    # var_type <- desc_data_frame[2,]
  
  # create linke
  linker <- build_linker(data_frame, variable_description = var_desc, variable_type = var_type)
  
  # create dictionary
  dict <- build_dict(my.data = data_frame, 
                     linker = linker, 
                     option_description = var_units,
                     prompt_varopts = FALSE)
  
  # add main title (main_desc)
  my_new_data <- incorporate_attr(my.data = data_frame, data.dictionary = dict, main_string = main_desc)
  
  # rename columns
  colnames(attributes(my_new_data)$dictionary) <- c("variable_name", "variable_description", "variable_options", "variable_units")
  
  # add further attributes
  more_attr <- list(...)
  attr_names <- names(more_attr)
  for(i in seq_along(more_attr))
  {
    attr(my_new_data, attr_names[i]) <- more_attr[[i]]
  }
  
  # Exporting dictionary only
  attr_all <- attributes(my_new_data)
  
  # Create filename and path
  file.name <- deparse(substitute(data_frame))
  file.path <- paste0("./cache/", type, "/", type, ".", file.name, ".xml")
  file.path_no_ext <- tools::file_path_sans_ext(file.path)
  
  # write txt file
  write("General description:", file.path)
  write(paste("\t", attr_all$main, "\n"), file.path, append = T)
  write("Variables:", file.path, append = T)
  write(paste("\t", attr_all$dictionary$variable_name, 
              "--->", attr_all$dictionary$variable_units,
              "--->", attr_all$dictionary$variable_description,
              "--->", attr_all$dictionary$variable_options), file.path, append = T)
  write("", file.path, append = T)
  write("Last edited:", file.path, append = T)
  write(paste("\t", attr_all$last_edit_date, "\n"), file.path, append = T)
  write("Author:", file.path, append = T)
  write(paste("\t", attr_all$author, "\n"), file.path, append = T)
  
  for(i in seq_along(more_attr)) {
    write(paste0(str_to_title(attr_names[i]),":"), file.path, append = T)
    write(paste("\t", more_attr[[i]], "\n"), file.path, append = T)
  }
  
  # Saving as .rds (dataset with appended dictionary) in cache
  save_it(x = my_new_data, name_of_file = file.path_no_ext)
  
  # return data frame with metadata
  return(my_new_data)
  
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Convert pdf to png ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.pdf.2.png <- function(file, dpi = 150,uuid="") {
  # https://stackoverflow.com/questions/41732862/error-running-imagemagick-from-r-invalid-parameter-80
  # imagemackig syntax has changed --> better using magick package from CRAN
  # 
  # # "convert -loop 0 -density 150 -delay 100 C:\\GIT_clones\\BAFU_R_all\\figures\\image87a.pdf \"C:\\GIT_clones\\BAFU_R_all\\figures\\image87a.png\""
  # 
  # proj.main.wd.tmp <- gsub("/", "\\", getwd(), fixed = T)
  # proj.main.wd <- gsub("Users\\ruma\\Documents\\GIT_clones_lokal", "GIT_clones", proj.main.wd.tmp, fixed = T)
  # 
  # cmd <- paste("convert -loop 0 -density", dpi, "-delay 100",
  #              paste0(proj.main.wd,"\\figures\\", file, ".pdf"),
  #              paste0("\"", proj.main.wd, "\\figures\\", file, ".png", "\""))
  # 
  # shell(cmd)


  # install ghostscript 64bit version if you have 64bit OS otherwise package magick will not work.
  # https://stackoverflow.com/questions/48834749/error-using-magick-r-to-import-pdf
  # 
  # https://imagemagick.org/
  # https://cran.r-project.org/web/packages/magick/vignettes/intro.html#converting_formats
  # 
    pacman::p_load("magick")
    pacman::p_load("pdftools")
    fpath <- paste0(file.path("C:","GIT_clones",basename(getwd()),"figures",file),".pdf")
    
    im <- magick::image_read_pdf(path =fpath, density = dpi)
    im <- magick::image_convert(image = im, format = "png",colorspace = "cmyk",antialias = T,depth = 16)
    magick::image_write(im, gsub(".pdf",".png",fpath),format = "png",depth = 16,density = dpi,comment = paste0("uuid: ", uuid))
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Copy figures (with meta data .txt) to latex figures folder ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.latex.figures <- function(fig.name, working.dir, ext = ".png") {
  
  file.copy(from = paste0("./figures/", fig.name, ext),
            to = paste0(working.dir, fig.name, ext),
            overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  
  file.copy(from = paste0("./figures/", fig.name, ".txt"),
            to = paste0(working.dir, fig.name, ".txt"),
            overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Install and load multiple R packages. ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# source: https://gist.github.com/stevenworthington/3178163
# 
# usage: 
# packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid")
# ipak(packages)

.project.ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Write R versions log (.txt) ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.r.ver <- function() {
  
  # get current R version (major and minor)
  r.ver <- paste0(R.version$major, ".", R.version$minor)
  
  if (file.exists("./R-Version-log.txt")){
  # read log file
  log.ver <- read.table("./R-Version-log.txt")$V2
  
  # add new version (with Date Time) if exists
  if (r.ver != tail(log.ver,1)) {
    # initialize log-file
    write(paste0("R-version: ", r.ver, " (", Sys.time(), ")"),
          append = TRUE,
          file = "./R-Version-log.txt")
  }
  } else {
    # initialize log-file
    write(paste0("R-version: ", r.ver, " (", Sys.time(), ")"),
          file = "./R-Version-log.txt")
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Get Project UUID from main project working directory ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.uuid <- function(working.dir = getwd()) {
  
  # get #PJ-Project file
  txt.files <- list.files(working.dir, pattern = "#PJ_*")
  
  # get UUID
  uuid.prj <- read.table(txt.files)$V1
  
  return(uuid.prj)
  
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Get SHA (UUID) GIT commit of main project working directory ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.git.sha <- function(working.dir = getwd()) {
  
  library(git2r)
  
  repos <- git2r::repository(working.dir)
  sha <- git2r::revparse_single(repo = repos,"HEAD")$sha
  
  return(sha)
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Create metadata file with UUID ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.uuid.metafile <- function(file.name, uuid.prj, uuid.fig, uuid.git) {
  
  
  # library(pacman)
  # p_load(uuid, git2r)
  # pj_id = "unknown"
  # wd = getwd()
  # if(!length(list.files(wd, pattern = "#PJ_Info_*"))==0){
  #   pj_id = readLines(file.path(wd,list.files(wd, pattern = "#PJ_Info_*")))[1]
  # }
  # 
  # repos <- git2r::repository(wd)
  # sha <- git2r::revparse_single(repo = repos,"HEAD")$sha
  

}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Clear workspace except for project functions ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.rm.ls <- function() {

  rm(list = setdiff(ls(envir = globalenv()), 
                    lsf.str(pattern = "*project.*", envir = globalenv())), envir = globalenv())
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Copy and source "GeneralFunctions.R" from global to local (to folder "functions") ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.update.GenFct <- function(source_flag = TRUE) {
  
  # install/load git2r
  p_load(git2r)
  
  # move (back-up) (locale) GeneralFunctions.R to folder "old GeneralFunctions"
  if( file.exists("./functions/GeneralFunctions.R") ) {
    time.stamp <- format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S", tz = "GMT")
    file.copy(from = "./functions/GeneralFunctions.R",
              to = paste0("./functions/old GeneralFunctions/", 
                          time.stamp, "_GeneralFunctions.R"),
              overwrite = TRUE,  copy.mode = TRUE, copy.date = TRUE,recursive = TRUE)
    file.copy(from = "./functions/GeneralFunctions.txt",
              to = paste0("./functions/old GeneralFunctions/", 
                          time.stamp, "_GeneralFunctions.txt"),
              overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE, recursive = TRUE)
    print("Copy GeneralOwnFunctions.R to <old> folder completed")
  }
  
  # copy GeneralOwnFunctions.R and rename
  file.copy(from = "../_NewProject/Copy_Project_Template_INSIDE/functions/GeneralFunctions.R",
            to = "./functions/GeneralFunctions.R",
            overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)

  # write GIT SHA of current GeneralOwnFunctions.R
  write(paste0("GIT SHA of repo <_NewProject>:\n", .project.git.sha("../_NewProject")), 
        file = "./functions/GeneralFunctions.txt")
  
  if (source_flag) {
    source("./functions/GeneralFunctions.R")
  }
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Copy and source "ProjectSetup.R" from global to local (to folder "functions") ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.update.ProjectSetup <- function(source_flag = TRUE) {
  
  # install/load git2r
  p_load(git2r)
  if( file.exists("../_NewProject/Copy_Project_Template_INSIDE/functions/ProjectSetup.R") ) {
  # move (back-up) (local) ProjectSetup.R to folder "old ProjectSetup"
  if( file.exists("./functions/ProjectSetup.R") ) {
    time.stamp <- format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S", tz = "GMT")
    file.copy(from = "./functions/ProjectSetup.R",
              to = paste0("./functions/old ProjectSetup/", 
                          time.stamp, "_ProjectSetup.R"),
              overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE,recursive = TRUE)
    file.copy(from = "./functions/ProjectSetup.txt",
              to = paste0("./functions/old ProjectSetup/", 
                          time.stamp, "_ProjectSetup.txt"),
              overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE,recursive = TRUE)
    print("Copy ProjectSetup.R to <old> folder completed")
  }
  
  
    # copy ProjectSetup and rename
    file.copy(from = "../_NewProject/Copy_Project_Template_INSIDE/functions/ProjectSetup.R",
              to = "./functions/ProjectSetup.R", 
              overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    
    # write GIT SHA of current ProjectSetup.R
    write(paste0("GIT SHA of repo <_NewProject>:\n", .project.git.sha("../_NewProject")), 
          file = "./functions/ProjectSetup.txt")
  } else {
    stop("Project Setup File not updated")
  }
  
  if (source_flag) {
    source("./functions/ProjectSetup.R")
  }
  
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Write caption for munge.R  ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.write.caption <- function(title = "some string", script = "munge", level = 1) {
  
  if (script == "munge") {
      write("# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###", 
            file="./munge/Munge.R", append = T)
      write(paste("#", title, "----"), 
            file="./munge/Munge.R", append = T)
      write("# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###\n\n", 
            file="./munge/Munge.R", append = T)
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Rename and copy raw data variables (and add _class) ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.rename.var <- function(variable, variable.name, del.raw.flag = F, add.flag = F) {
  # [variable] to be renamed
  # new [variable.name] (string)
  # del.raw.flag = remove raw data variabel from environment (default = TRUE)
  
  # select variable class
  if (add.flag) {
    class.var <- switch (class(variable),
                      "data.frame" = "df",
                      "SpatialPolygonsDataFrame" = "spdf",
                      "SpatialPolygons" = "sp")

    # add class to new variable name
    variable.name <- paste0(variable.name, "_", class.var)
  }
  
  
  # assign variable to new variable name and save globally
  assign(variable.name, variable, envir = .GlobalEnv)
  
  # Print structure of data
  str(variable)

  # remove variables from workspace
  if (del.raw.flag) {
    rm(list=deparse(substitute(variable)), envir=.GlobalEnv)
  } 
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Reset type/class of variable (e.g. string to numeric) ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.retype.var <- function(df, variable, type = "num") {
  # df = data.frame
  # [variable] (colume) to be renamed (string)
  # [type] (eg. numeric, factor, character)

  assign(substitute(df), 
           df[,variable] <- switch(type,
                                  "num" = {as.numeric(df[,variable])},
                                  "fac" = {as.factor(df[,variable])},
                                  "char" = {as.character(df[,variable])}), 
           envir = .GlobalEnv)

}
  

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# Load additional packages --- (deprecated) use pacman::p_load instead
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# .project.library <- function(library_name) {
#   
#   # list packages in checkpoint
#   checkpoint_path <- .libPaths()[1]
#   checkpoint_list <- list.files(checkpoint_path)
# 
#   # install packages (if not installed yet)
#   if (library_name %in% checkpoint_list) {
#       print(paste(library_name, "installed in checkpoint"))
#   } else {install.packages(library_name, lib = .libPaths()[1], dep = T)}
#     
#   # load packages
#   library(library_name, character.only = T)
# 
#   print(paste("Package", library_name, "has been loaded ..."))
#   
# }

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Checkpoint ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# .project.checkpoint <- function(checkpoint.date = Sys.Date()-1, copy.flag = F) {
#   # Sourcer: https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html
#   # checkpoint.date = "today" [default] or e.g "2017-12-18"
#   
#   # start checkpoint library from user library
#   if ("checkpoint" %in% (.packages())) {
#     library("checkpoint")
#   }
# 
#   # initialize Checkpoint
#   checkpoint(snapshotDate = checkpoint.date,
#              checkpointLocation = ".",
#              scanForPackages = FALSE)
#   
#   # remove tmp. files (file4a....)
#   list.file4a <- list.files(.libPaths()[1], pattern = "file*", full.names = T)
#   sapply(list.file4a, function(x) unlink(x, recursive = T, force = T))
#   
#   if(copy.flag) {
#     
#   # list of packages in standard_packages
#   list_of_packages <- list.files("../_NewProject_PackratPackages/standard_packages")
#     
#   # copy standard folders (if not existing)
#     print("*** Copy standard packages ...")
#     file.copy(from = file.path("../_NewProject_PackratPackages/standard_packages",list_of_packages),
#               to = .libPaths()[1],
#               overwrite = TRUE,
#               recursive = TRUE,
#               copy.mode = TRUE,
#               copy.date = TRUE)
#     print("*** Standard packages copied ...")
#   }
#   
# 
#   
# }

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Read raw data ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.read.data <- function(refresh.flag = F) {
  # refresh.flag = FALSE --> read only new files
  # refresh.flag = TRUE --> read all files again


  # subfolders in "data" (--> csv, shp, ...)
  subfolders.data <- list.dirs(path = "./input_data/", full.names = F, recursive = F)
  if (file.exists("./input_data/acceptedDataTypes.R")) {
    source("./input_data/acceptedDataTypes.R",encoding = "UTF-8")
    print(paste0("Accepted data types are: ", paste0(File_Extensions, sep = " ", collapse = "")))
  }
  else {
    File_Extensions <- c("csv", "rds", "shp", "xls", "xlsx")
    warning(paste0("Could not find '",
                   "./input_data/acceptedDataTypes.R",
                   "'. Used these file extensions instead: ",
                   File_Extensions))
  }

  # read files loop
  for (j in 1:length(subfolders.data)) {
    path.tmp <- paste0("./input_data/", subfolders.data[j])
    if (!identical(tools::list_files_with_exts(path.tmp,File_Extensions,full.names = F), character(0))) {

    # check for lokal data_read_func.R and load it
      if (file.exists(paste0(path.tmp, "/data_read_func.R"))) {
        source(paste0(path.tmp, "/data_read_func.R"),encoding = "UTF-8")
      }
      else {
        stop(paste0("data_read_func is missing in ",path.tmp))
      }
    # checking if data_read_func was defined in local data_read_func.R
      if (!exists("data_read_func", mode="function")) {
        stop(paste0("Loading data_read_func function from ", path.tmp,"/data_read_func.R", " failed!"))
      }

      # full path
      list.full <- tools::list_files_with_exts(path.tmp, File_Extensions, full.names = T)

      # only file names (no extention, path, ..)
      list.names <- tools::file_path_sans_ext(tools::list_files_with_exts(path.tmp, File_Extensions,
                                                                          full.names = F))

      # abbreviate file names
      # list.names.abbr <- substring(list.names, 1, 12)

      # read files
      for (i in 1:length(list.full)) {
      # read all files again
        if (refresh.flag) {
          assign(list.names[i],
                 data_read_func(list.full[i]),
                 envir = .GlobalEnv)
          .project.save.cache.raw(list.names[i], type = "raw")  # save .rda to cache
        }
      # read only new files
        else if (!file.exists(paste0("./cache/raw/raw.", list.names[i], ".Rdata"))) {
          assign(list.names[i],
                 data_read_func(list.full[i]),
                 envir = .GlobalEnv)
          .project.save.cache.raw(list.names[i], type = "raw")  # save .rda to cache
        }
        else {
          print(paste0(list.names[i], ".Rdata already exists"))
        }

      }

      if(exists("data_read_func", mode="function")){
        rm("data_read_func", envir = .GlobalEnv)
      }
    }
  }
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Write data to data_output [e.g. csv]   ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.save.output <- function(..., type = "csv") {
  
  # create list
  list <- sapply(substitute(list(...))[-1], deparse) 
  # https://stackoverflow.com/questions/5754367/using-substitute-to-get-argument-name-with
  
  n = length(list)
  
  # save as csv to cache
  for (i in 1:n) {
    if (type == "csv") {
      # https://stackoverflow.com/questions/5542945/opposite-of-rs-deparsesubstitutevar
      data.table::fwrite(x = eval(parse(text=list[i]), envir = .GlobalEnv),
                         file = paste0("./output_data/", list[i], ".", type))
    }
    
    # get UUID of item (e.g. dataset) or create new one if it does not exist
    if (file.exists(paste0("./output_data/", list[i], ".txt"))) {
      # get UUID
      df = read.table(paste0("./output_data/", list[i], ".txt"), sep = "=")
      uuid = df$V2[which(df$V1 == "Item-UUID ")] # with white space !!!
    } else {
      uuid = uuid::UUIDgenerate()
    }
    
    # Write txt file with UUID etc
    write(paste("Source-Path =", getwd(),
                "\nProject-UUID =", readLines(file.path(getwd(), list.files(".", pattern = "#PJ_Info_*")))[1], 
                "\nItem-Type =", class(eval(parse(text = list[i])))[1],
                "\nItem-UUID =", uuid,
                "\nCOMMIT-SHA =", .project.git.sha()),
          file = paste0("./output_data/", list[i], ".", type, ".txt"),
          append = FALSE)
    
    print(paste(list[i], " saved in data_output ..."))
  }
} 


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Save munged/analysed variables to cache ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.save.cache <- function(..., type = "munged") {

  # create list
  list <- sapply(substitute(list(...))[-1], deparse) 
  # https://stackoverflow.com/questions/5754367/using-substitute-to-get-argument-name-with

  # save as .rda to cache
  for (i in 1:length(list)) {
    save(list = list[i], file = paste0("./cache/", type, "/", type, ".", list[i], ".Rdata"))
    print(paste(list[i], " saved in cache ..."))
  }

}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Save raw data to cache ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.save.cache.raw <- function(list = names(...), type = "raw") {
  # Source --> https://stackoverflow.com/questions/4675755/how-to-save-with-a-particular-variable-name
  for (i in 1:length(list)) {
    save(list = list[i], file = paste0("./cache/", type, "/", type, ".", list[i], ".Rdata"))
    print(paste(list[i], " saved as .rda ..."))
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load variables from cache ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.load.cache <- function(type = "munged", var.name = NULL) {  
  # type = c("raw", "munged", "analysed")
  # var.name = single varialbe to load from cache
  # use Feather
  
  # if (!identical(list.files("./cache/munged"), character(0)) & type == "munged") { 
  #   type = "munged"
  # } else if (!identical(list.files("./cache/analysed"), character(0)) & type == "analysed") {
  #   type = "analysed"
  # } else {
  #   type = "raw"
  # }

  list.rda <- list.files(paste0("./cache/", type),
                                pattern = paste0(type, ".*.Rdata"), 
                                full.names = T)
  
  list.rda.short <- list.files(paste0("./cache/", type),
                         pattern = paste0(type, ".*.Rdata"))
  
  # load single variable [var.name] (string) from cache
  if (!is.null(var.name)){
    load(paste0("./cache/", type, "/", paste0(type, ".", var.name, ".Rdata")), 
         envir = .GlobalEnv)
    print(paste(var.name, "has been loaded ..."))
  # load all variables from cache
  } else { 
      if (length(list.rda)>0) {
        for (i in 1:length(list.rda)) {
          load(list.rda[i], envir = .GlobalEnv)
          print(paste(list.rda.short[i], "has been loaded ..."))
        }
      }
      else {
        warning("Cache is empty. Not loading anything.")
        warning("Trying to load new raw data.")
        .project.newdata()
      }
  }
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Save and load analysed cache (variables) ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# .rda.save.load.project <- function(refresh = F, variable, FUN, ...) {
#   list.rda <- list.files("./cache/analysed")
#   if (refresh) {
#     tmp <- FUN(...)
#     assign(variable, tmp, envir = .GlobalEnv)
#     .project.save.rda(variable, type = "analysed")
#   } else if (paste0("analysed.", variable, ".Rdata") %in% list.rda) {
#     load(paste0("./cache/analysed/analysed.", variable, ".Rdata"), envir = .GlobalEnv)
#   } else {
#     tmp <- FUN(...)
#     assign(variable, tmp, envir = .GlobalEnv)
#     .project.save.rda(variable, type = "analysed")
#   }
#   print("variable (saved and) loaded ...")
# }

  
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Source own functions ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.own.fcts <- function () {
  source("./functions/OwnFunctions.R",encoding = "UTF-8")
  print("Own functions loaded ...")
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Source Munge "data" ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.munge <- function() {
  # !! encoding UTF-8 muss angegeben werden falls Umlaute in der Datei sind. 
  # Einzelne Zeilen ausgeführ machen kein Problem aber das Script mit source geladen erzeugt riesen Problem.
  source("./munge/Munge.R", encoding = "UTF-8") 
  print("data has been munged ...")
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load packages from local package library ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
.project.load.packages <- function(except=NULL) {
  
  # get current R version (major and minor)
  r.ver <- paste0(R.version$major, ".", R.version$minor)
  
  # get path to current R.version folder
  path.r.ver <- paste0("./local_package_library/", r.ver)

  # create the local package library if it not exists
  if(!dir.exists(path.r.ver)){
    dir.create(path.r.ver,recursive = T)
  }
  # set local package library to current R.version
  .libPaths(path.r.ver)
  
  # list packages in local package library
  package.list <- list.files(path.r.ver)
  if(!is.null(except) & is.character(except)){
    package.list = package.list[(package.list%in%except)]
  }
  
  # load packages with pacman
  if (length(package.list)==0){ # TODO: work around to check if lib path has been newly created 
    install.packages("pacman") # install pacman first if there is an empty lokal library folder
  }
  library(pacman)
  p_load(char = package.list)

}


# === === === === === === === === === === === === === === === === === ===
# Reload project (not from .Rdata!) ####
# === === === === === === === === === === === === === === === === === ===
project.load <- function(type = "munged", update_functions = F, clear.warnings=F) {
  
  # clearing all previous warnings  
  if(clear.warnings){assign("last.warning", NULL, envir = baseenv())}
  
  # check for new r-version and add to log (if changed)
  .project.r.ver()
  
  # remove workspace expect project functions
  .project.rm.ls()
  
  # get current R version (major and minor)
  r.ver <- paste0(R.version$major, ".", R.version$minor)
  
  # set .libPaths to current R.version
  path.r.ver <- paste0("./local_package_library/", r.ver)
  
  # set local package library
  if (dir.exists(path.r.ver)){
    .libPaths(path.r.ver)
  }
  else{
    dir.create(path.r.ver)
    .libPaths(path.r.ver)
    
    # stop(paste0("The path ",path.r.ver,"does not exist. Consider installing the right version of R or creating a library folder with correct R version number."))
  }
  
  # update ProjectSetup.R and GeneralFunctions
  if (update_functions) {
    .project.update.GenFct()
    .project.update.ProjectSetup()
  }
  
  # source "ProjectSetup.R"
  source("./functions/ProjectSetup.R",encoding = "UTF-8")
  
  # source "GeneralFunctions.R"
  source("./functions/GeneralFunctions.R",encoding = "UTF-8")
  
  # load all local packages
  
  .project.load.packages()
  print("Packages loaded")
  
  # load own functions and additional packages therein
  .project.own.fcts()
  
  # load cached datasets
  print(paste0("Loading cached data of type ",type," ..."))
  .project.load.cache(type = type)
  print(paste0("Cached data loaded."))
  
  # prioritise (masked) functions (e.g dplyr --> select)
  # .project.prioritise.fcts()
  
  # Open files
  # file.edit("./functions/OwnFunctions.R")
  # file.edit("./munge/Munge.R")
  # file.edit("./scripts/Main_Script.R")
  
  # cat("\014") # clear console
  print("*** Project has been successfully loaded ...")
  
  # print warnings
  warnings()
  
}

# project.reload <- function(type = "all") {
#   
#   # remove workspace expect project functions
#   rm(list = setdiff(ls(envir = globalenv()), 
#                     lsf.str(pattern = "*project*", envir = globalenv())), envir = globalenv())
#   
#   .project.own.fcts()
#   
#   switch (type,
#     "all" = {.project.load.cache(type = "raw")
#              .project.load.cache(type = "munged")
#              .project.load.cache(type = "analysed")},
#     "munged" = {.project.load.cache(type = type)},
#     "raw" = {.project.load.cache(type = type)}
#     
#   )
#   
#   # packrat::on()
#   # .project.load.packages()
# 
#   
#   # cat("\014") # clear console
#   print("*** Project has been successfully reloaded ...")
#   Sys.sleep(2)
#   # cat("\014") # clear console
#   
# }


# === === === === === === === === === === === === === === === === === ===
# Re-Evaluate data (e.g with updated data) ####
# === === === === === === === === === === === === === === === === === ===
.project.newdata <- function(refresh.flag = F, munge.flag = F) {

  .project.read.data(refresh.flag = refresh.flag)
  print("*** New data has been read ...")
  
  if (munge.flag) {
    .project.munge()
    print("*** New data has been munged ...")
  } 

}
