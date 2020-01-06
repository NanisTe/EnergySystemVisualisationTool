#'---
#'title: "Title_of_your_document"
#'author: "Your_Name"
#'output:
#'  html_document
#'runtime: shiny
#'---

# fwrite(sessioninfo::platform_info(),file.path(getwd(),"platform_info.csv"))
# fwrite(sessioninfo::package_info(),file.path(getwd(),"package_info.csv"))

project.load(type="raw") # load project 
source("./munge/Munge.R",encoding = "UTF-8")

#' <!--##################################################%##
#                                                          #
####        Example commenting wit little boxes         ####
#                                                          #
##%######################################################%##
#' --> # Example commenting wit little boxes