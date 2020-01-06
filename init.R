### -- Auto Start-up File -- ###


# === === === === === === === === === === === === === === === === === ===
# Start ####
# === === === === === === === === === === === === === === === === === ===
rm(list = ls());  print("Cleared workspace.") # clear workspace variables
print("Auto start-up project ...")

project_R_ver <<- "3.6.1"

# get R version
r.ver <<- paste0(R.version$major, ".", R.version$minor)

# set local package library
if(dir.exists(paste0("./local_package_library/", r.ver))){
  .libPaths(paste0("./local_package_library/", r.ver))
  print(".libPaths was set to ...")
  print(.libPaths())
}else{
  dir.create(paste0("./local_package_library/", r.ver),recursive = TRUE)
}



if(paste0(R.Version()$major,".",R.Version()$minor)!= project_R_ver){
  warning(paste0("Your R Version does not correspond with the projects R Version ",project_R_ver))
}

# === === === === === === === === === === === === === === === === === ===
# R Options ####
# === === === === === === === === === === === === === === === === === ===
options(digits = 3);print("activated options(digits = 3)")
options(stringsAsFactors = F);print("activated options(stringsAsFactors = F)")
options(java.parameters = "-Xmx8000m");print("activated options(java.parameters = \"-Xmx8000m\")")
options(Ncpus = 4); print("activated options(Ncpus = 4)")

#options(encoding = "UTF-8") #produced problems with character strings in dygraph titles and text.

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
# !!!! need to set R studio project to default UTF-8 encoding !!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# === === === === === === === === === === === === === === === === === ===
# Source "ProjectInit.R from "./" ----
# === === === === === === === === === === === === === === === === === ===
if (file.exists("./ProjectInit.R")) {
  source("./ProjectInit.R", encoding = "UTF-8")
  print("Loading Project initialisation functions from local copy ./ProjectInit.R")
}


# === === === === === === === === === === === === === === === === === ===
# Source "ProjectSetup.R from "./" ----
# === === === === === === === === === === === === === === === === === ===
if (file.exists("./functions/ProjectSetup.R")) {
  source("./functions/ProjectSetup.R", encoding = "UTF-8")
  print("Loading Project Functions from local copy ./functions/ProjectSetup.R")
} else { 
  stop("Loading Project Functions failed. Consider to copy ProjectSetup.R into ./functions/")
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# End auto start-up ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
print("Auto start-up done ...")


### -- End Auto Start-up -- ###