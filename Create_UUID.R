if(!require("pacman")){
	install.packages("pacman")
	library("pacman")
}
p_load(
	    uuid,
		git2r,
		dplyr
      )
pj_id = "unknown"
wd = getwd()
if(!length(list.files(wd, pattern = "#PJ_Info_*"))==0){
	pj_id = readLines(file.path(wd,list.files(wd, pattern = "#This_PJ_Info_*")))[1]
}
	  
repos <- git2r::repository(wd)
  sha <- git2r::revparse_single(repo = repos,"HEAD")$sha

writeClipboard(
	paste(
		"PJ-UUID: ", pj_id,"\n",
		"UUID:    ",UUIDgenerate(FALSE) %>% str_replace_all("-", "") ,"\n",
		"GIT-SHA: ",sha, sep = " "
	))
print(getwd())
print(readClipboard())