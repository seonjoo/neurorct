
#' Generate list of files for complete case analysis
#'
#' @param folder folder path that contains .img and .hdr files
#' @param complete if TRUE, return filepaths for complete-case analysis
#'
#' @importFrom magrittr "%>%"
#'
#' @return list of imaging files for complete case analysis
#' @export
#'
#' @examples
#' library(neurorct)
#' myfolder = "./con_0001/"
#' list_files(folder = myfolder)
#'
list_files = function(folder, complete = TRUE){
  if (complete){
    bl.filelist = list.files(paste(folder, sep = ''), pattern = ".*S0001.*\\.hdr")
    fu.filelist = list.files(paste(folder, sep = ''), pattern = ".*S0002.*\\.hdr")

    ## Select patient ID with complete data
    ID_bl <- regmatches(bl.filelist, regexpr("058.*S", bl.filelist))
    ID_fu <- regmatches(fu.filelist, regexpr("058.*S", fu.filelist))
    complete.ID <- sapply(intersect(ID_bl, ID_fu),  function(x){substr(x, 2, 6)})  %>% unname()

    bl_cp.filelist <- sapply(complete.ID, function(x){list.files(paste(folder, sep=''),pattern=paste0('.*',x,".*S0001.*\\.hdr"))})%>% unname()
    fu_cp.filelist <- sapply(complete.ID, function(x){list.files(paste(folder, sep=''),pattern=paste0('.*',x,".*S0002.*\\.hdr"))})%>% unname()
    return(list(baseline = bl_cp.filelist, followup = fu_cp.filelist, id = complete.ID))
  }

}
