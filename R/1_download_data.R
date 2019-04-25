download_data <- function(nhd_dir, nhd_file, nhdplus_url) {
  print("checking for and/or downloading source data")
  out <- file.path(nhd_dir, nhd_file)
  if(!dir.exists(out)) {
    # NHDPlus https://www.epa.gov/waterdata/nhdplus-national-data
    
    dir.create(nhd_dir, showWarnings = FALSE)
    
    download.file(nhdplus_url, file.path(nhd_dir, paste0(nhd_file, ".7z")))
    
    system(paste0("7z -o", nhd_dir, " x ", file.path(nhd_dir, paste0(nhd_file, ".7z"))))
  }
  
  return(out)
}
