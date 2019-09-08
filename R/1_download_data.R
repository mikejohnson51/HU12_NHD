download_nhd_gdb <- function(nhd_dir, nhd_file, nhdplus_url) {
  print("checking for and/or downloading source data")
  out <- file.path(nhd_dir, nhd_file)
  if(!dir.exists(out)) {
    # NHDPlus https://www.epa.gov/waterdata/nhdplus-national-data
    
    dir.create(nhd_dir, showWarnings = FALSE)
    
    if(grepl("7z", nhdplus_url)) {
      download.file(nhdplus_url, file.path(nhd_dir, paste0(nhd_file, ".7z")))
    
      system(paste0("7z -o", nhd_dir, " x ", file.path(nhd_dir, paste0(nhd_file, ".7z"))))
    }
  }
  
  return(out)
}

download_wbd <- function(wbd_dir, wbd_zip_file, wbd_gdb_file, wbd_url) {
  out <- file.path(wbd_dir, wbd_gdb_file)
  
  if(is.na(wbd_url)) {
    return(out)
  }
  
  if(!dir.exists(out)) {
    dir.create(wbd_dir, showWarnings = FALSE)
    
    zip_file <- file.path(wbd_dir, paste0(wbd_zip_file))
    
    if(!file.exists(zip_file)) {
      download.file(wbd_url, zip_file)
    }
    
    unzip(zip_file, exdir = wbd_dir)
  }
  return(out)
}

download_rf1 <- function(rf1_url, rf1_dir) {
  dir.create(rf1_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(rf1_dir, "erf1_2.e00")
  if(file.exists(out_file)) {
    return(out_file)
  } else {
    download.file(rf1_url, destfile = paste0(out_file, ".gz"))
    out_file <- gunzip(paste0(out_file, ".gz"))
    return(as.character(out_file))
  }
}
