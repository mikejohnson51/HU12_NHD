download_data <- function(nhd_dir, nhd_file, nhdplus_url) {
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

download_nhdhr <- function(nhd_dir, hu02_list, nhdhr_bucket, nhdhr_file_list) {
  out <- c()
  for(hu02 in hu02_list) {
    print("checking for and/or downloading source data")
    out <- c(out, file.path(nhd_dir, hu02))
    
    if(!dir.exists(out[length(out)])) {
      
      dir.create(out, recursive = TRUE, showWarnings = FALSE)
      
      file_list <- read_xml(paste0(nhdhr_bucket, nhdhr_file_list, 
                                   "NHDPLUS_H_", hu02)) %>%
        xml_ns_strip() %>%
        xml2::xml_find_all(xpath = "//Key") %>%
        xml2::xml_text()
      
      file_list <- file_list[grepl("_GDB.zip", file_list)]
      
      for(key in file_list) {
        out_file <- paste0(out, "/", tail(strsplit(key, "/")[[1]], 1))
        download.file(paste0(nhdhr_bucket, key), out_file)
        unzip(out_file, exdir = out)
        unlink(out_file)
      }
    }
  }
  return(out)
}