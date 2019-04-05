
print("checking for and/or downloading source data")

if(!dir.exists(file.path(nhd_dir, nhd_file))) {
  # NHDPlus https://www.epa.gov/waterdata/nhdplus-national-data
  nhdplus_url <- "https://s3.amazonaws.com/nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_CONUS_Seamless_Geodatabase_05.7z"

  dir.create(nhd_dir, showWarnings = FALSE)

  download.file(nhdplus_url, file.path(nhd_dir, paste0(nhd_file, ".7z")))

  system(paste0("7z -o", nhd_dir, " x ", file.path(nhd_dir, paste0(nhd_file, ".7z"))))
}


