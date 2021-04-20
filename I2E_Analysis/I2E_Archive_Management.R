## Import a single I2E dataset
Call.I2E.Data <- function(so_dropbox_dir, req_dtype) {
  files <- readRDS(paste0(so_dropbox_dir, "I2E_Aerosol/I2E_current_dataset_list.rds"))
  if (req_dtype %in% files$dtype) {
    file_loc <- paste0(so_dropbox_dir,files$loc[which(files$dtype == req_dtype)])
  } else {
    stop("Invalid dataset. Check I2E_current_dataset_list.rds")
  }
  return(readRDS(file_loc))
}

## Save a single I2E dataset and update the reference table
Save.I2E.Data <- function(dataset,so_dropbox_dir,req_dtype) {
  files <- readRDS(paste0(so_dropbox_dir, "I2E_Aerosol/I2E_current_dataset_list.rds"))
  dtype_ind <- which(files$dtype == req_dtype)
  if(length(dtype_ind > 0)) {
    files$loc[dtype_ind] <- paste0("I2E_Aerosol/Analysis/I2E_", req_dtype, "_", format(Sys.Date(),"%Y%m%d"), ".rds")
    files$Modified_Date[dtype_ind] <- format(Sys.time(),"%Y-%m-%d %H:%M",tz="Australia/Brisbane")
    saveRDS(dataset,paste0(so_dropbox_dir, files$loc[dtype_ind]))
  } else {
    files <- rbind(files, data.frame(dtype=req_dtype, 
                                     loc=paste0("I2E_Aerosol/Analysis/I2E_", req_dtype, "_", format(Sys.Date(),"%Y%m%d"), ".rds"),
                                     Modified_Date=format(Sys.time(),"%Y-%m-%d %H:%M",tz="Australia/Brisbane")))
    saveRDS(dataset,paste0(so_dropbox_dir, files$loc[nrow(files)]))
  }
  saveRDS(files,paste0(so_dropbox_dir, "I2E_Aerosol/I2E_current_dataset_list.rds"))
  rm(files,dataset,so_dropbox_dir,req_dtype,dtype_ind)
}

## Output the reference table
List.I2E.Data <- function(so_dropbox_dir) {
  return(readRDS(paste0(so_dropbox_dir, "I2E_Aerosol/I2E_current_dataset_list.rds")))
}