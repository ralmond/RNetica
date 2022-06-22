files <- file.path(R_PACKAGE_SOURCE,
                   paste0('libs', R_ARCH),
                   paste0("*", SHLIB_EXT))
dest <- file.path(R_PACKAGE_DIR, paste0('libs', R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.copy(Sys.glob(files), dest, overwrite = TRUE)
file.copy(Sys.glob(paste0("*",SHLIB_EXT)), dest, overwrite = TRUE)
if(file.exists("symbols.rds"))
    file.copy("symbols.rds", dest, overwrite = TRUE)
