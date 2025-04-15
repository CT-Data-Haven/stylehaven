local_wd <- function(path = ".") {
    # p <- devtools::as.package(path)
    # p$path
    here::here()
}

tmpdir_if_none <- function(subdir) {
    dir <- file.path(tempdir(), subdir)
    if (!dir.exists(dir)) {
        dir.create(dir)
    }
    dir
}
