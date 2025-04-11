align_cols <- function(l = NULL, r = NULL, total = NULL) {
    # need any 2 of the 3 args
    missing_args <- c(l = is.null(l), r = is.null(r), total = is.null(total))
    supplied <- names(missing_args[!missing_args])
    if (length(supplied) < 2) {
        cli::cli_abort(c(
            "This function requires 2 of the 3 arguments to be non-`NULL`, but you supplied {cli::no(length(supplied))} argument{?s}."
        ))
    }
    if (sum(missing_args) == 0) {
        cli::cli_alert_info("When {.arg l}, {.arg r}, and {.arg total} are all supplied, {.arg l} and {.arg r} take priority.")
    }
    if (!is.null(l) & is.null(r)) {
        r <- total - l
    }
    if (is.null(l) & !is.null(r)) {
        l <- total - r
    }
    ls <- paste(rep("l", l), collapse = "")
    rs <- paste(rep("r", r), collapse = "")
    paste(ls, rs, sep = "")
}