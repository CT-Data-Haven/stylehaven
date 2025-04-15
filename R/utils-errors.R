num_error <- function(x) {
    if (!is.numeric(x)) {
        cli::cli_abort("{.arg x} should be numeric", call = rlang::caller_env())
    }
}

names_in_df_error <- function(df, df_arg, nms) {
    if (!all(nms %in% names(df))) {
        missing <- setdiff(nms, names(df))
        cli::cli_abort("{.arg {df_arg}} is missing columns with the following name(s): {.arg {missing}}", call = parent.frame())
    }
}
