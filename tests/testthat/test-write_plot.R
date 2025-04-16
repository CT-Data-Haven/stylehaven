test_that("write_plot defaults to working directory", {
    p <- test_plot()
    write_plot(p, "test_wd",
        separate_dirs = TRUE,
        devs = list(
            png = grDevices::png
        )
    )
    files <- list.files(testthat::test_path("png"), recursive = TRUE)
    expect_true(all(c("test_wd.png") %in% files))
    unlink(testthat::test_path("png"), recursive = TRUE)
})

test_that("write_plot writes to correct directories", {
    p <- test_plot()
    dir <- tmpdir_if_none("write1")
    write_plot(p, "test_sep",
        dir = dir, separate_dirs = TRUE,
        devs = list(
            jpg = grDevices::jpeg,
            png = grDevices::png
        )
    )
    files_sep <- list.files(dir, recursive = TRUE)
    expect_true(all(c("jpg/test_sep.jpg", "png/test_sep.png") %in% files_sep))

    write_plot(p, "test_lump",
        dir = dir, separate_dirs = FALSE,
        devs = list(
            tiff = grDevices::tiff
        )
    )
    files_lump <- list.files(dir, recursive = TRUE)
    expect_true(all(c("test_lump.tiff") %in% files_lump))

    unlink(dir, recursive = TRUE)
})

test_that("write_plot writes correct file types", {
    p <- test_plot()
    dir <- tmpdir_if_none("write2")

    write_plot(p, "test_types",
        dir = dir, separate_dirs = FALSE,
        devs = list(
            jpg = grDevices::jpeg,
            png = grDevices::png,
            pdf = grDevices::pdf
        )
    )
    files <- list.files(dir, "test_types", full.names = TRUE)
    info <- purrr::map(files, plot_info) |> dplyr::bind_rows()
    expect_setequal(info$format, c("JPEG", "PNG", "PDF"))

    unlink(dir, recursive = TRUE)
})

test_that("write_plot correctly adjusts image size with logo", {
    p <- test_plot()
    dir <- tmpdir_if_none("write3")

    w <- 4
    h <- 4
    abs <- 1
    rel <- 0.5
    dpi <- 300

    params <- tibble::tribble(
        ~id,           ~add_logo, ~place_inside, ~use_abs_logo, ~logo_abs, ~logo_scale,
        "base",        FALSE,     FALSE,         FALSE,         0,         0,
        "abs_inside",  TRUE,      TRUE,          TRUE,          abs,       0,
        "abs_outside", TRUE,      FALSE,         TRUE,          abs,       0,
        "rel_inside",  TRUE,      TRUE,          FALSE,         0,         rel,
        "rel_outside", TRUE,      FALSE,         FALSE,         0,         rel
    )

    files <- purrr::pmap(params, function(id, add_logo, place_inside, use_abs_logo, logo_abs, logo_scale) {
        write_plot(p, id,
            dir = dir,
            width = w, height = h, separate_dirs = FALSE,
            add_logo = add_logo, use_abs_logo = use_abs_logo,
            place_inside = place_inside,
            logo_abs = logo_abs, logo_scale = logo_scale,
            devs = list(jpg = grDevices::jpeg)
        )
        file.path(dir, paste(id, "jpg", sep = "."))
    }) |>
        rlang::set_names(params$id)

    info <- purrr::map(files, plot_info)
    expect_equal(info[["base"]]$height, h * dpi)
    expect_equal(info[["abs_inside"]]$height, h * dpi)
    expect_equal(info[["abs_outside"]]$height, (h + abs) * dpi)
    expect_equal(info[["rel_inside"]]$height, h * dpi)
    expect_equal(info[["rel_outside"]]$height, (h + h * rel) * dpi)

    unlink(dir, recursive = TRUE)
})

test_that("write_plot passes ... to ggsave", {
    p <- test_plot()
    dir <- tmpdir_if_none("write4")

    w <- 4
    h <- 4
    dpi <- 100

    write_plot(p, "test_dots",
        dir = dir,
        width = w, height = h, separate_dirs = FALSE,
        devs = list(jpg = grDevices::jpeg),
        dpi = dpi
    )
    info <- plot_info(file.path(dir, "test_dots.jpg"))
    # format seems to depend on platform
    density_patt <- sprintf("^\\+?%sx\\+?%s", dpi, dpi)
    expect_true(grepl(density_patt, info$density))
    expect_equal(info$width, w * dpi)

    unlink(dir, recursive = TRUE)
})

test_that("write_plot respects verbose argument", {
    p <- test_plot()
    dir <- tmpdir_if_none("write5")
    expect_message(
        dummy <- write_plot(p, "test_verbose", dir = dir, verbose = TRUE, devs = list(jpg = grDevices::jpeg)),
        "test_verbose.jpg' saved"
    )
    skip_on_ci()
    expect_silent(
        dummy <- write_plot(p, "test_quiet", dir = dir, verbose = FALSE, devs = list(jpg = grDevices::jpeg))
    )
})
