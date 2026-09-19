# Replace literal text in every .html file under a folder.
#
# Start with dry_run <- TRUE to preview the affected files. After checking the
# output, change it to FALSE to apply the replacements.
slug <- "switzerland"
folder <- paste0(
  "//ihx-vdm05/LIVE_var_www_performance$/prb-monitoring/rp4/test/rp3/rp3"
)
old_text <- 'collapse list-unstyled sidebar-section depth2'
new_text <- 'collapse list-unstyled sidebar-section depth2 show'

recursive <- TRUE
make_backups <- FALSE
dry_run <- FALSE

replace_in_html <- function(
  folder,
  old_text,
  new_text,
  recursive = TRUE,
  make_backups = TRUE,
  dry_run = TRUE
) {
  if (!dir.exists(folder)) {
    stop("Folder does not exist: ", folder)
  }
  if (!nzchar(old_text)) {
    stop("old_text must not be empty.")
  }

  html_files <- list.files(
    path = folder,
    pattern = "\\.html$",
    recursive = recursive,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(html_files) == 0L) {
    message("No HTML files found in: ", normalizePath(folder))
    return(invisible(character()))
  }

  read_file <- function(path) {
    size <- file.info(path)$size
    con <- file(path, open = "rb")
    on.exit(close(con))
    rawToChar(readBin(con, what = "raw", n = size))
  }

  write_file <- function(path, contents) {
    con <- file(path, open = "wb")
    on.exit(close(con))
    writeBin(charToRaw(contents), con)
  }

  count_matches <- function(contents, text) {
    positions <- gregexpr(text, contents, fixed = TRUE, useBytes = TRUE)[[1L]]
    if (positions[1L] == -1L) 0L else length(positions)
  }

  changed_files <- character()
  total_replacements <- 0L
  backup_suffix <- paste0(".bak-", format(Sys.time(), "%Y%m%d-%H%M%S"))

  for (path in html_files) {
    contents <- read_file(path)
    matches <- count_matches(contents, old_text)

    if (matches == 0L) {
      next
    }

    changed_files <- c(changed_files, path)
    total_replacements <- total_replacements + matches
    message(
      if (dry_run) "Would update: " else "Updating: ",
      path,
      " (",
      matches,
      " replacement(s))"
    )

    if (!dry_run) {
      if (make_backups) {
        backup_path <- paste0(path, backup_suffix)
        if (!file.copy(path, backup_path, overwrite = FALSE)) {
          stop("Could not create backup: ", backup_path)
        }
      }

      updated <- gsub(
        pattern = old_text,
        replacement = new_text,
        x = contents,
        fixed = TRUE,
        useBytes = TRUE
      )
      write_file(path, updated)
    }
  }

  action <- if (dry_run) "would be replaced" else "were replaced"
  message(
    total_replacements,
    " occurrence(s) ",
    action,
    " across ",
    length(changed_files),
    " file(s)."
  )

  if (dry_run && length(changed_files) > 0L) {
    message("Set dry_run <- FALSE to apply the changes.")
  }

  invisible(changed_files)
}

replace_in_html(
  folder = folder,
  old_text = old_text,
  new_text = new_text,
  recursive = recursive,
  make_backups = make_backups,
  dry_run = dry_run
)
