# Insert an additional navigation block immediately before the existing
# "Investments - Country" sidebar item in every matching HTML file.
#
# The country slug is derived from the visible country name. The script starts
# in dry-run mode and creates timestamped backups when changes are applied.

folder <- "//ihx-vdm05/LIVE_var_www_performance$/prb-monitoring/rp4/test/rp3/investments"
original_example_file <- "G:/HQ/dgof-pru/Data/DataProcessing/Covid19/Oscar/Develop/original_investment_block.txt"
additional_block_file <- "G:/HQ/dgof-pru/Data/DataProcessing/Covid19/Oscar/Develop/additional_block.txt"

recursive <- TRUE
make_backups <- TRUE
dry_run <- FALSE

insert_investment_blocks <- function(
  folder,
  original_example_file,
  additional_block_file,
  recursive = TRUE,
  make_backups = TRUE,
  dry_run = TRUE
) {
  insertion_marker <- "[insert code here]"

  if (!dir.exists(folder)) {
    stop("Folder does not exist: ", folder)
  }
  if (!file.exists(original_example_file)) {
    stop("Original example does not exist: ", original_example_file)
  }
  if (!file.exists(additional_block_file)) {
    stop("Additional block does not exist: ", additional_block_file)
  }

  read_binary_text <- function(path) {
    size <- file.info(path)$size
    con <- file(path, open = "rb")
    on.exit(close(con))
    rawToChar(readBin(con, what = "raw", n = size))
  }

  write_binary_text <- function(path, contents) {
    con <- file(path, open = "wb")
    on.exit(close(con))
    writeBin(charToRaw(contents), con)
  }

  normalize_to_lf <- function(text) {
    text <- gsub("\r\n", "\n", text, fixed = TRUE)
    gsub("\r", "\n", text, fixed = TRUE)
  }

  use_target_newlines <- function(text, target_uses_crlf) {
    text <- normalize_to_lf(text)
    if (target_uses_crlf) {
      text <- gsub("\n", "\r\n", text, fixed = TRUE)
    }
    text
  }

  first_capture <- function(pattern, text, description) {
    match <- regexec(pattern, text, perl = TRUE)
    values <- regmatches(text, match)[[1L]]
    if (length(values) < 2L) {
      stop("Could not detect ", description, ".")
    }
    values[2L]
  }

  slugify <- function(country_name) {
    country_name <- trimws(country_name)
    ascii <- iconv(country_name, from = "", to = "ASCII//TRANSLIT")
    if (is.na(ascii)) {
      ascii <- country_name
    }
    slug <- tolower(ascii)
    slug <- gsub("[^a-z0-9]+", "-", slug)
    slug <- gsub("(^-+|-+$)", "", slug)
    if (!nzchar(slug)) {
      stop("Could not derive a slug from country name: ", country_name)
    }
    slug
  }

  locate_sidebar <- function(contents) {
    nav_start <- regexpr(
      '<nav\\b[^>]*id="quarto-sidebar"[^>]*>',
      contents,
      perl = TRUE
    )[1L]
    if (nav_start == -1L) {
      return(NULL)
    }

    remainder <- substr(contents, nav_start, nchar(contents))
    end_match <- regexpr("</nav\\s*>", remainder, perl = TRUE)
    relative_end <- end_match[1L]
    if (relative_end == -1L) {
      return(NULL)
    }

    end_length <- attr(end_match, "match.length")[1L]
    nav_end <- nav_start + relative_end + end_length - 2L
    list(
      start = nav_start,
      end = nav_end,
      text = substr(contents, nav_start, nav_end)
    )
  }

  locate_investment_item <- function(contents, label_prefix) {
    sidebar <- locate_sidebar(contents)
    if (is.null(sidebar)) {
      return(NULL)
    }

    label_pattern <- paste0(
      '<b\\b[^>]*>\\s*\\Q',
      label_prefix,
      '\\E\\s*-\\s*([^<]+?)\\s*</b>'
    )
    label_match <- regexec(label_pattern, sidebar$text, perl = TRUE)
    label_values <- regmatches(sidebar$text, label_match)[[1L]]
    label_positions <- label_match[[1L]]

    if (length(label_values) < 2L || label_positions[1L] == -1L) {
      return(NULL)
    }

    country_name <- trimws(label_values[2L])
    text_before_label <- substr(sidebar$text, 1L, label_positions[1L])
    item_positions <- gregexpr(
      '<li\\b[^>]*class="[^"]*sidebar-item[^"]*"[^>]*>',
      text_before_label,
      perl = TRUE
    )[[1L]]
    item_positions <- item_positions[item_positions != -1L]
    if (length(item_positions) == 0L) {
      return(NULL)
    }

    relative_item_start <- tail(item_positions, 1L)
    absolute_item_start <- sidebar$start + relative_item_start - 1L

    list(
      insert_at = absolute_item_start,
      country_name = country_name,
      country_slug = slugify(country_name),
      sidebar_text = sidebar$text
    )
  }

  line_indentation_before <- function(contents, position) {
    prefix <- if (position > 1L) substr(contents, 1L, position - 1L) else ""
    newline_positions <- gregexpr("\n", prefix, fixed = TRUE)[[1L]]
    newline_positions <- newline_positions[newline_positions != -1L]
    line_start <- if (length(newline_positions)) {
      tail(newline_positions, 1L) + 1L
    } else {
      1L
    }
    substr(prefix, line_start, nchar(prefix))
  }

  insert_before <- function(contents, position, block) {
    before <- if (position > 1L) substr(contents, 1L, position - 1L) else ""
    after <- substr(contents, position, nchar(contents))
    indentation <- line_indentation_before(contents, position)
    newline <- if (grepl("\r\n", contents, fixed = TRUE)) "\r\n" else "\n"
    paste0(before, trimws(block), newline, indentation, after)
  }

  original_example <- normalize_to_lf(read_binary_text(original_example_file))
  additional_template <- normalize_to_lf(read_binary_text(
    additional_block_file
  ))

  marker_position <- regexpr(insertion_marker, original_example, fixed = TRUE)[
    1L
  ]
  if (marker_position == -1L) {
    stop("The original example does not contain: ", insertion_marker)
  }
  if (!grepl("[country]", additional_template, fixed = TRUE)) {
    stop("The additional block does not contain the [country] placeholder.")
  }

  after_marker <- substr(
    original_example,
    marker_position + nchar(insertion_marker),
    nchar(original_example)
  )
  following_open_tag <- first_capture(
    '^\\s*(<li\\b[^>]*>)',
    after_marker,
    "the first list-item tag following the insertion marker"
  )

  # The supplied additional block ends with the same opening <li> as the
  # existing Investments item. Remove that duplicate seam tag before inserting.
  duplicate_pattern <- paste0(
    "\\s*\\Q",
    following_open_tag,
    "\\E\\s*$"
  )
  additional_template <- sub(
    duplicate_pattern,
    "",
    additional_template,
    perl = TRUE
  )

  example_label <- first_capture(
    '<b\\b[^>]*>\\s*([^<]+?)\\s*</b>',
    after_marker,
    "the Investments label following the insertion marker"
  )
  label_prefix <- sub("\\s*-\\s*[^-]+$", "", trimws(example_label), perl = TRUE)
  if (!nzchar(label_prefix)) {
    stop("Could not derive the label prefix from: ", example_label)
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

  changed_files <- character()
  skipped_files <- character()
  backup_suffix <- paste0(".bak-", format(Sys.time(), "%Y%m%d-%H%M%S"))

  for (path in html_files) {
    contents <- read_binary_text(path)
    target <- locate_investment_item(contents, label_prefix)

    if (is.null(target)) {
      skipped_files <- c(skipped_files, path)
      next
    }

    inserted_url_part <- paste0(
      "/investments/rp3/",
      target$country_slug,
      "/index.html"
    )
    if (grepl(inserted_url_part, target$sidebar_text, fixed = TRUE)) {
      message("Skipped (already inserted): ", path)
      skipped_files <- c(skipped_files, path)
      next
    }

    block <- gsub(
      "[country]",
      target$country_slug,
      additional_template,
      fixed = TRUE
    )
    block <- use_target_newlines(
      block,
      grepl("\r\n", contents, fixed = TRUE)
    )

    message(
      if (dry_run) "Would update: " else "Updating: ",
      path,
      " [country=",
      target$country_name,
      ", slug=",
      target$country_slug,
      "]"
    )
    changed_files <- c(changed_files, path)

    if (!dry_run) {
      if (make_backups) {
        backup_path <- paste0(path, backup_suffix)
        if (!file.copy(path, backup_path, overwrite = FALSE)) {
          stop("Could not create backup: ", backup_path)
        }
      }
      updated <- insert_before(contents, target$insert_at, block)
      write_binary_text(path, updated)
    }
  }

  action <- if (dry_run) "would be updated" else "were updated"
  message(length(changed_files), " file(s) ", action, ".")
  message(length(skipped_files), " file(s) were skipped.")
  if (dry_run && length(changed_files) > 0L) {
    message("Set dry_run <- FALSE to apply the changes.")
  }

  invisible(list(changed = changed_files, skipped = skipped_files))
}

insert_investment_blocks(
  folder = folder,
  original_example_file = original_example_file,
  additional_block_file = additional_block_file,
  recursive = recursive,
  make_backups = make_backups,
  dry_run = dry_run
)
