# Replace the Quarto "Year report" sidebar section with a new template.
# The old section is located from its label and balanced <li> boundaries.

folder <- "//ihx-vdm05/LIVE_var_www_performance$/prb-monitoring/rp4/test/rp3/rp3"
# original_example_file <- "G:/HQ/dgof-pru/Data/DataProcessing/Covid19/Oscar/Develop/original_block.txt"
replacement_example_file <- "G:/HQ/dgof-pru/Data/DataProcessing/Covid19/Oscar/Develop/replacement_block.txt"

recursive <- TRUE
make_backups <- FALSE
dry_run <- FALSE

replace_year_report_blocks <- function(
  folder,
  replacement_example_file,
  recursive = TRUE,
  make_backups = TRUE,
  dry_run = TRUE
) {
  if (!dir.exists(folder)) {
    stop("Folder does not exist: ", folder)
  }
  if (!file.exists(replacement_example_file)) {
    stop("Replacement example does not exist: ", replacement_example_file)
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

  extract_active_item <- function(block) {
    first_capture(
      "<span\\s+style=\"color:\\s*#2151bf\">\\s*(RP3|[0-9]{4})\\s*✓\\s*</span>",
      block,
      "the active blue RP3/year item"
    )
  }

  extract_old_state <- function(block) {
    first_capture(
      paste0(
        "https://www\\.eurocontrol\\.int/performance/prb-monitoring/",
        "rp4/test/rp3/(?:rp3|[0-9]{4})/([^/\"?#]+)/"
      ),
      block,
      "a state slug in the existing Year report links"
    )
  }

  extract_template_state <- function(block) {
    first_capture(
      paste0(
        "https://www\\.eurocontrol\\.int/performance/prb-monitoring/",
        "rp4/test/(?:[0-9]{4}|rp3/(?:rp3|[0-9]{4}))/([^/\"?#]+)/"
      ),
      block,
      "a state slug in the replacement template"
    )
  }

  remove_active_highlight <- function(text) {
    gsub(
      "<span\\s+style=\"color:\\s*#2151bf\">\\s*(RP3|[0-9]{4})\\s*✓\\s*</span>",
      "\\1",
      text,
      perl = TRUE
    )
  }

  add_active_highlight <- function(text, active_item) {
    needle <- paste0('<span class="menu-text">', active_item, '</span>')
    replacement <- paste0(
      '<span class="menu-text"><span style="color: #2151bf">',
      active_item,
      ' ✓</span></span>'
    )
    if (!grepl(needle, text, fixed = TRUE)) {
      stop("The replacement template has no item named: ", active_item)
    }
    sub(needle, replacement, text, fixed = TRUE)
  }

  replace_state_slug <- function(text, template_state, target_state) {
    gsub(
      paste0("/", template_state, "/"),
      paste0("/", target_state, "/"),
      text,
      fixed = TRUE
    )
  }

  active_item_is_nested <- function(template, active_item) {
    nested_position <- regexpr(
      'id="quarto-sidebar-section-2"',
      template,
      fixed = TRUE
    )[1L]
    item_position <- regexpr(
      paste0('<span class="menu-text">', active_item, '</span>'),
      template,
      fixed = TRUE
    )[1L]
    nested_position > 0L && item_position > nested_position
  }

  set_nested_rp3_state <- function(text, should_be_open) {
    if (should_be_open) {
      text <- sub(
        'class="sidebar-item-toggle text-start collapsed"',
        'class="sidebar-item-toggle text-start"',
        text,
        fixed = TRUE
      )
      text <- sub(
        'data-bs-target="#quarto-sidebar-section-2" role="navigation" aria-expanded="false"',
        'data-bs-target="#quarto-sidebar-section-2" role="navigation" aria-expanded="true"',
        text,
        fixed = TRUE
      )
      text <- sub(
        'id="quarto-sidebar-section-2" class="collapse list-unstyled sidebar-section depth2 "',
        'id="quarto-sidebar-section-2" class="collapse list-unstyled sidebar-section depth2 show"',
        text,
        fixed = TRUE
      )
    } else {
      text <- sub(
        paste0(
          'class="sidebar-item-toggle text-start" data-bs-toggle="collapse" ',
          'data-bs-target="#quarto-sidebar-section-2"'
        ),
        paste0(
          'class="sidebar-item-toggle text-start collapsed" data-bs-toggle="collapse" ',
          'data-bs-target="#quarto-sidebar-section-2"'
        ),
        text,
        fixed = TRUE
      )
      text <- sub(
        'data-bs-target="#quarto-sidebar-section-2" role="navigation" aria-expanded="true"',
        'data-bs-target="#quarto-sidebar-section-2" role="navigation" aria-expanded="false"',
        text,
        fixed = TRUE
      )
      text <- sub(
        'id="quarto-sidebar-section-2" class="collapse list-unstyled sidebar-section depth2 show"',
        'id="quarto-sidebar-section-2" class="collapse list-unstyled sidebar-section depth2 "',
        text,
        fixed = TRUE
      )
    }
    text
  }

  locate_year_report_block <- function(contents) {
    # Anchor on the actual label, not on a generic <li>.
    label_position <- regexpr(
      ">\\s*Year report\\s*<",
      contents,
      perl = TRUE
    )[1L]
    if (label_position == -1L) {
      return(NULL)
    }

    # Of all section <li> tags before the label, select the nearest one.
    prefix <- substr(contents, 1L, label_position)
    section_positions <- gregexpr(
      '<li\\b[^>]*class="[^"]*sidebar-item-section[^"]*"[^>]*>',
      prefix,
      perl = TRUE
    )[[1L]]
    section_positions <- section_positions[section_positions != -1L]
    if (length(section_positions) == 0L) {
      return(NULL)
    }
    block_start <- tail(section_positions, 1L)

    # Starting only at that selected <li>, balance nested <li>/</li> tags.
    remainder <- substr(contents, block_start, nchar(contents))
    li_positions <- gregexpr("</?li\\b[^>]*>", remainder, perl = TRUE)[[1L]]
    if (li_positions[1L] == -1L) {
      return(NULL)
    }
    li_lengths <- attr(li_positions, "match.length")
    li_tags <- regmatches(remainder, list(li_positions))[[1L]]

    depth <- 0L
    block_end <- NA_integer_
    for (i in seq_along(li_tags)) {
      if (grepl("^</li", li_tags[i], ignore.case = TRUE)) {
        depth <- depth - 1L
      } else {
        depth <- depth + 1L
      }
      if (depth == 0L) {
        end_in_remainder <- li_positions[i] + li_lengths[i] - 1L
        block_end <- block_start + end_in_remainder - 1L
        break
      }
    }
    if (is.na(block_end)) {
      return(NULL)
    }

    block <- substr(contents, block_start, block_end)
    if (!grepl(">\\s*Year report\\s*<", block, perl = TRUE)) {
      return(NULL)
    }

    list(start = block_start, end = block_end, block = block)
  }

  replace_range <- function(contents, start, end, replacement) {
    before <- if (start > 1L) substr(contents, 1L, start - 1L) else ""
    after <- if (end < nchar(contents)) {
      substr(contents, end + 1L, nchar(contents))
    } else {
      ""
    }
    paste0(before, trimws(replacement), after)
  }

  replacement_example <- normalize_to_lf(
    read_binary_text(replacement_example_file)
  )
  template_state <- extract_template_state(replacement_example)
  replacement_template <- remove_active_highlight(replacement_example)

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
    location <- locate_year_report_block(contents)
    if (is.null(location)) {
      message("Skipped (Year report boundaries not found): ", path)
      skipped_files <- c(skipped_files, path)
      next
    }

    state <- tryCatch(
      extract_old_state(location$block),
      error = function(e) NA_character_
    )
    active_item <- tryCatch(
      extract_active_item(location$block),
      error = function(e) NA_character_
    )
    if (is.na(state) || is.na(active_item)) {
      message(
        "Skipped (state or active item not found): ",
        path,
        " [state=",
        state,
        ", active=",
        active_item,
        "]"
      )
      skipped_files <- c(skipped_files, path)
      next
    }

    replacement <- replace_state_slug(
      replacement_template,
      template_state,
      state
    )
    nested_item_is_active <- active_item_is_nested(replacement, active_item)
    replacement <- add_active_highlight(replacement, active_item)
    replacement <- set_nested_rp3_state(replacement, nested_item_is_active)
    replacement <- use_target_newlines(
      replacement,
      grepl("\r\n", contents, fixed = TRUE)
    )

    message(
      if (dry_run) "Would update: " else "Updating: ",
      path,
      " [state=",
      state,
      ", active=",
      active_item,
      ", characters=",
      location$end - location$start + 1L,
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
      updated <- replace_range(
        contents,
        location$start,
        location$end,
        replacement
      )
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

replace_year_report_blocks(
  folder = folder,
  replacement_example_file = replacement_example_file,
  recursive = recursive,
  make_backups = make_backups,
  dry_run = dry_run
)
