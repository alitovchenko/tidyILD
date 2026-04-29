#' Convert strict wide ILD data to long form for ild_prepare (internal)
#' @noRd
ild_prepare_wide_to_long <- function(data,
                                     id,
                                     time_col,
                                     wide_cols = NULL,
                                     wide_names_pattern = "^(.+)_t(.+)$",
                                     wide_time_parser = c("numeric", "date", "datetime"),
                                     wide_time_format = NULL,
                                     wide_keep_cols = NULL) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.character(id) || length(id) < 1L || !nzchar(id[[1]])) {
    stop("'id' must be a non-empty character column name.", call. = FALSE)
  }
  id <- id[[1]]
  if (!id %in% names(data)) {
    stop("Column '", id, "' not found in data.", call. = FALSE)
  }
  if (!is.character(time_col) || length(time_col) < 1L || !nzchar(time_col[[1]])) {
    stop("'time' must be a non-empty character column name when input_format = 'wide'.", call. = FALSE)
  }
  time_col <- time_col[[1]]

  wide_time_parser <- match.arg(wide_time_parser)
  keep_cols <- unique(c(id, as.character(if (is.null(wide_keep_cols)) character() else wide_keep_cols)))
  unknown_keep <- setdiff(keep_cols, names(data))
  if (length(unknown_keep) > 0L) {
    stop(
      "wide_keep_cols contains unknown columns: ",
      paste(unknown_keep, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (is.null(wide_cols)) {
    candidate_cols <- setdiff(names(data), keep_cols)
    matched <- candidate_cols[grepl(wide_names_pattern, candidate_cols, perl = TRUE)]
    unmatched <- setdiff(candidate_cols, matched)
    if (length(unmatched) > 0L) {
      preview <- paste(utils::head(unmatched, 5L), collapse = ", ")
      stop(
        "When wide_cols is NULL, all non-id/non-keep columns must match wide_names_pattern. ",
        "Unmatched columns: ", preview,
        if (length(unmatched) > 5L) ", ..." else "",
        ". Add baseline columns to wide_keep_cols or specify wide_cols explicitly.",
        call. = FALSE
      )
    }
    wide_cols <- matched
  } else {
    wide_cols <- unique(as.character(wide_cols))
    unknown_wide <- setdiff(wide_cols, names(data))
    if (length(unknown_wide) > 0L) {
      stop(
        "wide_cols contains unknown columns: ",
        paste(unknown_wide, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  if (length(wide_cols) == 0L) {
    stop("No wide measurement columns found. Provide wide_cols or adjust wide_names_pattern.", call. = FALSE)
  }
  if (id %in% wide_cols) {
    stop("id column cannot be included in wide_cols.", call. = FALSE)
  }
  if (time_col %in% names(data) && !time_col %in% wide_cols) {
    stop(
      "Requested time column '", time_col, "' already exists in wide input. ",
      "Choose a new time name for the long result.",
      call. = FALSE
    )
  }

  parsed <- lapply(wide_cols, function(nm) {
    m <- regexec(wide_names_pattern, nm, perl = TRUE)
    reg <- regmatches(nm, m)[[1]]
    if (length(reg) != 3L) {
      return(NULL)
    }
    list(col = nm, measure = reg[[2]], time_token = reg[[3]])
  })
  ok <- !vapply(parsed, is.null, logical(1))
  if (!all(ok)) {
    bad <- wide_cols[!ok]
    stop(
      "The following wide columns do not match wide_names_pattern with exactly two capture groups (measure, time): ",
      paste(utils::head(bad, 10L), collapse = ", "),
      if (length(bad) > 10L) ", ..." else "",
      ".",
      call. = FALSE
    )
  }

  map_df <- tibble::as_tibble(do.call(rbind, lapply(parsed, as.data.frame, stringsAsFactors = FALSE)))
  names(map_df) <- c("col", "measure", "time_token")

  if (any(!nzchar(map_df$measure)) || any(!nzchar(map_df$time_token))) {
    stop("wide_names_pattern produced empty measure or time tokens; adjust the pattern.", call. = FALSE)
  }

  dup_pairs <- duplicated(map_df[c("measure", "time_token")]) | duplicated(map_df[c("measure", "time_token")], fromLast = TRUE)
  if (any(dup_pairs)) {
    bad <- unique(map_df[dup_pairs, c("measure", "time_token")])
    pairs <- paste0(bad$measure, "@", bad$time_token)
    stop(
      "Duplicate measure-time slots detected in wide columns: ",
      paste(pairs, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  parsed_time <- ild_parse_wide_time_tokens(
    tokens = map_df$time_token,
    parser = wide_time_parser,
    fmt = wide_time_format
  )
  map_df$parsed_time <- parsed_time

  uniq_keys <- unique(data[keep_cols])
  if (nrow(uniq_keys) != nrow(data)) {
    stop(
      "Wide input must be unique by id + wide_keep_cols before conversion. Found repeated rows for these keys.",
      call. = FALSE
    )
  }

  measures <- unique(map_df$measure)
  time_levels <- unique(map_df$time_token)
  rows <- vector("list", length(time_levels))

  for (i in seq_along(time_levels)) {
    token <- time_levels[[i]]
    token_map <- map_df[map_df$time_token == token, ]
    chunk <- data[keep_cols]
    chunk[[time_col]] <- token_map$parsed_time[[1]]
    for (measure in measures) {
      col_nm <- token_map$col[token_map$measure == measure]
      if (length(col_nm) == 1L) {
        chunk[[measure]] <- data[[col_nm]]
      } else if (length(col_nm) == 0L) {
        chunk[[measure]] <- NA
      } else {
        stop("Internal error: multiple columns map to ", measure, " at time token ", token, ".", call. = FALSE)
      }
    }
    rows[[i]] <- chunk
  }

  out <- dplyr::bind_rows(rows)
  if (time_col %in% measures || time_col %in% setdiff(keep_cols, id)) {
    stop("Output time column conflicts with keep or measure columns.", call. = FALSE)
  }
  out <- out[c(keep_cols, time_col, measures)]
  out <- tibble::as_tibble(out)

  list(
    data = out,
    meta = list(
      source = "wide",
      n_wide_cols = length(wide_cols),
      n_measures = length(measures),
      n_time = length(time_levels),
      wide_time_parser = wide_time_parser
    )
  )
}

#' Parse wide time tokens into a comparable vector (internal)
#' @noRd
ild_parse_wide_time_tokens <- function(tokens, parser, fmt = NULL) {
  u <- unique(as.character(tokens))
  if (parser == "numeric") {
    vals <- suppressWarnings(as.numeric(u))
    if (anyNA(vals)) {
      bad <- u[is.na(vals)]
      stop(
        "Could not parse some wide time tokens as numeric: ",
        paste(utils::head(bad, 10L), collapse = ", "),
        if (length(bad) > 10L) ", ..." else "",
        ".",
        call. = FALSE
      )
    }
    return(vals[match(as.character(tokens), u)])
  }
  if (parser == "date") {
    vals <- as.Date(u, format = fmt)
    if (anyNA(vals)) {
      bad <- u[is.na(vals)]
      stop(
        "Could not parse some wide time tokens as Date",
        if (!is.null(fmt)) paste0(" with format '", fmt, "'") else "",
        ": ",
        paste(utils::head(bad, 10L), collapse = ", "),
        if (length(bad) > 10L) ", ..." else "",
        ".",
        call. = FALSE
      )
    }
    return(vals[match(as.character(tokens), u)])
  }
  if (parser == "datetime") {
    vals <- as.POSIXct(u, format = fmt, tz = "UTC")
    if (anyNA(vals)) {
      bad <- u[is.na(vals)]
      stop(
        "Could not parse some wide time tokens as POSIXct",
        if (!is.null(fmt)) paste0(" with format '", fmt, "'") else "",
        ": ",
        paste(utils::head(bad, 10L), collapse = ", "),
        if (length(bad) > 10L) ", ..." else "",
        ".",
        call. = FALSE
      )
    }
    return(vals[match(as.character(tokens), u)])
  }
  stop("Unsupported wide_time_parser value.", call. = FALSE)
}
