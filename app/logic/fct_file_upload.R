box::use(
  DBI[dbGetQuery],
  tools[file_ext],
)

# ============================================================================
# FILE UPLOAD
# ----------------------------------------------------------------------------
# The old file carried FOUR versions of this function - file_upload,
# file_upload_function, file_upload_function0, file_upload_function2 - of which
# one was live and three were abandoned drafts. One of the dead ones contained
# Python syntax that is not valid R:
#     .options = list("positionClass": "toast-top-center",)
# It survived only because nothing called it. There is one function here.
#
# THE SECURITY BUG: the live file_upload() took allowed_types and max_size and
# then IGNORED BOTH. Line 50 of the original read:
#     # --- File Validation (omitted for brevity, but it's in the full function) ---
# There was no full function. Any file of any type and any size was copied
# straight to disk. Validation now actually happens, and it happens BEFORE
# anything touches the filesystem.
#
# THE ORDERING PROBLEM: filesystems are not transactional. The old code copied
# the file first, then inserted the row. If a LATER statement in the caller's
# transaction failed, the database rolled back and the file stayed on disk -
# and worse, the reverse ordering means a committed row can point at a file
# that was never written.
#
# So: INSERT THE ROW FIRST, then write the bytes. If the copy fails we raise,
# and the caller's transaction takes the row back out with it. The worst case
# becomes an orphaned file with no row pointing at it - harmless bytes - rather
# than a row pointing at a file that is not there, which breaks every download.
# ============================================================================

MIME_BY_EXT <- c(
  pdf = "application/pdf",
  doc = "application/msword",
  docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  txt = "text/plain", csv = "text/csv",
  png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif"
)

#' Validate and store an uploaded file, recording it in tbl_file.
#'
#' @param file_input   the shiny fileInput data frame (name/size/type/datapath)
#' @param description  free text
#' @param allowed_ext  permitted extensions, without dots
#' @param max_size_mb  per-file ceiling
#' @param order_number attach to an order (mutually exclusive with sample_code)
#' @param sample_code  attach to a sample
#' @param user         username; must exist in tbl_app_user
#' @param conn         a CONNECTION inside the caller's transaction - not the
#'                     pool. Passing the pool would put the row outside the
#'                     transaction and it would survive a rollback.
#' @param upload_dir   storage root
#' @return data frame of stored files (file_id, file_name, stored_path)
#' @export
file_upload <- function(
    file_input   = NULL,
    description  = NULL,
    allowed_ext  = c("pdf", "doc", "docx", "txt", "csv", "png", "jpg", "jpeg", "gif"),
    max_size_mb  = 10,
    order_number = NULL,
    sample_code  = NULL,
    user         = NULL,
    conn         = NULL,
    upload_dir   = file.path("app", "uploaded_files")
) {
  if (is.null(file_input) || !is.data.frame(file_input) || nrow(file_input) == 0) {
    return(empty_result())
  }
  if (is.null(conn)) stop("file_upload() needs a live connection.", call. = FALSE)
  if (is.null(order_number) && is.null(sample_code)) {
    stop("file_upload() needs either an order_number or a sample_code.", call. = FALSE)
  }
  if (!is.null(order_number) && !is.null(sample_code)) {
    stop("file_upload() takes an order_number OR a sample_code, not both.", call. = FALSE)
  }

  if (!dir.exists(upload_dir)) dir.create(upload_dir, recursive = TRUE)
  stored <- empty_result()

  for (i in seq_len(nrow(file_input))) {
    name <- file_input$name[i]
    size <- as.numeric(file_input$size[i])
    src  <- file_input$datapath[i]
    ext  <- tolower(file_ext(name))

    # ---- validation, BEFORE anything is written -------------------
    if (!nzchar(ext) || !(ext %in% tolower(allowed_ext))) {
      stop(sprintf("'%s' has an unsupported type (.%s). Allowed: %s",
                   name, ext, paste(allowed_ext, collapse = ", ")), call. = FALSE)
    }
    if (size > max_size_mb * 1024^2) {
      stop(sprintf("'%s' is %.1f MB; the limit is %d MB.",
                   name, size / 1024^2, max_size_mb), call. = FALSE)
    }
    if (size <= 0) stop(sprintf("'%s' is empty.", name), call. = FALSE)
    if (!file.exists(src)) stop(sprintf("Uploaded temp file for '%s' is missing.", name), call. = FALSE)

    mime <- unname(MIME_BY_EXT[ext])
    if (is.na(mime)) mime <- "application/octet-stream"

    safe_name <- gsub("[^A-Za-z0-9._-]", "_", basename(name))
    subject   <- if (is.null(order_number)) sample_code else order_number
    unique_nm <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), "_", subject, "_", safe_name)
    dest      <- file.path(upload_dir, unique_nm)

    # ---- 1. the row, inside the caller's transaction ---------------
    row <- dbGetQuery(conn, "
      INSERT INTO tbl_file
        (order_number, sample_code, file_name, stored_path, mime_type,
         size_bytes, description, uploaded_by)
      VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
      RETURNING file_id",
      params = list(
        order_number %||% NA, sample_code %||% NA,
        name, dest, mime,
        as.integer(size),            # BYTES. The old code stored rounded KB.
        description %||% NA, user
      ))

    # ---- 2. the bytes. If this fails we raise, and the row goes with
    #         the transaction.
    ok <- file.copy(src, dest, overwrite = FALSE)
    if (!isTRUE(ok)) {
      stop(sprintf("Could not write '%s' to %s.", name, dest), call. = FALSE)
    }

    stored <- rbind(stored, data.frame(
      file_id     = row$file_id[1],
      file_name   = name,
      stored_path = dest,
      stringsAsFactors = FALSE
    ))
  }

  stored
}

empty_result <- function() {
  data.frame(file_id = integer(0), file_name = character(0),
             stored_path = character(0), stringsAsFactors = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
