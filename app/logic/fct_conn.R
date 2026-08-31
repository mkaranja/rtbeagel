box::use(
  pool[dbPool, poolCheckout, poolReturn, poolWithTransaction],
  RPostgres[Postgres],
  DBI[dbGetQuery, dbExecute, dbAppendTable, dbBegin, dbCommit, dbRollback],
  glue[glue_sql],
)

# ============================================================================
# DATABASE CONNECTION
# ----------------------------------------------------------------------------

#' Build the pool. Credentials come from the environment:
#'   PGHOST PGPORT PGDATABASE PGUSER PGPASSWORD
#' @export
create_pool_safely <- function(max_retries = 3, delay = 5) {
  need <- c("PGHOST", "PGDATABASE", "PGUSER", "PGPASSWORD")
  missing <- need[!nzchar(Sys.getenv(need))]
  if (length(missing)) {
    stop("Database credentials missing from the environment: ",
         paste(missing, collapse = ", "),
         ". Set them in .Renviron - do not hardcode them.", call. = FALSE)
  }
  
  for (i in seq_len(max_retries)) {
    p <- tryCatch(
      dbPool(
        drv      = Postgres(),
        dbname   = Sys.getenv("PGDATABASE"),
        host     = Sys.getenv("PGHOST"),
        port     = as.integer(Sys.getenv("PGPORT", "5432")),
        user     = Sys.getenv("PGUSER"),
        password = Sys.getenv("PGPASSWORD"),
        minSize  = 1,
        maxSize  = 10,
        idleTimeout = 60000,
        options  = "-c statement_timeout=30000"
      ),
      error = function(e) { message("DB connect attempt ", i, " failed: ", conditionMessage(e)); NULL }
    )
    if (!is.null(p)) return(p)
    Sys.sleep(delay)
  }
  stop("All retries exhausted. Could not connect to the database.", call. = FALSE)
}

#' @export
pool <- create_pool_safely()


# ---------------------------------------------------------------------------
# SHINY CONTROL FLOW IS NOT AN ERROR
# ---------------------------------------------------------------------------
# req() and validate() do not return a value. They THROW. The condition they
# throw is an ordinary R error as far as tryCatch is concerned, so any
# tryCatch(error = ) between the req() and Shiny's own handler will eat it and
# the reactive will carry on with garbage instead of stopping.
#
# That is a real bug we shipped, and it looked like this:
#
#   order_summary(code())                  code() is a PROMISE, not a value
#     -> load_data(pool, sql, params = list(order_number))
#          `params` is ALSO a promise, and R does not force it until it is
#          first USED - which was here:
#            dbGetQuery(pool, query, params = params)   <- INSIDE the tryCatch
#     -> forcing it ran code() -> req() -> shiny.silent.error
#     -> tryCatch(error = ) caught it, warned, returned data.frame()
#
# The symptom was pathognomonic and we should have read it sooner: EVERY query
# taking `params` failed with an EMPTY message, and every query without params
# worked perfectly. The empty message was req()'s - it has no message, because
# it was never meant to be reported. Lazy evaluation had moved the promise's
# evaluation from the call site into the callee's tryCatch.
#
# Two defences, because either alone is thin:
#   1. force() every argument BEFORE the tryCatch opens. This moves promise
#      evaluation back out to where the caller expects it. It is the fix.
#   2. Rethrow anything wearing a Shiny control-flow class. Belt and braces
#      for conditions raised from somewhere we did not anticipate.
#
# Consequence, and it is a wanted one: an error while BUILDING a query (a
# sprintf() over a NULL, say) now propagates instead of being swallowed into a
# zero-row frame. A malformed query is a bug in our code, not a data
# condition, and it should be loud.
# ---------------------------------------------------------------------------

# req() throws shiny.silent.error; validate() throws shiny.custom.error /
# "validation", which also inherits shiny.silent.error. Listing all three
# means we do not depend on that inheritance staying put.
SHINY_CONTROL_FLOW <- c("shiny.silent.error", "shiny.custom.error", "validation")

is_control_flow <- function(e) any(class(e) %in% SHINY_CONTROL_FLOW)


#' Run a SELECT and ALWAYS get a data frame back.
#'
#' On a database failure this returns a ZERO-ROW data frame and warns. It
#' never returns a string and it never returns NULL - so `nrow(x)` behaves for
#' every caller even when the query blew up.
#'
#' NOTE ON THE SHAPE: the frame returned on failure has zero rows AND ZERO
#' COLUMNS. `nrow()` is safe on it; `names()` is not meaningful. Callers that
#' hand the result to reactable(), or to anything else that resolves columns
#' by name, must shape it - see shape_frame() in fct_tracking.R and
#' admin_list() in fct_admin.R. This function cannot do that shaping itself:
#' it does not know what the caller asked for.
#'
#' @param pool   pool or connection
#' @param query  SQL string
#' @param params optional list for $1, $2 placeholders - USE THIS rather than
#'               pasting values into the SQL
#' @export
load_data <- function(pool, query, params = NULL) {
  # ---- forcing, before the tryCatch opens. See the note above. ----
  # These three lines are the fix for the empty `load_data failed:` warning.
  # Do not move them inside the tryCatch, and do not delete them because they
  # "do nothing" - they do nothing only when nothing is a promise.
  force(pool)
  force(query)
  force(params)
  
  out <- tryCatch({
    if (is.null(params)) dbGetQuery(pool, query) else dbGetQuery(pool, query, params = params)
  }, error = function(e) {
    if (is_control_flow(e)) stop(e)   # Shiny's, not ours. Hand it back.
    warning("load_data failed: ", conditionMessage(e),
            "\n  query: ", substr(gsub("\\s+", " ", query), 1, 160), call. = FALSE)
    NULL
  })
  
  if (is.null(out) || !is.data.frame(out)) return(data.frame())
  out
}


# ---------------------------------------------------------------------------
# THE ZERO-COLUMN FRAME, AND WHY IT NEEDS SHAPING
# ---------------------------------------------------------------------------
# load_data() promises "always a data frame". It keeps that promise with
# data.frame(), which is 0 rows and 0 COLUMNS. nrow() works. names() returns
# nothing.
#
# That is fine for a caller asking `if (nrow(d) == 0)`. It is not fine for
# reactable(), which resolves its `columns = list(...)` against names(data)
# and refuses a frame with no columns outright:
#     "columns names must exist in data"
#     "data must have at least one column"
#
# And this is NOT an edge case. On a fresh KEPHIS database, empty IS the
# normal state - the same fact that made `d$x <- NA` bite us on a zero-row
# frame. A LIMS that cannot render its own empty tables cannot be used to
# enter the first order.
#
# So: give the caller back a frame with the columns it asked for and no rows.
# "No services yet" and "the services query blew up" then BOTH arrive as
# something the UI can draw - and the second one still warns, so it does not
# hide.
# ---------------------------------------------------------------------------

#' Guarantee a frame has the expected columns, with the expected types.
#'
#' @param d     whatever load_data() returned
#' @param proto named list of ZERO-LENGTH prototype vectors, e.g.
#'              list(order_number = character(), target_qty = integer())
#'              The names are the contract; the types keep colDef() cell
#'              functions and format() honest on empty input.
#' @param what  label for the warning, e.g. "order_services()"
#' @return d unchanged if it already conforms, otherwise a 0-row frame
#' @export
shape_frame <- function(d, proto, what = "query") {
  expected <- names(proto)
  
  if (is.data.frame(d) && ncol(d) > 0L && all(expected %in% names(d))) return(d)
  
  # ncol > 0 but columns missing means the QUERY and this prototype disagree -
  # a view was altered and this function was not. That is schema drift and it
  # must be noisy, not papered over.
  if (is.data.frame(d) && ncol(d) > 0L) {
    warning(what, ": expected columns missing from the result (",
            paste(setdiff(expected, names(d)), collapse = ", "),
            "). Schema drift - the query and its prototype disagree.",
            call. = FALSE)
  }
  
  as.data.frame(proto, stringsAsFactors = FALSE)
}


#' As load_data(), but raises instead of swallowing.
#' Use inside transactions, where continuing on a failed read is never right.
#' @export
load_data_strict <- function(pool, query, params = NULL) {
  force(pool); force(query); force(params)
  if (is.null(params)) dbGetQuery(pool, query) else dbGetQuery(pool, query, params = params)
}


#' Run a list of writes/statements in one transaction.
#'
#' @param pool       the pool
#' @param operations list of list(type = "write"|"exec", table=, data=, sql=, params=)
#' @param transaction wrap in BEGIN/COMMIT (default TRUE)
#' @export
safe_db_operation <- function(pool, operations, transaction = TRUE) {
  force(pool)
  # BEFORE dbBegin, deliberately. `operations` is usually built at the call
  # site out of input$... and reactives, so forcing it can trigger req(). If
  # that happened after dbBegin we would have opened a transaction we then
  # abandon; forcing here means the promise resolves while we still own
  # nothing.
  force(operations)
  force(transaction)
  
  conn <- poolCheckout(pool)
  on.exit(poolReturn(conn), add = TRUE)
  
  if (transaction) dbBegin(conn)
  tryCatch({
    for (op in operations) {
      if (identical(op$type, "write")) {
        # dbAppendTable, not dbWriteTable: it matches on column NAME and will
        # not silently invent or reorder columns.
        dbAppendTable(conn, op$table, op$data)
      } else if (identical(op$type, "exec") || identical(op$type, "query")) {
        if (is.null(op$params)) dbExecute(conn, op$sql) else dbExecute(conn, op$sql, params = op$params)
      } else {
        stop("Unknown operation type: ", op$type)
      }
    }
    if (transaction) dbCommit(conn)
    invisible(TRUE)
  }, error = function(e) {
    if (transaction) try(dbRollback(conn), silent = TRUE)
    # Rolled back either way - but a req() must not be reported to the user as
    # a database failure. It is not one.
    if (is_control_flow(e)) stop(e)
    stop("Database operation failed: ", conditionMessage(e), call. = FALSE)
  })
}


#' Is this code already taken, as an order or a sample?
#' @export
is_code_registered <- function(pool, code) {
  if (is.null(code) || !nzchar(code)) return(FALSE)
  res <- load_data(pool, "
    SELECT COUNT(*) AS total FROM (
      SELECT order_number AS id FROM tbl_order  WHERE order_number = $1
      UNION ALL
      SELECT sample_code  AS id FROM tbl_sample WHERE sample_code  = $1
    ) c", params = list(code))
  if (nrow(res) == 0) return(FALSE)
  as.numeric(res$total[1]) > 0
}


#' Find an order or a sample by code. Replaces global_search(), which queried
#' two tables that do not exist.
#' @export
find_by_code <- function(pool, code) {
  if (is.null(code) || !nzchar(code)) return(list(data = NULL, type = NA_character_, found = FALSE))
  
  o <- load_data(pool, "SELECT * FROM view_order_progress WHERE order_number = $1",
                 params = list(code))
  if (nrow(o) > 0) return(list(data = o, type = "order", found = TRUE))
  
  s <- load_data(pool, "SELECT * FROM view_stage_queue WHERE sample_code = $1",
                 params = list(code))
  if (nrow(s) > 0) return(list(data = s, type = "sample", found = TRUE))
  
  list(data = NULL, type = NA_character_, found = FALSE)
}


#' Laboratories, for pickers.
#' @export
get_labs <- function(pool) {
  load_data(pool, "SELECT laboratory_id, laboratory_name FROM tbl_laboratory WHERE active ORDER BY laboratory_name")
}

#' box::use(
#'   pool[dbPool, poolCheckout, poolReturn, poolWithTransaction],
#'   RPostgres[Postgres],
#'   DBI[dbGetQuery, dbExecute, dbAppendTable, dbBegin, dbCommit, dbRollback],
#'   glue[glue_sql],
#' )
#' 
#' # ============================================================================
#' # DATABASE CONNECTION
#' # ----------------------------------------------------------------------------
#' 
#' #' Build the pool. Credentials come from the environment:
#' #'   PGHOST PGPORT PGDATABASE PGUSER PGPASSWORD
#' #' @export
#' create_pool_safely <- function(max_retries = 3, delay = 5) {
#'   need <- c("PGHOST", "PGDATABASE", "PGUSER", "PGPASSWORD")
#'   missing <- need[!nzchar(Sys.getenv(need))]
#'   if (length(missing)) {
#'     stop("Database credentials missing from the environment: ",
#'          paste(missing, collapse = ", "),
#'          ". Set them in .Renviron - do not hardcode them.", call. = FALSE)
#'   }
#' 
#'   for (i in seq_len(max_retries)) {
#'     p <- tryCatch(
#'       dbPool(
#'         drv      = Postgres(),
#'         dbname   = Sys.getenv("PGDATABASE"),
#'         host     = Sys.getenv("PGHOST"),
#'         port     = as.integer(Sys.getenv("PGPORT", "5432")),
#'         user     = Sys.getenv("PGUSER"),
#'         password = Sys.getenv("PGPASSWORD"),
#'         minSize  = 1,
#'         maxSize  = 10,
#'         idleTimeout = 60000,
#'         options  = "-c statement_timeout=30000"
#'       ),
#'       error = function(e) { message("DB connect attempt ", i, " failed: ", conditionMessage(e)); NULL }
#'     )
#'     if (!is.null(p)) return(p)
#'     Sys.sleep(delay)
#'   }
#'   stop("All retries exhausted. Could not connect to the database.", call. = FALSE)
#' }
#' 
#' #' @export
#' pool <- create_pool_safely()
#' 
#' 
#' #' Run a SELECT and ALWAYS get a data frame back.
#' #'
#' #' On failure this returns a ZERO-ROW data frame and warns. It never returns
#' #' a string, and it never returns NULL - so `nrow(x)` and `x$col` behave for
#' #' every caller even when the query blew up.
#' #'
#' #' @param pool   pool or connection
#' #' @param query  SQL string
#' #' @param params optional list for $1, $2 placeholders - USE THIS rather than
#' #'               pasting values into the SQL
#' #' @export
#' load_data <- function(pool, query, params = NULL) {
#'   out <- tryCatch({
#'     if (is.null(params)) dbGetQuery(pool, query) else dbGetQuery(pool, query, params = params)
#'   }, error = function(e) {
#'     warning("load_data failed: ", conditionMessage(e),
#'             "\n  query: ", substr(gsub("\\s+", " ", query), 1, 160), call. = FALSE)
#'     NULL
#'   })
#' 
#'   if (is.null(out) || !is.data.frame(out)) return(data.frame())
#'   out
#' }
#' 
#' 
#' #' As load_data(), but raises instead of swallowing.
#' #' Use inside transactions, where continuing on a failed read is never right.
#' #' @export
#' load_data_strict <- function(pool, query, params = NULL) {
#'   if (is.null(params)) dbGetQuery(pool, query) else dbGetQuery(pool, query, params = params)
#' }
#' 
#' 
#' #' Run a list of writes/statements in one transaction.
#' #'
#' #' @param pool       the pool
#' #' @param operations list of list(type = "write"|"exec", table=, data=, sql=, params=)
#' #' @param transaction wrap in BEGIN/COMMIT (default TRUE)
#' #' @export
#' safe_db_operation <- function(pool, operations, transaction = TRUE) {
#'   conn <- poolCheckout(pool)
#'   on.exit(poolReturn(conn), add = TRUE)
#' 
#'   if (transaction) dbBegin(conn)
#'   tryCatch({
#'     for (op in operations) {
#'       if (identical(op$type, "write")) {
#'         # dbAppendTable, not dbWriteTable: it matches on column NAME and will
#'         # not silently invent or reorder columns.
#'         dbAppendTable(conn, op$table, op$data)
#'       } else if (identical(op$type, "exec") || identical(op$type, "query")) {
#'         if (is.null(op$params)) dbExecute(conn, op$sql) else dbExecute(conn, op$sql, params = op$params)
#'       } else {
#'         stop("Unknown operation type: ", op$type)
#'       }
#'     }
#'     if (transaction) dbCommit(conn)
#'     invisible(TRUE)
#'   }, error = function(e) {
#'     if (transaction) try(dbRollback(conn), silent = TRUE)
#'     stop("Database operation failed: ", conditionMessage(e), call. = FALSE)
#'   })
#' }
#' 
#' 
#' #' Is this code already taken, as an order or a sample?
#' #' @export
#' is_code_registered <- function(pool, code) {
#'   if (is.null(code) || !nzchar(code)) return(FALSE)
#'   res <- load_data(pool, "
#'     SELECT COUNT(*) AS total FROM (
#'       SELECT order_number AS id FROM tbl_order  WHERE order_number = $1
#'       UNION ALL
#'       SELECT sample_code  AS id FROM tbl_sample WHERE sample_code  = $1
#'     ) c", params = list(code))
#'   if (nrow(res) == 0) return(FALSE)
#'   as.numeric(res$total[1]) > 0
#' }
#' 
#' 
#' #' Find an order or a sample by code. Replaces global_search(), which queried
#' #' two tables that do not exist.
#' #' @export
#' find_by_code <- function(pool, code) {
#'   if (is.null(code) || !nzchar(code)) return(list(data = NULL, type = NA_character_, found = FALSE))
#' 
#'   o <- load_data(pool, "SELECT * FROM view_order_progress WHERE order_number = $1",
#'                  params = list(code))
#'   if (nrow(o) > 0) return(list(data = o, type = "order", found = TRUE))
#' 
#'   s <- load_data(pool, "SELECT * FROM view_stage_queue WHERE sample_code = $1",
#'                  params = list(code))
#'   if (nrow(s) > 0) return(list(data = s, type = "sample", found = TRUE))
#' 
#'   list(data = NULL, type = NA_character_, found = FALSE)
#' }
#' 
#' 
#' #' Laboratories, for pickers.
#' #' @export
#' get_labs <- function(pool) {
#'   load_data(pool, "SELECT laboratory_id, laboratory_name FROM tbl_laboratory WHERE active ORDER BY laboratory_name")
#' }
