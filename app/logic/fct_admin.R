box::use(
  DBI[dbGetQuery, dbExecute],
)

box::use(
  app/logic/fct_conn[pool, load_data],
)

# ============================================================================
# ADMIN REGISTRY
# ----------------------------------------------------------------------------
# The old app had a PAIR of modules per reference table - add_customer.R +
# customer.R, add_laboratory.R + laboratory.R, and so on. Thirteen files,
# 2191 lines, all doing the same three things: list rows, add a row, edit a
# row. add_laboratory took (name, description); add_sample_part took
# (part_name, description). Same module, different labels.
#
# So: one module (admin_crud.R), driven by the config below. Adding a new
# reference table is an entry in this list, not a new pair of files.
#
# TWO RULES THIS ENCODES, both of which matter:
#
#  1. NOTHING IS EVER DELETED. Every reference table has `active`, and the
#     UI deactivates rather than deletes. A customer with twenty years of
#     orders cannot be removed - the FK would refuse - and even if it could,
#     removing it would orphan the history. Deactivating hides it from
#     pickers while leaving every past order readable.
#
#  2. TEXT CODES ARE IMMUTABLE ONCE CREATED. sample_type_code, stage codes
#     and service codes are referenced BY NAME from the workflow YAML
#     (cassava.yaml contains `when: { sample_type: [cutting] }`). Renaming
#     'cutting' would silently break the branch - it would just stop
#     matching. So `code` fields can be set at creation and never edited.
#     Labels stay editable, because nothing branches on a label.
# ============================================================================

# Field types:
#   text | textarea | number | select | fk | code
#     code = text primary key, editable ONLY at creation (see rule 2)
#     fk   = choices come from another table

#' @export
ADMIN <- list(
  
  customer = list(
    table = "tbl_customer", pk = "customer_id",
    title = "Customers", singular = "customer",
    order_by = "customer_name",
    display = c("customer_name", "customer_type", "customer_category", "email", "phone"),
    fields = list(
      list(name = "customer_name",     label = "Customer name", type = "text",     required = TRUE),
      list(name = "customer_type",     label = "Type",          type = "select",   required = TRUE,
           choices = c("Institution", "Company", "Government", "Individual", "NGO")),
      list(name = "customer_category", label = "Category",      type = "select",   required = TRUE,
           choices = c("Research", "Commercial", "Regulatory", "Internal")),
      list(name = "group_name",        label = "Group",         type = "text"),
      list(name = "email",             label = "Email",         type = "text"),
      list(name = "phone",             label = "Phone",         type = "text"),
      list(name = "address",           label = "Address",       type = "textarea"),
      list(name = "alt_address",       label = "Alt. address",  type = "textarea"),
      list(name = "description",       label = "Notes",         type = "textarea")
    )
  ),
  
  crop = list(
    table = "tbl_crop", pk = "crop_id",
    title = "Crops", singular = "crop",
    order_by = "crop_name",
    display = c("crop_name", "scientific_name", "family"),
    fields = list(
      list(name = "crop_name",       label = "Crop name",       type = "text", required = TRUE),
      list(name = "scientific_name", label = "Scientific name", type = "text"),
      list(name = "family",          label = "Family",          type = "text")
    )
  ),
  
  variety = list(
    table = "tbl_variety", pk = "variety_id",
    title = "Varieties", singular = "variety",
    order_by = "variety_name",
    display = c("variety_name", "crop_id", "origin_country"),
    fields = list(
      list(name = "crop_id",        label = "Crop",           type = "fk", required = TRUE,
           fk_table = "tbl_crop", fk_id = "crop_id", fk_label = "crop_name"),
      list(name = "variety_name",   label = "Variety name",   type = "text", required = TRUE),
      list(name = "synonyms",       label = "Synonyms",       type = "text"),
      list(name = "origin_country", label = "Origin country", type = "text"),
      list(name = "description",    label = "Notes",          type = "textarea")
    )
  ),
  
  laboratory = list(
    table = "tbl_laboratory", pk = "laboratory_id",
    title = "Laboratories", singular = "laboratory",
    order_by = "laboratory_name",
    display = c("laboratory_name", "description"),
    fields = list(
      list(name = "laboratory_name", label = "Laboratory name", type = "text", required = TRUE),
      list(name = "description",     label = "Description",     type = "textarea")
    )
  ),
  
  pathogen = list(
    table = "tbl_pathogen", pk = "pathogen_id",
    title = "Pathogens", singular = "pathogen",
    order_by = "pathogen_name",
    display = c("pathogen_name", "crop_id", "synonyms"),
    fields = list(
      list(name = "crop_id",       label = "Crop",          type = "fk", required = TRUE,
           fk_table = "tbl_crop", fk_id = "crop_id", fk_label = "crop_name"),
      list(name = "pathogen_name", label = "Pathogen name", type = "text", required = TRUE),
      list(name = "synonyms",      label = "Synonyms",      type = "text")
    )
  ),
  
  test_method = list(
    table = "tbl_test_method", pk = "test_id",
    title = "Test methods", singular = "test method",
    order_by = "test_name",
    display = c("acronym", "test_name", "crop_id", "laboratory_id"),
    fields = list(
      list(name = "acronym",       label = "Acronym",    type = "text", required = TRUE),
      list(name = "test_name",     label = "Test name",  type = "text", required = TRUE),
      list(name = "crop_id",       label = "Crop",       type = "fk", required = TRUE,
           fk_table = "tbl_crop", fk_id = "crop_id", fk_label = "crop_name"),
      list(name = "laboratory_id", label = "Laboratory", type = "fk", required = TRUE,
           fk_table = "tbl_laboratory", fk_id = "laboratory_id", fk_label = "laboratory_name"),
      # optional: a test need not target a named pathogen
      list(name = "pathogen_id",   label = "Target pathogen", type = "fk",
           fk_table = "tbl_pathogen", fk_id = "pathogen_id", fk_label = "pathogen_name")
    )
  ),
  
  sample_part = list(
    table = "tbl_sample_part", pk = "part_id",
    title = "Sample parts", singular = "sample part",
    order_by = "part_name",
    display = c("part_name", "description"),
    fields = list(
      list(name = "part_name",   label = "Part name",   type = "text", required = TRUE),
      list(name = "description", label = "Description", type = "textarea")
    )
  ),
  
  sampling_bag = list(
    table = "tbl_sampling_bag", pk = "bag_id",
    title = "Sampling bags", singular = "sampling bag",
    order_by = "bag_name",
    display = c("bag_name", "description"),
    fields = list(
      list(name = "bag_name",    label = "Bag name",    type = "text", required = TRUE),
      list(name = "description", label = "Description", type = "textarea")
    )
  ),
  
  # ---- text-PK vocabularies. `code` fields are create-only (rule 2). ----
  
  sample_type = list(
    table = "tbl_sample_type", pk = "sample_type_code",
    title = "Sample types", singular = "sample type",
    order_by = "sort_order, label",
    display = c("sample_type_code", "label", "sort_order"),
    warn = paste("The workflow branches on these codes -", 
                 "cassava.yaml matches on sample_type 'cutting'.",
                 "New codes are safe; a code cannot be renamed once created."),
    fields = list(
      list(name = "sample_type_code", label = "Code", type = "code", required = TRUE,
           help = "lowercase, no spaces, e.g. cutting"),
      list(name = "label",      label = "Label",   type = "text",   required = TRUE),
      list(name = "sort_order", label = "Sort",    type = "number",  default = 0)
    )
  ),
  
  sample_condition = list(
    table = "tbl_sample_condition", pk = "condition_code",
    title = "Sample conditions", singular = "sample condition",
    order_by = "sort_order, label",
    display = c("condition_code", "label", "sort_order"),
    fields = list(
      list(name = "condition_code", label = "Code", type = "code", required = TRUE,
           help = "lowercase, no spaces, e.g. contaminated"),
      list(name = "label",      label = "Label", type = "text",   required = TRUE),
      list(name = "sort_order", label = "Sort",  type = "number", default = 0)
    )
  ),
  
  service_catalog = list(
    table = "tbl_service_catalog", pk = "service_code",
    title = "Services", singular = "service",
    order_by = "sort_order, service_label",
    display = c("service_code", "service_label", "service_kind", "unit", "weight"),
    warn = paste("Services drive what an order can request and how completion",
                 "is weighted. Adding one is safe. Deactivating hides it from",
                 "new orders without touching existing ones."),
    fields = list(
      list(name = "service_code",  label = "Code", type = "code", required = TRUE,
           help = "lowercase, no spaces, e.g. in_vitro_distribution"),
      list(name = "service_label", label = "Label", type = "text", required = TRUE),
      list(name = "service_kind",  label = "Kind",  type = "select", required = TRUE,
           choices = c("diagnostic", "fulfilment"),
           help = "diagnostic = produces a result; fulfilment = produces a quantity"),
      list(name = "unit",   label = "Unit",   type = "text",   required = TRUE, default = "plantlet"),
      list(name = "weight", label = "Weight", type = "number", default = 1,
           help = "relative weight in the order completion rollup"),
      list(name = "sort_order", label = "Sort", type = "number", default = 0)
    )
  )
)


# ---- generic data access ---------------------------------------------
# Column names come from the config above - never from user input - so they
# are safe to interpolate. Every VALUE goes through a bind parameter. The old
# insert_data() pasted values straight into the SQL, so any apostrophe in a
# customer name was an injection.

#' @export
admin_fields <- function(cfg) vapply(cfg$fields, function(f) f$name, character(1))

#' Always returns a frame with the EXPECTED COLUMNS, even when there are no
#' rows and even when the query failed.
#'
#' load_data() returns data.frame() on failure - zero rows AND zero columns.
#' A caller doing names(d) then sees nothing and builds a table with no
#' primary key in it, which fails later and somewhere else. Shaping the empty
#' frame here means "no rows yet" and "the query blew up" both arrive as
#' something the UI can render, and the second one warns.
#' @export
admin_list <- function(cfg, include_inactive = FALSE) {
  where <- if (include_inactive) "" else "WHERE active"
  d <- load_data(pool, sprintf("SELECT %s, %s, active FROM %s %s ORDER BY %s",
                               cfg$pk, paste(admin_fields(cfg), collapse = ", "),
                               cfg$table, where, cfg$order_by))
  expected <- c(cfg$pk, admin_fields(cfg), "active")
  if (ncol(d) == 0 || !all(expected %in% names(d))) {
    if (ncol(d) > 0) {
      warning("admin_list(", cfg$table, "): unexpected columns from the query.", call. = FALSE)
    }
    d <- as.data.frame(
      stats::setNames(replicate(length(expected), logical(0), simplify = FALSE), expected),
      stringsAsFactors = FALSE
    )
  }
  d
}

#' @export
admin_fk_choices <- function(f) {
  d <- load_data(pool, sprintf("SELECT %s AS id, %s AS label FROM %s WHERE active ORDER BY %s",
                               f$fk_id, f$fk_label, f$fk_table, f$fk_label))
  if (nrow(d) == 0) return(c("--SELECT--" = ""))
  c("--SELECT--" = "", stats::setNames(as.character(d$id), d$label))
}

#' @export
admin_insert <- function(conn, cfg, values) {
  cols <- names(values)
  ph   <- paste0("$", seq_along(cols))
  dbExecute(conn, sprintf("INSERT INTO %s (%s) VALUES (%s)",
                          cfg$table, paste(cols, collapse = ", "), paste(ph, collapse = ", ")),
            params = unname(values))
}

#' @export
admin_update <- function(conn, cfg, pk_value, values) {
  cols <- names(values)
  sets <- paste(sprintf("%s = $%d", cols, seq_along(cols)), collapse = ", ")
  dbExecute(conn, sprintf("UPDATE %s SET %s WHERE %s = $%d",
                          cfg$table, sets, cfg$pk, length(cols) + 1L),
            params = c(unname(values), list(pk_value)))
}

#' Deactivate or reactivate. Never DELETE - see rule 1.
#' @export
admin_set_active <- function(conn, cfg, pk_value, active) {
  dbExecute(conn, sprintf("UPDATE %s SET active = $1 WHERE %s = $2", cfg$table, cfg$pk),
            params = list(active, pk_value))
}

#' How many live rows point at this one? Shown before deactivating, so the
#' user knows what they are about to hide.
#' @export
admin_usage <- function(cfg, pk_value) {
  q <- switch(cfg$table,
              tbl_customer   = "SELECT count(*) AS n FROM tbl_order WHERE customer_id = $1",
              tbl_crop       = "SELECT count(*) AS n FROM tbl_order_detail WHERE crop_id = $1",
              tbl_variety    = "SELECT count(*) AS n FROM tbl_order_detail WHERE variety_id = $1",
              tbl_test_method = "SELECT count(*) AS n FROM tbl_order_test WHERE test_id = $1",
              tbl_service_catalog = "SELECT count(*) AS n FROM tbl_order_service WHERE service_code = $1",
              tbl_sample_part = "SELECT count(*) AS n FROM tbl_order_detail WHERE part_id = $1",
              tbl_sampling_bag = "SELECT count(*) AS n FROM tbl_order_detail WHERE bag_id = $1",
              tbl_laboratory = "SELECT count(*) AS n FROM tbl_test_method WHERE laboratory_id = $1",
              tbl_pathogen   = "SELECT count(*) AS n FROM tbl_test_method WHERE pathogen_id = $1",
              NULL)
  if (is.null(q)) return(NA_integer_)
  r <- load_data(pool, q, params = list(pk_value))
  if (nrow(r) == 0) NA_integer_ else as.integer(r$n[1])
}