box::use(
  app/logic/fct_conn[pool, load_data],
)

# ============================================================================
# DASHBOARD QUERIES
# ----------------------------------------------------------------------------
# Read-only roll-ups for the overview screen. Nothing here writes.
#
# Every column below was checked against the schema dump before it was written.
# Where a value could be derived two ways, this file uses the one the rest of
# the system already treats as authoritative:
#
#   order status   -> view_order_progress.derived_status  (never a stored flag)
#   sample position-> view_sample_current                 (last event, not
#                                                          tbl_sample.stage_code)
#
# Those two disagreeing is how a sample becomes invisible, so the dashboard is
# deliberately built on the same views the worklists use. If a number here
# disagrees with a module, the module is what is wrong - not this.
#
# Every function returns a data.frame, 0 rows on failure, never NULL. An empty
# lab is the normal state on a fresh database, not an error.
# ============================================================================

shape <- function(d, proto, where) {
  if (is.null(d) || !is.data.frame(d)) d <- data.frame()
  for (nm in names(proto)) {
    if (is.null(d[[nm]])) {
      # rep(NA, nrow(d)), NEVER <- NA. On a zero-row frame the latter raises
      # "replacement has 1 row, data has 0" - and zero rows is normal here.
      d[[nm]] <- rep(proto[[nm]][NA_integer_], nrow(d))
    }
  }
  d[, names(proto), drop = FALSE]
}

# ---------------------------------------------------------------------------
# 1. ORDER STATUS  -  the top-line counts
# ---------------------------------------------------------------------------

proto_dash_orders <- function() list(
  derived_status = character(),
  n              = integer(),
  avg_pct        = integer()
)

#' Orders grouped by derived status.
#' @export
dash_orders <- function() {
  d <- load_data(pool, "
    SELECT derived_status,
           count(*)::int                      AS n,
           COALESCE(avg(pct_complete), 0)::int AS avg_pct
    FROM view_order_progress
    GROUP BY derived_status
    ORDER BY n DESC")
  shape(d, proto_dash_orders(), "dash_orders()")
}

# ---------------------------------------------------------------------------
# 2. PIPELINE  -  where the material physically is
# ---------------------------------------------------------------------------

proto_dash_pipeline <- function() list(
  stage_code = character(),
  label      = character(),
  sort_order = integer(),
  n          = integer(),
  units      = integer()
)

#' Material by stage, in pipeline order.
#'
#' test_id IS NULL excludes virus-indexing TEST-SAMPLES. They are one-test
#' cuttings, not pipeline material; counting them here would inflate the
#' indexing bench by the number of tests requested rather than the number of
#' plants being tracked. The same exclusion is applied in view_stage_queue's
#' consumers, so the two agree.
#' @export
dash_pipeline <- function() {
  d <- load_data(pool, "
    SELECT st.stage_code, st.label, st.sort_order,
           count(*)::int                        AS n,
           COALESCE(sum(s.quantity), 0)::int    AS units
    FROM view_sample_current c
    JOIN tbl_sample s ON s.sample_code = c.sample_code
    JOIN tbl_stage  st ON st.stage_code = c.stage_code
    WHERE s.test_id IS NULL
      AND st.is_terminal = false
    GROUP BY st.stage_code, st.label, st.sort_order
    ORDER BY st.sort_order, st.label")
  shape(d, proto_dash_pipeline(), "dash_pipeline()")
}

# ---------------------------------------------------------------------------
# 3. NEEDS ATTENTION  -  one row per thing a human must do
# ---------------------------------------------------------------------------

proto_dash_attention <- function() list(
  kind     = character(),
  label    = character(),
  n        = integer(),
  tab      = character(),
  severity = character()
)

#' The work queue, as counts. This is the dashboard's reason to exist.
#'
#' Each row names a thing somebody must act on and the tab it lives in, so the
#' dashboard can hand off rather than just inform. `tab` values must match
#' menuItem tabNames in layout.R - they drive order_theme$goto().
#'
#' Deliberately UNION ALL of small independent counts rather than one clever
#' query: each row can be read, checked and changed on its own, and one of them
#' returning nothing does not blank the others.
#' @export
dash_attention <- function() {
  d <- load_data(pool, "
    SELECT 'approval' AS kind,
           'Orders awaiting registration approval' AS label,
           count(*)::int AS n, 'orders' AS tab, 'high' AS severity
    FROM tbl_order WHERE approval_state = 'pending'
    UNION ALL
    SELECT 'quarantine_review',
           'Consignments on a bench awaiting review',
           count(*)::int, 'quarantine', 'normal'
    FROM view_sample_current c
    JOIN tbl_sample s ON s.sample_code = c.sample_code
    WHERE s.test_id IS NULL
      AND c.stage_code IN ('quarantine_glasshouse','quarantine_growthroom')
      AND c.state_code IN ('established','received')
    UNION ALL
    SELECT 'test_results',
           'Test results awaiting review',
           count(*)::int, 'vx', 'high'
    FROM view_sample_current c
    JOIN tbl_sample s ON s.sample_code = c.sample_code
    WHERE s.test_id IS NOT NULL
      AND c.state_code = 'results_available'
    UNION ALL
    SELECT 'thermo_overdue',
           'Samples past their expected conviron exit',
           count(*)::int, 'thermotherapy', 'high'
    FROM tbl_thermotherapy_detail t
    WHERE t.exit_date IS NULL
      AND t.entered_on IS NOT NULL
      AND t.expected_days_in_conviron IS NOT NULL
      AND now() > t.entered_on + (t.expected_days_in_conviron || ' days')::interval
    UNION ALL
    SELECT 'disposition',
           'Samples awaiting a disposition decision',
           count(*)::int, 'quarantine', 'normal'
    FROM view_pending_disposition
    UNION ALL
    SELECT 'report_approval',
           'Reports generated but not approved',
           count(*)::int, 'orders', 'normal'
    FROM tbl_report r
    WHERE r.approved_on IS NULL")
  shape(d, proto_dash_attention(), "dash_attention()")
}

# ---------------------------------------------------------------------------
# 4. INDEXING  -  tests required vs tests started
# ---------------------------------------------------------------------------

proto_dash_indexing <- function() list(
  required  = integer(),
  initiated = integer(),
  resulted  = integer(),
  approved  = integer()
)

#' Test coverage across all available material.
#'
#' Mirrors indexing_material()'s definition of "required" - tbl_order_test,
#' falling back to every active method for the crop when none were requested.
#' The two must agree, or the dashboard and the bench will report different
#' totals for the same question.
#' @export
dash_indexing <- function() {
  d <- load_data(pool, "
    WITH avail AS (
      SELECT c.sample_code, c.order_number
      FROM view_sample_current c
      JOIN tbl_sample s ON s.sample_code = c.sample_code
      WHERE c.stage_code IN ('quarantine_glasshouse','quarantine_growthroom')
        AND c.state_code = 'approved'
        AND s.test_id IS NULL
    ),
    req AS (
      SELECT ot.order_number, t.test_id
      FROM tbl_order_test ot
      JOIN tbl_test_method t ON t.test_id = ot.test_id
      UNION
      SELECT od.order_number, t.test_id
      FROM tbl_order_detail od
      JOIN tbl_test_method t ON t.crop_id = od.crop_id AND t.active
      WHERE NOT EXISTS (SELECT 1 FROM tbl_order_test x
                        WHERE x.order_number = od.order_number)
    ),
    grid AS (
      SELECT a.sample_code, r.test_id, ts.sample_code AS test_sample_code,
             cur.state_code,
             (SELECT count(*) FROM tbl_test_result tr
               WHERE tr.sample_code = ts.sample_code) AS n_res
      FROM avail a
      JOIN req r ON r.order_number = a.order_number
      LEFT JOIN tbl_sample ts ON ts.parent_sample_code = a.sample_code
                             AND ts.test_id = r.test_id
      LEFT JOIN view_sample_current cur ON cur.sample_code = ts.sample_code
    )
    SELECT count(*)::int                                              AS required,
           count(test_sample_code)::int                               AS initiated,
           count(*) FILTER (WHERE n_res > 0)::int                     AS resulted,
           count(*) FILTER (WHERE state_code = 'approved')::int       AS approved
    FROM grid")
  shape(d, proto_dash_indexing(), "dash_indexing()")
}

# ---------------------------------------------------------------------------
# 5. THROUGHPUT  -  orders received per month
# ---------------------------------------------------------------------------

proto_dash_throughput <- function() list(
  month   = character(),
  orders  = integer(),
  samples = integer()
)

#' Last 12 months of intake.
#'
#' generate_series gives a row for every month even when nothing was received,
#' so a quiet month reads as a gap rather than silently collapsing the axis and
#' making the trend look continuous.
#' @export
dash_throughput <- function() {
  d <- load_data(pool, "
    WITH months AS (
      SELECT date_trunc('month', now()) - (n || ' months')::interval AS m
      FROM generate_series(0, 11) AS n
    )
    SELECT to_char(m.m, 'Mon YYYY') AS month,
           (SELECT count(*)::int FROM tbl_order o
             WHERE date_trunc('month', o.created_on) = m.m)  AS orders,
           (SELECT count(*)::int FROM tbl_sample s
             WHERE date_trunc('month', s.created_on) = m.m
               AND s.test_id IS NULL)                        AS samples
    FROM months m
    ORDER BY m.m")
  shape(d, proto_dash_throughput(), "dash_throughput()")
}

# ---------------------------------------------------------------------------
# 6. RECENT ACTIVITY
# ---------------------------------------------------------------------------

proto_dash_activity <- function() list(
  username    = character(),
  module      = character(),
  action      = character(),
  subject     = character(),
  occurred_on = as.POSIXct(character())
)

#' The last 12 things anybody did.
#' @export
dash_activity <- function() {
  d <- load_data(pool, "
    SELECT username, module, action, subject, occurred_on
    FROM tbl_activity_log
    ORDER BY occurred_on DESC
    LIMIT 12")
  shape(d, proto_dash_activity(), "dash_activity()")
}