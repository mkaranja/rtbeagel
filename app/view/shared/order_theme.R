box::use(
  shiny,
  reactable[reactableTheme, reactableLang, colDef],
)

# ============================================================================
# RTB-EAGEL DESIGN SYSTEM
# ----------------------------------------------------------------------------
# ONE visual language for every module. This file defines no inputs and no
# server logic - it is the presentation layer only.
#
# The contract every module follows:
#
#   page(
#     page_header(title =, sub =, hint =, actions =),   sticky, identical everywhere
#     shiny$uiOutput(ns("kpis")),                       stat_row() of stat_tile()s
#     toolbar(search_box(), scan_box(), filter_select()),
#     workbench(                                        two-pane clinical split
#       list_ui   = table_card(table_note(...), reactableOutput(ns("tbl"))),
#       detail_ui = shiny$uiOutput(ns("detail"))
#     )
#   )
#
# Everything is scoped under `.rtb-intake` so it cannot leak into bs4Dash.
# The stylesheet is linked ONCE from layout.R via theme_css().
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================
# 1. DOCUMENT HEAD
# ============================================================================

#' Link the design-system stylesheet and fonts. Call ONCE, from layout.R.
#'
#' Served from app/static/css/style.css at the URL static/css/style.css -
#' the same mount the sidebar logo already uses.
#' @export
theme_css <- function() {
  shiny$tags$head(
    shiny$tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    shiny$tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    shiny$tags$link(
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=Fraunces:opsz,wght@9..144,500;9..144,600&",
        "family=Inter:wght@400;500;600;700&",
        "family=IBM+Plex+Mono:wght@500;600&display=swap"
      ),
      rel = "stylesheet"
    ),
    shiny$tags$link(rel = "stylesheet", type = "text/css", href = "static/css/style.css")
  )
}

#' Deprecated per-module head. Returns NULL.
#'
#' Every module used to call this, and it emitted the font <link> tags again.
#' With 15 call sites in quarantine.R alone that is 15 duplicate <link>s in the
#' served HTML - htmltools does not de-duplicate arbitrary head tags. The fonts
#' and the stylesheet are now linked exactly once, by theme_css() in layout.R.
#'
#' Kept as a no-op so none of the existing call sites have to change.
#' @export
head_orders <- function() NULL

#' @rdname head_orders
#' @export
head <- function() NULL

# ============================================================================
# 2. PAGE SCAFFOLD  -  every module starts here
# ============================================================================

#' The standard module page.
#'
#' Wraps the whole module in the theme scope and gives every screen the same
#' vertical rhythm. Modules previously each opened with their own
#' div(class="rtb-intake") + head_orders() + useShinyjs() preamble, which is
#' how the header markup drifted apart in the first place.
#'
#' @param ... page contents, normally page_header(), a kpi uiOutput, toolbar()
#'   and workbench()
#' @param class extra classes for the page root
#' @export
page <- function(..., class = NULL) {
  shiny$div(class = trimws(paste("rtb-intake rtb-page", class %||% "")), ...)
}

#' The one page header: a title and a single line saying what the screen is for.
#'
#' It used to take `eyebrow` and `hint` as well. Both are gone, and their
#' removal is the point rather than a side effect. Counted across five modules
#' the screens carried 26 separate prose blocks - eyebrow, sub, hint, a
#' table note, a call-to-action banner, plus a sub-label on every stepper step -
#' all visible at once, before any data. Guidance that is always on screen is
#' guidance nobody reads.
#'
#' What replaces them: the stepper says where you are, and guide() says what to
#' do HERE, changing as you move between tabs. One instruction, always the
#' relevant one.
#'
#' @param title h1 text
#' @param sub one line: what this screen is for. Keep it to one line.
#' @param actions optional right-aligned tag list (buttons, pills, badges)
#' @export
page_header <- function(title, sub = NULL, actions = NULL) {
  shiny$tags$header(
    class = "page-header intake-header page-head",
    shiny$div(
      class = "ph-main",
      shiny$tags$h1(title),
      if (!is.null(sub)) shiny$tags$p(sub)
    ),
    if (!is.null(actions)) shiny$div(class = "ph-actions actions", actions)
  )
}

#' The single instruction strip: what to do on THIS tab, and where it goes next.
#'
#' This is the only guidance surface on a screen. It sits between the stepper
#' (where am I) and the toolbar (how do I find things), and it answers the one
#' question in between: what am I supposed to do now.
#'
#' Because it re-renders with the active tab it is always current, which is what
#' lets everything else be silent. A static hint has to describe the whole
#' screen and so describes nothing in particular.
#'
#' @param ... the instruction. One sentence. If it needs two, the tab is
#'   probably doing two jobs.
#' @param tone "info" (neutral: what this view is), "do" (there is work waiting),
#'   "done" (this step is clear - usually paired with a `next_stage`)
#' @param action optional control, normally goto()
#' @export
guide <- function(..., tone = "info", action = NULL) {
  ico <- switch(tone, do = "circle-arrow-right", done = "circle-check", "circle-info")
  shiny$div(
    class = paste("guide", paste0("guide-", tone)),
    role = "status",
    shiny$span(class = "g-ico", shiny$icon(ico)),
    shiny$div(class = "g-text", ...),
    if (!is.null(action)) shiny$div(class = "g-act", action)
  )
}

#' A button that moves the user to another module.
#'
#' The handoff between benches was previously invisible: a sample approved in
#' thermotherapy simply appeared in another module's queue, and the operator had
#' to know that and go looking. This makes the next step a thing you can click.
#'
#' It sets a TOP-LEVEL Shiny input rather than a namespaced one, deliberately.
#' A namespaced input would have to be threaded back to layout.R through every
#' module's server signature; one unnamespaced input needs a single observer in
#' layout.R and works from anywhere, including modules not yet written.
#'
#' @param label button text
#' @param tab the sidebar tabName to open. Must match a menuItem in layout.R.
#' @export
goto <- function(label, tab) {
  shiny$tags$button(
    class = "btn btn-sm goto-btn", type = "button",
    onclick = sprintf(
      "Shiny.setInputValue('rtb_goto', {tab: '%s', n: Math.random()}, {priority: 'event'})",
      tab),
    label,
    shiny$icon("arrow-right")
  )
}

#' Section card wrapper (registration / detail forms).
#' @param index short marker shown in the header chip (e.g. "1", "2", "A")
#' @param title section title
#' @param ... body content
#' @param sub optional right-aligned subtitle text
#' @param accent one of "brand" (default), "teal", "amber"
#' @export
section <- function(index, title, ..., sub = NULL, accent = "brand") {
  accent_cls <- if (accent == "brand") "" else paste0("accent-", accent)
  shiny$div(
    class = trimws(paste("section", accent_cls)),
    shiny$div(
      class = "section-head",
      shiny$span(class = "idx", index),
      shiny$span(class = "ttl", title),
      if (!is.null(sub)) shiny$span(class = "sub", sub)
    ),
    shiny$div(class = "section-body", ...)
  )
}

#' Small uppercase sub-heading used inside a section body.
#' @export
subhead <- function(label) {
  shiny$div(class = "subhead", label)
}

#' The order-code pill for the sticky header (wraps a textOutput or value).
#' @export
order_pill <- function(value_ui) {
  shiny$div(
    class = "order-pill",
    shiny$span(class = "k", "Lab order no"),
    shiny$span(class = "v", value_ui)
  )
}

# ============================================================================
# 3. TOOLBAR  -  search, scan, filter
# ============================================================================

#' Filter / search toolbar.
#'
#' @param ... controls, left to right
#' @param right optional tag list pinned to the right edge (bulk actions)
#' @param stack TRUE lays the controls out in rows rather than one line -
#'   administration's entity picker needs this
#' @export
toolbar <- function(..., right = NULL, stack = FALSE) {
  shiny$div(
    class = trimws(paste("toolbar", if (isTRUE(stack)) "toolbar-stack" else "")),
    shiny$div(class = "tb-left", ...),
    if (!is.null(right)) shiny$div(class = "tb-right", right)
  )
}

#' Search field with a leading icon.
#'
#' Every module hand-rolled this as a raw tags$input with a sibling span. Same
#' markup, four slightly different widths and placeholders.
#'
#' @param id already namespaced input id
#' @param placeholder placeholder text
#' @param width CSS width for the wrapper
#' @export
search_box <- function(id, placeholder = "Search...", width = NULL) {
  shiny$div(
    class = "search-wrap",
    style = if (!is.null(width)) sprintf("max-width:%s;", width) else NULL,
    shiny$span(class = "ico", shiny$icon("search")),
    # type="text", NOT type="search". The existing modules all use a bare
    # type="text" input read straight off `input$q`, and that is proven to
    # bind. type="search" would add a native clear button, but whether
    # Shiny's text binding claims it is an assumption this codebase has not
    # tested - and unverified assumptions about bindings are exactly what
    # the column-name rule exists to prevent.
    shiny$tags$input(
      id = id, type = "text", class = "form-control form-control-sm",
      placeholder = placeholder, autocomplete = "off",
      `aria-label` = placeholder
    )
  )
}

#' Barcode scan field that fires a hidden action button on Enter.
#'
#' virus_indexing, thermotherapy, meristem_culture and barcode_station each
#' carried a byte-identical copy of the same document-level keydown listener.
#' Four copies meant four listeners bound to `document` for the lifetime of the
#' session, each one testing every keystroke in the app. This binds ONE handler
#' to the field itself.
#'
#' @param id namespaced id of the scan field
#' @param go_id namespaced id of the action button to click on Enter
#' @param placeholder placeholder text
#' @param label visually hidden label for screen readers
#' @export
scan_box <- function(id, go_id, placeholder = "Scan barcode...",
                     label = "Scan a sample barcode") {
  shiny$tagList(
    shiny$div(
      class = "search-wrap scan-wrap",
      shiny$span(class = "ico", shiny$icon("barcode")),
      shiny$tags$input(
        id = id, type = "text", class = "form-control form-control-sm",
        placeholder = placeholder, autocomplete = "off",
        `aria-label` = label,
        onkeydown = sprintf(
          "if(event.key==='Enter'){event.preventDefault();var b=document.getElementById('%s');if(b)b.click();}",
          go_id
        )
      )
    ),
    shiny$actionButton(go_id, "Scan", class = "btn btn-primary btn-sm rtb-visually-hidden")
  )
}

#' Compact filter dropdown for the toolbar.
#' @export
filter_select <- function(id, choices, selected = NULL, width = "190px") {
  shiny$div(
    class = "tb-select",
    shiny$selectizeInput(id, NULL, choices = choices, selected = selected,
                         width = width)
  )
}

#' A dashed next-step call-to-action banner.
#' @export
flow_cta <- function(...) {
  shiny$div(
    class = "flow-cta",
    shiny$span(class = "fc-ico", shiny$icon("arrow-right")),
    shiny$span(...)
  )
}

# ============================================================================
# 4. WORKBENCH  -  the two-pane clinical layout
# ============================================================================

#' Two-pane worklist shell: worklist left, detail panel right.
#'
#' The detail pane is closed at zero width until the module renders something
#' into it, then slides open. That is the whole reason this exists: the
#' fluidRow(column(9), column(3)) split every module hand-rolled reserves a
#' quarter of the screen permanently, including the time when nothing is
#' selected and the panel is blank.
#'
#' Opening is driven by CSS `:has()` against the detail slot's content, so no
#' module needs a server-side toggle. `open = TRUE` forces it open for modules
#' that always show something on the right.
#'
#' @param list_ui the worklist, normally table_card(reactableOutput(...))
#' @param detail_ui the detail slot, normally uiOutput(...)
#' @param open force the panel open
#' @export
workbench <- function(list_ui, detail_ui, open = FALSE) {
  shiny$div(
    class = trimws(paste("worklist", if (isTRUE(open)) "open" else "")),
    shiny$div(class = "wl-list", role = "region", `aria-label` = "Worklist", list_ui),
    shiny$tags$aside(class = "wl-detail", `aria-label` = "Selected record", detail_ui)
  )
}

#' @rdname workbench
#' @export
worklist <- workbench

#' Detail-panel header: title, optional subtitle, and a close (x).
#' @export
detail_head <- function(title, sub = NULL, close_input = NULL) {
  shiny$div(
    class = "wl-detail-head",
    shiny$div(
      shiny$div(class = "wl-title", title),
      if (!is.null(sub)) shiny$div(class = "wl-sub", sub)
    ),
    if (!is.null(close_input)) {
      shiny$tags$button(
        class = "wl-close", type = "button",
        `aria-label` = "Close detail panel",
        shiny$HTML("&times;"),
        onclick = sprintf("Shiny.setInputValue('%s', Math.random())", close_input)
      )
    }
  )
}

#' Standard padded wrapper for detail-panel content.
#'
#' Also the hook the CSS uses to decide the panel has content and should open.
#' @export
detail_body <- function(...) {
  shiny$div(class = "wl-detail-inner", ...)
}

#' The action bar at the foot of a detail panel.
#' @export
detail_actions <- function(...) {
  shiny$div(class = "wl-actions", ...)
}

# ============================================================================
# 5. DATA DISPLAY
# ============================================================================

#' Card wrapper for a table.
#' @export
table_card <- function(...) {
  shiny$div(class = "table-card", ...)
}

#' Explanatory note above a table.
#'
#' thermotherapy.R and meristem_culture.R both used class "wl-table-caption",
#' which is not defined anywhere in the stylesheet - it rendered as unstyled
#' body text sitting on top of the table. This is the styled version.
#' @export
table_note <- function(..., title = NULL) {
  shiny$div(
    class = "table-note wl-table-caption",
    if (!is.null(title)) shiny$strong(title),
    ...
  )
}

#' Empty / zero-state block.
#'
#' An empty table is the NORMAL state on a fresh database, so it gets a real
#' design rather than a blank rectangle.
#' @export
empty_state <- function(title, message = NULL, icon = "inbox", action = NULL) {
  shiny$div(
    class = "empty-state",
    shiny$div(class = "es-ico", shiny$icon(icon)),
    shiny$tags$h3(title),
    if (!is.null(message)) shiny$tags$p(message),
    if (!is.null(action)) shiny$div(class = "es-action", action)
  )
}

#' One KPI stat tile. `value_ui` may be a textOutput or a literal.
#'
#' NOTE the argument order: VALUE FIRST, then label. Swapping them renders a
#' tile with the caption where the number goes and has done so in production.
#' @export
stat_tile <- function(value_ui, label, tone = "brand", id = NULL) {
  shiny$div(
    id = id,
    class = paste("stat-tile", tone),
    shiny$div(class = "n", value_ui),
    shiny$div(class = "l", label)
  )
}


#' A stat tile that NAVIGATES. Same shape as stat_tile, different consequence.
#'
#' The distinction matters and is why this is a separate component rather than
#' an argument on stat_tile. In the stage modules a clickable tile FILTERS the
#' list underneath it - you stay put. Here it takes you to another screen. Two
#' different consequences must not share one affordance, so a nav tile carries
#' an arrow and lifts on hover, and a filter tile does not.
#'
#' Rendered as a real <button>, not a div with tabindex. A button is reachable
#' by Tab, activates on both Enter and Space, and announces itself as a control
#' to a screen reader - all for free, and all of which a div has to reimplement
#' and usually gets half right.
#'
#' @param value_ui the number
#' @param label caption
#' @param tone brand | amber | teal | ink
#' @param tab sidebar tabName to open. Must match a menuItem in layout.R.
#' @param anchor instead of `tab`, the id of an element on THIS page to scroll
#'   to. For a figure whose detail is further down rather than elsewhere.
#' @param note optional second line, e.g. what the number counts
#' @export
stat_link <- function(value_ui, label, tone = "brand", tab = NULL,
                      anchor = NULL, note = NULL) {
  js <- if (!is.null(tab)) {
    sprintf("Shiny.setInputValue('rtb_goto', {tab: '%s', n: Math.random()}, {priority: 'event'})", tab)
  } else {
    sprintf("var e=document.getElementById('%s'); if(e){e.scrollIntoView({behavior:'smooth', block:'start'});}", anchor)
  }
  shiny$tags$button(
    class = paste("stat-tile nav", tone),
    type = "button",
    onclick = js,
    `aria-label` = paste(label, if (!is.null(tab)) "\u2014 open module" else "\u2014 show detail"),
    shiny$div(class = "n", value_ui),
    shiny$div(class = "l", label),
    if (!is.null(note)) shiny$div(class = "st-note", note),
    shiny$span(class = "st-go", `aria-hidden` = "true", shiny$icon("arrow-right"))
  )
}

#' Row of stat tiles.
#'
#' Accepts BOTH shapes, because both read naturally at the call site:
#'   stat_row(list(list(value=, label=, tone=), ...))   spec list
#'   stat_row(stat_tile(...), stat_tile(...))           pre-built tiles
#' @export
stat_row <- function(...) {
  items <- list(...)
  if (length(items) == 1 && is.list(items[[1]]) && !inherits(items[[1]], "shiny.tag")) {
    items <- items[[1]]
  }
  shiny$div(class = "stat-row", lapply(items, function(it) {
    if (inherits(it, "shiny.tag") || inherits(it, "shiny.tag.list")) return(it)
    stat_tile(it$value, it$label, it$tone %||% "brand", it$id)
  }))
}

#' Status chip. tone: brand | amber | teal | red | ink.
#' @export
chip <- function(label, tone = "ink") {
  shiny$span(class = paste("chip", tone), shiny$span(class = "d"), label)
}

#' A count badge for a tab title. tone: default | amber | brand.
#' @export
tab_badge <- function(n, tone = "default") {
  if (is.null(n) || is.na(n) || n == 0) return(NULL)
  shiny$span(
    class = trimws(paste("tab-badge", if (tone == "default") "" else tone)),
    as.character(n)
  )
}

#' Property row (label + value) for detail panels.
#' @export
prop <- function(label, value_ui) {
  shiny$div(
    class = "prop",
    shiny$span(class = "k", label),
    shiny$span(class = "v", value_ui)
  )
}

#' Grid of property rows.
#' @export
prop_grid <- function(...) {
  shiny$div(class = "prop-grid", ...)
}

#' Inline fulfilment bar for a percentage.
#' @export
mini_bar <- function(pct) {
  if (is.null(pct) || length(pct) == 0 || is.na(pct)) pct <- 0
  pct <- max(0, min(100, as.numeric(pct)))
  tone <- if (pct >= 100) "var(--brand)" else if (pct > 0) "var(--teal)" else "var(--border)"
  shiny$div(
    class = "mbar",
    shiny$div(
      class = "mbar-track",
      role = "progressbar", `aria-valuenow` = as.character(pct),
      `aria-valuemin` = "0", `aria-valuemax` = "100",
      shiny$div(class = "mbar-fill", style = sprintf("width:%s%%; background:%s;", pct, tone))
    ),
    shiny$span(class = "mbar-txt", paste0(pct, "%"))
  )
}

#' A service line with its fulfilment bar.
#' @export
service_line <- function(label, sub, fulfilled, target, pct, status) {
  tone <- switch(status, fulfilled = "", in_progress = "amber", cancelled = "ink", "ink")
  shiny$div(
    class = "svc",
    shiny$div(class = "nm", label, if (!is.null(sub) && nzchar(sub)) shiny$tags$small(sub)),
    shiny$div(class = "qty", sprintf("%s / %s", fulfilled, target)),
    shiny$div(class = paste("bar", tone), shiny$tags$i(style = sprintf("width:%d%%;", as.integer(pct)))),
    chip(gsub("_", " ", status),
         switch(status, fulfilled = "brand", in_progress = "teal",
                requested = "ink", cancelled = "red", "ink"))
  )
}

# ============================================================================
# 6. REACTABLE  -  one table style, defined once
# ============================================================================

#' The shared reactable theme.
#'
#' The literal reactableTheme(borderColor = "#E9EFE6", highlightColor =
#' "#F4F7F2") was pasted at 13 call sites across 9 files, hard-coding two
#' colours that already exist as --border and --bg. Changing the palette meant
#' finding all 13. Now it is one function.
#' @export
rt_theme <- function() {
  # These hexes MUST track the CSS custom properties, and cannot reference
  # them: reactable renders its theme into inline styles from R, where
  # var(--border) does not resolve. They are the literal values of --border,
  # --brand-soft, --ink-soft and --ink from the palette in style.css, and the
  # two have to be changed together.
  reactableTheme(
    borderColor      = "#DEE2E6",   # --border
    highlightColor   = "#E9F7EF",   # --brand-soft  (the main.R badge green)
    stripedColor     = "#F7FCF9",
    cellPadding      = "9px 12px",
    headerStyle      = list(
      background    = "#F1FAF5",
      borderColor   = "#DEE2E6",
      color         = "#495057",    # --ink-soft
      fontSize      = "11px",
      fontWeight    = 700,
      letterSpacing = ".06em",
      textTransform = "uppercase"
    ),
    style            = list(fontSize = "13px", color = "#212529"),  # --ink
    searchInputStyle = list(width = "100%")
  )
}

#' Shared reactable language pack with a module-specific empty message.
#' @export
rt_lang <- function(no_data = "Nothing here yet.") {
  reactableLang(
    noData            = no_data,
    pageNext          = "Next",
    pagePrevious      = "Previous",
    pageInfo          = "{rowStart}\u2013{rowEnd} of {rows}",
    searchPlaceholder = "Filter..."
  )
}

#' onClick handler that publishes the clicked row's key to a Shiny input.
#'
#' Every worklist module wrote this sprintf by hand. `n = Math.random()` is what
#' makes re-clicking the same row fire the observer again.
#'
#' @param input_id namespaced input to set
#' @param key column name holding the row's identifier
#' @export
rt_click_js <- function(input_id, key = "sample_code") {
  sprintf(
    "function(rowInfo){ if(!rowInfo) return; Shiny.setInputValue('%s', {code: rowInfo.row['%s'], n: Math.random()}); }",
    input_id, key
  )
}

#' rowClass handler that marks the selected row.
#' @export
rt_selected_js <- function(selected, key = "sample_code") {
  sel <- selected %||% ""
  if (length(sel) == 0 || is.na(sel[1])) sel <- ""
  sprintf(
    "function(rowInfo){ return rowInfo && rowInfo.row['%s'] === '%s' ? 'wl-selected' : null; }",
    key, as.character(sel[1])
  )
}

#' Keep only the colDefs whose column is actually present.
#'
#' reactable rejects the ENTIRE table when `columns` names something absent from
#' `data` - one stale name blanks a whole worklist with
#' "columns names must exist in data". That is a reasonable default for a
#' static report and a bad one for a bench screen: the operator loses every row
#' because one column drifted.
#'
#' Passing the list through this renders what CAN be rendered. It does not hide
#' drift - shape_frame() still warns by name when a query and its prototype
#' disagree - it just stops one mismatch taking the table down with it.
#'
#' @param cols named list of colDef()
#' @param d the data frame being rendered
#' @export
rt_cols <- function(cols, d) {
  keep <- names(cols) %in% names(d)
  if (!all(keep)) {
    warning("reactable: dropping colDef(s) with no column in the data: ",
            paste(names(cols)[!keep], collapse = ", "), call. = FALSE)
  }
  cols[keep]
}

#' Show only the named columns; hide the rest.
#'
#' For a table shared across tabs. Every tab was showing every column, so a
#' worklist about "which test still needs starting" also carried the test
#' sample code, the result and the bench - none of which exist yet at that
#' point, and one of which (the material code) sent operators looking for a
#' tube on someone else's bench.
#'
#' Columns are HIDDEN rather than dropped. Dropping a colDef does not remove
#' the column - reactable then renders it with default formatting, so the
#' unwanted column comes back looking worse.
#'
#' @param cols named list of colDef()
#' @param keep character vector of column names to show
#' @export
rt_only <- function(cols, keep) {
  for (nm in names(cols)) {
    if (!nm %in% keep) cols[[nm]] <- colDef(show = FALSE)
  }
  cols
}

#' rowStyle handler that makes rows look clickable.
#' @export
rt_pointer_js <- function() {
  "function(rowInfo){ return {cursor:'pointer'}; }"
}

# ============================================================================
# 7. PROGRESS  -  trackers and steppers
# ============================================================================

#' Horizontal step tracker.
#'
#' `steps` is a data.frame with: label, state ("done" | "now" | "next" | ""),
#' and optionally `when` (a date/time string shown under the label).
#'
#' All three answers are visible at once - where the order has BEEN, where it
#' IS, and where it can go.
#' @export
tracker <- function(steps) {
  if (is.null(steps) || nrow(steps) == 0) {
    return(shiny$div(class = "tracker-empty", "No steps recorded yet."))
  }
  shiny$div(class = "tracker", lapply(seq_len(nrow(steps)), function(i) {
    st <- steps$state[i]
    mark <- if (identical(st, "done")) "\u2713" else as.character(i)
    shiny$div(
      class = paste("step", st),
      shiny$div(class = "dot", `aria-hidden` = "true", mark),
      shiny$div(class = "lbl", steps$label[i]),
      if (!is.null(steps$when) && !is.na(steps$when[i]) && nzchar(steps$when[i])) {
        shiny$div(class = "when", steps$when[i])
      }
    )
  }))
}

#' One step in the vertical tracker rail.
#' @param state one of "done", "current", "next", "future"
#' @export
tracker_step <- function(label, detail = "", state = "future") {
  shiny$div(
    class = paste0("trk trk-", state),
    shiny$div(class = "trk-dot"),
    shiny$div(
      class = "trk-body",
      shiny$div(class = "trk-label", label),
      if (!is.null(detail) && nzchar(detail)) shiny$div(class = "trk-detail", detail)
    )
  )
}

# Client-side handler for a stepper used as navigation.
#
# It does TWO things, and the second is why it exists:
#   1. sets the Shiny input to this step's data-value
#   2. moves the `on` class to this step IMMEDIATELY, in the browser
#
# (2) decouples the highlight from a server round trip, so the caller can
# render the stepper on data changes only - not on tab changes - and switching
# tabs does not re-render and flash the whole header.
flow_pick_js <- function(input_id) {
  sprintf(paste0(
    "Shiny.setInputValue('%s', this.getAttribute('data-value'));",
    "Array.prototype.forEach.call(this.parentNode.children, function(k){",
    "k.classList.remove('on'); k.setAttribute('aria-selected','false'); });",
    "this.classList.add('on'); this.setAttribute('aria-selected','true');"
  ), input_id)
}

#' Consignment lifecycle stepper, and the standard tab bar for a worklist.
#'
#' @param steps list of list(title=, sub=, count=, unit=, active=, waiting=,
#'   value=, num=). `value` is what the step sets the input to when the stepper
#'   is navigational; it defaults to the step's position.
#'
#'   `num` overrides the circle's contents AND takes the step out of the
#'   ordinal count. That matters because the numbered circles are a claim: they
#'   say these things happen in this order. Most worklists mix true sequence
#'   steps with cross-cutting views - "everything on this bench", "overdue" -
#'   which are filters over the sequence rather than positions in it. Numbering
#'   those would assert an order that does not exist, and would push the real
#'   step numbers along so that stage one stopped being called one.
#'
#'   So: leave `num` unset for a genuine step and it takes the next ordinal;
#'   set it (SIGMA for a total, "!" for an exception view) and the step is
#'   marked instead of numbered, with the ordinals continuing around it.
#'
#' @param input_id when supplied, the stepper BECOMES the tab bar: each step is
#'   a real tab control that sets this Shiny input. When NULL the stepper is a
#'   read-only progress display.
#' @export
flow_stepper <- function(steps, input_id = NULL) {
  nav <- !is.null(input_id)
  
  # Ordinals are assigned only to steps without an explicit `num`, so a
  # sequence reads 1,2,3 however many marked views are interleaved with it.
  marks <- character(length(steps))
  ord <- 0L
  for (i in seq_along(steps)) {
    if (!is.null(steps[[i]]$num)) {
      marks[i] <- as.character(steps[[i]]$num)
    } else {
      ord <- ord + 1L
      marks[i] <- as.character(ord)
    }
  }
  
  shiny$div(
    class = trimws(paste("flow", if (nav) "flow-nav" else "")),
    role = if (nav) "tablist" else NULL,
    lapply(seq_along(steps), function(i) {
      s <- steps[[i]]
      cls <- paste("flow-step",
                   if (isTRUE(s$active)) "on" else "",
                   if (isTRUE(s$waiting)) "waiting" else "")
      shiny$div(
        class = trimws(cls),
        role = if (nav) "tab" else NULL,
        tabindex = if (nav) "0" else NULL,
        `aria-selected` = if (nav) tolower(as.character(isTRUE(s$active))) else NULL,
        `data-value` = if (nav) (s$value %||% as.character(i)) else NULL,
        onclick = if (nav) flow_pick_js(input_id) else NULL,
        onkeydown = if (nav) sprintf(
          "if(event.key==='Enter'||event.key===' '){event.preventDefault();%s}",
          flow_pick_js(input_id)) else NULL,
        shiny$div(
          class = trimws(paste("fs-num", if (!is.null(s$num)) "mark" else "")),
          marks[i]),
        shiny$div(class = "fs-body",
                  shiny$div(class = "fs-title", s$title),
                  if (!is.null(s$sub)) shiny$div(class = "fs-sub", s$sub)),
        shiny$div(class = "fs-count", as.character(s$count %||% 0),
                  shiny$tags$small(s$unit %||% "waiting"))
      )
    })
  )
}