box::use(
  stats[setNames],
)

# ============================================================================
# ZPL LABEL GENERATION  -  Zebra ZT411
# ----------------------------------------------------------------------------
# Pure functions: code in, ZPL string out. No I/O, no Shiny, no printer.
# That is deliberate - it means the label content can be checked, diffed and
# eyeballed in an R console with no hardware attached, which is the only way
# any of this could be developed here.
#
# WHY ZPL AND NOT A PDF
#   The ZT411 is a thermal-transfer printer that speaks ZPL II natively. Sending
#   ZPL gives exact dot placement, a real barcode rendered by the printer's own
#   firmware, and no driver in the path. Printing a PDF through an OS driver
#   rasterises the barcode, and a rasterised barcode on a 25mm label scans
#   badly - which is the one thing a sample label must not do.
#
# UNITS
#   ZPL positions in DOTS, not mm. Dots depend on printhead density, and the
#   ZT411 ships as 203, 300 or 600 dpi. Get this wrong and every label is
#   silently the wrong size. dpi is therefore an explicit argument with no
#   clever default - see label_spec().
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Millimetres to printer dots.
#' @export
mm_to_dots <- function(mm, dpi) round(mm * dpi / 25.4)

#' A label geometry.
#'
#' @param width_mm,height_mm the physical label stock
#' @param dpi printhead density: 203, 300 or 600. CHECK THE PRINTER - it is on
#'   the configuration label, and a ZT411 can be any of the three.
#' @param darkness ^MD value, -30..30. Thermal transfer on synthetic label stock
#'   usually needs more than the default.
#' @param speed ^PR print speed in inches/sec. Slower prints scan better.
#' @export
label_spec <- function(width_mm = 50, height_mm = 25, dpi = 203,
                       darkness = 10, speed = 4) {
  if (!dpi %in% c(203, 300, 600)) {
    stop("dpi must be 203, 300 or 600 - check the ZT411 configuration label", call. = FALSE)
  }
  list(
    width_mm = width_mm, height_mm = height_mm, dpi = dpi,
    darkness = darkness, speed = speed,
    width_dots  = mm_to_dots(width_mm, dpi),
    height_dots = mm_to_dots(height_mm, dpi),
    scale = dpi / 203   # everything below is designed at 203dpi and scaled
  )
}

# ZPL is a control language: ^ and ~ start commands, so they cannot appear
# inside field data. A caret in a variety name would silently truncate the
# label or emit garbage. ^FH lets us hex-escape, but stripping is safer and
# these characters never legitimately appear in a lab code.
zpl_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[\\^~]", " ", x)
  x <- gsub("[\r\n\t]+", " ", x)
  trimws(x)
}

px <- function(n, spec) round(n * spec$scale)

#' ZPL for one label.
#'
#' Layout, top to bottom: a small caption (what kind of code this is), the
#' Code 128 barcode, the code again as human-readable text, then up to two
#' context lines. The code appears twice on purpose - when a barcode is
#' smudged or the scanner is across the room, somebody types it.
#'
#' @param code the value encoded in the barcode. This is the system-generated
#'   ID - sample_code, order_number, and so on.
#' @param title small caption above the barcode, e.g. "EXPLANT" or "ORDER"
#' @param line1,line2 context under the code, e.g. crop/variety, date
#' @param spec a label_spec()
#' @param qty how many copies
#' @export
zpl_label <- function(code, title = "", line1 = "", line2 = "",
                      spec = label_spec(), qty = 1L) {
  code <- zpl_escape(code)
  if (!nzchar(code)) stop("zpl_label(): code is empty", call. = FALSE)
  qty <- max(1L, as.integer(qty))
  
  m  <- px(20, spec)                       # margin
  bw <- if (nchar(code) > 18) 2 else 3     # ^BY module width, narrowed for
  # long codes so they still fit the
  # label rather than running off it
  paste0(
    "^XA",
    "^CI28",                                        # UTF-8
    sprintf("^PW%d", spec$width_dots),
    sprintf("^LL%d", spec$height_dots),
    "^LH0,0",
    sprintf("^MD%d", spec$darkness),
    sprintf("^PR%d", spec$speed),
    "^MTT",                                         # thermal TRANSFER (ribbon)
    "^MMT",                                         # tear-off
    # caption
    if (nzchar(title))
      sprintf("^FO%d,%d^A0N,%d,%d^FD%s^FS", m, px(12, spec),
              px(20, spec), px(20, spec), zpl_escape(title)) else "",
    # barcode: Code 128 auto subset, no interpretation line (we draw our own,
    # so the font matches the rest of the label)
    sprintf("^FO%d,%d^BY%d,3,%d^BCN,%d,N,N,N^FD%s^FS",
            m, px(38, spec), px(bw, spec), px(52, spec), px(52, spec), code),
    # human-readable code
    sprintf("^FO%d,%d^A0N,%d,%d^FD%s^FS", m, px(96, spec),
            px(26, spec), px(26, spec), code),
    if (nzchar(line1))
      sprintf("^FO%d,%d^A0N,%d,%d^FD%s^FS", m, px(126, spec),
              px(18, spec), px(18, spec), zpl_escape(line1)) else "",
    if (nzchar(line2))
      sprintf("^FO%d,%d^A0N,%d,%d^FD%s^FS", m, px(148, spec),
              px(18, spec), px(18, spec), zpl_escape(line2)) else "",
    sprintf("^PQ%d,0,1,Y", qty),
    "^XZ"
  )
}

#' ZPL for many labels in one job.
#'
#' One string, one write to the printer. Sending them separately makes the
#' printer stop and start between labels and turns a rack of 40 explants into
#' 40 round trips through the browser bridge.
#'
#' @param df data.frame with a `code` column; optional title/line1/line2/qty
#' @export
zpl_batch <- function(df, spec = label_spec()) {
  if (is.null(df) || nrow(df) == 0) return("")
  g <- function(nm, default = "") {
    if (is.null(df[[nm]])) rep(default, nrow(df)) else df[[nm]]
  }
  paste0(vapply(seq_len(nrow(df)), function(i) {
    zpl_label(df$code[i], g("title")[i], g("line1")[i], g("line2")[i],
              spec = spec, qty = as.integer(g("qty", 1L)[i]))
  }, character(1)), collapse = "")
}

#' A calibration label: prints the geometry so the stock can be checked.
#'
#' Print this first on a new roll. If the box is clipped, the label_spec() does
#' not match the stock and every subsequent label will be wrong in the same way.
#' @export
zpl_test_label <- function(spec = label_spec()) {
  paste0(
    "^XA^CI28",
    sprintf("^PW%d^LL%d^LH0,0^MD%d", spec$width_dots, spec$height_dots, spec$darkness),
    # a box at the exact label bounds - if any edge is missing, the geometry is
    # wrong, and that is the whole point of this label
    sprintf("^FO2,2^GB%d,%d,2^FS", spec$width_dots - 4, spec$height_dots - 4),
    sprintf("^FO%d,%d^A0N,%d,%d^FDRTB-EAGEL^FS", px(20, spec), px(14, spec),
            px(24, spec), px(24, spec)),
    sprintf("^FO%d,%d^A0N,%d,%d^FD%.0f x %.0f mm @ %d dpi^FS",
            px(20, spec), px(44, spec), px(18, spec), px(18, spec),
            spec$width_mm, spec$height_mm, spec$dpi),
    sprintf("^FO%d,%d^BY2,3,%d^BCN,%d,N,N,N^FDCALIBRATION^FS",
            px(20, spec), px(70, spec), px(40, spec), px(40, spec)),
    "^PQ1,0,1,Y^XZ"
  )
}