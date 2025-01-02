sep_ <- function(n) {
  x <- vctrs::vec_rep("|", times = n)
  class(x) <- "sep!"
  x
}

#' @export
`vec_ptype_abbr.sep!` <- function(x, ..., prefix_named, suffix_shape) {
  NULL
}

class_vec_phantom <- S7::new_S3_class("vec_phantom")

#' @rdname plyxp-printing
#' @export
vec_phantom <- function(x) {
  vctrs::new_vctr(
    seq_len(length.out = length(x)),
    phantomData = x,
    class = "vec_phantom"
  )
}

#' @export
vec_restore.vec_phantom <- function(x, to, ...) {
  # cannot make assumptions on what
  # the phantomData is, we use base subset
  phantom_data <- attr(to, "phantomData")[x]
  vec_phantom(phantom_data)
}

#' @export
vec_ptype_abbr.vec_phantom <- function(x, ...) {
  vec_ptype_abbr(attr(x, "phantomData"), ...)
}

# x <- vec_phantom(letters)
# attr(x[1], "phantomData")

#' @export
pillar_shaft.vec_phantom <- function(x, ...) {
  fmt <- plyxp_pillar_format(attr(x, "phantomData"), ...)
  cur_width <- max(nchar(fmt), 2, na.rm = TRUE)
  min_width <- min(10, cur_width)
  pillar::new_pillar_shaft_simple(
    formatted = fmt,
    ...,
    width = cur_width,
    min_width = min_width,
    shorten = "mid"
  )
}

#' @name plyxp-printing
#' @title Printing within tibble with S4 objects
#' @description
#' `plyxp` uses [pillar][pillar::pillar-package] for its printing.
#' If you want to change how your S4 object is printed within
#' `plyxp`'s print method, consider writing a method for
#' this function.
#'
#' To print S4 objects in a tibble, `plyxp` hacks a custom
#' integer vector built from [`vctrs`][vctrs::new_vctr] where
#' the S4 object lives in an attribute named "phantomData".
#' You can create your own S4 phantom vector with `vec_phantom()`.
#' This function is not used outside of printing for `plyxp`
#'
#' The default method for formatting a `vec_phantom()` is to call
#' [`showAsCell()`][S4Vectors::showAsCell].
#'
#' @section tidy printing:
#'
#' By default, `plyxp` will not affect the show method for
#' `SummarizedExperiment` objects. In order to use a tibble abstraction, use
#' `use_show_tidy()` to enable or `use_show_default()` to disable this feature.
#' These functions are called for their side effects, modifying the global
#' option "show_SummarizedExperiment_as_tibble_abstraction".
#'
#' To show an object as the tibble abstraction regardless of the set option,
#' use the S3 generic `show_tidy(...)`.
#'
#'
#' @param x The S4 object
#' @param ... other arguments passed from [`pillar_shaft`][pillar::pillar_shaft]
#' @examples
#'
#' if(require("IRanges")) {
#'   ilist <- IRanges::IntegerList(list(c(1L,2L,3L),c(5L,6L)))
#'   phantom <- vec_phantom(ilist)
#'   pillar::pillar_shaft(phantom)
#'
#'   plyxp_pillar_format.CompressedIntegerList <- function(x) {
#'    sprintf("Int: [%i]", lengths(x))
#'   }
#'   pillar::pillar_shaft(phantom)
#'   rm(plyxp_pillar_format.CompressedIntegerList)
#' }
#'
#' # default printing
#' se_simple
#' # use `plyxp` tibble abstraction
#' use_show_tidy()
#' se_simple
#' # restore default print
#' use_show_default()
#' se_simple
#' # explicitly using tibble abstraction
#' show_tidy(se_simple)
#'
#' @returns
#' `plyxp_pillar_format` -> formatted version of your S4 vector
#' `vec_phantom` -> integer vector with arbitrary object in `phatomData` attribute.
#' @export
plyxp_pillar_format <- function(x, ...) {
  UseMethod("plyxp_pillar_format")
}

#' @export
plyxp_pillar_format.default <- function(x, ...) {
  S4Vectors::showAsCell(x)
}

maybe_phantom <- function(x) {
  if (isS4(x)) return(vec_phantom(x))
  x
}

#' @export
length.vec_phantom <- function(x, ...) {
  length(attr(x, "phantomData"))
}

#' @rdname plyxp-printing
#' @export
show_tidy <- function(x, ...) {
  UseMethod("show_tidy")
}

#' @export
show_tidy.default <- function(x, ...) {
  methods::show(object = x)
}

#' @export
show_tidy.SummarizedExperiment <- function(x, n = 10, ...) {
  top_n <- ceiling(n / 2)
  bot_n <- floor(n / 2)
  onr <- nr <- nrow(x)
  row_slice <- if (nr < 2 * n) {
    seq_len(nr)
  } else {
    c(seq_len(n), (nr - n + 1):nr)
  }

  onc <- nc <- ncol(x)
  col_slice <- if (nc < 2 * n) {
    seq_len(nc)
  } else {
    c(seq_len(n), (nc - n + 1):nc)
  }
  # get first 5 and last 5 rows and cols
  x_ <- x[row_slice, col_slice]
  nr <- nrow(x_)
  nc <- ncol(x_)
  .features <- rownames(x_) %||% seq_len(onr)[row_slice]
  .samples <- colnames(x_) %||% seq_len(onc)[col_slice]
  assays_ <- map(assays(x_), as_vec)
  row_ <- map(rowData(x_), vec_rep, times = nc) |> map(maybe_phantom)
  col_ <- map(colData(x_), vec_rep_each, times = nr) |> map(maybe_phantom)
  nn <- nc * nr
  out <- c(
    list(
      .features = vctrs::vec_rep(.features, times = nc),
      .samples = vctrs::vec_rep_each(.samples, times = nr)
    ),
    list(sep_(nn)),
    assays_,
    list(sep_(nn)),
    row_,
    list(sep_(nn)),
    col_
  )
  # browser()
  attr(out, "row.names") <- c(NA_integer_, -nn)
  class(out) <- c("SE_abstraction", "tbl_df", "tbl", "data.frame")

  # browser()
  sub_seq <- if (nn < 2 * top_n) {
    seq_len(nn)
  } else {
    c(
      seq_len(top_n),
      (nn - bot_n + 1):nn
    )
  }
  # if (diff(min(top_half):max(bot_half)))
  out_sub <- out[sub_seq, ]

  if (nrow(out_sub) == nn) {
    attr(out_sub, "plyxp:::has_break_at") <- 0L
  } else {
    attr(out_sub, "plyxp:::has_break_at") <- max(top_n)
  }
  attr(out_sub, "plyxp:::data") <- x
  print(out_sub, n = n, ...)
  invisible(x)
}

collapse <- function(x) {
  paste(x, collapse = ", ")
}

#' @export
format.plyxp_pillar_rid_type <- function(x, width = NULL, ...) {
  ""
}

#' @export
format.plyxp_pillar_rid_shaft <- function(x, width, ...) {
  new_ornament(
    style_subtle(
      align(x$row_ids, width = width, align = "right")
    ),
    width = width,
    align = "right"
  )
}

#' @export
ctl_new_rowid_pillar.SE_abstraction <- function(
  controller,
  x,
  width,
  ...,
  title = NULL,
  type = NULL
) {
  if (val <- attr(controller, "plyxp:::has_break_at")) {
    # browser()
    template <- names(
      ctl_new_pillar(controller, vector(), width, title = title)
    )
    if (!length(template)) {
      return(NULL)
    }
    out <- map(set_names(template), function(.x) "")
    top_half <- seq_len(val)
    bot_half <- rev(seq_len(nrow(controller) - (val + 1L)))
    data <- if (length(bot_half)) {
      spec <- sprintf("n-%i", bot_half)
      c(
        top_half,
        align(
          c(spec, "n"),
          align = "left"
        )
      )
    } else {
      c(top_half, "n")
    }
    if ("type" %in% template) {
      out$type <- pillar_component(
        structure(
          list(),
          class = "plyxp_pillar_rid_type",
          width = 1L
        )
      )
    }
    data <- new_pillar_shaft(
      list(row_ids = data),
      width = max(nchar(data)),
      class = "plyxp_pillar_rid_shaft"
    )
    if ("data" %in% template) {
      out$data <- pillar_component(data)
    }
    new_pillar(out, width = attr(data, "width", exact = TRUE))
  } else {
    NextMethod()
  }
}

#' @export
`pillar_shaft.sep!` <- function(x, ...) {
  new_pillar_shaft_simple(
    style_subtle(x),
    align = "left",
    min_width = 1L
  )
}

#' @export
tbl_sum.SE_abstraction <- function(x) {
  se <- attr(x, "plyxp:::data")
  out <- dim_desc(se)
  out <- sprintf("A %s-tibble Abstraction: %s", class(se), out)
  # names(out) <- sprintf("A %s-tibble Abstraction", class(se))
  if (!is.null(groups <- metadata(se)[["group_data"]])) {
    gv <- group_vars_se_impl(se)
    vars <- c(
      if (!is_empty(gv$row_groups))
        sprintf("rows(%s)", collapse(gv$row_groups)),
      if (!is_empty(gv$col_groups)) sprintf("cols(%s)", collapse(gv$col_groups))
    )
    out <- c(out, sprintf("Groups: %s", collapse(vars)))
  }
  out
}

#' @export
tbl_format_setup.SE_abstraction <- function(
  x,
  width,
  ...,
  n,
  max_extra_cols,
  max_footer_lines,
  focus
) {
  # pillar 1.10.0 now passes 'setup' as NULL on first call
  # If we fail to check, the setup$body will always be NULL
  dots_setup <- ...names() %in% "setup"
  is_first_call <- any(dots_setup) && is.null(...elt(which(dots_setup)))
  setup <- NextMethod()
  setup$rows_total <- prod(dim(attr(x, "plyxp:::data")))
  # on second call, setup$body should exist
  if (
    !is_first_call &&
      !is.null(setup$body) &&
      (val <- attr(x, "plyxp:::has_break_at"))
  ) {
    body_idx <- val + 2L
    prev_line <- cli::ansi_strip(setup$body[body_idx])
    position <- gregexec("[^ |]+", prev_line)[[1]]
    len <- attr(position, "match.length")
    new_line <- vctrs::vec_rep(" ", times = nchar(prev_line))
    new_line[position + floor(len / 2)] <- style_subtle(cli::symbol$continue)
    setup$body <- append(
      setup$body,
      paste(new_line, collapse = ""),
      body_idx
    ) |>
      glue::as_glue()
  }
  setup
}

#' @export
ctl_new_pillar.SE_abstraction <- function(
  controller,
  x,
  width,
  ...,
  title = NULL
) {
  if (inherits(x, "sep!")) {
    p <- pillar(x, title = "|", ...)
    class(p$title[[1]]) <- "blank_pillar_title"
    class(p$type[[1]]) <- "blank_pillar_type"
    attr(p$type, "width") <- 1L
    attr(p$type, "min_width") <- 1L
    attr(p$type[[1]], "width") <- 1L
    attr(p$type[[1]], "min_width") <- 1L
    p
  } else {
    NextMethod()
  }
}

#' @export
tbl_format_footer.SE_abstraction <- function(x, setup, ...) {
  out <- NextMethod()
  if (attr(x, "plyxp:::has_break_at")) {
    out <- c(
      style_subtle(
        sprintf(
          "# %s n = %s",
          cli::symbol$info,
          format(
            setup$rows_total,
            big.mark = ",",
            scientific = FALSE,
            digits = 1
          )
        )
      ),
      out
    )
  }
  out
}

#' @export
format.blank_pillar_type <- function(x, width, ...) {
  style_subtle("|")
}

#' @export
format.blank_pillar_title <- format.blank_pillar_type

setMethod(
  f = "show",
  signature = "SummarizedExperiment",
  definition = function(object) {
    if (
      isTRUE(
        x = getOption(
          x = "show_SummarizedExperiment_as_tibble_abstraction",
          default = FALSE
        )
      )
    ) {
      show_tidy(object)
    } else {
      methods::getMethod(
        f = "show",
        signature = "SummarizedExperiment",
        where = asNamespace(ns = "SummarizedExperiment")
      )(object)
    }
  }
)

#' @rdname plyxp-printing
#' @export
use_show_tidy <- function() {
  options(show_SummarizedExperiment_as_tibble_abstraction = TRUE)
}

#' @rdname plyxp-printing
#' @export
use_show_default <- function() {
  options(show_SummarizedExperiment_as_tibble_abstraction = FALSE)
}
