#' @title Chop data lazily
#' @name lazy_chop
#' @param .data input data
#' @param .chops indices to chop at as a list
#' @examples
#'
#' if (require("GenomicRanges")) {
#'   register_lazy_generic("GRanges", "start")
#'   gr <- GenomicRanges::GRanges(
#'     ranges = IRanges(start = 1:10, width = 2:11),
#'     seqnames = "chr1",
#'     strand = rep(c("+","-"), each = 5))
#'   gr_chopped <- lazy_chop(gr, .chops = list(1:3, c(5,8,10), c(4,6,7,9)))
#'   
#'   gr_shifted <- lapply(gr_chopped,
#'          \(x) {
#'           start(x) <- start(x) - vctrs::vec_data(x)
#'           x
#'           })
#'   # is a list
#'   gr_shifted
#'   # return to "_lazy_chop"
#'   gr_shifted <- list_unchop2(gr_shifted)
#'   # return to original class
#'   list_unchop2(gr_shifted)
#'   
#' }
#'
lazy_chop <- function(.data, .chops, .size = 3) {
  data_class <- class(.data)
  .map <- hashtab(size = .size)
  .sets <- hashtab(size = .size)
  sethash(.map, key = ".__values_set__.", .sets)
  vctrs::new_vctr(
    seq_len(length(.chops)),
    .phantomData = .data,
    .map = .map,
    .chops = .chops,
    class = c(sprintf("%s_lazy_chopped", data_class), "lazy_chopped_data")
  )
}

hashkeys <- function(h) {
  val <- vector("list", numhash(h))
  idx <- 0
  maphash(h, function(k, v) {
    idx <<- idx + 1
    val[idx] <<- list(k)
  })
  val
}

get_generic <- function(.chopped, .generic, ...) {
  #browser()
  map <- attr(.chopped, '.map')
  val <- gethash(map, key = .generic, nomatch = NULL)
  if (is.null(val)) {
    val <- vec_chop(.generic(attr(.chopped, ".phantomData"), ...), attr(.chopped, ".chops"))
    sethash(map, key = .generic, value = val)
  }
  val[[vctrs::vec_data(.chopped)]]
}

set_generic <- function(.chopped, .generic, .value, ...) {
  #browser()
  map <- attr(.chopped, ".map")
  val <- gethash(map, key = .generic, nomatch = NULL)
  if (is.null(val)) {
    val <- vec_chop(.generic(attr(.chopped, ".phantomData"), ...), attr(.chopped, ".chops"))
  }
  # record that this generic was used
  # to set a value
  map[[".__values_set__."]][[.generic]] <- NULL
  val[[vctrs::vec_data(.chopped)]] <- .value
  sethash(map, key = .generic, value = val)
  invisible(.chopped)
}

find_s4_method_formals <- function(.Generic, .Class) {
  extensions <- extends(.Class)
  for (extension in extensions) {
    def <- try_fetch(
      getMethod(
        f = .Generic,
        signature = extension
      ),
      error = function(cnd) {
        NULL
      }
    )
    if (is.function(def)) {
      frmls <- formals(def)
      attr(frmls, "from_class") <- extension
      return(frmls)
    }
  }
  abort(sprintf("Could not find method for class %s", .Class))
}

find_generic <- function(.Generic) {
  out <- get(.Generic)
  if (!is(out, "standardGeneric")) {
    abort(sprintf("%s is not a standard Generic function", .Generic))
  }
  out
}

register_lazy_generic <- function(.Class, .Generic, env = caller_env()) {
  .Generic_set <- sprintf("%s<-", .Generic)
  old_class <- sub("_lazy_chopped$", "", .Class)
  s4class <- sprintf("%s_lazy_chopped", old_class)
  get_s4_generic_method <- find_generic(.Generic)
  set_s4_generic_method <- find_generic(.Generic_set)
  generic_sym <- sym(.Generic)
  get_s4_formals <- find_s4_method_formals(.Generic, .Class)
  get_s4_syms <- syms(names(get_s4_formals))
  # move this outside this function later
  setOldClass(s4class, where = topenv(env))
  # define the 'getter' generic for our class.
  # this should always be called on the length(1) version
  # of the _lazy_chopped variant
  setMethod(
    .Generic,
    s4class,
    new_function(
      args = get_s4_formals,
      body = expr(
        get_generic(
          .chopped = !!(get_s4_syms[[1]]),
          .generic = !!generic_sym,
          !!!(get_s4_syms[[-1]])
        )
      )
    ),
    where = topenv(env)
  )
  # define the 'setter' generic for our class.
  # this should always be called on the length(1) version
  # of the _lazy_chopped variant
  setMethod(
    .Generic_set,
    s4class,
    new_function(
      args = c(get_s4_formals, pairlist2(value = )),
      body = expr(
        set_generic(
          .chopped = !!(get_s4_syms[[1]]),
          .generic = !!generic_sym,
          .value = value,
          !!!(get_s4_syms[[-1]]),
        )
      )
    ),
    where = topenv(env)
  )
  # defines the reconstructor to original class.
  # Takes the original object, checks if anything was "set"
  # if so, pulls the new 'setted' object, unchops it,
  # re-assigns to orignal object with the associated 'setter' 
  # standard generic.
  method(list_unchop2, getClass(s4class)) <- function(
    x,
    ...,
    indices = NULL
  ) {
    .data <- attr(x, ".phantomData")
    map <- attr(x, ".map")
    keys <- hashkeys(map[[".__values_set__."]])
    chops_ind <- attr(x, ".chops")
    if (!is.null(indices)) abort("`indices` is not used")
    for (generic in keys) {
      val <- gethash(map, key = generic) |>
        list_unchop2(indices = chops_ind, ...)
      generic_setter <- find_generic(sprintf("%s<-", generic@generic))
      .data <- generic_setter(.data, value = val)
    }
    .data
  }
}

#register_lazy_generic("GRanges", "start")
