

#' @export
.AtNames.plyxp_envs <- function(x, pattern = "") {
  options <- setdiff(names(attributes(x)), c("class", "names"))
  if (identical(pattern, "")) {
    options
  } else {
    grep(pattern, options, value = TRUE)
  }
}

#' @export
`@.plyxp_envs` <- function(obj, name) {
  attr(obj, name, exact = TRUE)
}

#' @export
`@<-.plyxp_envs` <- function(obj, name, value) {
  attr(obj, name) <- value
  invisible(obj)
}

#' @title New Mask Binding Function
#' @description
#' Utility function to make new bindings on the fly.
#' @param .expr an expression that will be evaluated and bound in `.env_expr`.
#' It is recommened to use `base::quote()` so that `.expr` can contain diffusion
#' operators such as `!!`
#' @param .env_expr the environment that `.expr` is evaluated in, such as a quosure
#' @param .env_bind where the expression will be bound
#' @param type the type of binding function to implement, "standard",
#'  "lazy", or "active"
#' @return a function taking a single argument `name` and performs a binding
#' determined by `type`.
#' @keywords internal
#' @examples
#'
#' # example code
#'
#' env <- new_environment(list(
#'   .iris = iris,
#'   .nrow = 10),
#'   baseenv())
#' binding_func <- add_bind(
#'   .expr = quote(lapply(seq_len(.nrow), function(i, x) x[i], x = .iris[[!!name]])),
#'   .expr_env = env,
#'   type = "lazy")
#' binding_func("Sepal.Width")
#' env$Sepal.Width
#' @noRd
add_bind <- function(.expr, .env_expr,
                     .env_bind = .env_expr,
                     type = c("standard", "lazy","active")) {
  # type <- match.arg(type, c("standard", "lazy", "active"))
  fun <- switch(type,
                standard = expr(env_bind),
                lazy = expr(env_bind_lazy),
                active = expr(env_bind_active))
  name_unquo <- quote(!!name)
  quosure_unquo <- quote(!!quosure)
  if (type == "active") {
    new_function(
      args = alist(name=),
      body = expr({
        name_sym <- as.name(name)
        actv_fun <- new_function(pairlist(),
                                 inject(quote(!!.expr)),
                                 env = !!.env_expr)
        # active_fun <- eval_tidy(quosure, data = as_data_mask(base::baseenv()))
        (!!fun)(!!.env_bind, !!name_unquo := actv_fun)
      }))
  } else {
    new_function(
      args = alist(name=),
      body = expr({
        name_sym <- as.name(name)
        quosure <- new_quosure(expr(!!.expr), env = !!.env_expr)
        (!!fun)(!!.env_bind, !!name_unquo := !!quosure_unquo)
      }))
  }

}

## due to challenges in passing BiocCheck, no longer documenting non-exported
## objects

#' @title `plyxp` Data Mask Object
#' @name plyxp
#' @description
#' An R6 Object that tracks bindings of a list-like object.
#' This includes DFrame objects. There are several inherited
#' environments that the data is stored within.
#' 
#' Environments:
#' 
#' .shared_env --> curr_group_ctx --> foreign --> lazy --> chops --> active_mask
#' 
#' * .shared_env : environment provided at initialization. This may be shared
#'                 with multiple other BiocDataMasks.
#' * curr_group  : Currently not used.
#' * foreign     : space to put foreign bindings, i.e. object unrelated to `.data`
#'                 provided at initialization. This space is currently used to
#'                 place the pronouns into related contexts.
#' * lazy        : A strict lazy binding to the data within `.data`. This binding
#'                 is made only at initialization.
#' * chops       : lazy data but chopped into list by `.indices`. New bindings 
#'                 for this BiocDataMask context are expected to be in a 
#'                 "chopped" format and are assigned here.
#' * active_mask : An active binding to chops in which the proper list index is
#'                 used depending on the current group context. The current group
#'                 context is at this moment determined by the .shared_env NOT
#'                 the curr_group. I have plans to remove the curr_group
#'                 environment.
#'
#' @return an R6 object of class `plyxp`
#' @examples
#' # note: this R6 class is not exported at this moment
#' 
#' mask <- getNamespace("plyxp")$plyxp$new(iris,
#'                      .env_bot = rlang::env(`plyxp:::ctx:::group_id` = 1L))
#' mask$eval(quote(Sepal.Width))
#' 
#' 
#' 
plyxp <- R6::R6Class(
  "plyxp",
  cloneable = FALSE,
  public = list(
    #' @description
    #' Create a plyxp from `.data`. `.data` is chopped by
    #' `.indices`, and environments are built from `.env`
    #' 
    #' @param .data a named list like object to create a mask
    #' @param .indices the indices that will be used to chop `.data`
    #' @param .env_bot an environment that the resulting mask will be built from.
    #' @param .env_top an environment that `.env_bot` inherits from
    initialize = function(.data, .indices = NULL, .env_bot, .env_top = .env_bot) {
      private$.shared_env <- .env_bot
      private$.top_env <- .env_top
      private$.data <- .data
      private$.indices <- .indices
      private$init_current_group_info()

      private$.names <- setNames(nm = names(.data))
      private$.ptype <- lapply(.data, vec_slice, i = 0L)
      private$.env_size <- length(private$.names) + 20L

      private$init_foreign_data()

      private$init_data_lazy()

      private$init_data_chop()

      # get current chop
      private$init_mask_bind()
      
      private$init_environments()
      invisible(self)

    },
    #' @description
    #' appends a callback function that is executed after a value is bound
    #' to this mask. Mainly used to inform other masks of new values
    #' 
    #' @param .fun a function created from `add_bind()`
    on_bind = function(.fun) {
      private$.on_bind <- append(private$.on_bind, .fun)
      invisible(self)
    },
    #' @description
    #' binds value to an name within the chops environment.
    #' 
    #' @param name a character scalar
    #' @param value results from `$eval` in the form of chops
    bind = function(name, value) {
      needs_unbind <- private$.bind_self(name, value)
      # needs_unbind <- private$push(name)
      binding_funcs <- private$.on_bind
      if (needs_unbind) {
        for (i in seq_along(binding_funcs)) {
          func <- binding_funcs[[i]]
          env_unbind(environment(func)$.env_bind, name)
          func(name)
        }
      } else {
        for (func in binding_funcs) {
          func(name)
        }
      }
      invisible(value)
    },
    #' @param name name of binding to retrieve and unchop
    unchop = function(name) {
      if (is.null(private$.indices)) {
        .subset2(private$env_data_chop[[name]], 1L)
      } else {
        list_unchop(
          private$env_data_chop[[name]],
          indices = private$.indices
        )
      }
    },
    #' @return named list of evaluated expression, unchopped
    results = function() {
      added <- private$.added
      names(added) <- added
      lapply(added, self$unchop)
    },
    #' @description
    #' evaluates a quoted expression within a new datamask
    #' @param quo a quosure to evaluate
    #' @param env an environment to search after mask
    eval = function(quo, env = caller_env()) {
      mask <- new_data_mask(private$env_mask_bind, top = private$.top_env)
      eval_tidy(quo, data = mask, env = env)
    }
  ),
  active = list(
    #' @field environments the hierarchy of environments for this mask
    environments = function() {
      private$.environments
    },
    #' @field names the associated names of data in mask
    names = function() {
      private$.names
    },
    #' @field ptype get zero-length slices of underlying data
    ptype = function() {
      private$.ptype
    },
    #' @field added newly added names to the mask
    added = function() {
      private$.added
    }
  ),
  private = list(
    init_current_group_info = function() {
      private$env_current_group_info <- new_environment(
        list(
          .indices = private$.indices
        ),
        private$.shared_env
      )
    },
    init_foreign_data = function() {
      private$env_foreign_data <- new.env(parent = private$env_current_group_info)
    },
    init_data_lazy = function() {
      .data <- private$.data
      # normal data ... do we need it to be lazy??
      private$env_data_lazy <- new.env(
        parent = private$env_foreign_data,
        size = private$.env_size)
      env_bind_lazy(
        private$env_data_lazy,
        !!! lapply(private$.names, function(x) quo(.data[[!!x]])))
    },
    init_data_chop = function() {
      # chops
      private$env_data_chop <- new.env(
        parent = private$env_data_lazy,
        size = private$.env_size)
      private$handle_chops(private$.indices)
      env_bind_lazy(
        private$env_data_chop,
        !!! lapply(private$.names, as.name) |>
          lapply(private$chop_data) |>
          lapply(new_quosure,
                 env = private$env_data_lazy)
      )
    },
    init_mask_bind = function() {
      private$env_mask_bind <- new.env(parent = private$env_data_chop, size = private$.env_size)
      env_bind_active(
        private$env_mask_bind,
        !!! lapply(private$.names,
                   function(name, env) {
                     name <- sym(name)
                     new_function(
                       args = pairlist(),
                       body = expr(.subset2(!!name, `plyxp:::ctx:::group_id`)),
                       env = env
                     )
                   },
                   env = private$env_data_chop)
      )
    },
    init_environments = function() {
      out  <- c(
        list(private$env_mask_bind),
        env_parents(private$env_mask_bind, private$.shared_env)
      )
      class(out) <- c("plyxp_envs", "rlang_envs")
      attr(out, "env_mask_bind") <- private$env_mask_bind
      attr(out, "env_data_chop") <- private$env_data_chop
      attr(out, "env_data_lazy") <- private$env_data_lazy
      attr(out, "env_foreign_data") <- private$env_foreign_data
      attr(out, "env_current_group_info") <- private$env_current_group_info
      private$.environments <- out
      invisible(NULL)
    },
    handle_chops = function(indices) {
      private$.indices <- indices
      private$.ngroups <- if (is.null(indices)) 1L else length(indices)
      fun <- private$get_chop_fun()
      test_output <- fun(test)
      stopifnot(is_expression(test_output))
      # environment(fun) <- private$env_data_lazy
      private$chop_data <- fun
      invisible(self)
    },
    # extending function
    get_chop_fun = function() {
      if (is.null(private$.indices)) {
        function(name) {
          name <- enexpr(name)
          expr(list(!!name))
        }
      } else {
        function(name) {
          name <- enexpr(name)
          expr(vec_chop(!!name, indices = .indices))
        }
      }
    },
    # function to chop data
    chop_data = NULL,
    # @param name scalar character vector
    # @param value vector in the form of chops
    .bind_self = function(name, value) {
      private$env_data_chop[[name]] <- value
      name_sym <- sym(name)
      fun <- new_function(
        args = pairlist(),
        body = expr(.subset2(!!name_sym, `plyxp:::ctx:::group_id`)),
        env = private$env_data_chop
      )
      env_bind_active(
        private$env_mask_bind,
        !!name := fun
      )
      
      
      needs_unbind <- name %in% private$.names
      private$.names[name] <- name
      private$.added[name] <- name
      private$.ptype[[name]] <- vec_slice(value[[1]], 0L)
      invisible(needs_unbind)
      
    },
    .on_bind = list(),

    # data input
    .data = NULL,
    # list of indices
    .indices = NULL,
    # type of grouping, "none", "group"
    .grouped = NULL,
    .ngroups = NULL,
    .environments = NULL,
    #names of `.data`
    .names = NULL,
    #size 0 vectors of `.data`
    .ptype = NULL,
    # newly added names
    .added = character(),
    push = function(name) {
      needs_unbind <- name %in% private$.names
      private$.names <- union(private$.names, name)
      private$.added <- union(private$.added, name)
      invisible(needs_unbind)
    },
    # initial size of environments
    # number of elements of `.data` + 20L
    .env_size = NULL,

    .shared_env = NULL,
    .top_env = NULL, # should at least inherit from `baseenv()`
    #' holds grouping information
    #' for this object
    env_current_group_info = NULL,
    #' foreign bindings
    env_foreign_data = NULL,
    env_data_lazy = NULL,
    env_data_chop = NULL,
    env_mask_bind = NULL
  )
)


#' @title `plyxp` for SummarizedExperiment `assays()`
#' @name BiocDataMask-assays
#' @description
#' A more specialized version of the plyxp R6 object for the 
#' assays list object. This includes chopping and unchopping 
#' of matrix like objects.
#' @return an object inheriting [`plyxp`][plyxp::BiocDataMask].
#' @noRd
plyxp_assay <- R6::R6Class(
  "plyxp_assay",
  inherit = plyxp, 
  cloneable = FALSE,
  public = list(
    #' @description
    #' Create a plyxp from `.data`. `.data` is chopped by
    #' `.indices`, and environments are built from `.env`
    #' 
    #' @param .data a named list like object to create a mask
    #' @param .indices the indices that will be used to chop `.data`
    #' @param .env_bot an environment that the resulting mask will be built from.
    #' @param .env_top an environment that `.env_bot` inherits from
    #' @param .nrow,.ncol the number of rows and columns of each element of `.data` respectively
    initialize = function(.data, .indices, .env_bot, .env_top = .env_bot, .nrow, .ncol) {
      super$initialize(.data, .indices = .indices, .env_bot = .env_bot, .env_top = .env_top)
      env_bind(
        private$env_current_group_info,
        .nrow = .nrow,
        .ncol = .ncol
      )
      private$.nrow <- .nrow
      private$.ncol <- .ncol
      
  

    },
    #' @description
    #' unchop data within the mask, returns a matrix
    #' @param name name of binding to retrieve and unchop
    unchop = function(name) {
      unchopped <- if (is.null(private$.indices)) {
        .subset2(private$env_data_chop[[name]], 1L)
      } else {
        list_unchop(
          lapply(private$env_data_chop[[name]], as.vector),
          indices = private$.indices
        )
      }
      if (is.null(unchopped)) return(unchopped)
      matrix(
        unchopped,
        nrow = private$.nrow,
        ncol = private$.ncol
      )
    }
  ),
  private = list(
    get_chop_fun = function() {
      .indices <- private$.indices
      if (is.null(.indices)) {
        # private$.e <- private$.env_col_chop <- private$env_data_chop
        return(function(name) {
          name <- enexpr(name)
          expr(list(!!name))
        })
      } else {
        type <- attr(.indices, "type")
        private$.ngroups <- nrow(.indices)
        fun <- switch(
          type,
          rowcol = function(name) {
            name <- enexpr(name)
            expr(vec_chop_assays(!!name, .indices))
          },
          row = function(name) {
            name <- enexpr(name)
            expr(vec_chop_assays_row(!!name, .indices))
          },
          col = function(name) {
            name <- enexpr(name)
            expr(vec_chop_assays_col(!!name, .indices))
          }
        )
        return(fun)
      }
    },
    .nrow = NULL,
    .ncol = NULL
  )
)
