#' @param ctx character name of the source contest
#' @param from character name of context we are viewing `ctx` from
#' @param ... additional bindings from the `from` context that should be
#' made available to the new `view`.
#' @param mapper a mapping of `from`'s index to the corresponding chop index
#' (or indices) in `ctx`. use the `.mapper` variable to retrieve the indices
#' for `ctx`. the mapper function is bound to the `from`'s bottom environment.
#' @param asis an expression on how `ctx` should be viewed from the `from`
#' context. The `asis` expression is lazily evaluated in `ctx`'s chop
#' environment, but bound to an environment that is a child of `from`'s bottom
#' enviornment. note: `.x` will be replaced dynamically with the correct name.
#' Only symbols found within the `ctx`'s mask will be evaluated, thus the
#' developer is responsible for how dynamic these bindings are.
#' @param mold an expression on how to change `asis` to fit the `from` context
new_view_spec <- function(ctx, from, ...,
                          mapper = ~.x,
                          # lazy binding of new chops by from groups
                          #
                          asis = ~ .subset(.x, 1L),
                          reshape = asis) {
  structure(
    list(
      ctx = ctx,
      from = from,
      installs = rlang::enexprs(..., .named = TRUE),
      mapper = mapper,
      asis = asis,
      reshape = reshape
    ),
    class = "view_spec"
  )
}

print.view_spec <- function(x) {
  cat("View - `", x$ctx, "` from `", x$from, "`\n", sep = "")
}


m_assays <- bm$masks[["assays"]]
m_rows <- bm$masks[["rows"]]
m_cols <- bm$masks[["cols"]]

.view <- new_view_spec(
  "rows", "assays",
  mapper = ~ {
    # browser()
    .subset2(attr(.indices, "biocmask:::row_chop_ind"), .x)
  },
  asis = ~ {
    browser()
    chops <- .subset2(.x, .mapper)
    chops
  }
)


link_view <- function(bm, .view) {
  # browser()
  masks <- bm$masks
  # envs of target mask
  view_envs <- masks[[.view$ctx]]$environments
  # envs of current mask
  from_envs <- masks[[.view$from]]$environments
  ctx_chops <- view_envs@env_data_chop
  env_view <- env(ctx_chops)
  #
  env_from <- env(from_envs@env_mask_bind)
  # setup map function
  env_from$.map_fn <- new_function(pairlist2(.x = ), f_rhs(.view$mapper),
    env = env_from
  )
  env_bind_lazy(
    env_from,
    !!!lapply(.view$installs,
      new_quosure,
      env = env_from
    )
  )

  # if ...cache_map is ever triggered
  # the .map_fn will evaluate for ALL groups, unbinding .map_fn
  # and saving the results into the env_view.
  # this will be triggered by .mapper active binding in env_view.
  env_bind_lazy(
    env_view,
    .__cache_map__. = !!new_quosure(
      expr({
        .res <- lapply(
          seq_len(
            .subset2(`biocmask:::ctx:::n_groups`, !!.view$from)
          ), .map_fn
        )
        # ideally this releases the environment made from `from`  ctx
        rlang::env_unbind(
          rlang::env_parent(n = 2),
          ".map_fn"
        )
        .res
      }),
      env_from
    ),
    .__uniq_indx__. = !!new_quosure(
      expr({
        # if from context will always map to a small set
        # of indices, this ensures we don't store a very
        # redundant list
        match(.__cache_map__., .__uniq_map__.)
      }),
      env_view
    ),
    .__n_uniq__. = !!new_quosure(
      # cache the upper size of the list
      expr(max(.__uniq_indx__.)),
      env_view
    ),
    .__uniq_map__. = !!new_quosure(
      # smaller list, the range of `.__uniq_map__.`
      expr(unique(.__cache_map__.)),
      env_view
    ),
    .__from_n_groups__. = !!new_quosure(expr(length(.indices)), env_from),
    !!!names(.view$installs) |>
      rlang::set_names() |>
      syms() |>
      lapply(new_quosure, env = env_from)
  )
  env_bind_active(
    env_view,
    # .mapper will tell us which index of .__uniq_map__.
    # we are in given any `biocmask:::ctx:::group_id` value.
    # helpful to access the correct chops handled by this function
    .mapper = new_function(
      pairlist(),
      # all contexts are dictated by some global "group_id"
      # to translate to the new view context we map
      # `biocmask:::ctx:::group_id` -> .__uniq_indx__.
      # .__uniq_indx__. -> .__uniq_map__.
      expr(.subset2(.__uniq_indx__., `biocmask:::ctx:::group_id`)),
      env_view
    )
  )

  chops_view <- env(env_view)
  # new call per new binding in the target context ("view")
  # caches the subsets of whatever binding of "name_sym"
  new_chops_lazy_bind <- biocmask:::add_bind(
    .expr = quote(lapply(.__uniq_map__., \(.x) .subset(!!name_sym, .x))),
    .env_expr = env_view,
    .env_bind = chops_view,
    type = "lazy"
  )
  asis_view <- new.env(parent = env_view)
  asis_lazy_bind <- biocmask:::add_bind(
    .expr = substitute(
      lapply(!!name_sym, \(.x) .y),
      list(.y = rlang::f_rhs(.view$asis))
    ),
    .env_expr = chops_view,
    .env_bind = asis_view,
    type = "lazy"
  )
  asis_access_view <- new.env(parent = env_view)
  # only called ONCE per new symbol. ensures the
  # below lazy bind is accessed
  asis_access_bind <- biocmask:::add_bind(
    .expr = quote(.subset2(!!name_sym, .mapper)),
    .env_expr = asis_view,
    .env_bind = asis_access_view,
    type = "active"
  )

  reshape_view <- new.env(parent = asis_access_view)
  reshape_lazy_bind <- biocmask:::add_bind(
    # reshape should be same length as `biocmask:::ctx:::n_group`
    .expr = substitute(
      .subset(!!name_sym, .__uniq_indx__.) |>
        Map(\(.x, .i) .y,
          .x = _,
          .i = seq_len(.__from_n_groups__.)
        ),
      list(.y = rlang::f_rhs(.view$reshape))
    ),
    # we don't use the asis_access_view because that
    # will return the exact value for the current group
    # this expression evaluates for all groups at once to
    # cache.
    .env_expr = asis_view,
    .env_bind = reshape_view,
    type = "lazy"
  )
  reshape_access_view <- new.env(parent = reshape_view)
  reshape_access_bind <- biocmask:::add_bind(
    .expr = quote(.subset2(!!name_sym, `biocmask:::ctx:::group_id`)),
    .env_expr = reshape_view,
    .env_bind = reshape_access_view,
    type = "active"
  )

  out <- new_environment(list(
    on_new_bind = list(
      asis = asis_access_bind,
      reshape = reshape_access_bind
    ),
    on_bind = list(
      chop = new_chops_lazy_bind,
      asis = asis_lazy_bind,
      reshape = reshape_lazy_bind
    ),
    top_env = env_view,
    bot_env = reshape_view,
    asis_access = asis_access_view,
    reshape_access = reshape_access_view
  ), parent = baseenv())
  out$bind_new <- new_function(
    alist(name = ),
    expr({
      lapply(on_bind, \(fn, nm) fn(nm), nm = name)
      lapply(on_new_bind, \(fn, nm) fn(nm), nm = name)
      invisible(NULL)
    }),
    env = out
  )
  out$rebind <- new_function(
    alist(name = ),
    expr({
      lapply(on_bind, \(fn, nm) fn(nm), nm = name)
      invisible(NULL)
    }),
    env = out
  )

  class(out) <- "View"
  out
}

# example of viewing "rows" from the "assays" context
#' bm <- biocmask:::new_biocmask_manager(group_by(se_simple, rows(direction)))
#' V <- link_view(bm,
#'           new_view_spec(
#'             "rows", "assays",
#'             # installs from `from`
#'             .__from_n_groups__. = length(.indices),
#'             .row_ind = attr(.indices, "biocmask:::row_chop_ind"),
#'             .ncols = vapply(attr(.indices, "biocmask:::col_chop_ind"), length, 1L),
#'             .new_map = match(.row_ind, .row_ind),
#'             # required args
#'             mapper = ~ .new_map[.x],
#'             # should only expect a single chop in this context
#'             # default is fine
#'             asis = ~ .subset2(.x, 1L),
#'             reshape = ~ vec_rep(.x, times = .ncols[.i])
#'           ))
#' V$bind_new("length")
#'
