

#*******************************************************************************
# Helpers: design + ordering
#*******************************************************************************

.nparld_find_identical_cells <- function(dat, factor_cols) {

  if (length(factor_cols) == 0L) {
    return(character(0))
  }

  cell_id <- interaction(
    dat[, factor_cols, drop = FALSE],
    drop = TRUE,
    lex.order = TRUE
  )

  cells <- split(dat, cell_id)

  cell_vectors <- lapply(
    cells,
    function(x) {

      x <- x[order(x$subject), , drop = FALSE]

      keep <- !is.na(x[[1]])

      setNames(
        x[[1]][keep],
        x$subject[keep]
      )
    }
  )

  nm <- names(cell_vectors)

  if (length(nm) < 2L) {
    return(character(0))
  }

  out <- utils::combn(
    nm,
    2L,
    FUN = function(z) {

      x <- cell_vectors[[z[1L]]]
      y <- cell_vectors[[z[2L]]]

      if (
        identical(names(x), names(y)) &&
        identical(unname(x), unname(y))
      ) {
        paste(z, collapse = " == ")
      } else {
        NA_character_
      }
    }
  )

  out[!is.na(out)]
}

.nparld_build_design <- function(formula, data, subject, replicate = NULL) {

  dat.Model0 <- model.frame(formula, data = data, na.action = na.pass)

  # subject column
  if (!(subject %in% names(data))) {
    stop("Subject variable '", subject, "' not found in data.", call. = FALSE)
  }
  dat.Model0$subject <- as.factor(data[[subject]])

  # optional replicate column (NOT part of the design factors)
  if (!is.null(replicate)) {
    if (!(replicate %in% names(data))) {
      stop("replicate variable '", replicate, "' not found in data.", call. = FALSE)
    }
    dat.Model0$replicate <- as.factor(data[[replicate]])
  }

  N <- nlevels(dat.Model0$subject)

  # design variables = all columns except response, subject, replicate
  response_name <- names(dat.Model0)[1]
  design_cols <- setdiff(names(dat.Model0), c(response_name, "subject", "replicate"))

  for (nm in design_cols) dat.Model0[[nm]] <- as.factor(dat.Model0[[nm]])

  # safety: no missing values in design factors
  bad_design <- design_cols[vapply(dat.Model0[design_cols], function(z) any(is.na(z)), logical(1))]
  if (length(bad_design) > 0) {
    stop(
      "Missing values in design factors are not allowed: ",
      paste(bad_design, collapse = ", "),
      call. = FALSE
    )
  }

  bysubjects <- split(dat.Model0, dat.Model0$subject)

  # helper to create a subject-specific schedule signature
  .schedule_signature <- function(df, cols) {
    if (length(cols) == 0L) return("")
    sch <- unique(df[cols])
    sch[] <- lapply(sch, as.character)
    sch <- sch[do.call(order, sch), , drop = FALSE]
    row_sig <- do.call(paste, c(sch, sep = "\r"))
    paste(row_sig, collapse = "\n")
  }

  # determine which factors vary within subject
  factor_info <- lapply(design_cols, function(nm) {
    nuniq <- vapply(bysubjects, function(df) length(unique(df[[nm]])), integer(1))
    list(nuniq = nuniq)
  })
  names(factor_info) <- design_cols

  WP.names <- names(Filter(function(info) all(info$nuniq == 1L), factor_info))
  SP.names <- names(Filter(function(info) all(info$nuniq > 1L), factor_info))

  # factors that vary within some subjects but not others -> invalid schedule
  bad_mix <- names(Filter(function(info) {
    !(all(info$nuniq == 1L) || all(info$nuniq > 1L))
  }, factor_info))

  if (length(bad_mix) > 0) {
    stop(
      "Inconsistent within-subject variation for factor(s): ",
      paste(bad_mix, collapse = ", "),
      ". Some subjects are missing subplot levels.",
      call. = FALSE
    )
  }

  n.wholeplots <- length(WP.names)
  n.subplots <- length(SP.names)

  if (n.subplots == 0L) {
    stop("There is no subplot factor provided. Please check the model! Otherwise, please use the package rankFD for the analysis of independent observations.", call. = FALSE)
  }

  # strict schedule check, but ignoring replicate replication:
  # every subject must have the same unique subplot combinations
  sp_sig <- vapply(bysubjects, .schedule_signature, character(1), cols = SP.names)

  if (length(unique(sp_sig)) != 1L) {
    ref <- sp_sig[1]
    bad <- names(sp_sig)[sp_sig != ref]
    stop(
      "Subjects do not share the same subplot schedule. Please check subject(s): ",
      paste(bad, collapse = ", "),
      ". Unequal replicate sizes are allowed, but missing subplot combinations are not.",
      call. = FALSE
    )
  }

  if (n.wholeplots == 0) {
    Design <- paste("LD-F", n.subplots, "-Design \n", sep = "")
  }
  if (n.wholeplots > 0 && n.subplots > 0) {
    Design <- paste("F", n.wholeplots, "-LD-F", n.subplots, "-Design \n", sep = "")
  }

  # sort order: replicate may be used only for stable ordering
  sort_order <- c(WP.names, "subject", SP.names, if (!is.null(replicate)) "replicate" else NULL)
  dat.Model.sorted <- dat.Model0[do.call(order, dat.Model0[sort_order]), , drop = FALSE]

  factor_cols <- c(WP.names, SP.names)
  n.levels <- sapply(factor_cols, function(x) nlevels(dat.Model.sorted[[x]]))
  names.levels <- lapply(factor_cols, function(x) levels(dat.Model.sorted[[x]]))
  names(names.levels) <- factor_cols

  # rewrite formula in correct factor order (WP first, SP second)
  new_formula <- as.formula(
    paste(response_name, "~", paste(factor_cols, collapse = "*"))
  )

  list(
    dat = dat.Model.sorted,
    formula = new_formula,
    WP.names = WP.names,
    SP.names = SP.names,
    factor_cols = factor_cols,
    n.levels = n.levels,
    names.levels = names.levels,
    N = N,
    N.info = N,
    Design = Design,
    wholeplots = WP.names,
    subplots = SP.names
  )
}

.nparld_make_keys <- function(df, fac_names){
  if(length(fac_names)==0)
    return(rep("1", nrow(df)))

  do.call(paste,
          c(lapply(fac_names, function(v) as.character(df[[v]])),
            sep=":"))
}

.nparld_cell_keys <- function(dat, WP.names, SP.names){

  WP.names <- as.character(WP.names)
  SP.names <- as.character(SP.names)

  fac_names <- c(WP.names, SP.names)

  if(length(fac_names)==0)
    return("1")

  # expand.grid trick: reverse order so that the last factor varies fastest
  grid <- expand.grid(
    lapply(rev(fac_names), function(v) levels(dat[[v]])),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  names(grid) <- rev(fac_names)

  # restore original factor order
  grid <- grid[fac_names]

  # build keys
  cell_keys <- do.call(
    paste,
    c(lapply(grid, as.character), sep=":")
  )

  cell_keys
}
