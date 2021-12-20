#' @title Summary of a netrankr_full object
#' @description Summarizes the result of an object obtained from [exact_rank_prob] to terminal
#' @param object A netrankr_full object
#' @param ... additional arguments to summary
#' @author David Schoch
#' @export
summary.netrankr_full <- function(object, ...) {

  # if(!"netrankr_full"%in% class(x)){
  #   stop("x is not a netrankr_full object")
  # }
  n <- length(object$mse)
  cat("Number of possible centrality rankings: ", object$lin.ext, "\n")
  cat("Equivalence Classes (max. possible): ", length(unique(object$mse)), " (", n, ")\n", sep = "")
  # cat(rep("-",pmin(n,10)),"\n",sep = "")
  # cat("Rank Probabilities (rows:nodes/cols:ranks)\n")
  # cat(apply(object$rank.prob,1,print_bars),sep="\n")
  # cat(rep("-",pmin(n,10)),"\n",sep = "")
  # cat("Relative Rank Probabilities (row ranked lower than col)\n")
  # cat(apply(object$relative.rank,1,print_bars),sep="\n")
  # cat(rep("-",pmin(n,10)),"\n",sep = "")
  # cat("Expected Ranks (higher values are better)\n")
  # cat(print_bars(object$expected.rank,min = 1,max = length(unique(object$mse))),sep="\n")
  # cat(rep("-",pmin(n,10)),"\n",sep = "")
  # cat("SD of Rank Probabilities\n")
  # cat(print_bars(object$rank.spread,min = 0,max = max(object$rank.spread)),sep="\n")
  # cat(rep("-",pmin(n,10)),"\n",sep = "")
  # if(!is.null(object$topo.order)){
  #   cat("(no summary method available for intermediate data structures)")
  # }
  invisible(object)
}

#' @title Print netrankr_full object to terminal
#' @description Prints the result of an object obtained from [exact_rank_prob] to terminal
#' @param x A netrankr_full object
#' @param ... additional arguments to print
#' @author David Schoch
#' @export
print.netrankr_full <- function(x, ...) {

  # if(!"netrankr_full"%in% class(x)){
  #   stop("x is not a netrankr_full object")
  # }
  n <- length(x$mse)
  cat("Number of possible centrality rankings: ", x$lin.ext, "\n")
  cat("Equivalence Classes (max. possible): ", length(unique(x$mse)), " (", n, ")\n", sep = "")
  cat(rep("-", 10), "\n")
  cat("Rank Probabilities (rows:nodes/cols:ranks)\n")
  print(x$rank.prob)
  cat(rep("-", 10), "\n")
  cat("Relative Rank Probabilities (row ranked lower than col)\n")
  print(x$relative.rank)
  cat(rep("-", 10), "\n")
  cat("Expected Ranks (higher values are better)\n")
  print(x$expected.rank)
  cat(rep("-", 10), "\n")
  cat("SD of Rank Probabilities\n")
  print(x$rank.spread)
  cat(rep("-", 10), "\n")
  if (!is.null(x$topo.order)) {
    cat("(no printing method available for intermediate data structures)")
  }
  invisible(x)
}

#' @title Plot netrankr_full object
#' @importFrom graphics arrows barplot image legend par points layout axis box
#' @importFrom utils stack
#' @description Plots the result of an object obtained from [exact_rank_prob]
#' @param x A netrankr_full object
#' @param icols a list of colors (an internal palette is used if missing)
#' @param bcol color used for the barcharts
#' @param ecol color used for errorbars
#' @param ... additional plot parameters
#' @author David Schoch
#' @export
plot.netrankr_full <- function(x, icols = NULL, 
                               bcol = "grey66", ecol = "black", ...) {
  if(is.null(icols)){
    icols <- c("#4B0055", "#45256B", "#30437F", "#005F8E", "#007896", "#009097", 
               "#00A691", "#00B983", "#3AC96D", "#8BD650", "#C8DF32", "#FDE333"
    )
  }
  op <- par(no.readonly = TRUE)

  layout.matrix <- matrix(c(1, 1, 2, 2, 5, 3, 3, 4, 4, 0), nrow = 2, ncol = 5, byrow = TRUE)

  layout(
    mat = layout.matrix,
    heights = c(2, 2),
    widths = c(2, 2, 2, 2, 1)
  )

  rk_pr <- x$rank.prob
  rk_pr[rk_pr == 0] <- NA
  rk_rl <- x$relative.rank
  rk_rl[rk_rl == 0] <- NA

  par(mar = c(5, 4, 2, 2), las = 1) # bltr
  image(
    x = 1:ncol(rk_pr), y = 1:nrow(rk_pr), t(rk_pr), col = icols, xlab = "ranks",
    ylab = "nodes", tck = 0, breaks = seq(0, 1, length.out = length(icols) + 1), main = "rank probabilities"
  )

  par(mar = c(5, 2, 2, 2), las = 1) # bltr
  image(
    x = 1:ncol(rk_rl), y = 1:nrow(rk_rl), t(rk_rl), col = icols, xlab = "",
    ylab = "", tck = 0, breaks = seq(0, 1, length.out = length(icols) + 1), main = "relative rank probabilities"
  )

  par(mar = c(2, 2, 5, 2)) # bltr
  mid <- barplot(x$expected.rank, main = "expected ranks", col = bcol, ylim = c(0, nrow(rk_pr)))
  sd_min <- pmax(x$expected.rank - x$rank.spread, 0)
  sd_max <- pmin(x$expected.rank + x$rank.spread, nrow(rk_pr))
  idx <- !(sd_min == sd_max)

  # par(mar = c(2, 2, 5, 1))#bltr
  barplot(x$expected.rank, main = "expected ranks and spread", col = bcol, ylim = c(0, nrow(rk_pr)))
  arrows(x0 = mid[idx], y0 = sd_min[idx], x1 = mid[idx], y1 = sd_max[idx], code = 3, angle = 90, length = 0.1, col = ecol)


  breaks <- seq(0, 1, length.out = length(icols) + 2)
  ix <- 1:2
  iy <- breaks
  nBreaks <- length(breaks)
  midpoints <- (breaks[1:(nBreaks - 1)] + breaks[2:nBreaks]) / 2
  iz <- matrix(midpoints, nrow = 1, ncol = length(midpoints))

  par(mar = c(1, 2, 1, 3)) # bltr
  image(ix, iy, iz,
    axes = FALSE, xlab = "",
    ylab = "", col = c("white", icols), breaks = breaks
  )
  axis(4)
  box()

  on.exit(par(op))
  invisible(NULL)
}

#' @title Extract probabilities from netrankr_full object
#' @description extract probabilities as matrices from the result of an object obtained from [exact_rank_prob]
#' @param x A netrankr_full object
#' @param type which probabilities to return. "rank" for rank probabilities, "relative" for relative rank probabilities and "expected" for expected rank probabilities and their variants
#' @param ... additional parameters for as.matrix
#' @author David Schoch
#' @export
as.matrix.netrankr_full <- function(x, type = "rank", ...) {
  type <- match.arg(type, c("rank", "relative", "expected"))
  mat <- switch(type,
    "rank" = x$rank.prob,
    "relative" = x$relative.rank,
    "expected" = cbind(x$expected.rank, x$rank.spread)
  )
  mat
}

#' @title Print netrankr_interval object to terminal
#' @description Prints the result of an object obtained from [rank_intervals] to terminal
#' @param x A netrankr_interval object
#' @param ... additional arguments to print
#' @author David Schoch
#' @export
print.netrankr_interval <- function(x, ...) {
  # if(!"netrankr_interval"%in% class(x)){
  #   stop("x is not a netrankr_interval object")
  # }
  prows <- min(c(nrow(x), getOption("max.print")))
  int_string <- paste0("node:", x$node, " rank interval: [", x$min_rank, ", ", x$max_rank, "] mid point: ", x$mid_point, "\n", sep = "")
  int_string <- int_string[1:prows]
  cat("", int_string)
}

#' @title plot netrankr_interval objects
#' @importFrom graphics arrows legend par points
#' @importFrom utils stack
#' @description Plots results from [rank_intervals]
#' @param x A netrank object
#' @param cent_scores A data frame containing centrality scores of indices (optional)
#' @param cent_cols colors for centrality indices. If NULL a default palette is used. Length must be equal to columns in cent_scores.
#' @param ties.method how to treat ties in the rankings. see [rank] for details
#' @param ... additional arguments to plot
#' @author David Schoch
#' @export
plot.netrankr_interval <- function(x, cent_scores = NULL, cent_cols = NULL, ties.method = "min", ...) {
  ord <- order(x$mid_point)
  if (!is.null(cent_scores)) {
    m <- ncol(cent_scores)
    op <- par(mar = c(4, 4, 6, 4), xpd = TRUE)
    cent_rk_wide <- as.data.frame(apply(cent_scores[ord, ], 2, rank, ties.method = ties.method))
    cent_rk_long <- cbind(id = (1:nrow(x)), stack(cent_rk_wide))
    if (is.null(cent_cols)) {
      if (m <= 8) {
        cent_cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")[1:m]
      } else {
        warning("plotting more than 8 rankings is not recommended")
        cent_cols <- c("#FFC5D0", "#FBC9BF", "#F1CEB0", "#E2D4A8", "#CFDAA8", "#BBDEB1", 
                       "#A8E1BF", "#9BE2D0", "#9AE1E1", "#A4DDEF", "#B8D8F8", "#CFD1FC", 
                       "#E4CBF9", "#F4C6EF", "#FDC4E1")[1:ncol(cent_scores)]
      }
    } else if (length(cent_cols) != ncol(cent_scores)) {
      stop("the number of colors must equal the number of columns in cent_score.")
    }
  }
  plot(
    x = factor(x$node, levels = x$node[ord]),
    y = x$mid_point, pch = 0,
    ylim = c(0, nrow(x)),
    ylab = "ranks",
    xlab = "",
    ...
  )
  idx <- !(x$min_rank[ord] == x$max_rank[ord])
  arrows(
    x0 = (1:nrow(x))[idx],
    y0 = x$min_rank[ord[idx]],
    x1 = (1:nrow(x))[idx],
    y1 = x$max_rank[ord[idx]], code = 3, angle = 90, length = 0.1, col = "black"
  )
  if (!is.null(cent_scores)) {
    points(
      x = jitter(cent_rk_long$id, 0.5), y = cent_rk_long$values,
      col = cent_cols[as.integer(cent_rk_long$ind)],
      pch = 20, cex = 1.5
    )
    legend("top",
      inset = c(0, -0.15), bty = "n", horiz = TRUE, pt.cex = 2,
      legend = names(cent_rk_wide),
      col = cent_cols, pch = 20
    )
    on.exit(par(op))
  }
  invisible(NULL)
}


#' @title Print netrankr_mcmc object to terminal
#' @description Prints the result of an object obtained from [mcmc_rank_prob] to terminal
#' @param x A netrank object
#' @param ... additional arguments to print
#' @author David Schoch
#' @export
print.netrankr_mcmc <- function(x, ...) {

  # if(!"netrankr_mcmc"%in% class(x)){
  #   stop("x is not a netrankr_mcmc object")
  # }
  # maxpr  <- getOption("width")
  cat("MCMC approximated Relative Rank Probabilities (row ranked lower than col)\n")
  print(x$relative.rank)
  cat(rep("-", 10), "\n")
  cat("MCMC approximated Expected Ranks (higher values are better)\n")
  print(x$expected.rank)
  cat(rep("-", 10), "\n")
  invisible(x)
}

#' @title Plot netrankr_mcmc object
#' @importFrom graphics barplot image par layout
#' @description Plots the result of an object obtained from [mcmc_rank_prob]
#' @param x A netrankr_mcmc object
#' @param icols a list of colors (an internal)
#' @param bcol color used for the barcharts
#' @param ... additional plot parameters
#' @author David Schoch
#' @export
plot.netrankr_mcmc <- function(x, icols = NULL, bcol = "grey66", ...) {
  op <- par(no.readonly = TRUE)

  if(is.null(icols)){
    icols <- c("#4B0055", "#45256B", "#30437F", "#005F8E", "#007896", "#009097", 
               "#00A691", "#00B983", "#3AC96D", "#8BD650", "#C8DF32", "#FDE333"
    )
  }
  
  layout.matrix <- matrix(c(1, 2, 3, 0), nrow = 2, ncol = 2, byrow = TRUE)

  layout(
    mat = layout.matrix,
    heights = c(4, 1),
    widths = c(2, 2)
  )

  rk_rl <- x$relative.rank
  rk_rl[rk_rl == 0] <- NA
  par(mar = c(1, 2, 2, 2), las = 1) # bltr
  image(
    x = 1:ncol(rk_rl), y = 1:nrow(rk_rl), t(rk_rl), col = icols, xlab = "",
    ylab = "", tck = 0, breaks = seq(0, 1, length.out = length(icols) + 1),
    main = "MCMC approximated relative rank probabilities"
  )
  barplot(x$expected.rank,
    main = "MCMC approximated expected ranks",
    col = bcol, ylim = c(0, ncol(rk_rl))
  )

  breaks <- seq(0, 1, length.out = length(icols) + 2)
  ix <- 1:2
  iy <- breaks
  nBreaks <- length(breaks)
  midpoints <- (breaks[1:(nBreaks - 1)] + breaks[2:nBreaks]) / 2
  iz <- matrix(midpoints, nrow = 1, ncol = length(midpoints))

  par(mar = c(2, 1, 2, 1)) # bltr
  image(iy, ix, t(iz),
    yaxt = "n", xlab = "",
    ylab = "", col = c("white", icols), breaks = breaks, useRaster = TRUE
  )

  on.exit(par(op))
  invisible(NULL)
}


# print_bars <- function(x,min=0,max=1){
#   bars <- c(
#     "bar_0" = " ",
#     "bar_1" = "\u2581",
#     "bar_2" = "\u2582",
#     "bar_3" = "\u2583",
#     "bar_4" = "\u2584",
#     "bar_5" = "\u2585",
#     "bar_6" = "\u2586",
#     "bar_7" = "\u2587",
#     "bar_8" = "\u2588"
#   )
#   w <- getOption("width")
#   # x_bars <- paste0(bars[cut(x,breaks = 8)],collapse = "")
#   x_bars <- bars[cut(c(min,x,max),breaks = 9)]
#   x_bars <- paste0(x_bars[-c(1,length(x_bars))],collapse="")
#   if(nchar(x_bars)>w){
#     paste0(substr(x_bars,1,w-1),"\u22EF")
#   } else{
#     x_bars
#   }
# }

is.binary <- function(x) {
  if (inherits(x, "Matrix")) {
    all(x@x == 1)
  } else {
    all(x %in% c(0, 1))
  }
}
