# Input validation and helpers shared by the partial ranking functions

# Validates a partial ranking and returns it with a zero diagonal
check_partial_order <- function(P, arg = "P") {
    if (!inherits(P, "Matrix") && !is.matrix(P)) {
        stop(arg, " must be a dense or sparse matrix", call. = FALSE)
    }
    if (nrow(P) != ncol(P)) {
        stop(arg, " must be a square matrix", call. = FALSE)
    }
    if (anyNA(matrix_values(P))) {
        stop(arg, " must not contain NA", call. = FALSE)
    }
    if (!is.binary(P)) {
        stop(arg, " is not a binary matrix", call. = FALSE)
    }
    if (inherits(P, "nsparseMatrix")) {
        # igraph cannot read pattern matrices
        P <- P * 1
    } else if (inherits(P, "Matrix") && !inherits(P, "sparseMatrix")) {
        # nor dense Matrix objects, which have no advantage over base matrices
        P <- as.matrix(P)
    }
    if (any(Matrix::diag(P) != 0)) {
        warning(arg, " has non-zero diagonal entries. Setting them to 0.", call. = FALSE)
        if (inherits(P, "Matrix")) {
            Matrix::diag(P) <- 0
            P <- Matrix::drop0(P)
        } else {
            diag(P) <- 0
        }
    }
    P
}

is.binary <- function(x) {
    all(matrix_values(x) %in% c(0, 1))
}

# stored values of a dense or sparse matrix (without the implicit zeros)
matrix_values <- function(x) {
    if (inherits(x, "nsparseMatrix")) {
        # pattern matrices store no values and are binary by construction
        return(1)
    }
    if (inherits(x, "sparseMatrix")) {
        return(x@x)
    }
    as.vector(x)
}

# Collapses structurally equivalent elements (P[u,v] = P[v,u] = 1) into one.
# `mse[i]` is the row of the reduced matrix that element i belongs to.
collapse_mse <- function(P) {
    MSE <- Matrix::which((P + Matrix::t(P)) == 2, arr.ind = TRUE)
    if (length(MSE) >= 1) {
        MSE <- t(apply(MSE, 1, sort))
        MSE <- MSE[!duplicated(MSE), , drop = FALSE]
        g <- igraph::make_empty_graph(n = nrow(P), directed = FALSE)
        g <- igraph::add_edges(g, c(t(MSE)))
        MSE <- igraph::components(g)$membership
        equi <- which(duplicated(MSE))
        P <- P[-equi, -equi, drop = FALSE]
    } else {
        MSE <- seq_len(nrow(P))
    }
    if (length(unique(MSE)) == 1) {
        stop("all elements are structurally equivalent and have the same rank", call. = FALSE)
    }
    list(P = P, mse = MSE)
}

# Maps expected ranks of the reduced partial order back to all elements.
# Equivalent elements share their class' rank shifted by the class sizes below.
expand_expected <- function(expected, mse) {
    expected_full <- unname(expected[mse])
    for (val in sort(unique(expected_full), decreasing = TRUE)) {
        idx <- which(expected_full == val)
        expected_full[idx] <- expected_full[idx] + sum(duplicated(mse[expected_full <= val]))
    }
    expected_full
}

# Exact expected ranks of all elements from the relative rank probabilities of the
# reduced partial order (`relative[d, c]` = P(d ranked lower than c)). Equivalent
# elements are tied at the highest rank of their class, i.e.
# E[rank(c)] = |c| + sum_d |d| P(d < c)
expand_expected_relative <- function(relative, mse) {
    sizes <- tabulate(mse)
    diag(relative) <- 0
    expected <- sizes + colSums(relative * sizes)
    unname(expected[mse])
}
