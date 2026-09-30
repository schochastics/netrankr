#' @title Rank interval of nodes
#' @description Calculate the maximal and minimal rank possible for each node
#'    in any ranking that is in accordance with the partial ranking `P`.
#' @param P A partial ranking as matrix object calculated with [neighborhood_inclusion]
#'    or [positional_dominance].
#' @details For exact rank probabilities use [exact_rank_prob].
#' @return An object of type netrankr_interval, a data frame with columns
#' `node`, `min_rank` and `max_rank`.
#' @author David Schoch
#' @seealso [exact_rank_prob]
#'
#' @examples
#' P <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)
#' rank_intervals(P)
#' @export
rank_intervals <- function(P) {
    P <- check_partial_order(P)

    n <- nrow(P)
    max_rank_all <- n - Matrix::rowSums((P - Matrix::t(P)) == 1) - Matrix::rowSums(P == 1 & Matrix::t(P) == 1) # CAUTION!!!!
    min_rank_all <- Matrix::colSums((P - Matrix::t(P)) == 1) + 1

    if (is.null(rownames(P))) {
        names <- paste0("V", seq_len(nrow(P)))
    } else {
        names <- rownames(P)
    }

    res <- data.frame(
        node = names,
        min_rank = min_rank_all,
        max_rank = max_rank_all
    )
    class(res) <- c("netrankr_interval", class(res))
    return(res)
}
