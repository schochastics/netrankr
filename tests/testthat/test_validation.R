P5 <- matrix(c(0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, rep(0, 10)), 5, 5, byrow = TRUE)

test_that("reflexive partial rankings are treated like irreflexive ones", {
    Pr <- P5 + diag(5)
    expect_warning(res <- exact_rank_prob(Pr), "diagonal")
    expect_equal(res, exact_rank_prob(P5))
    expect_warning(expect_equal(approx_rank_expected(Pr), approx_rank_expected(P5)), "diagonal")
    expect_warning(expect_equal(approx_rank_relative(Pr), approx_rank_relative(P5)), "diagonal")
    expect_warning(expect_equal(rank_intervals(Pr), rank_intervals(P5)), "diagonal")
    expect_warning(expect_equal(comparable_pairs(Pr), comparable_pairs(P5)), "diagonal")
    set.seed(1)
    expect_warning(m1 <- mcmc_rank_prob(Pr, rp = 100), "diagonal")
    set.seed(1)
    expect_equal(m1, mcmc_rank_prob(P5, rp = 100))
})

test_that("invalid partial rankings are rejected", {
    expect_error(exact_rank_prob("a"), "dense or sparse")
    expect_error(exact_rank_prob(P5[1:4, ]), "square")
    Pna <- P5
    Pna[1, 2] <- NA
    expect_error(exact_rank_prob(Pna), "NA")
    expect_error(approx_rank_expected(Matrix::Matrix(Pna, sparse = TRUE)), "NA")
    expect_error(comparable_pairs(P5 * 2), "binary")
    expect_error(approx_rank_expected(P5, method = "foo"), "should be one of")
})

test_that("sparse and dense Matrix input give the same results as base matrices", {
    inputs <- list(
        sparse = Matrix::Matrix(P5, sparse = TRUE),
        dense = Matrix::Matrix(P5, sparse = FALSE),
        pattern = methods::as(Matrix::Matrix(P5, sparse = TRUE), "nMatrix")
    )
    for (P in inputs) {
        expect_equal(exact_rank_prob(P), exact_rank_prob(P5))
        for (m in c("lpom", "glpom", "loof1", "loof2")) {
            expect_equal(approx_rank_expected(P, m), approx_rank_expected(P5, m))
        }
        expect_equal(approx_rank_relative(P), approx_rank_relative(P5))
        expect_equal(comparable_pairs(P), comparable_pairs(P5))
        expect_equal(transitive_reduction(P), transitive_reduction(P5))
        expect_true(is_preserved(P, 1:5))
    }
    expect_equal(
        rank_intervals(inputs$sparse)[, c("min_rank", "max_rank")],
        rank_intervals(P5)[, c("min_rank", "max_rank")]
    )
})

test_that("loof approximations match the reference values", {
    expect_equal(round(approx_rank_expected(P5, "loof1"), 4), c(1.3333, 2.1429, 2.9167, 4.2500, 4.3571))
    expect_equal(round(approx_rank_expected(P5, "loof2"), 4), c(1.3396, 2.2036, 2.9103, 4.1977, 4.3488))
})

test_that("is_preserved validates scores", {
    expect_error(is_preserved(P5, 1:2), "one entry per row")
    expect_error(is_preserved(P5, c(1:4, NA)), "NA")
    expect_true(is_preserved(P5, colSums(P5)))
})

test_that("compare_ranks rejects NA and non-numeric input", {
    expect_error(compare_ranks(c(1, NA, 3), 1:3), "NA")
    expect_error(compare_ranks(letters[1:3], 1:3), "numeric")
    expect_equal(compare_ranks(1:3, 1:3)$concordant, 3)
})

test_that("get_rankings handles linear orders and gives a correct hint", {
    P <- matrix(0, 3, 3)
    P[upper.tri(P)] <- 1
    res <- suppressWarnings(exact_rank_prob(P, only.results = FALSE))
    expect_equal(get_rankings(res), matrix(c(1, 2, 3), ncol = 1))
    res <- exact_rank_prob(P5, only.results = FALSE)
    expect_equal(ncol(get_rankings(res)), res$lin.ext)
    res$lin.ext <- 1e6
    expect_error(get_rankings(res), "force = TRUE")
})

test_that("two-mode positional dominance honours benefit, map and names", {
    A <- matrix(c(1, 2, 3, 2, 3, 4, 3, 1, 2), 3, 3, byrow = TRUE,
                dimnames = list(c("a", "b", "c"), c("x", "y", "z")))
    A <- cbind(A, w = c(0, 1, 0))
    D <- positional_dominance(A, type = "two-mode")
    expect_equal(dimnames(D), list(c("a", "b", "c"), c("a", "b", "c")))
    expect_equal(unname(D), matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0), 3, 3, byrow = TRUE))
    D_cost <- positional_dominance(A, type = "two-mode", benefit = FALSE)
    expect_equal(unname(D_cost), t(unname(D)))
    D_map <- positional_dominance(A, type = "two-mode", map = TRUE)
    expect_equal(unname(D_map), matrix(c(0, 1, 1, 0, 0, 0, 1, 1, 0), 3, 3, byrow = TRUE))
    expect_error(positional_dominance(A, type = "three-mode"), "should be one of")
})

test_that("neighborhood_inclusion simplifies graphs with loops and multi-edges", {
    g <- igraph::make_graph(c(1, 2, 2, 3), directed = FALSE)
    P <- neighborhood_inclusion(g)
    g_loop <- igraph::add_edges(g, c(3, 3, 1, 2))
    expect_warning(P_loop <- neighborhood_inclusion(g_loop), "simplify")
    expect_equal(P_loop, P)
    expect_equal(dim(neighborhood_inclusion(igraph::make_empty_graph(0, directed = FALSE))), c(0, 0))
})

test_that("S3 methods work", {
    res <- exact_rank_prob(P5)
    expect_output(summary(res), "Number of possible centrality rankings")
    expect_output(print(res), "Expected Ranks")
    ri <- rank_intervals(P5)
    expect_output(out <- withVisible(print(ri)), "node:V1 rank interval")
    expect_false(out$visible)
    expect_identical(out$value, ri)
    set.seed(1)
    mc <- mcmc_rank_prob(P5, rp = 100)
    expect_output(print(mc), "MCMC")
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    op <- graphics::par(no.readonly = TRUE)
    expect_no_error(plot(res))
    expect_no_error(plot(mc))
    expect_no_error(plot(ri))
    expect_no_error(plot(ri, cent_scores = data.frame(a = 5:1)))
    expect_warning(plot(ri, cent_scores = as.data.frame(matrix(runif(5 * 20), 5, 20))), "more than 8")
    expect_equal(graphics::par("mar"), op$mar)
})
