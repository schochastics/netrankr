test_that("comparable pairs work ", {
    P <- matrix(1, 5, 5)
    P[lower.tri(P, diag = TRUE)] <- 0
    expect_equal(comparable_pairs(P), 1)
    P <- matrix(0, 5, 5)
    expect_equal(comparable_pairs(P), 0)
    expect_error(comparable_pairs(matrix(3, 4, 4)))
    expect_error(comparable_pairs("a"))
})

test_that("incomparable pairs work ", {
    P <- matrix(1, 5, 5)
    P[lower.tri(P)] <- 0
    expect_equal(incomparable_pairs(P), 0)
    P <- matrix(0, 5, 5)
    expect_equal(incomparable_pairs(P), 1)
    expect_error(incomparable_pairs(matrix(3, 4, 4)))
    expect_error(incomparable_pairs("a"))
})
