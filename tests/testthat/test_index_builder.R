test_that("index_builder_code generates runnable code", {
    code <- index_builder_code("net", "dist_walk", "dist_powd", "sum", pipe = FALSE, dwparam = 0.3, alpha = 0.5)
    expect_match(code, "indirect_relations(net, type = \"dist_walk\", dwparam = 0.3, FUN = dist_powd, alpha = 0.5)", fixed = TRUE)
    expect_match(code, "aggregate_positions(rel, type = \"sum\")", fixed = TRUE)

    code <- index_builder_code("g", "walks", "walks_uptok", "self", pipe = TRUE, alpha = 2, k = 3)
    expect_match(code, "FUN = walks_uptok, alpha = 2, k = 3", fixed = TRUE)
    expect_no_match(index_builder_code("g", "dist_sp", "dist_inv", "sum"), "alpha")

    g <- igraph::make_ring(6)
    for (relation in c("dist_sp", "dist_lf", "dist_walk", "depend_netflow", "depend_rsps", "depend_rspn")) {
        code <- index_builder_code("g", relation, "identity", "sum", pipe = FALSE)
        env <- new.env()
        env$g <- g
        eval(parse(text = code), envir = env)
        expect_length(env$cent, 6)
    }
})

test_that("index_builder presets select relation and transformation", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("miniUI")
    app <- index_builder_app()
    expect_s3_class(app$ui, "shiny.tag.list")
    shiny::testServer(app$server, {
        session$setInputs(relation = "adjacency", transformation = "identity", index = "buildself")
        session$setInputs(index = "scall")
        # the preset's transformation waits for the relation to be updated ...
        expect_equal(pending_transformation(), "walks_exp")
        session$setInputs(relation = "walks")
        # ... and is then used instead of resetting to the first choice
        expect_null(pending_transformation())
    })
})
