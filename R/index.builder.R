#' Centrality Index Builder
#'
#' This shiny gadget can be used to build centrality indices based on specific indirect relations,
#' transformations and aggregation functions. use the dropdown menus to select
#' components that make up the index. Depending on your choices, some options
#' are not available at later stages. At the end, code is being inserted into
#' the current script to use the index
#'
#' @return code to calculate the specified index.
#' @export
index_builder <- function() {
    for (pkg in c("shiny", "miniUI", "rstudioapi")) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(pkg, " is needed for the addin to work. Please install it.", call. = FALSE)
        }
    }
    app <- index_builder_app()
    viewer <- shiny::dialogViewer("Index Builder", width = 840, height = 400)
    shiny::runGadget(app$ui, app$server, viewer = viewer)
}

# ui and server of index_builder()
index_builder_app <- function() {
    dist_transform <- c(
        identity = "identity", `1/x` = "dist_inv", `2^-x` = "dist_2pow",
        `a^x` = "dist_powd", `x^-a` = "dist_dpow"
    )
    walk_transform <- c(
        `limit proportion` = "walks_limit_prop", `exponential` = "walks_exp",
        `even exponential` = "walks_exp_even", `odd exponential` = "walks_exp_odd",
        `attenuated walks` = "walks_attenuated",
        `up to length k` = "walks_uptok"
    )

    transforms <- list(
        adjacency = "identity",
        dist_sp = dist_transform,
        dist_resist = dist_transform,
        dist_lf = dist_transform,
        dist_walk = dist_transform,
        dist_rwalk = dist_transform,
        depend_sp = c(identity = "identity"),
        depend_netflow = c(identity = "identity"),
        depend_curflow = c(identity = "identity"),
        depend_exp = c(identity = "identity"),
        depend_rsps = c(identity = "identity"),
        depend_rspn = c(identity = "identity"),
        walks = walk_transform
    )

    indices <- list(
        "degree" = c("adjacency", "identity", "sum"),
        "ccclassic" = c("dist_sp", "identity", "invsum"),
        "bcsp" = c("depend_sp", "identity", "sum"),
        "eigen" = c("walks", "walks_limit_prop", "sum"),
        "scall" = c("walks", "walks_exp", "self"),
        "sceven" = c("walks", "walks_exp_even", "self"),
        "scodd" = c("walks", "walks_exp_odd", "self"),
        "katz" = c("walks", "walks_attenuated", "sum"),
        "comall" = c("walks", "walks_exp", "sum"),
        "comeven" = c("walks", "walks_exp_even", "sum"),
        "comodd" = c("walks", "walks_exp_odd", "sum"),
        "netflow" = c("depend_netflow", "identity", "sum"),
        "curflow" = c("depend_curflow", "identity", "sum"),
        "combet" = c("depend_exp", "identity", "sum"),
        "rsps" = c("depend_rsps", "identity", "sum"),
        "rspn" = c("depend_rspn", "identity", "sum"),
        "hcc" = c("dist_sp", "dist_inv", "sum"),
        "rcc" = c("dist_sp", "dist_2pow", "sum"),
        "inf" = c("dist_resist", "identity", "invsum"),
        "gencc" = c("dist_sp", "dist_dpow", "sum"),
        "decay" = c("dist_sp", "dist_powd", "sum"),
        "drwalk" = c("dist_rwalk", "identity", "invsum")
    )
    # slider range (min, max, default) of alpha for each transformation
    alpha_ranges <- list(
        walks_exp = c(0.001, 100, 1),
        walks_exp_even = c(0.001, 100, 1),
        walks_exp_odd = c(0.001, 100, 1),
        walks_attenuated = c(0.001, 0.5, 0.01),
        walks_uptok = c(0.001, 10, 1),
        dist_dpow = c(0, 10, 0.3),
        dist_powd = c(0, 1, 0.33)
    )
    # ui ----
    ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("Centrality Index Builder"),
        shiny::fluidRow(
            shiny::column(6, shiny::selectInput(
                "index", "Prebuild Indices",
                list(
                    "Build your own" = "buildself",
                    "Classic Indices" = c(
                        "Degree" = "degree", "Closeness" = "ccclassic",
                        "Betweenness" = "bcsp", "Eigenvector" = "eigen"
                    ),
                    "Feedback" = c(
                        "Subgraph" = "scall", "Subgraph even" = "sceven",
                        "Subgraph odd" = "scodd", "Katz Status" = "katz",
                        "Communicability" = "comall", "Communcability even" = "comeven",
                        "Communcability odd" = "comodd"
                    ),
                    "Betweenness Type" = c(
                        "Flow Betweenness" = "netflow",
                        "Current Flow Betweenness" = "curflow",
                        "Communicability Betweenness" = "combet",
                        "Simple RSP Beteenness" = "rsps",
                        "Net RSP Betweenness" = "rspn"
                    ),
                    "Closeness Type" = c(
                        "Harmonic Closeness" = "hcc",
                        "Residual Closeness" = "rcc",
                        "Information Centrality" = "inf",
                        "Generalized Closeness" = "gencc",
                        "Decay Centrality" = "decay",
                        "Random Walk Closeness" = "drwalk"
                    )
                )
            )),
            shiny::column(6)
        ),
        shiny::fluidRow(
            shiny::column(3, shiny::textInput("network", "network", value = "g", width = NULL, placeholder = NULL)),
            shiny::column(3, shiny::selectInput(
                "relation", "Indirect Relation",
                list(
                    "Adjacency" = c("Adjacency" = "adjacency"),
                    "Distances" = c(
                        "Shortest Path Distance" = "dist_sp",
                        "Resistance Distance" = "dist_resist",
                        "Log Forest Distance" = "dist_lf",
                        "Walk Distance" = "dist_walk",
                        "Random Walk Distance" = "dist_rwalk"
                    ),
                    "Walks" = c("Walk Counts" = "walks"),
                    "Dependencies" = c(
                        "Shortest Path Dep." = "depend_sp",
                        "Network Flow Dep. " = "depend_netflow",
                        "Current Flow Dep." = "depend_curflow",
                        "Exponential Walks Dep." = "depend_exp",
                        "Simple RSP Dep." = "depend_rsps",
                        "Net RSP Dep." = "depend_rspn"
                    )
                )
            )),
            shiny::column(3, shiny::selectInput("transformation", "Transformation", c(""))),
            shiny::column(3, shiny::selectInput(
                "aggregation", "Aggregation",
                c(
                    "Sum" = "sum", "Mean" = "mean", "Max" = "max", "Min" = "min",
                    "Inverse Sum" = "invsum", "Self" = "self",
                    "Product" = "prod"
                )
            ))
        ),
        shiny::fluidRow(
            shiny::column(3),
            shiny::column(
                3,
                shiny::conditionalPanel(
                    "input.relation=='depend_netflow'",
                    shiny::selectInput("netflow", "netflow mode", c("Raw" = "raw", "Fraction" = "frac", "Normalized" = "norm"))
                ),
                shiny::conditionalPanel(
                    "input.relation=='dist_lf'",
                    shiny::sliderInput("lfparam", "Log Forest Parameter", 0.1, 500, 1, 0.1)
                ),
                shiny::conditionalPanel(
                    "input.relation=='dist_walk'",
                    shiny::sliderInput("dwparam", "Walk Distance Parameter", 0.1, 500, 1, 0.1)
                ),
                shiny::conditionalPanel(
                    "input.relation=='depend_rsps' || input.relation=='depend_rspn'",
                    shiny::sliderInput("rspxparam", "Randomized SP Parameter", 0, 500, 1, 0.1)
                )
            ),
            shiny::column(
                3,
                shiny::conditionalPanel(
                    paste0("[", paste0("'", names(alpha_ranges), "'", collapse = ","), "].includes(input.transformation)"),
                    shiny::sliderInput("alpha", "alpha", 0.001, 100, 1, 0.1)
                )
            ),
            shiny::column(3, shiny::checkboxInput("pipe", "Use pipes", value = TRUE, width = NULL))
        ),
        shiny::fluidRow(
            shiny::column(3),
            shiny::column(3),
            shiny::column(
                3,
                shiny::conditionalPanel(
                    "input.transformation=='walks_uptok'",
                    shiny::sliderInput("tok", "k", 1, 10, 4, 1)
                )
            ),
            shiny::column(3)
        )
    )

    # server ----
    server <- function(input, output, session) {
        # transformation requested by a prebuilt index, applied once the relation is updated
        pending_transformation <- shiny::reactiveVal(NULL)

        shiny::observeEvent(input$relation, {
            choices <- transforms[[input$relation]]
            selected <- pending_transformation()
            if (is.null(selected) || !selected %in% choices) {
                selected <- choices[1]
            }
            pending_transformation(NULL)
            shiny::updateSelectInput(session, "transformation",
                label = "Transformation",
                choices = choices,
                selected = selected
            )
        })
        shiny::observeEvent(input$index, {
            if (input$index != "buildself") {
                index_focus <- indices[[input$index]]
                if (!identical(index_focus[1], input$relation)) {
                    pending_transformation(index_focus[2])
                }
                shiny::updateSelectInput(session, "relation", selected = index_focus[1])
                shiny::updateSelectInput(session, "transformation",
                    choices = transforms[[index_focus[1]]],
                    selected = index_focus[2]
                )
                shiny::updateSelectInput(session, "aggregation", selected = index_focus[3])
            }
        })
        shiny::observeEvent(input$transformation, {
            range <- alpha_ranges[[input$transformation]]
            if (!is.null(range)) {
                shiny::updateSliderInput(session, "alpha", min = range[1], max = range[2], value = range[3])
            }
        })
        shiny::observeEvent(input$done, {
            indexText <- index_builder_code(
                network = input$network,
                relation = input$relation,
                transformation = input$transformation,
                aggregation = input$aggregation,
                pipe = input$pipe,
                lfparam = input$lfparam,
                dwparam = input$dwparam,
                netflowmode = input$netflow,
                rspxparam = input$rspxparam,
                alpha = input$alpha,
                k = input$tok
            )
            rstudioapi::insertText(indexText)
            shiny::stopApp()
        })

        shiny::observeEvent(input$cancel, {
            shiny::stopApp()
        })
    }

    list(ui = ui, server = server)
}

# code inserted by index_builder()
index_builder_code <- function(network, relation, transformation, aggregation, pipe = TRUE,
                               lfparam = 1, dwparam = 1, netflowmode = "raw", rspxparam = 1,
                               alpha = 1, k = 4) {
    lfparam_text <- if (relation == "dist_lf") paste0(", lfparam = ", lfparam) else ""
    dwparam_text <- if (relation == "dist_walk") paste0(", dwparam = ", dwparam) else ""
    netflow_text <- if (relation == "depend_netflow") paste0(", netflowmode = \"", netflowmode, "\"") else ""
    rspx_text <- if (relation %in% c("depend_rsps", "depend_rspn")) paste0(", rspxparam = ", rspxparam) else ""
    no_alpha <- c("identity", "dist_2pow", "dist_inv", "walks_limit_prop")
    alpha_text <- if (transformation %in% no_alpha) "" else paste0(", alpha = ", alpha)
    tok_text <- if (transformation == "walks_uptok") paste0(", k = ", k) else ""
    args <- paste0(
        "type = \"", relation, "\"", lfparam_text, dwparam_text, netflow_text, rspx_text,
        ", FUN = ", transformation, alpha_text, tok_text
    )
    if (pipe) {
        paste0(
            "cent <- ", network, " %>% \n\t",
            "indirect_relations(", args, ") %>%\n\t",
            "aggregate_positions(type = \"", aggregation, "\")"
        )
    } else {
        paste0(
            "rel <- indirect_relations(", network, ", ", args, ")\n",
            "cent <- aggregate_positions(rel, type = \"", aggregation, "\")"
        )
    }
}
