#' Centrality Index Builder
#'
#' This gadget lets you build centrality indices based on indirect relations,
#' transformations and aggregation functions.
#'
#' @return code to calculate the specified index.
#' @export
index_builder <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny is needed for the addin to work. Please install it.", call. = FALSE)
  }
  transforms <- list(identity="identity",
                     dist_sp=c(identity="identity",`1/x`="dist_inv",`2^-x`="dist_2pow",`a^x`="dist_powd",`x^-a`="dist_dpow"),
                     dist_resist=c(identity="identity",`1/x`="dist_inv",`2^-x`="dist_2pow",`a^x`="dist_powd",`x^-a`="dist_dpow"),
                     dist_lf=c(identity="identity",`1/x`="dist_inv",`2^-x`="dist_2pow",`a^x`="dist_powd",`x^-a`="dist_dpow"),
                     depend_sp=c(identity="identity"),
                     depend_netflow=c(identity="identity"),
                     depend_curflow=c(identity="identity"),
                     depend_exp=c(identity="identity"),
                     depend_rsps=c(identity="identity"),
                     depend_rspn=c(identity="identity"),
                     walks=c(`limit proportion`="walks_limit_prop",`exponential`="walks_exp",
                             `even exponential`="walks_exp_even",`odd exponential`="walks_exp_odd",
                             `attenuated walks`="walks_attenuated",
                             `up to length k` = "walks_uptok"))
  
  #ui ----    
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Centrality Index Builder"),
    shiny::fluidRow(
      shiny::column(6,shiny::selectInput("index", "Prebuild Indices",
                                         list("Classic Indices"=c("Degree"="degree","Closeness"="ccclassic","Betweenness"="bcsp",
                                                                  "Eigenvector"="eigen"),
                                              "Feedback"=c("Subgraph"="scall","Subgraph even"="sceven",
                                                           "Subgraph odd"="scodd",
                                                           "Katz Status"="katz",
                                                           "Communicability"="comall",
                                                           "Communcability even"="comeven",
                                                           "Communcability odd"="comodd"),
                                              "Betweenness Type"=c("Flow Betweenness"="netflow",
                                                                   "Current Flow Betweenness"="curflow",
                                                                   "Communicability Betweenness"="combet",
                                                                   "Simple RSP Beteenness"="rsps",
                                                                   "Net RSP Betweenness"="rspn"),
                                              "Closeness Type"=c("Harmonic Closeness"="hcc",
                                                                 "Residual Closeness"="rcc",
                                                                 "Information Centrality"="inf",
                                                                 "Generalized Closeness"="gencc",
                                                                 "Decay Centrality"="decay")))),
      shiny::column(6)
    ),
    shiny::fluidRow(
      shiny::column(3, shiny::textInput("network", "network", value = "g", width = NULL, placeholder = NULL)),
      shiny::column(3, shiny::selectInput("relation", "Indirect Relation",
                                          list("Adjacency"=c("Adjacency"="identity"),
                                               "Distances"=c("Shortest Paths" = "dist_sp",
                                                             "Resistance" = "dist_resist",
                                                             "Log Forest" = "dist_lf"),
                                               "Walks"=c("Walk Counts" = "walks"),
                                               "Dependencies"=c("Shortes Paths" = "depend_sp",
                                                                "Network Flow"="depend_netflow",
                                                                "Current Flow"="depend_curflow",
                                                                "Exponential Walks"="depend_exp",
                                                                "Simple RSP" = "depend_rsps",
                                                                "Net RSP" = "depend_rspn")))),
      shiny::column(3, shiny::selectInput("transformation", "Transformation",c(""))),
      shiny::column(3,shiny::selectInput("aggregation", "Aggregation",
                                         c("Sum"="sum","Mean"="mean","Max"="max","Min"="min",
                                           "Inverse Sum"="invsum","Self"="self",
                                           "Product"="prod")))
    ),
    shiny::fluidRow(
      shiny::column(3),
      shiny::column(3,
                    shiny::conditionalPanel("input.relation=='depend_netflow'",
                                            shiny::selectInput("netflow","netflow mode",c("Raw"="raw","Fraction"="frac","Normalized"="norm"))
                    ),
                    shiny::conditionalPanel("input.relation=='dist_lf'",
                                            shiny::sliderInput("lfparam","Log Forest Parameter",0,500,1,0.1)
                    ),
                    shiny::conditionalPanel("input.relation=='depend_rsps'",
                                            shiny::sliderInput("rspxparam","Randomized SP Parameter",0,500,1,0.1)
                    ),
                    shiny::conditionalPanel("input.relation=='depend_rspn'",
                                            shiny::sliderInput("rspxparam","Randomized SP Parameter",0,500,1,0.1)
                    )
                    
      ),
      shiny::column(3,
                    shiny::conditionalPanel("input.transformation=='walks_exp'",
                                            shiny::sliderInput("alpha","alpha",0.001,100,1,0.1)
                    ),
                    shiny::conditionalPanel("input.transformation=='walks_exp_even'",
                                            shiny::sliderInput("alpha","alpha",0.001,100,1,0.1)
                    ),
                    shiny::conditionalPanel("input.transformation=='walks_exp_odd'",
                                            shiny::sliderInput("alpha","alpha",0.001,100,1,0.1)
                    ),
                    shiny::conditionalPanel("input.transformation=='walks_attenuated'",
                                            shiny::sliderInput("alpha","alpha",0.001,0.5,0.01,0.1)
                    ),
                    shiny::conditionalPanel("input.transformation=='walks_uptok'",
                                            shiny::sliderInput("alpha","alpha",0.001,10,1,0.1)
                    ),
                    shiny::conditionalPanel("input.transformation=='dist_dpow'",
                                            shiny::sliderInput("alpha","alpha",0,10,0.3,0.1)
                    ),
                    shiny::conditionalPanel("input.transformation=='dist_powd'",
                                            shiny::sliderInput("alpha","alpha",0,1,0.33,0.1)
                    )
      ),
      shiny::column(3)
    ),
    shiny::fluidRow(
      shiny::column(3),
      shiny::column(3),
      shiny::column(3,
                    shiny::conditionalPanel("input.transformation=='walks_uptok'",
                                            shiny::sliderInput("tok","k",1,10,4,1)
                    )
      ),
      shiny::column(3)
    )
  )
  
  #server ----    
  server <- function(input, output, session) {
    
    shiny::observe({
      shiny::updateSelectInput(session, "transformation", 
                        label = "Transformation", 
                        choices = transforms[[input$relation]])
    })
    shiny::observe({
      if(input$index=='degree'){
        shiny::updateSelectInput(session,"relation",selected="identity")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="ccclassic"){
        shiny::updateSelectInput(session,"relation",selected="dist_sp")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="invsum")
      } else if(input$index=="bcsp"){
        shiny::updateSelectInput(session,"relation",selected="depend_sp")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="eigen"){
        shiny::updateSelectInput(session,"relation",selected="walks")
        shiny::updateSelectInput(session,"transformation",selected="walks_limit_prop")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="scall"){
        shiny::updateSelectInput(session,"relation",selected="walks")
        shiny::updateSelectInput(session,"transformation",selected="walks_exp")
        shiny::updateSelectInput(session,"aggregation",selected="self")
      } else if(input$index=="sceven"){
        shiny::updateSelectInput(session,"relation",selected="walks")
        shiny::updateSelectInput(session,"transformation",selected="walks_exp_even")
        shiny::updateSelectInput(session,"aggregation",selected="self")
      } else if(input$index=="scodd"){
        shiny::updateSelectInput(session,"relation",selected="walks")
        shiny::updateSelectInput(session,"transformation",selected="walks_exp_odd")
        shiny::updateSelectInput(session,"aggregation",selected="self")
      }else if(input$index=="katz"){
        shiny::updateSelectInput(session,"relation",selected="walks")
        shiny::updateSelectInput(session,"transformation",selected="walks_attenuated")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="comall"){
        shiny::updateSelectInput(session,"relation",selected="walks")
        shiny::updateSelectInput(session,"transformation",selected="walks_exp")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="comeven"){
        shiny::updateSelectInput(session,"relation",selected="walks")
        shiny::updateSelectInput(session,"transformation",selected="walks_exp_even")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="comodd"){
        shiny::updateSelectInput(session,"relation",selected="walks")
        shiny::updateSelectInput(session,"transformation",selected="walks_exp_odd")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="netflow"){
        shiny::updateSelectInput(session,"relation",selected="depend_netflow")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="curflow"){
        shiny::updateSelectInput(session,"relation",selected="depend_curflow")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="combet"){
        shiny::updateSelectInput(session,"relation",selected="depend_exp")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="rsps"){
        shiny::updateSelectInput(session,"relation",selected="depend_rsps")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="rspn"){
        shiny::updateSelectInput(session,"relation",selected="depend_rspn")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="hcc"){
        shiny::updateSelectInput(session,"relation",selected="dist_sp")
        shiny::updateSelectInput(session,"transformation",selected="dist_inv")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="rcc"){
        shiny::updateSelectInput(session,"relation",selected="dist_sp")
        shiny::updateSelectInput(session,"transformation",selected="dist_2pow")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="gencc"){
        shiny::updateSelectInput(session,"relation",selected="dist_sp")
        shiny::updateSelectInput(session,"transformation",selected="dist_dpow")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      } else if(input$index=="decay"){
        shiny::updateSelectInput(session,"relation",selected="dist_sp")
        shiny::updateSelectInput(session,"transformation",selected="dist_powd")
        shiny::updateSelectInput(session,"aggregation",selected="sum")
      }  else if(input$index=="inf"){
        shiny::updateSelectInput(session,"relation",selected="dist_resist")
        shiny::updateSelectInput(session,"transformation",selected="identity")
        shiny::updateSelectInput(session,"aggregation",selected="invsum")
      }
    })
    shiny::observeEvent(input$done, {
      lfparam_text <- ifelse(input$relation!="dist_lf","",paste0(", lfparam = ",input$lfparam))
      netflow_text <- ifelse(input$relation!="depend_netflow","",paste0(", netflowmode = \"",input$netflow,"\""))
      rspx_text <- ifelse(!input$relation%in%c("depend_rsps","depend_rspn"),
                          "",paste0(", rspxparam = ",input$rspxparam))
      
      alpha_text <- ifelse(input$transformation%in%c("identity","dist_2pow","dist_inv","walks_limit_prop"),
                           "",paste0(", alpha = ",input$alpha))
      tok_text <- ifelse(input$transformation!="walks_uptok","",paste0(", k = ",input$tok))
      
      indexText <- paste0(input$network,
                          " %>% \n\t",
                          "indirect_relations(",
                          "type = \"", 
                          input$relation,"\"",lfparam_text,netflow_text,rspx_text,
                          ", FUN = ",input$transformation,alpha_text,tok_text,") %>%\n\t",
                          "aggregate_position(type = \"",input$aggregation,"\")")
      rstudioapi::insertText(indexText)
      shiny::stopApp()
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
    
  }
  
  viewer <- shiny::dialogViewer("Index Builder", width = 840, height = 400)
  shiny::runGadget(ui, server, viewer = viewer)
  
}