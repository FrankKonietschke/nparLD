#' Launch the nparLD Shiny application
#'
#' Starts an interactive Shiny application for fitting and exploring
#' nonparametric longitudinal models using \code{nparLD()}.
#'
#' The application allows users to import data, specify the response,
#' subject and factor variables, choose the inferential hypothesis,
#' request multiple contrast procedures, inspect numerical results,
#' and display graphical summaries.
#'
#' @return A Shiny application object, launched interactively.
#'
#' @examples
#' \dontrun{
#' nparLDApp()
#' }
#'
#' @export
#' 


nparLDApp <- function() {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required to run the nparLD app. ",
      "Please install it with install.packages('shiny').",
      call. = FALSE
    )
  }

  # ============================================================
  # Helper functions
  # ============================================================
  
quote_name <- function(x) {

  if (make.names(x) == x &&
      !grepl("^[.][0-9]", x)) {
    return(x)
  }

  paste0(
    "`",
    gsub("`", "\\\\`", x),
    "`"
  )
}
  
  quote_string <- function(x) {
    encodeString(x, quote = "\"")
  }
  
canonical_contrast_term <- function(term, factor_order) {

  if (is.null(term) || length(term) == 0L) {
    stop(
      "No contrast term selected.",
      call. = FALSE
    )
  }

  term <- as.character(term)[1L]
  factor_order <- as.character(factor_order)

  parts <- strsplit(
    term,
    ":",
    fixed = TRUE
  )[[1L]]

  if (!all(parts %in% factor_order)) {
    stop(
      paste0(
        "Factor not found in model. Selected term: ",
        term,
        ". Available factors: ",
        paste(factor_order, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  parts <- factor_order[
    factor_order %in% parts
  ]

  paste(
    parts,
    collapse = ":"
  )
}
  
  # ============================================================
  # UI
  # ============================================================
  
  ui <- shiny::fluidPage(
    
    shiny::titlePanel("nparLD interactive analysis"),
    
    shiny::tags$style(
      shiny::HTML("
        body {
          font-size: 17px;
          font-weight: 400;
        }

        h1 {
          font-size: 30px;
        }

        h3 {
          font-size: 21px;
          font-weight: 700;
        }

        h4 {
          font-size: 18px;
          font-weight: 700;
          margin-top: 18px;
        }

        .control-label {
          font-weight: 700;
        }

        .well {
          background-color: #f6f8fa;
        }

        .help-block {
          font-size: 15px;
        }

        .checkbox label,
        .radio label {
          font-size: 17px;
        }

        .btn {
          font-size: 17px;
          font-weight: 700;
        }

        pre {
          font-size: 16px !important;
          line-height: 1.5 !important;
          overflow-x: auto;
          white-space: pre;
        }

        table {
          font-size: 16px;
        }

        .variable-box,
        .analysis-box {
          padding: 10px 14px;
          margin-bottom: 15px;
          border: 1px solid #dddddd;
          border-radius: 5px;
          background-color: white;
        }

        .plot-note {
          font-size: 15px;
          margin-top: 8px;
          margin-bottom: 18px;
        }
      ")
    ),
    
    
    shiny::sidebarLayout(
      
      # ========================================================
      # SIDEBAR
      # ========================================================
      
      shiny::sidebarPanel(
        width = 4,
        
        # ------------------------------------------------------
        # Data
        # ------------------------------------------------------
        
        shiny::h3("Data"),
        
  shiny::fileInput(
  "file",
  "Select data file",
  accept = c(
    ".csv",
    ".txt",
    ".dat",
    ".xlsx",
    ".xls"
  )
),
        
    shiny::uiOutput("text_import_options"),
    shiny::hr(),
        
        
        # ------------------------------------------------------
        # Variables
        # ------------------------------------------------------
        
        shiny::h3("Variables"),
        
        shiny::helpText(
          paste(
            "Choose one response variable, one subject variable,",
            "optionally one replicate variable, and one or more factors."
          )
        ),
        
        shiny::uiOutput("variable_ui"),
        
        shiny::hr(),
        
        
        # ------------------------------------------------------
        # Analysis controls
        # ------------------------------------------------------
        
        shiny::uiOutput("analysis_ui")
      ),
      
      
      # ========================================================
      # MAIN PANEL
      # ========================================================
      
      shiny::mainPanel(
        width = 8,
        
      shiny::tabsetPanel(
       id = "main_tabs",
          
          # ----------------------------------------------------
          # Data
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "Data",
            shiny::br(),
            
            shiny::h3("Data preview"),
            
            shiny::textOutput("data_info"),
            
            shiny::br(),
            
            shiny::tableOutput("preview")
          ),
          
          
          # ----------------------------------------------------
          # Model
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "Model",
            shiny::br(),
            
            shiny::h3("Current model specification"),
            
            shiny::verbatimTextOutput("model_output")
          ),
          
          
          # ----------------------------------------------------
          # Overview
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "Overview",
            shiny::br(),
            
            shiny::verbatimTextOutput("fit_output")
          ),
          
          
          # ----------------------------------------------------
          # Summary
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "Summary",
            shiny::br(),

              shiny::downloadButton(
    "download_results",
    "Download results"
  ),
            
            shiny::verbatimTextOutput("summary_output")
          ),
          
          
          # ----------------------------------------------------
          # Effects
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "Effects",
            shiny::br(),
            
            shiny::verbatimTextOutput("effects_output")
          ),
          
          
          # ----------------------------------------------------
          # ATS
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "ATS",
            shiny::br(),
            
            shiny::verbatimTextOutput("ats_output")
          ),
          
          
          # ----------------------------------------------------
          # WTS
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "WTS",
            shiny::br(),
            
            shiny::verbatimTextOutput("wts_output")
          ),
          
          
          # ----------------------------------------------------
          # MCTP
          # ----------------------------------------------------
          
     shiny::tabPanel(
  "MCTP",
  shiny::br(),

  shiny::verbatimTextOutput("mctp_output"),

  shiny::br(),

  shiny::checkboxInput(
    "show_contrast_matrix",
    "Show contrast matrix",
    value = FALSE
  ),

  shiny::verbatimTextOutput("contrast_matrix_output")
),
          
          
          # ----------------------------------------------------
          # Factor information
          # ----------------------------------------------------
          
         shiny::tabPanel(
            "Factor information",
            shiny::br(),
            
            shiny::verbatimTextOutput("factor_output")
          ),
          
          
          # ----------------------------------------------------
          # Covariance
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "Covariance",
            shiny::br(),
            
            shiny::verbatimTextOutput("covariance_output")
          ),
          
          # ----------------------------------------------------
          # Plots
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "Plots",
            shiny::br(),
            
            shiny::tabsetPanel(
              type = "pills",
              
              
              # ------------------------------------------------
              # Cell-level effects
              # ------------------------------------------------
              
              shiny::tabPanel(
                "Cell effects",
                
                shiny::br(),
                
                shiny::h3("Cell-level relative effects"),
                
                shiny::plotOutput(
                  "cell_effects_plot",
                  height = "600px"
                ),
                shiny::downloadButton(
              "download_cell_plot",
                "Download plot"
                ),
                
                shiny::div(
                  class = "plot-note",
                  "Generated by plot(fit)."
                )
              ),
              
              
              # ------------------------------------------------
              # Term-specific effects
              # ------------------------------------------------
              
          shiny::tabPanel(
  "Term effects",

  shiny::br(),

  shiny::h3("Factor-specific relative effects"),

  shiny::uiOutput("term_effects_ui")
),
              
              
              # ------------------------------------------------
              # MCTP plot
              # ------------------------------------------------
              
              shiny::tabPanel(
                "MCTP",
                
                shiny::br(),
                
                shiny::h3("Multiple contrast procedure"),
                
                shiny::plotOutput(
                  "mctp_plot",
                  height = "650px"
                ),
             shiny::uiOutput("download_mctp_plot_ui"),
                shiny::verbatimTextOutput(
                  "mctp_plot_message"
                )
              )
            )
          ),
          
          
          # ----------------------------------------------------
          # R code
          # ----------------------------------------------------
          
          shiny::tabPanel(
            "R code",
            shiny::br(),
            
            shiny::h3("Reproducible R code"),

              shiny::downloadButton(
              "download_code",
              "Download R code"
              ),
            
            shiny::verbatimTextOutput("rcode_output")
          )
        )
      )
    )
  )
  
  
  # ============================================================
  # SERVER
  # ============================================================
  
  server <- function(input, output, session) {

     paired_design_available <- shiny::reactive({

  if (is.null(input$subject) ||
      is.null(input$response) ||
      is.null(input$factors) ||
      length(input$factors) == 0L) {
    return(FALSE)
  }

  d <- loaded()

  by_subject <- split(
    d,
    d[[input$subject]]
  )

  repeated_factors <- input$factors[
    vapply(
      input$factors,
      function(nm) {

        nuniq <- vapply(
          by_subject,
          function(df) {
            length(unique(stats::na.omit(df[[nm]])))
          },
          integer(1)
        )

        all(nuniq == 2L)
      },
      logical(1)
    )
  ]

  if (length(repeated_factors) != 1L) {
    return(FALSE)
  }

  repeated_factor <- repeated_factors[1L]

  repeated_levels <- unique(
    stats::na.omit(
      d[[repeated_factor]]
    )
  )

  if (length(repeated_levels) != 2L) {
    return(FALSE)
  }

  complete_data <- !anyNA(
    d[c(
      input$response,
      input$subject,
      repeated_factor
    )]
  )

  isTRUE(complete_data)
})
    
    
    # ==========================================================
    # Read uploaded data
    # ==========================================================

    # ==========================================================
# Import options for text files
# ==========================================================

output$text_import_options <- shiny::renderUI({

  shiny::req(input$file)

  ext <- tolower(
    tools::file_ext(input$file$name)
  )

  # Excel files do not need text-import options
  if (ext %in% c("xlsx", "xls")) {
    return(NULL)
  }

  shiny::tagList(

    shiny::checkboxInput(
      "header",
      "First row contains variable names",
      value = TRUE
    ),

    shiny::selectInput(
      "sep",
      "Field separator",
      choices = c(
        "Automatic" = "auto",
        "Whitespace" = "",
        "Tab" = "\t",
        "Comma" = ",",
        "Semicolon" = ";"
      ),
      selected = "auto"
    ),

    shiny::selectInput(
      "dec",
      "Decimal separator",
      choices = c(
        "Point" = ".",
        "Comma" = ","
      ),
      selected = "."
    )
  )
})
    
loaded <- shiny::reactive({

  shiny::req(input$file)

  tryCatch({

    ext <- tolower(
      tools::file_ext(input$file$name)
    )

    # ------------------------------------------------------
    # Excel
    # ------------------------------------------------------

    if (ext %in% c("xlsx", "xls")) {

      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop(
          "Package 'readxl' is required to import Excel files. ",
          "Please install it with install.packages('readxl').",
          call. = FALSE
        )
      }

      d <- as.data.frame(
        readxl::read_excel(
          input$file$datapath
        ),
        check.names = FALSE
      )

    # ------------------------------------------------------
    # Text files
    # ------------------------------------------------------

    } else {

      shiny::req(
        input$header,
        input$sep,
        input$dec
      )

      sep_use <- input$sep

      if (identical(sep_use, "auto")) {

        first_line <- readLines(
          input$file$datapath,
          n = 1L,
          warn = FALSE
        )

        if (grepl("\t", first_line, fixed = TRUE)) {

          sep_use <- "\t"

        } else if (grepl(";", first_line, fixed = TRUE)) {

          sep_use <- ";"

        } else if (grepl(",", first_line, fixed = TRUE)) {

          sep_use <- ","

        } else {

          sep_use <- ""
        }
      }

      d <- utils::read.table(
        input$file$datapath,
        header = input$header,
        sep = sep_use,
        dec = input$dec,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }


    # ------------------------------------------------------
    # Basic check
    # ------------------------------------------------------

    shiny::validate(
      shiny::need(
        ncol(d) >= 2L,
        paste0(
          "Only ",
          ncol(d),
          " column was detected. ",
          "Please check the imported file."
        )
      )
    )

    d

  },

  error = function(e) {

    shiny::validate(
      shiny::need(
        FALSE,
        paste(
          "Import error:",
          conditionMessage(e)
        )
      )
    )
  })
})
    
    
    # ==========================================================
    # Variable selection
    # ==========================================================
    
    output$variable_ui <- shiny::renderUI({
      
      d <- loaded()
      vars <- names(d)
      
      shiny::validate(
        shiny::need(
          length(vars) >= 3L,
          paste0(
            "Only ",
            length(vars),
            " variable(s) were detected. ",
            "Please check the field separator."
          )
        )
      )
      
      
      # --------------------------------------------------------
      # Guess response
      # --------------------------------------------------------
      
      response_guess <- grep(
        "^(resp|response|outcome|y)$",
        vars,
        ignore.case = TRUE,
        value = TRUE
      )
      
      numeric_vars <- vars[
        vapply(
          d,
          is.numeric,
          logical(1)
        )
      ]
      
      if (length(response_guess) > 0L) {
        
        response_default <- response_guess[1L]
        
      } else if (length(numeric_vars) > 0L) {
        
        response_default <- numeric_vars[1L]
        
      } else {
        
        response_default <- vars[1L]
      }
      
      
      # --------------------------------------------------------
      # Guess subject
      # --------------------------------------------------------
      
      subject_guess <- grep(
        "subject|^id$|patient|person|culture",
        vars,
        ignore.case = TRUE,
        value = TRUE
      )
      
      subject_guess <- setdiff(
        subject_guess,
        response_default
      )
      
      if (length(subject_guess) > 0L) {
        
        subject_default <- subject_guess[1L]
        
      } else {
        
        remaining <- setdiff(
          vars,
          response_default
        )
        
        subject_default <- remaining[1L]
      }
      

     
      
      # --------------------------------------------------------
      # Controls
      # --------------------------------------------------------
      
shiny::tagList(

  shiny::div(
    class = "variable-box",

    shiny::selectInput(
      "response",
      "Response variable",
      choices = vars,
      selected = response_default
    )
  ),

  shiny::div(
    class = "variable-box",

    shiny::selectInput(
      "subject",
      "Subject variable",
      choices = vars,
      selected = subject_default
    )
  ),

  shiny::div(
    class = "variable-box",

    shiny::selectInput(
      "replicate",
      "Replicate variable",
      choices = c(
        "None" = "",
        vars
      ),
      selected = ""
    )
  ),

  shiny::div(
    class = "variable-box",

    shiny::selectInput(
      "factors",
      "Factor variables",
      choices = vars,
      selected = character(0),
      multiple = TRUE
    )
  )
)

}) 
    
    # ==========================================================
    # Analysis controls
    # ==========================================================
    
    output$analysis_ui <- shiny::renderUI({
      
      shiny::req(loaded())
      
      shiny::tagList(
        
        # ------------------------------------------------------
        # Inference
        # ------------------------------------------------------
        
        shiny::h3("Inference"),
        
        shiny::div(
          class = "analysis-box",
          
          shiny::radioButtons(
            "hypothesis",
            "Hypothesis",
            choices = c(
              "H0p: relative marginal effects" = "H0p",
              "H0F: marginal distribution functions" = "H0F"
            ),
            selected = "H0p"
          ),
          
          shiny::uiOutput("effect_ui"),
          
          shiny::conditionalPanel(
            condition = "input.hypothesis == 'H0p'",

            
            
            shiny::selectInput(
              "ci_method",
              "Confidence interval method",
              choices = c(
                "Logit" = "logit",
                "Normal" = "normal"
              ),
              selected = "logit"
            ),
                      shiny::numericInput(
            "alpha",
            "Significance level alpha",
            value = 0.05,
            min = 0.001,
            max = 0.5,
            step = 0.01
          ),
          shiny::helpText(
  "For example, alpha = 0.05 corresponds to 95% confidence intervals."
)
          )

        ),
        
        
        # ------------------------------------------------------
        # Replicates
        # ------------------------------------------------------
        
        shiny::uiOutput("replicate_options_ui"),
        
        
        # ------------------------------------------------------
        # MCTP
        # ------------------------------------------------------
        
        shiny::h3("Multiple contrasts"),
        
        shiny::div(
          class = "analysis-box",
          
          shiny::checkboxInput(
            "use_contrast",
            "Perform multiple contrast procedure",
            value = FALSE
          ),
          
         shiny::conditionalPanel(
            condition = "input.use_contrast == true",
            
            shiny::uiOutput("contrast_term_ui"),
            
     shiny::uiOutput("contrast_type_ui"),
            
            shiny::radioButtons(
              "sci_method",
              "SCI method",
              choices = c(
                "Fisher" = "fisher",
                "Multivariate t" = "multi.t"
              ),
              selected = "fisher"
            )
          )
        ),
        
        
        # ------------------------------------------------------
        # Additional options
        # ------------------------------------------------------
        
        shiny::h3("Additional options"),
        
        shiny::div(
          class = "analysis-box",
          
          shiny::checkboxInput(
            "factor_information",
            "Compute factor-specific information and plots",
            value = TRUE
          ),
          
          shiny::checkboxInput(
            "compute_covariance",
            "Show covariance matrix",
            value = FALSE
          ),
          shiny::uiOutput("permutation_options_ui")
          ),
        
        
        shiny::actionButton(
          "run",
          "Run analysis",
          class = "btn-primary",
          width = "100%"
        ),
        
        shiny::br(),
        shiny::br()
      )
    })
    
    
    # ==========================================================
    # Ranking / effect
    # ==========================================================
    
    output$effect_ui <- shiny::renderUI({
      
      shiny::req(input$hypothesis)
      
      if (identical(input$hypothesis, "H0p")) {
        
        shiny::tagList(
          shiny::strong("Ranking: "),
          "pseudo-ranks (unweighted)"
        )
        
      } else {
        
        shiny::radioButtons(
          "effect",
          "Ranking",
          choices = c(
            "Ordinary ranks (weighted)" = "weighted",
            "Pseudo-ranks (unweighted)" = "unweighted"
          ),
          selected = "weighted"
        )
      }
    })
    
    
    # ==========================================================
    # Replicate options
    # ==========================================================
    
    output$replicate_options_ui <- shiny::renderUI({
      
      shiny::req(loaded())
      
      if (
        !is.null(input$replicate) &&
        nzchar(input$replicate)
      ) {
        
        shiny::tagList(
          
          shiny::h3("Replicate measurements"),
          
          shiny::div(
            class = "analysis-box",
            
            shiny::radioButtons(
              "cell_weights",
              "Cell weights",
              choices = c(
                "Subjects" = "subjects",
                "Observations" = "observations"
              ),
              selected = "subjects"
            ),
            
            shiny::helpText(
              paste(
                "'subjects' gives equal weight to subject-condition cells;",
                "'observations' weights cells according to the number",
                "of replicate observations."
              )
            )
          )
        )
      }
    })

    # ==========================================================
# Permutation test availability
# ==========================================================

output$permutation_options_ui <- shiny::renderUI({

  if (!paired_design_available()) {
    return(NULL)
  }

  shiny::tagList(

    shiny::h4("Paired-data permutation test"),

    shiny::numericInput(
      "B",
      "Number of permutations",
      value = 999,
      min = 99,
      step = 100
    ),

    shiny::helpText(
      paste(
        "A studentized permutation test will automatically be",
        "computed for this paired two-level design."
      )
    )
  )
})
    
    
    # ==========================================================
    # Formula
    # ==========================================================
    
model_formula <- shiny::reactive({

  shiny::req(
    input$response,
    input$factors
  )

  shiny::validate(
    shiny::need(
      length(input$factors) >= 1L,
      "Please select at least one factor variable."
    )
  )

  fac <- design_factor_order()

  rhs <- paste(
    vapply(
      fac,
      quote_name,
      character(1)
    ),
    collapse = " * "
  )

  as.formula(
    paste(
      quote_name(input$response),
      "~",
      rhs
    )
  )
})
    
    
    # ==========================================================
    # Model terms
    # ==========================================================

    design_factor_order <- shiny::reactive({

  d <- loaded()

  shiny::req(
    input$subject,
    input$factors
  )

  fac <- input$factors

  by_subject <- split(
    d,
    d[[input$subject]]
  )

  factor_info <- lapply(
    fac,
    function(nm) {

      nuniq <- vapply(
        by_subject,
        function(df) {
          length(unique(df[[nm]]))
        },
        integer(1)
      )

      list(nuniq = nuniq)
    }
  )

  names(factor_info) <- fac

  WP.names <- names(
    Filter(
      function(info) all(info$nuniq == 1L),
      factor_info
    )
  )

  SP.names <- names(
    Filter(
      function(info) all(info$nuniq > 1L),
      factor_info
    )
  )

  c(
    WP.names,
    SP.names
  )
})
    
model_terms <- shiny::reactive({

  fac <- design_factor_order()

  shiny::validate(
    shiny::need(
      length(fac) >= 1L,
      "Please select at least one factor variable."
    )
  )

  out <- character(0)

  for (k in seq_along(fac)) {

    cmb <- utils::combn(
      fac,
      k,
      simplify = FALSE
    )

    out <- c(
      out,
      vapply(
        cmb,
        paste,
        collapse = ":",
        FUN.VALUE = character(1)
      )
    )
  }

  out
})
    
    
    # ==========================================================
    # MCTP term selector
    # ==========================================================
    
    output$contrast_term_ui <- shiny::renderUI({
      
      shiny::req(input$factors)
      
      terms_available <- model_terms()
      
      shiny::radioButtons(
        "contrast_term",
        "Factor or interaction",
        choices = terms_available,
        selected = terms_available[1L]
      )
    })
    
    
    # ==========================================================
    # Validation
    # ==========================================================
    
    validate_selection <- shiny::reactive({
      
      shiny::req(
        input$response,
        input$subject,
        input$factors
      )
      
      d <- loaded()
      
      shiny::validate(
        
        shiny::need(
          is.numeric(d[[input$response]]),
          "The response variable must be numeric."
        ),
        
        shiny::need(
          length(input$factors) >= 1L,
          "Please select at least one factor variable."
        ),
        
        shiny::need(
          !identical(
            input$response,
            input$subject
          ),
          "Response and subject must be different variables."
        ),
        
        shiny::need(
          !input$response %in% input$factors,
          "The response variable cannot also be a factor."
        ),
        
        shiny::need(
          !input$subject %in% input$factors,
          "The subject variable cannot also be a factor."
        )
      )
      
      
      if (
        !is.null(input$replicate) &&
        nzchar(input$replicate)
      ) {
        
        shiny::validate(
          
          shiny::need(
            !identical(
              input$replicate,
              input$response
            ),
            "Response and replicate must be different variables."
          ),
          
          shiny::need(
            !identical(
              input$replicate,
              input$subject
            ),
            "Subject and replicate must be different variables."
          ),
          
          shiny::need(
            !input$replicate %in% input$factors,
            "The replicate variable cannot also be a factor."
          )
        )
      }
      
      TRUE
    })
    
    output$contrast_matrix_output <- shiny::renderPrint({

  shiny::req(input$show_contrast_matrix)

  a <- result()

  shiny::validate(
    shiny::need(
      a$contrast,
      "No multiple contrast procedure was requested."
    )
  )

  z <- contrastMatrix(a$fit)

  shiny::validate(
    shiny::need(
      !is.null(z),
      "No contrast matrix is available."
    )
  )

  cat("Contrast matrix\n")
  cat("------------------------------\n")
  print(z)
})
    
    # ==========================================================
    # Data information
    # ==========================================================
    
    output$data_info <- shiny::renderText({
      
      d <- loaded()
      
      paste0(
        nrow(d),
        " observations, ",
        ncol(d),
        " variables"
      )
    })
    
    
    # ==========================================================
    # Data preview
    # ==========================================================
    
    output$preview <- shiny::renderTable({
      
      utils::head(
        loaded(),
        12
      )
    })
    
    
    # ==========================================================
    # Model display
    # ==========================================================
    
    output$model_output <- shiny::renderPrint({
      
      shiny::req(loaded())
      
      cat("Response:\n")
      cat(input$response, "\n\n")
      
      cat("Subject:\n")
      cat(input$subject, "\n\n")
      
      cat("Replicate:\n")
      
      if (
        is.null(input$replicate) ||
        !nzchar(input$replicate)
      ) {
        
        cat("None\n\n")
        
      } else {
        
        cat(
          input$replicate,
          "\n\n"
        )
      }
      
      
      cat("Factors:\n")
      
      if (
        is.null(input$factors) ||
        length(input$factors) == 0L
      ) {
        
        cat("None selected\n")
        return(invisible(NULL))
      }
      
      cat(
        paste(
          input$factors,
          collapse = ", "
        ),
        "\n\n"
      )
      
      
      cat("Formula:\n")
      print(model_formula())
      
      
cat("\nHypothesis:\n")
cat(input$hypothesis, "\n\n")

cat("Ranking:\n")

if (identical(input$hypothesis, "H0p")) {

  cat("unweighted (pseudo-ranks)\n")

  cat("\nConfidence interval method:\n")
  cat(input$ci_method, "\n")

  cat("\nAlpha level:\n")
  cat(input$alpha, "\n")

  cat("\nConfidence level:\n")
  cat(
    format(
      100 * (1 - input$alpha),
      trim = TRUE
    ),
    "%\n",
    sep = ""
  )

} else {

  cat(
    input$effect,
    "\n"
  )
}
      
      
if (isTRUE(input$use_contrast)) {

term_use <- canonical_contrast_term(
  input$contrast_term,
  design_factor_order()
)

  cat("\nMultiple contrasts:\n")
  cat(term_use, "\n")

  cat("\nContrast type:\n")

  if (grepl(":", term_use, fixed = TRUE)) {
    cat("default\n")
  } else {
    cat(input$contrast_type, "\n")
  }

  cat("\nSCI method:\n")
  cat(input$sci_method, "\n")
}
    })
    
    
    # ==========================================================
    # Contrast argument
    # ==========================================================
    
contrast_argument <- shiny::reactive({

  if (!isTRUE(input$use_contrast)) {
    return(NULL)
  }

  shiny::req(input$contrast_term)

  term_use <- canonical_contrast_term(
    input$contrast_term,
    design_factor_order()
  )

  if (is.null(input$contrast_type) ||
      identical(input$contrast_type, "default")) {

    return(
      list(term_use)
    )
  }

  if (identical(input$contrast_type, "user")) {

    C <- tryCatch(
      parse_user_contrast(
        input$user_contrast
      ),
      error = function(e) {
        shiny::validate(
          shiny::need(
            FALSE,
            conditionMessage(e)
          )
        )
      }
    )

    return(
      list(
        term_use,
        C
      )
    )
  }

  list(
    term_use,
    input$contrast_type
  )
})
    
    # ==========================================================
    # Reproducible code
    # ==========================================================
    
build_code <- shiny::reactive({

  shiny::req(
    input$response,
    input$subject,
    input$factors
  )

  perm_available <- paired_design_available()

  ## Optional code placed before nparLD(), e.g. user-defined contrast matrix
  C_code <- ""

  args <- c(
    paste0(
      "  ",
      deparse(model_formula())
    ),
    "  data = dat",
    paste0(
      "  subject = ",
      quote_string(input$subject)
    )
  )

  ## Replicate variable
  if (!is.null(input$replicate) &&
      nzchar(input$replicate) &&
      !identical(input$replicate, "None")) {

    args <- c(
      args,
      paste0(
        "  replicate = ",
        quote_string(input$replicate)
      )
    )
  }

  ## Cell weighting
  if (!is.null(input$cell_weights)) {
    args <- c(
      args,
      paste0(
        "  cell.weights = ",
        quote_string(input$cell_weights)
      )
    )
  }

  ## Hypothesis
  args <- c(
    args,
    paste0(
      "  hypothesis = ",
      quote_string(input$hypothesis)
    )
  )

  ## Effect / confidence interval method
  if (identical(input$hypothesis, "H0F")) {

    args <- c(
      args,
      paste0(
        "  effect = ",
        quote_string(input$effect)
      )
    )

  } else {

    args <- c(
      args,
      '  effect = "unweighted"',
      paste0(
        "  CI.method = ",
        quote_string(input$ci_method)
      )
    )
  }

  ## Multiple contrast procedure
  if (isTRUE(input$use_contrast)) {

    shiny::req(input$contrast_term)

    term_use <- canonical_contrast_term(
      input$contrast_term,
      design_factor_order()
    )

    if (is.null(input$contrast_type) ||
        identical(input$contrast_type, "default")) {

      contrast_code <- paste0(
        "  contrast = list(",
        quote_string(term_use),
        ")"
      )

    } else if (identical(input$contrast_type, "user")) {

      C <- parse_user_contrast(
        input$user_contrast
      )

      rows <- apply(
        C,
        1L,
        function(z) {
          paste0(
            "c(",
            paste(z, collapse = ", "),
            ")"
          )
        }
      )

      if (nrow(C) == 1L) {

        C_code <- paste0(
          "C <- matrix(\n",
          "  ",
          rows[1L],
          ",\n",
          "  nrow = 1\n",
          ")\n\n"
        )

      } else {

        C_code <- paste0(
          "C <- rbind(\n  ",
          paste(
            rows,
            collapse = ",\n  "
          ),
          "\n)\n\n"
        )
      }

      contrast_code <- paste0(
        "  contrast = list(",
        quote_string(term_use),
        ", C)"
      )

    } else {

      contrast_code <- paste0(
        "  contrast = list(",
        quote_string(term_use),
        ", ",
        quote_string(input$contrast_type),
        ")"
      )
    }

    args <- c(
      args,
      contrast_code,
      paste0(
        "  sci.method = ",
        quote_string(input$sci_method)
      )
    )
  }

  ## Additional options
  args <- c(
    args,
    paste0(
      "  Factor.Information = ",
      if (isTRUE(input$factor_information)) {
        "TRUE"
      } else {
        "FALSE"
      }
    ),
    paste0(
      "  covariance = ",
      if (isTRUE(input$compute_covariance)) {
        "TRUE"
      } else {
        "FALSE"
      }
    ),
    paste0(
      "  alpha = ",
      input$alpha
    ),
paste0(
  "  perm.test = ",
  if (perm_available) "TRUE" else "FALSE"
)
  )
if (perm_available &&
    !is.null(input$B)) {

  args <- c(
    args,
    paste0(
      "  B = ",
      input$B
    )
  )
}

  paste0(
    C_code,
    "fit <- nparLD(\n",
    paste(
      args,
      collapse = ",\n"
    ),
    "\n)"
  )
})
    
    
    # ==========================================================
    # Run analysis
    # ==========================================================
    
    analysis <- shiny::eventReactive(
  input$run,
  {
   
    shiny::req(input$run > 0)

    validate_selection()

    shiny::withProgress(
      message = "Computation in progress",
      detail = "Preparing analysis...",
      value = 0,
      {

        d <- loaded()

        shiny::incProgress(
          0.1,
          detail = "Preparing data..."
        )

        effect_value <- if (
          identical(input$hypothesis, "H0p")
        ) {
          "unweighted"
        } else {
          input$effect
        }

       perm_available <- paired_design_available()
args <- list(
  formula = model_formula(),
  data = d,
  subject = input$subject,
  hypothesis = input$hypothesis,
  effect = effect_value,
  alpha = input$alpha,
  Factor.Information =
    isTRUE(input$factor_information),
  covariance =
    isTRUE(input$compute_covariance),
  perm.test =
    perm_available
)

if (perm_available) {
  args$B <- input$B
}

        if (identical(input$hypothesis, "H0p")) {
          args$CI.method <- input$ci_method
        }

        if (
          !is.null(input$replicate) &&
          nzchar(input$replicate)
        ) {
          args$replicate <- input$replicate
          args$cell.weights <- input$cell_weights
        }

        if (isTRUE(input$use_contrast)) {
          args$contrast <- contrast_argument()
          args$sci.method <- input$sci_method
        }

    

        shiny::incProgress(
          0.2,
          detail = "Fitting nparLD model..."
        )
      
        fit <- tryCatch(

          do.call(
            nparLD,
            args
          ),

          error = function(e) {

            message(
              "\n--- nparLD app analysis error ---\n",
              conditionMessage(e),
              "\n\nCall generated by the app:\n",
              build_code(),
              "\n--------------------------------\n"
            )

            shiny::showNotification(
              paste(
                "Analysis error:",
                conditionMessage(e)
              ),
              type = "error",
              duration = NULL
            )

            NULL
          }
        )

        shiny::incProgress(
          0.6,
          detail = "Preparing output..."
        )

        if (is.null(fit)) {
          return(NULL)
        }

        list(
          fit = fit,
          contrast =
            isTRUE(input$use_contrast),
          factor_information =
            isTRUE(input$factor_information),
          covariance =
            isTRUE(input$compute_covariance),
         permutation =
  !is.null(fit$perm) &&
  isTRUE(fit$perm$available),
          code =
            build_code()
        )
      }
    )
  }
)
    
    
    # ==========================================================
    # Result helper
    # ==========================================================
    
    result <- shiny::reactive({
      
      a <- analysis()
      
      shiny::validate(
        shiny::need(
          !is.null(a),
          "Click 'Run analysis' to display results."
        )
      )
      
      a
    })

    permutation_tab_inserted <- shiny::reactiveVal(FALSE)

shiny::observeEvent(analysis(), {

  a <- analysis()

  available <-
    !is.null(a) &&
    !is.null(a$fit$perm) &&
    isTRUE(a$fit$perm$available)

  if (available &&
      !isTRUE(permutation_tab_inserted())) {

    shiny::insertTab(
      inputId = "main_tabs",
tab = shiny::tabPanel(
  "Permutation",

  shiny::br(),

  shiny::verbatimTextOutput(
    "permutation_output"
  ),

  shiny::br(),

  shiny::h3(
    "Permutation distribution"
  ),

  shiny::plotOutput(
    "permutation_plot",
    height = "500px"
  ),

  shiny::downloadButton(
    "download_permutation_plot",
    "Download plot"
  )
),
      target = "Covariance",
      position = "after",
      select = FALSE,
      session = session
    )

    permutation_tab_inserted(TRUE)

  } else if (
    !available &&
    isTRUE(permutation_tab_inserted())
  ) {

    shiny::removeTab(
      inputId = "main_tabs",
      target = "Permutation",
      session = session
    )

    permutation_tab_inserted(FALSE)
  }
})
    
    
    # ==========================================================
    # Analysis outputs
    # ==========================================================
    
    output$fit_output <- shiny::renderPrint({
      
      a <- result()
      
      print(
        a$fit
      )
    })
    
    
    output$summary_output <- shiny::renderPrint({
      
      a <- result()
      
      print(
        summary(a$fit)
      )
    })
        
    
    output$mctp_output <- shiny::renderPrint({
      
      a <- result()
      
if (is.null(a$fit$MCTP)) {
  cat("No multiple contrast results are available.\n")
} else {

  print(MCTP(a$fit))
}
    })

    output$term_effects_ui <- shiny::renderUI({

  a <- result()

if (is.null(a$fit$factor.info)) {
  return(
    shiny::div(
      class = "plot-note",
      paste(
        "Factor-specific plots were not computed.",
        "Enable 'Compute factor-specific information and plots'",
        "and rerun the analysis."
      )
    )
  )
}

fi <- factorInfo(a$fit)

  if (is.null(fi) || length(fi) == 0L) {

    return(
      shiny::div(
        class = "plot-note",
        "No factor-specific information is available."
      )
    )
  }

  shiny::tagList(

    shiny::uiOutput("plot_term_ui"),

    shiny::plotOutput(
      "term_effects_plot",
      height = "650px"
    ),

    shiny::downloadButton(
      "download_term_plot",
      "Download plot"
    ),

    shiny::div(
      class = "plot-note",
      paste(
        "Generated by plot(fit, term = ...).",
        "For H0p, confidence intervals are displayed.",
        "For H0F, point estimates are displayed."
      )
    )
  )
})

output$download_mctp_plot_ui <- shiny::renderUI({

  a <- result()

  if (is.null(a$fit$MCTP)) {
    return(NULL)
  }

  z <- MCTP(a$fit)

  if (is.null(z$Local.Results)) {
    return(NULL)
  }

  if (!all(c("Estimate", "Lower", "Upper") %in% names(z$Local.Results))) {
    return(NULL)
  }

  shiny::downloadButton(
    "download_mctp_plot",
    "Download plot"
  )
})
    
    output$factor_output <- shiny::renderPrint({
      
      a <- result()
      
if (is.null(a$fit$factor.info)) {
  cat("Factor-specific information was not requested or is unavailable.\n")
} else {
   cat("Factor or Interaction specific results\n")
    cat("--------------------------------------\n")
  print(factorInfo(a$fit))
}
    })
    
    
output$covariance_output <- shiny::renderPrint({
  a <- result()

  if (is.null(a$fit$covariance.info)) {
    cat("Covariance matrix computation was not requested or is unavailable.\n")
  } else {
    cat("Estimated covariance of sqrt(N) * (theta_hat - theta)\n")
    cat("-----------------------------------------------------\n")
    print(covariance(a$fit))
  }
})
    
    
output$permutation_output <- shiny::renderPrint({

  a <- result()

  if (is.null(a$fit$perm) ||
      !isTRUE(a$fit$perm$available)) {

cat(
  "No permutation-test result is available. ",
  "The permutation test is only available for supported paired designs.\n",
  sep = ""
)

  } else {

 cat("Studentized Permutation Test\n")
    cat("------------------------------\n")
    print(permTest(a$fit))
  }
})

output$contrast_type_ui <- shiny::renderUI({

  shiny::req(input$contrast_term)

  shiny::tagList(

    shiny::selectInput(
      "contrast_type",
      "Contrast type",
      choices = c(
        "Default" = "default",
        "Dunnett" = "Dunnett",
        "Tukey" = "Tukey",
        "Sequential" = "Sequen",
        "Average" = "AVE",
        "Changepoint" = "Changepoint",
        "Williams" = "Williams",
        "Marcus" = "Marcus",
        "McDermott" = "McDermott",
        "Umbrella Williams" = "UmbrellaWilliams",
        "Grand mean" = "GrandMean",
        "User-defined" = "user"
      ),
      selected = "default"
    ),

    shiny::conditionalPanel(
      condition = "input.contrast_type == 'user'",

      shiny::textAreaInput(
        "user_contrast",
        "User-defined contrast matrix",
        value = "",
        placeholder = paste(
          "-1, 1, 0, 0",
          "0, -1, 1, 0",
          sep = "\n"
        ),
        rows = 5,
        width = "100%"
      ),

shiny::helpText(
  paste(
    "Enter one contrast per line.",
    "Separate coefficients by commas or spaces.",
    "Each contrast must contain positive and negative coefficients.",
    "Rows are automatically normalized so that positive coefficients sum to 1",
    "and negative coefficients sum to -1."
  )
)
    )
  )
})

output$ats_output <- shiny::renderPrint({

  a <- result()

  z <- ATS(a$fit)

  if (is.null(z)) {

    cat(
      "No ANOVA-type statistic is available.\n"
    )

  } else {

    cat("ANOVA-type statistic\n")
    cat("------------------------------\n")
    print(z)
  }
})

output$wts_output <- shiny::renderPrint({

  a <- result()

  z <- WTS(a$fit)

  if (is.null(z)) {

    cat(
      "No Wald-type statistic is available.\n"
    )

  } else {

    cat("Wald-type statistic\n")
    cat("------------------------------\n")
    print(z)
  }
})

output$effects_output <- shiny::renderPrint({

  a <- result()

  z <- effects(a$fit)

  if (is.null(z)) {

    cat(
      "No Effects are available.\n"
    )

  } else {

    cat("Estimated relative marginal effects (RTE)\n")
    cat("-----------------------------------------\n")
    print(z)
  }
})

 
    # ==========================================================
    # Plot term selector
    # ==========================================================
    
output$plot_term_ui <- shiny::renderUI({

    a <- result()

  if (is.null(a$fit$factor.info)) {
    return(NULL)
  }

  fi <- factorInfo(a$fit)

  shiny::req(
    !is.null(fi),
    length(fi) > 0L
  )

  terms_available <- names(fi)

  shiny::checkboxGroupInput(
    "plot_terms",
    "Model term(s)",
    choices = terms_available,
    selected = terms_available[1L]
  )
})
    
    
    # ==========================================================
    # Cell-level effects plot
    # ==========================================================
    
    output$cell_effects_plot <- shiny::renderPlot({
      
      a <- result()
      
      tryCatch({
        
        p <- plot(
          a$fit
        )
        
        if (!is.null(p)) {
          print(p)
        }
        
      },
      
      error = function(e) {
        
        graphics::plot.new()
        
        graphics::text(
          0.5,
          0.5,
          paste(
            "Plot not available:",
            conditionMessage(e)
          ),
          cex = 1.1
        )
      })
    })
    
    
    # ==========================================================
    # Factor-specific effects plot
    # ==========================================================
output$term_effects_plot <- shiny::renderPlot({

  a <- result()

  shiny::req(
    !is.null(a$fit$factor.info),
    input$plot_terms,
    length(input$plot_terms) > 0L
  )

  p <- plot(
    a$fit,
    term = input$plot_terms
  )

  if (!is.null(p)) {
    print(p)
  }
})
    
    # ==========================================================
    # MCTP plot
    # ==========================================================
    
   output$mctp_plot <- shiny::renderPlot({

  a <- result()

  shiny::validate(
    shiny::need(
      !is.null(a$fit$MCTP),
      "No multiple contrast results are available."
    )
  )

  z <- MCTP(a$fit)

  tryCatch({

    p <- plot(z)

    if (!is.null(p)) {
      print(p)
    }

  },

  error = function(e) {

    graphics::plot.new()
    graphics::text(
      0.5,
      0.5,
      paste(
        "MCTP plot not available:",
        conditionMessage(e)
      ),
      cex = 1.1
    )
  })
})
    
parse_user_contrast <- function(txt) {

  if (is.null(txt) || !nzchar(trimws(txt))) {
    stop(
      "Please enter at least one user-defined contrast.",
      call. = FALSE
    )
  }

  lines <- strsplit(
    txt,
    "\n",
    fixed = TRUE
  )[[1L]]

  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  rows <- lapply(
    lines,
    function(z) {

      z <- gsub(
        ",",
        " ",
        z,
        fixed = TRUE
      )

      vals <- scan(
        text = z,
        what = numeric(),
        quiet = TRUE
      )

      if (length(vals) == 0L) {
        stop(
          "Each line must contain at least one numeric coefficient.",
          call. = FALSE
        )
      }

      vals
    }
  )

  lengths_rows <- lengths(rows)

  if (length(unique(lengths_rows)) != 1L) {
    stop(
      paste(
        "All user-defined contrasts must contain",
        "the same number of coefficients."
      ),
      call. = FALSE
    )
  }

  C <- do.call(
    rbind,
    rows
  )

  storage.mode(C) <- "double"

  rownames(C) <- paste0(
    "User",
    seq_len(nrow(C))
  )

  C
}

output$permutation_plot <- shiny::renderPlot({

  a <- result()

  shiny::validate(
    shiny::need(
      !is.null(a$fit$perm) &&
        isTRUE(a$fit$perm$available),
      "No permutation distribution is available."
    )
  )

  z <- permTest(a$fit)

  plot(z)
})

output$download_permutation_plot <- shiny::downloadHandler(

  filename = function() {
    "nparLD_permutation_distribution.pdf"
  },

  content = function(file) {

    a <- result()

    shiny::req(
      !is.null(a$fit$perm),
      isTRUE(a$fit$perm$available)
    )

    z <- permTest(a$fit)

    grDevices::pdf(
      file,
      width = 8,
      height = 6
    )

    on.exit(
      grDevices::dev.off(),
      add = TRUE
    )

    plot(z)
  },

  contentType = "application/pdf"
)

    
 
    
    # ==========================================================
    # Reproducible R code
    # ==========================================================

output$download_cell_plot <- shiny::downloadHandler(

  filename = function() {
    "nparLD_cell_effects.pdf"
  },

  content = function(file) {

    a <- result()

    p <- plot(a$fit)

    ggplot2::ggsave(
      filename = file,
      plot = p,
      device = "pdf",
      width = 8,
      height = 6,
      units = "in"
    )
  },

  contentType = "application/pdf"
)

output$download_term_plot <- shiny::downloadHandler(

  filename = function() {
    "nparLD_term_effects.pdf"
  },

  content = function(file) {

    a <- result()

    shiny::req(
      isTRUE(a$factor_information),
      input$plot_terms,
      length(input$plot_terms) > 0L
    )

    p <- plot(
      a$fit,
      term = input$plot_terms
    )

    ggplot2::ggsave(
      filename = file,
      plot = p,
      device = "pdf",
      width = 8,
      height = 6,
      units = "in"
    )
  },

  contentType = "application/pdf"
)

output$download_mctp_plot <- shiny::downloadHandler(

  filename = function() {
    "nparLD_MCTP.pdf"
  },

  content = function(file) {

    a <- result()

    shiny::req(!is.null(a$fit$MCTP))

    z <- MCTP(a$fit)

    p <- plot(z)

    ggplot2::ggsave(
      filename = file,
      plot = p,
      device = "pdf",
      width = 8,
      height = 6,
      units = "in"
    )
  },

  contentType = "application/pdf"
)

output$download_results <- shiny::downloadHandler(

  filename = function() {
    "nparLD_results.txt"
  },

  content = function(file) {

    a <- result()

    txt <- utils::capture.output({

      cat("nparLD analysis\n")
      cat("==============================\n\n")

      cat("Model\n")
      cat("------------------------------\n")
      cat(a$code, "\n\n")

      cat("Summary\n")
      cat("------------------------------\n")
      print(
        summary(a$fit)
      )

      if (!is.null(a$fit$MCTP)) {

        cat("\n\nMultiple contrast procedure\n")
        cat("--------------------------------\n")

        print(MCTP(a$fit))
      }

      if (!is.null(a$fit$perm) &&
          isTRUE(a$fit$perm$available)) {

        cat("\n\nPermutation test\n")
        cat("------------------------------\n")

        print(permTest(a$fit))
      }
    })

    writeLines(
      txt,
      con = file
    )
  },

  contentType = "text/plain"
)

output$download_code <- shiny::downloadHandler(

  filename = function() {
    "nparLD_analysis.R"
  },

  content = function(file) {

    code <- build_code()

    writeLines(
      code,
      con = file
    )
  },

  contentType = "text/plain"
)
    output$rcode_output <- shiny::renderText({

      shiny::req(
        input$response,
        input$subject,
        input$factors
      )

      build_code()
    })

  }  # end server


  # ============================================================
  # START APP
  # ============================================================

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
