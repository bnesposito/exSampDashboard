## app.R ##
#devtools::load_all()
#rsconnect::deployApp()

library(shiny)
library(shinydashboard)
library(rstanarm)
library(ggplot2)
library(dplyr)
library(shinycssloaders)

source("./Rtemp/sampling.R")
source("./Rtemp/ploting.R")


ui <- dashboardPage(
  dashboardHeader(title = "Exploration Sampling"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Start", tabName = "downloadtest"),
      menuItem("Upload data", tabName = "rawdata"),
      menuItem("Select model", tabName = "model"),
      menuItem("Results", tabName = "results"),
      menuItem("Next Wave", tabName = "next_wave")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("downloadtest",
              fluidRow(
                box(title = "Overview",
                    paste0("This app allows you to model and analize a dataset from an adaptive experiment. Upload your dataset or a test set ",
                           "on the Upload data tab. Then, select a model in the Select model tab. There are three models available: ",
                           "(i) Bernoulli, (ii) Binomial - logit GLM and (iii) Linear model. In the current version of the app, it is not possible to ",
                           "include random effects on the model specficiation."),
                    br(),
                    br(),
                    paste0("Finally, results from the fitted model ",
                           "are displayed on the tab Results. Treatment assignment for a next wave is based on Exploration Sampling. ",
                           "A lower limit to the treatment allocation proportion can be specified if needed.")
                    ),
                box(title = "Download a test dataset",
                    h5("Parental Engagement"),
                    helpText("Dataset from the paper", tags$a(href="https://drive.google.com/file/d/1HJCZQzavbKgGE3n8w4TiGviMq7wi37Vf/view?usp=sharing",
                           " \"Adaptive Experiments for Policy Choice: Phone Calls for Home Reading in Kenya\" ", target="_blank")),
                    downloadButton("download_our_data", "Download"),
                    tags$hr(),
                    h5("Precision Agriculture for Development"),
                    helpText("Dataset from the paper \"Adaptive treatment assignment in experiments for policy choice\" "),
                    downloadButton("download_pad_data", "Download"),
                    tags$hr()
                    )
                )
              ),

      tabItem("rawdata",
              fluidRow(
                box(title = "Upload data",
                  helpText(paste0("Upload your dataset -or a test dataset downloaded in the Start tab-. The dataset must have the following variable names ",
                                  "in any order:"),
                  br(),
                  "1. treatment: treatment allocation of the adaptive experiment. It can be numerical or string.",
                  br(),
                  "2. outcome: targeted outcome that is used for treatment allocation in next waves",
                  br(),
                  "3. wave: wave index. It can be numerical or date. If your wave variable is in date format please select the option \"Wave variable is a date\" on the right panel."
                  )
                  ),

                box(
                  # # Input: Select a file ----
                  # TODO: add support for excel
                  fileInput("file1", " ",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),


                  # Horizontal line ----
                  tags$hr(),

                  # Input: Checkbox if you are using dates instead of waves ----
                  checkboxInput("wave_date_flag", "Wave variable is a date", FALSE),

                  # Input: Checkbox if file has header ----
                  checkboxInput("header", "Header", TRUE),

                  # Input: Select separator ----
                  radioButtons("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ","),

                  # Input: Select quotes ----
                  radioButtons("quote", "Quote",
                               choices = c(None = "",
                                           "Double Quote" = '"',
                                           "Single Quote" = "'"),
                               selected = '"'),

                  # Horizontal line ----
                  tags$hr(),

                  # Input: Select number of rows to display ----
                  radioButtons("disp", "Display",
                               choices = c(Head = "head",
                                           All = "all"),
                               selected = "head"),

                  mainPanel(

                    # Output: Data file ----
                    tableOutput("contents")

                  )
                )
              )
      ),


      tabItem("model",
              fluidPage(
                box(title = "Select a model",
                    helpText("There are three model specifications available: ",
                    br(),
                    br(),
                    "1. Bernoulli: outcome variable has values 1 or 0. The model fitted is a bayesian bernoulli model with uninformative beta priors.",
                    br(),
                    br(),
                    paste0("2. Binomial - logit: outcome variable has integer values. The maximum number of successes must be selected. ",
                           "The model fitted is a bayesian Binomial GLM model with uninformative priors on the coefficients."),
                    br(),
                    br(),
                    "3. Linear: outcome variable: The model fitted is a bayesian linear regression with uninformative priors on the coefficients."),

                  selectInput("select_model",
                              h4("Select a model"),
                              choices = list("Bernoulli" = 1,
                                             "Binomial (logit)" = 2,
                                             "Linear model" = 3), selected = 1),
                  conditionalPanel(
                    condition = "input.select_model == 2",
                    h5("Please select the maximum number of successes"),
                    numericInput("max_trials",
                               "",
                               1,
                               min = 1,
                               step = 1)
                  )
                )

              )
      ),


      tabItem("results",
              fluidPage(
                titlePanel("Results"),

                fluidRow(
                  infoBoxOutput("sample_n_obs"),
                  infoBoxOutput("avg_outcome")
                  ),

                fluidRow(
                  box(
                    shinycssloaders::withSpinner(
                      plotOutput("average_outcome_plot")
                    ),

                  ),
                  box(
                    shinycssloaders::withSpinner(
                      plotOutput("posterior_distributions_plot")
                    )
                  )

                ),

                fluidRow(
                  box(h4("Exploration sampling shares for next wave"),
                      shinycssloaders::withSpinner(
                        tableOutput("exsamp_table")
                        )
                    ),
                  box(title = "Success rate per wave from observed raw data",
                      shinycssloaders::withSpinner(
                        tableOutput("success_table")
                      )
                  )


                )
              )
          ),

      tabItem("next_wave",
              fluidPage(
                  box(h4("Exploration sampling shares for next wave"),
                      shinycssloaders::withSpinner(
                        tableOutput("exsamp_table2")
                        ),
                      tags$hr(),
                      numericInput("n_obs_next_wave", h4("Select observations for next wave"), value = 1),
                      br(),
                      numericInput("treatment_kept_percent",
                                   h5("Select minimum proportion of observations per treatment arm"),
                                   0,
                                   min = 0,
                                   step = 0.1),
                      downloadButton('downloadData', 'Download allocation')
                  )


                )

           )

      )
    )
  )

server <- function(input, output) {

  raw_data <- reactive({
      read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)

  })

  treatment_encoding <- reactive ({
    req(input$file1)

    treatment_encoding = generateEncoding(raw_data()$treatment)
    return(treatment_encoding)
  })

  output_draws <- reactive({
    req(input$file1)
    req(input$select_model)
    req(input$max_trials)

    if (input$select_model == 1) {
      encoded_treatment = encodeLabels(raw_data()$treatment, treatment_encoding())
      dist_cache = bayesianUpdateBernoulli(encoded_treatment, raw_data()$outcome)
      theta_matrix = sampleDistribution(dist_cache)
    }
    if (input$select_model == 2) {
      encoded_treatment = encodeLabels(raw_data()$treatment, treatment_encoding())
      theta_matrix = bayesianBinomial(encoded_treatment, raw_data()$outcome/input$max_trials,
                                      max_trials = input$max_trials, include_constant = FALSE)
      theta_matrix = t(theta_matrix)
    }
    if (input$select_model == 3) {
      encoded_treatment = encodeLabels(raw_data()$treatment, treatment_encoding())
      theta_matrix = bayesianNormal(encoded_treatment, raw_data()$outcome, include_constant = FALSE)
      theta_matrix = t(theta_matrix)
    }
    return(theta_matrix)
  })

  output_exsamp <- reactive({
    req(input$select_model)
    req(input$treatment_kept_percent)

    percent_numerical = (as.numeric(input$treatment_kept_percent))

    theta_matrix = output_draws()
    thompson_shares = thompsonSampling(theta_matrix)
    exsamp_shares = as.numeric(explorationSampling(thompson_shares))

    if (input$treatment_kept_percent == 0) {
      return(exsamp_shares)
    } else {
      temp_shares = exsamp_shares
      temp_shares[which(exsamp_shares <= percent_numerical)] = percent_numerical

      probability_remaining = (1-percent_numerical*sum(exsamp_shares < percent_numerical))
      temp_shares[which(exsamp_shares > percent_numerical)] = probability_remaining * exsamp_shares[which(exsamp_shares > percent_numerical)] / sum(exsamp_shares[exsamp_shares > percent_numerical])
      exsamp_shares = temp_shares
    }
    return(exsamp_shares)
  })

  output_new_allocation <- reactive({
    req(input$file1)
    req(input$n_obs_next_wave)
    req(input$select_model)

    new_allocation = data.frame("treatment" = proportionalAssignment(output_exsamp(), input$n_obs_next_wave))
    return(new_allocation)
  })

  output$sample_n_obs <- renderInfoBox({
    req(input$select_model)
    req(input$file1)

    infoBox(
      "Number of observations", dim(raw_data())[1], icon = icon("list"), fill = FALSE
    #  "Observations", paste0(testing_rand()), icon = icon("list"), fill = FALSE
    )
  })

  output$avg_outcome <- renderInfoBox({
    req(input$select_model)
    req(input$file1)

    infoBox(
      "Average success rate (from raw data)", paste0(round(mean(raw_data()$outcome)/input$max_trials *100, 2), " %"), icon = icon("list"), fill = FALSE
    #  "Success rate", paste0(max(raw_data()$wave)), icon = icon("list"), fill = FALSE
    )
  })

  output$contents <- renderTable({
    req(input$select_model)
    req(input$file1)

    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })

  generate_exsamp_table <- reactive({
    encoded_treatment = encodeLabels(raw_data()$treatment, treatment_encoding())
    n_treatments = dim(treatment_encoding())[1]
    exsamp_shares = output_exsamp()
    #mean_success = dist_cache$parameters$alpha / (dist_cache$parameters$alpha + dist_cache$parameters$beta)
    mean_success = as.numeric(tapply(raw_data()$outcome, encoded_treatment, mean))
    model_coeffs = apply(output_draws(), 1, mean)

    if (input$select_model == 1) {
      output_proportions = data.frame('Treatment' = decodeLabels(1:n_treatments, treatment_encoding()),
                                      'Share of Successes (raw data)' = paste0(round(mean_success*100, 2), '%'),
                                      'ExSamp Proportions' = paste0(round(exsamp_shares,6)),
                                      check.names = FALSE)
    }
    if (input$select_model == 2) {
      output_proportions = data.frame('Treatment' = decodeLabels(1:n_treatments, treatment_encoding()),
                                      'Success Rate' = paste0(round(mean_success/input$max_trials*100, 2), '%'),
                                      'Model Coefficients'= paste0(round(model_coeffs, 2)),
                                      'ExSamp Proportions' = paste0(round(exsamp_shares,6)),
                                      check.names = FALSE)
    }
    if (input$select_model == 3) {
      output_proportions = data.frame('Treatment' = decodeLabels(1:n_treatments, treatment_encoding()),
                                      'Average Outcome' = paste0(round(mean_success, 2), '%'),
                                      'Model Coefficients'= paste0(round(model_coeffs, 2)),
                                      'ExSamp Proportions' = paste0(round(exsamp_shares,6)),
                                      check.names = FALSE)
    }

    return(output_proportions)
  })

  output$exsamp_table <- renderTable({
    req(input$file1)
    req(input$select_model)

    return(generate_exsamp_table())
  })

  output$exsamp_table2 <- renderTable({
    req(input$file1)
    req(input$select_model)

    return(generate_exsamp_table())
  })

  output$success_table <- renderTable({
    req(input$file1)
    req(input$select_model)

    encoded_treatment = encodeLabels(raw_data()$treatment, treatment_encoding())
    n_treatments = dim(treatment_encoding())[1]
    waves = unique(raw_data()$wave)
    n_waves = length(waves)
    output_success_mat = matrix("", n_treatments, n_waves)
    names_success = c()

    for (i in 1:n_waves) {
      temp_data = raw_data()[raw_data()$wave == waves[i], ]
      mean_on_wave = tapply(temp_data$outcome/input$max_trials, factor(temp_data$treatment,
                                                                treatment_encoding()$original),
                            mean)
      output_success_mat[ , i] = paste0(round(mean_on_wave*100,2), " %")
      names_success = c(names_success, paste0('Wave ', waves[i]))
    }

    output_success = data.frame('treatment' = treatment_encoding()$original, output_success_mat)
    colnames(output_success) = c('Treatment', names_success)

    output_success_temp = as.data.frame(t(output_success[, -1]))
    colnames(output_success_temp) = rownames(output_success)
    output_success_temp$Wave = names_success
    output_success_temp = output_success_temp[c("Wave", colnames(output_success_temp)[1:dim(output_success_temp)[2]-1])]
    output_success = output_success_temp

    return(output_success)
  })

  output$posterior_distributions_plot <- renderPlot({
    req(input$select_model)
    req(input$file1)

    if (input$select_model == 1) {
      x_label = "Theta"
    }
    if (input$select_model == 2) {
      x_label = "Beta"
    }
    if (input$select_model == 3) {
      x_label = "Beta"
    }

    return(plotPosteriorDistributions(output_draws(), treatment_encoding(), x_label = x_label))
  })

  output$average_outcome_plot <- renderPlot({
    req(input$select_model)
    req(input$file1)

    return(plotAverageOutcome(raw_data(), max(raw_data()$wave), date_wave = input$wave_date_flag))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("exsamp-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(output_new_allocation(), file)
    })

  our_dataset <- reactive ({
    read.csv("./data/wave2_engagement_reduced_test.csv")
  })
  pad_dataset <- reactive ({
    read.csv("./data/combined_outcomes_PAD.csv")
  })

  output$download_our_data <- downloadHandler(
    filename = function() {
      paste0("test-data", ".csv")
    },
    content = function(file) {
      write.csv(our_dataset(), file, row.names = FALSE)
    })

  output$download_pad_data <- downloadHandler(
    filename = function() {
      paste0("test-data", ".csv")
    },
    content = function(file) {
      write.csv(pad_dataset(), file, row.names = FALSE)
    })

}

shinyApp(ui, server)
