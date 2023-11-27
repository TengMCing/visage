library(shiny)
library(tidyverse)
library(visage)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  withMathJax(),
  # shinyWidgets::setBackgroundColor(color = "grey"),
  fluidRow(
    column(3,
           h1("Training data simulation", style = "color: #fcba03"),
           sliderInput("j", "j", 2, 18, 2, step = 1),
           sliderInput("include_x2", "include_x2", 0, 1, 0, step = 1),
           sliderInput("include_z", "include_z", 0, 1, 0, step = 1),
           sliderInput("a", "a", -1, 1, -1, step = 0.1),
           sliderInput("b", "b", 0, 128, 0, step = 2),
           selectInput("x1_dist", "x1_dist", c("uniform", "normal", "lognormal", "discrete"), "uniform"),
           sliderInput("x1_sigma", "x1_sigma (only useful for normal and lognormal distribution)", 0.2, 0.8, 0.2, step = 0.1),
           sliderInput("x1_levels", "x1_levels (only useful for discrete distribution)", 5, 20, 5, step = 1),
           sliderInput("x1_even", "x1_even (only useful for discrete distribution)", 0, 1, 0, step = 1)
    ),
    column(6, align = "center", style = "background-color: grey;",
           fluidRow(column(2),
                    column(8, h4("The model is defined as $$\\begin{align*} y &= 1 + x1 + include_{x2} * x2 + include_z * (z + include_{x2} * w) + k * e\\\\ z &\\propto He_j(x1) \\\\ w &\\propto He_j(x2) \\\\ k &= \\sqrt{1 + (2 - |a|) * ((x1 + include_{x2} * x2) - a)^2 * b} \\end{align*}$$")),
                    column(2)),
           h3("Visual signal strength: ", textOutput("current_vss", inline = TRUE)),
           div(style = "background-color: grey; height: 540px; ", plotOutput("resplot"))),
    column(3,
           br(),
           br(),
           selectInput("x2_dist", "x2_dist", c("uniform", "normal", "lognormal", "discrete"), "uniform"),
           sliderInput("x2_sigma", "x2_sigma (only useful for normal and lognormal distribution)", 0.2, 0.8, 0.2, step = 0.1),
           sliderInput("x2_levels", "x2_levels (only useful for discrete distribution)", 5, 20, 5, step = 1),
           sliderInput("x2_even", "x2_even (only useful for discrete distribution)", 0, 1, 0, step = 1),
           selectInput("e_dist", "e_dist", c("uniform", "normal", "lognormal", "discrete", "t"), "discrete"),
           sliderInput("e_sigma", "e_sigma (useful for all distributions)", 0.1, 4, 0.5, step = 0.1),
           sliderInput("e_levels", "e_levels (only useful for discrete distribution)", 5, 20, 5, step = 1),
           sliderInput("e_even", "e_even (only useful for discrete distribution)", 0, 1, 0, step = 1),
           sliderInput("e_df", "e_df (only useful for t distribution)", 3, 10, 3, step = 1),
           sliderInput("n", "n", 30, 2000, 100, step = 1))
  )
)


# server ------------------------------------------------------------------

# Ensure the support of the predictor is [-1, 1]
stand_dist <- function(x) (x - min(x))/max(x - min(x)) * 2 - 1

get_x_var <- function(dist_name, sigma = 0.3, k = 5, even = TRUE) {

  # Define the x variable
  rand_uniform_x <- rand_uniform(-1, 1)
  rand_normal_raw_x <- rand_normal(sigma = sigma)
  rand_normal_x <- closed_form(~stand_dist(rand_normal_raw_x))
  rand_lognormal_raw_x <- rand_lognormal(sigma = sigma)
  rand_lognormal_x <- closed_form(~stand_dist(rand_lognormal_raw_x/3 - 1))
  rand_discrete_x <- rand_uniform_d(-1, 1, k = k, even = even)

  switch(as.character(dist_name),
         uniform = rand_uniform_x,
         normal = rand_normal_x,
         lognormal = rand_lognormal_x,
         discrete = rand_discrete_x)
}


get_e_var <- function(dist_name, df, e_sigma, e_even, e_k) {

  lognormal_sigma_table <- map_dbl(seq(0.001, 2, 0.001), ~sqrt((exp(.x^2) - 1) * exp(.x^2)))
  names(lognormal_sigma_table) <- seq(0.001, 2, 0.001)

  dist_name <- as.character(dist_name)

  if (dist_name == "uniform") {
    return(rand_uniform(a = -sqrt(12 * e_sigma^2)/2,
                        b = sqrt(12 * e_sigma^2)/2,
                        env = new.env(parent = .GlobalEnv)))
  }

  if (dist_name == "lognormal") {
    table_index <- which.min(abs(lognormal_sigma_table - e_sigma))
    mod_sigma <- as.numeric(names(lognormal_sigma_table))[table_index]
    return(rand_lognormal(mu = 0,
                          sigma = mod_sigma,
                          env = new.env(parent = .GlobalEnv)))
  }

  if (dist_name == "discrete") {
    return(rand_uniform_d(a = -sqrt(12 * e_sigma^2)/2,
                          b = sqrt(12 * e_sigma^2)/2,
                          even = e_even,
                          k = e_k,
                          env = new.env(parent = .GlobalEnv)))
  }

  if (dist_name == "t") {
    tau <- 1
    if (df > 2) tau <- sqrt(e_sigma^2 * (df - 2)/df)
    return(rand_t(tau = tau,
                  df = df,
                  env = new.env(parent = .GlobalEnv)))
  }

  return(rand_normal(sigma = e_sigma,
                     env = new.env(parent = .GlobalEnv)))
}


server <- function(input, output, session) {

  vss <- reactiveVal(value = 0)

  mod <- reactive({
    x1 <- get_x_var(input$x1_dist, sigma = input$x1_sigma, k = input$x1_levels, even = input$x1_even)
    x2 <- get_x_var(input$x2_dist, sigma = input$x2_sigma, k = input$x2_levels, even = input$x2_even)
    e <- get_e_var(input$e_dist, df = input$e_df, e_sigma = input$e_sigma, e_even = input$e_even, e_k = input$e_levels)

    phn_model(j = input$j,
              include_x2 = input$include_x2,
              include_z = input$include_z,
              a = input$a,
              b = input$b,
              x1 = x1,
              x2 = x2,
              e = e)
  })

  dat <- reactive({
    mod()$gen(input$n)
  })

  vss <- reactive({
    log(mod()$sample_effect_size(dat()) + 1)
  })

  output$current_vss <- renderText({
    vss()
  })

  output$resplot <- renderPlot({
    VI_MODEL$plot(dat(),
                  theme = theme_light(base_size = 11/5),
                  remove_axis = TRUE,
                  remove_legend = TRUE,
                  remove_grid_line = TRUE) +
      ggtitle(paste("Visual signal strength: ", vss()))

  }, height = 2100/4, width = 2100/5)
}


# app ---------------------------------------------------------------------

shinyApp(ui, server)

