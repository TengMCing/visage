
# CLOSED_FORM -------------------------------------------------------------

class_CLOSED_FORM <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  new_class(BASE, env = env, class_name = "CLOSED_FORM")


# init --------------------------------------------------------------------

  init_ <- function(expr) {

    # The expression has to be provided as formula
    if (!'formula' %in% class(expr)) stop("`expr` is not a formula!")

    # Only keeps the RHS of the last "~" character
    self$expr <- str2lang(gsub("^.*~", "", deparse(expr)))

    # Save all the symbols and symbol types
    self$sym <- list()
    self$sym_name <- list()
    self$sym_type <- list()

    # Get the evaluation environment
    env <- attr(expr, ".Environment")

    # Flatten the AST
    all_syms <- as.list(unlist(self$ast(self$expr)))

    for (sym in all_syms) {

      # If `sym` is not a constant
      if (is.symbol(sym)) {

        # Get the value of the symbol from the environment
        self$sym[[sym]] <- eval(sym, envir = env)

        # If it is an `oop` object
        if ("oop" %in% class(self$sym[[sym]])) {

          # And it has the method `gen`
          if (is.function(self$sym[[sym]]$gen)) {

            # Then mark it as random variable or closed form
            self$sym_type[[sym]] <- "rand_var or closed_form"
          }
        } else {

          # Otherwise, mark it as other
          self$sym_type[[sym]] <- "other"
        }
      }

    }

    self$sym_name <- as.list(names(self$sym))

    return(invisible(NULL))

  }


# ast ---------------------------------------------------------------------

  # Get the AST of an expression
  ast_ <- function(expr) {

    if (is.call(expr)) {
      lapply(as.list(expr), self$ast)
    } else {
      expr
    }

  }

# compute -----------------------------------------------------------------

  compute_ <- function() {

    # Compute the closed form expression without generating any random values via `gen` method
    eval(self$expr, envir = self$var)
  }

# gen_rhs -----------------------------------------------------------------

  gen_rhs_ <- function(n) {
    # If there is no random variables, then repeat the result for n times
    if (!"rand_var or closed_form" %in% unlist(self$sym_type)) {
      return(list(lhs = rep(self$compute(), n), rhs = list()))
    }

    # A list that store all the pre-evaluated values
    value_list <- self$sym

    # A list that store all the random values
    rhs <- list()

    # Symbols that needs to be pre-evaluated
    rand_index <- which(unlist(self$sym_type) == "rand_var or closed_form")
    rand_name <- unlist(self$sym_name)[rand_index]

    for (i in rand_name) {

      # Use the `gen` method to generate random values
      # If symbol is a closed form object
      if ("CLOSED_FORM" %in% self$sym[[i]]$..class..) {

        gen_value <- self$sym[[i]]$gen(n, rhs = TRUE)

        # Store the pre-evaluated left hand side result
        value_list[[i]] <- gen_value$lhs

        # Store the random values
        rhs[[i]] <- gen_value$rhs

      } else {

        gen_value <- self$sym[[i]]$gen(n)

        # Otherwise, store the vector and the random values
        value_list[[i]] <- gen_value
        rhs[[i]] <- gen_value
      }

    }

    # Evaluate the expression in the pre-evaluated list
    lhs <- eval(self$expr, envir = value_list)

    return(list(lhs = lhs, rhs = rhs))
  }

# gen ---------------------------------------------------------------------

  gen_ <- function(n, rhs = FALSE) {

    if (rhs) return(self$gen_rhs(n))

    # If there is no random variables, then repeat the result for n times
    if (!"rand_var or closed_form" %in% unlist(self$sym_type)) {
      return(rep(self$compute(), n))
    }

    # A list that store all the pre-evaluated values
    value_list <- self$sym

    # Symbols that needs to be pre-evaluated
    rand_index <- which(unlist(self$sym_type) == "rand_var or closed_form")
    rand_name <- unlist(self$sym_name)[rand_index]

    for (i in rand_name) {

      # Use the `gen` method to generate random values
      gen_value <- self$sym[[i]]$gen(n)

      value_list[[i]] <- gen_value
    }

    return(eval(self$expr, envir = value_list))
  }

# str ---------------------------------------------------------------------

  str_ <- function() {
    result <- use_method(self, BASE$..str..)()
    result <- paste0(result, "\n EXPR = ", deparse(self$expr))
    rand_index <- which(unlist(self$sym_type) == "rand_var or closed_form")
    for (i in rand_index) {
      con_string <- gsub("\n", "\n   ", self$sym[[i]]$..str..(), fixed = TRUE)
      result <- paste0(result, "\n  - ", self$sym_name[[i]], ": ", con_string)
    }

    return(result)
  }

  register_method(env,
                  ..init.. = init_,
                  ..str.. = str_,
                  ast = ast_,
                  compute = compute_,
                  gen = gen_,
                  gen_rhs = gen_rhs_)

  return(env)
}

# CLOSED_FORM <- class_CLOSED_FORM()
