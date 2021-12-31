
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
    self$expr <- str2lang(gsub("^.*~", "", paste(deparse(expr, width.cutoff = 500L), collapse = " ")))

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

# gen ---------------------------------------------------------------------

  gen_ <- function(n, rhs_val = FALSE, computed = NULL) {

    # If there is no random variables, then repeat the result for n times
    if (!"rand_var or closed_form" %in% unlist(self$sym_type)) {
      if (!rhs_val) return(rep(self$compute(), n))
      return(list(lhs = rep(self$compute(), n), rhs = list()))
    }

    # A list that the expression needs to be evaluated at
    value_list <- self$sym

    # A list that stores all the computed random values, or computed expression which contains random variables
    rhs <- list()
    if (!is.null(computed)) {
      rhs <- computed
    }

    # Symbols that needs to be evaluated
    rand_name <- unlist(self$sym_name)[which(unlist(self$sym_type) == "rand_var or closed_form")]

    for (i in rand_name) {

      # Skip computed values or computed expressions
      if (!is.null(rhs[[i]])) {
        value_list[[i]] <- rhs[[i]]
        next
        }

      # Use the `gen` method to generate random values
      if ("CLOSED_FORM" %in% self$sym[[i]]$..class..) {

        # Pass the computed values to avoid re-computation of the same variable or expression
        gen_value <- self$sym[[i]]$gen(n, rhs_val = TRUE, computed = rhs)

        # Store the pre-evaluated left hand side result
        value_list[[i]] <- gen_value$lhs

        # Update RHS
        rhs <- gen_value$rhs

        # Also keep a record in the computed list
        rhs[[i]] <- gen_value$lhs

      } else {

        gen_value <- self$sym[[i]]$gen(n)

        # Otherwise, store the vector and the random values
        value_list[[i]] <- gen_value
        rhs[[i]] <- gen_value
      }

    }

    # Evaluate the expression in the pre-evaluated list
    lhs <- eval(self$expr, envir = value_list)

    if (!rhs_val) return(lhs)
    return(list(lhs = lhs, rhs = rhs))
  }

# str ---------------------------------------------------------------------

  str_ <- function() {
    if (!self$..instantiated..) {
      return(paste0("<", self$..type.., " class>"))
    }

    result <- use_method(self, BASE$..str..)()
    result <- paste0(result, "\n EXPR = ", paste(deparse(self$expr, width.cutoff = 500L), collapse =  " "))
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
                  gen = gen_)

  return(env)
}

