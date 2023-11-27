
# RAND_VAR ----------------------------------------------------------------

RAND_VAR <- new.env()

#' RAND_VAR class environment
#'
#' @name RAND_VAR
#'
#' @description This is the base class of random variable, inherited from
#' [bandicoot::BASE]. It is an environment with S3 class `bandicoot_oop`.
#'
#' @param dist Character. Distribution name.
#' @param prm List. List of parameters.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [bandicoot::BASE]
#'
#' ## New attributes
#' * D:
#'    * [RAND_VAR$dist]
#' * P:
#'    * [RAND_VAR$prm]
#'
#' ## New methods
#' * E:
#'    * [RAND_VAR$E()]
#' * I:
#'    * [RAND_VAR$..init..()]
#' * S:
#'    * [RAND_VAR$set_prm()]
#'    * [RAND_VAR$..str..()]
#' * V:
#'    * [RAND_VAR$Var()]
#'
#' @export
RAND_VAR

#' @describeIn RAND_VAR Class constructor, same as `RAND_VAR$instantiate()`.
#' @export
rand_var <- function(dist = "",
                     prm = list(),
                     env = new.env(parent = parent.frame()),
                     init_call = sys.call()) {
  RAND_VAR$instantiate(dist = dist,
                       prm = prm,
                       env = env,
                       init_call = init_call)
}

#' Distribution name
#'
#' @name RAND_VAR$dist
#'
#' @description A string, will be initialized after an instance is built.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_var(dist = "uniform", prm = list(a = 1, b = 2))
#' test$prm
#' test$dist
RAND_VAR$dist

#' List of parameters
#'
#' @name RAND_VAR$prm
#'
#' @description A list, will be initialized after an instance is built.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_var(dist = "uniform", prm = list(a = 1, b = 2))
#' test$prm
#' test$dist
RAND_VAR$prm

#' Initialization method
#'
#' @name RAND_VAR$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#'
#' ## Usage
#' ```
#' RAND_VAR$..init..(dist = "", prm = list())
#' ```
#' @param dist Character. Distribution name.
#' @param prm List. List of parameters.
#' @return Return the object itself.
#'
#' @examples
#'
#' RAND_VAR$..init..
#'
#' # Instantiate
#' test <- rand_var(dist = "uniform", prm = list(a = 1, b = 2))
#' test$prm
#' test$dist
RAND_VAR$..init..

#' Expectation of the random variable
#'
#' @name RAND_VAR$E
#'
#' @description Expectation of the random variable.
#' User could override this method in derived class.
#'
#' ## Usage
#' ```
#' RAND_VAR$E()
#' ```
#'
#' @return NA
#'
#' @examples
#'
#' RAND_VAR$E()
RAND_VAR$E

#' Variance of the random variable
#'
#' @name RAND_VAR$Var
#'
#' @description Variance of the random variable.
#' User could override this method in derived class.
#'
#' ## Usage
#' ```
#' RAND_VAR$Var()
#' ```
#'
#' @return NA
#'
#' @examples
#'
#' RAND_VAR$Var()
RAND_VAR$Var

#' Generate random values
#'
#' @name RAND_VAR$gen
#'
#' @description This function generates random values from the random variable.
#'
#' ## Usage
#' ```
#' RAND_VAR$gen(n, ...)
#' ```
#'
#' @param n Integer. Number of observations.
#' @param ... Ignored.
#' @return NA. User needs to define their own `gen` method in derived class.
#'
#' @examples
#'
#' test <- RAND_VAR$gen(10)
RAND_VAR$gen

#' Generate random values
#'
#' @name RAND_VAR$set_prm
#'
#' @description This function updates the parameters.
#'
#' ## Usage
#' ```
#' RAND_VAR$set_prm(prm_name, prm_value)
#' ```
#'
#' @param prm_name List or Vector. A sequence of character parameter names.
#' @param prm_value List or Vector. A sequence of parameter values.
#' @return Return the object itself.
#'
#' @examples
#'
#' test <- rand_var()
#' test$set_prm("a", 1)
#' test
RAND_VAR$set_prm

#' String representation of the object
#'
#' @name RAND_VAR$..str..
#'
#' @description This function returns a string representation of the object.
#'
#' ## Usage
#' ```
#' RAND_VAR$..str..()
#' ```
#'
#' @return A string.
#'
#' @examples
#'
#' RAND_VAR$..str..()
#'
#' test <- rand_var(dist = "uniform", prm = list(a = 1, b = 2))
#' test$..str..()
RAND_VAR$..str..



# RAND_UNIFORM ------------------------------------------------------------

RAND_UNIFORM <- new.env()

#' RAND_UNIFORM class environment
#'
#' @name RAND_UNIFORM
#'
#' @description This is the class of the uniform random variable, inherited from
#' [RAND_VAR]. It is an environment with S3 class `bandicoot_oop`.
#'
#' @param a Numeric. Lower bound.
#' @param b Numeric. Upper bound.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [RAND_VAR]
#' * Indirect:
#'    * [bandicoot::BASE]
#'
#' ## New methods
#' * E
#'    * `RAND_UNIFORM$E()`
#' * G
#'    * [RAND_UNIFORM$gen()]
#' * I
#'    * [RAND_UNIFORM$..init..()]
#' * V
#'    * `RAND_UNIFORM$Var()`
#'
#' @export
RAND_UNIFORM

#' @describeIn RAND_UNIFORM Class constructor, same as `RAND_UNIFORM$instantiate()`.
#' @export
rand_uniform <- function(a = 0,
                         b = 1,
                         env = new.env(parent = parent.frame()),
                         init_call = sys.call()) {
  RAND_UNIFORM$instantiate(a = a,
                           b = b,
                           env = env,
                           init_call = init_call)
}

#' Initialization method
#'
#' @name RAND_UNIFORM$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#'
#' ## Usage
#' ```
#' RAND_UNIFORM$..init..(a = 0, b = 1)
#' ```
#'
#' @param a Numeric. Lower bound.
#' @param b Numeric. Upper bound.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_uniform(a = 1, b = 2)
#' test
RAND_UNIFORM$..init..

#' Generate random values
#'
#' @name RAND_UNIFORM$gen
#'
#' @description This function generates random values from the random variable.
#'
#' ## Usage
#' ```
#' RAND_UNIFORM$gen(n, a = NULL, b = NULL)
#' ```
#'
#' @param n Integer. Number of observations.
#' @param a Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param b Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::runif()]
#'
#' @examples
#'
#' test <- rand_uniform(a = 1, b = 2)
#' test$gen(10)
#'
#' test$gen(3, a = c(1,2,3), b = c(2,3,4))
RAND_UNIFORM$gen



# RAND_NORMAL -------------------------------------------------------------

RAND_NORMAL <- new.env()

#' RAND_NORMAL class environment
#'
#' @name RAND_NORMAL
#'
#' @description This is the class of the normal random variable, inherited from
#' [RAND_VAR]. It is an environment with S3 class `bandicoot_oop`.
#'
#' @param mu Numeric. Mean.
#' @param sigma Numeric. Standard deviation.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [RAND_VAR]
#' * Indirect:
#'    * [bandicoot::BASE]
#'
#' ## New methods
#' * E
#'    * `RAND_NORMAL$E()`
#' * G
#'    * [RAND_NORMAL$gen()]
#' * I
#'    * [RAND_NORMAL$..init..()]
#' * V
#'    * `RAND_NORMAL$Var()`
#'
#' @export
RAND_NORMAL

#' @describeIn RAND_NORMAL Class constructor, same as `RAND_NORMAL$instantiate()`.
#' @export
rand_normal <- function(mu = 0,
                        sigma = 1,
                        env = new.env(parent = parent.frame()),
                        init_call = sys.call()) {
  RAND_NORMAL$instantiate(mu = mu,
                          sigma = sigma,
                          env = env,
                          init_call = init_call)
}

#' Initialization method
#'
#' @name RAND_NORMAL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#'
#' ## Usage
#' ```
#' RAND_NORMAL$..init..(mu = 0, sigma = 1)
#' ```
#'
#' @param mu Numeric. Mean.
#' @param sigma Numeric. Standard deviation.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_normal(mu = 1, sigma = 2)
#' test
RAND_NORMAL$..init..

#' Generate random values
#'
#' @name RAND_NORMAL$gen
#'
#' @description This function generates random values from the random variable.
#'
#' ## Usage
#' ```
#' RAND_NORMAL$gen(n, mu = NULL, sigma = NULL)
#' ```
#'
#' @param n Integer. Number of observations.
#' @param mu Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param sigma Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::rnorm()]
#'
#' @examples
#'
#' test <- rand_normal(mu = 1, sigma = 2)
#' test$gen(10)
#'
#' test$gen(3, mu = c(0,1,2), sigma = c(1,2,4))
RAND_NORMAL$gen


# RAND_LOGNORMAL ----------------------------------------------------------

RAND_LOGNORMAL <- new.env()

#' RAND_LOGNORMAL class environment
#'
#' @name RAND_LOGNORMAL
#'
#' @description This is the class of the log-normal random variable, inherited from
#' [RAND_VAR]. It is an environment with S3 class `bandicoot_oop`.
#'
#' @param mu Numeric. Mean of the log of the random variable.
#' @param sigma Numeric. Mean of the log of the random variable.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [RAND_VAR]
#' * Indirect:
#'    * [bandicoot::BASE]
#'
#' ## New methods
#' * E
#'    * `RAND_LOGNORMAL$E()`
#' * G
#'    * [RAND_LOGNORMAL$gen()]
#' * I
#'    * [RAND_LOGNORMAL$..init..()]
#' * V
#'    * `RAND_LOGNORMAL$Var()`
#'
#' @export
RAND_LOGNORMAL

#' @describeIn RAND_LOGNORMAL Class constructor, same as `RAND_LOGNORMAL$instantiate()`.
#' @export
rand_lognormal <- function(mu = 0,
                           sigma = 1,
                           env = new.env(parent = parent.frame()),
                           init_call = sys.call()) {
  RAND_LOGNORMAL$instantiate(mu = mu,
                             sigma = sigma,
                             env = env,
                             init_call = init_call)
}

#' Initialization method
#'
#' @name RAND_LOGNORMAL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#'
#' ## Usage
#' ```
#' RAND_LOGNORMAL$..init..(mu = 0, sigma = 1)
#' ```
#'
#' @param mu Numeric. Mean of the log of the random variable.
#' @param sigma Numeric. Standard deviation of the log the random variable.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_lognormal(mu = 1, sigma = 2)
#' test
RAND_LOGNORMAL$..init..

#' Generate random values
#'
#' @name RAND_LOGNORMAL$gen
#'
#' @description This function generates random values from the random variable.
#'
#' ## Usage
#' ```
#' RAND_LOGNORMAL$gen(n, mu = NULL, sigma = NULL)
#' ```
#'
#' @param n Integer. Number of observations.
#' @param mu Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param sigma Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::rlnorm()]
#'
#' @examples
#'
#' test <- rand_lognormal(mu = 1, sigma = 2)
#' test$gen(10)
#'
#' test$gen(3, mu = c(0,1,2), sigma = c(1,2,3))
RAND_LOGNORMAL$gen



# RAND_UNIFORM_D ----------------------------------------------------------

RAND_UNIFORM_D <- new.env()

#' RAND_UNIFORM_D class environment
#'
#' @name RAND_UNIFORM_D
#'
#' @description This is the class of the discrete uniform random variable, inherited from
#' [RAND_VAR]. It is an environment with S3 class `bandicoot_oop`.
#'
#' @param a Numeric. Lower bound.
#' @param b Numeric. Upper bound.
#' @param k Integer. Number of unique discrete values.
#' @param even Boolean. Whether or not candidate values are evenly spaced.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [RAND_VAR]
#' * Indirect:
#'    * [bandicoot::BASE]
#'
#' ## New methods
#' * E
#'    * `RAND_UNIFORM_D$E()`
#' * G
#'    * [RAND_UNIFORM_D$gen()]
#' * I
#'    * [RAND_UNIFORM_D$..init..()]
#' * V
#'    * `RAND_UNIFORM_D$Var()`
#'
#' @export
RAND_UNIFORM

#' @describeIn RAND_UNIFORM_D Class constructor, same as `RAND_UNIFORM_D$instantiate()`.
#' @export
rand_uniform_d <- function(a = 0,
                           b = 1,
                           k = 5,
                           even = FALSE,
                           env = new.env(parent = parent.frame()),
                           init_call = sys.call()) {
  RAND_UNIFORM$instantiate(a = a,
                           b = b,
                           k = k,
                           even = even,
                           env = env,
                           init_call = init_call)
}

#' Initialization method
#'
#' @name RAND_UNIFORM_D$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#'
#' ## Usage
#' ```
#' RAND_UNIFORM_D$..init..(a = 0, b = 1, k = 5, even = FALSE)
#' ```
#'
#' @param a Numeric. Lower bound.
#' @param b Numeric. Upper bound.
#' @param k Integer. Number of unique discrete values.
#' @param even Boolean. Whether or not candidate values are evenly spaced.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_uniform_d(a = 1, b = 2, k = 3)
#' test
RAND_UNIFORM_D$..init..

#' Generate random values
#'
#' @name RAND_UNIFORM_D$gen
#'
#' @description This function generates random values from the random variable.
#'
#' ## Usage
#' ```
#' RAND_UNIFORM_D$gen(n, a = NULL, b = NULL, k = NULL, even = NULL)
#' ```
#'
#' @param n Integer. Number of observations.
#' @param a Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param b Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param k Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param even Boolean. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::runif()], [sample()]
#'
#' @examples
#'
#' test <- rand_uniform_d(a = 1, b = 2, k = 2, even = TRUE)
#' test$gen(10)
#'
#' test$gen(3, a = c(1,2,3), b = c(2,3,4), k = 1, even = c(TRUE, TRUE, FALSE))
RAND_UNIFORM_D$gen


# RAND_T ------------------------------------------------------------------

RAND_T <- new.env()

#' RAND_T class environment
#'
#' @name RAND_T
#'
#' @description This is the class of the student-t random variable, inherited from
#' [RAND_VAR]. It is an environment with S3 class `bandicoot_oop`.
#'
#' @param mu Numeric.
#' @param tau Numeric. Scale parameter.
#' @param df Integer. Degree of freedom.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [RAND_VAR]
#' * Indirect:
#'    * [bandicoot::BASE]
#'
#' ## New methods
#' * E
#'    * `RAND_T$E()`
#' * G
#'    * [RAND_T$gen()]
#' * I
#'    * [RAND_T$..init..()]
#' * V
#'    * `RAND_T$Var()`
#'
#' @export
RAND_T

#' @describeIn RAND_T Class constructor, same as `RAND_T$instantiate()`.
#' @export
rand_t <- function(mu = 0,
                   tau = 1,
                   df = 10,
                   env = new.env(parent = parent.frame()),
                   init_call = sys.call()) {
  RAND_T$instantiate(mu = mu,
                     tau = tau,
                     df = df,
                     env = env,
                     init_call = init_call)
}


#' Initialization method
#'
#' @name RAND_T$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#'
#' ## Usage
#' ```
#' RAND_T$..init..(mu = 0, tau = 1, df = 10)
#' ```
#'
#' @param mu Numeric. Mean. Default is 0.
#' @param tau Numeric. Scale parameter. Default is 1.
#' @param df Integer. Degree of freedom. Default is 10.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- rand_t(mu = 0, tau = 1, df = 3)
#' test
RAND_T$..init..

#' Generate random values
#'
#' @name RAND_T$gen
#'
#' @description This function generates random values from the random variable.
#'
#' ## Usage
#' ```
#' RAND_T$gen(n, mu = NULL, tau = NULL, df = NULL)
#' ```
#'
#' @param n Integer. Number of observations.
#' @param mu Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param tau Numeric. If it is provided and has length `n`, values will be used in
#' each realization.
#' @param df Integer. If it is provided and has length `n`, values will be used in
#' each realization.
#' @return A vector of numeric values.
#' @seealso [stats::rt()], [sample()]
#'
#' @examples
#'
#' test <- rand_t(df = 12)
#' test$gen(10)
#'
#' test$gen(3, mu = c(1,2,3), tau = c(2,3,4), df = 10)
RAND_T$gen

