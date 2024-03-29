% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_RAND_VAR.R
\name{RAND_NORMAL}
\alias{RAND_NORMAL}
\alias{rand_normal}
\title{RAND_NORMAL class environment}
\usage{
rand_normal(
  mu = 0,
  sigma = 1,
  env = new.env(parent = parent.frame()),
  init_call = sys.call()
)
}
\arguments{
\item{mu}{Numeric. Mean.}

\item{sigma}{Numeric. Standard deviation.}

\item{env}{Environment. The instance environment.}

\item{init_call}{Call. Contents of the \code{..init_call...} It is recommended to leave it
as default.}
}
\value{
An instance environment.
}
\description{
This is the class of the normal random variable, inherited from
\link{RAND_VAR}. It is an environment with S3 class \code{bandicoot_oop}.
}
\section{Functions}{
\itemize{
\item \code{rand_normal()}: Class constructor, same as \code{RAND_NORMAL$instantiate()}.

}}
\section{Class information}{
\subsection{Parent classes}{
\itemize{
\item Direct:
\itemize{
\item \link{RAND_VAR}
}
\item Indirect:
\itemize{
\item \link[bandicoot:BASE]{bandicoot::BASE}
}
}
}

\subsection{New methods}{
\itemize{
\item E
\itemize{
\item \code{RAND_NORMAL$E()}
}
\item G
\itemize{
\item \code{\link[=RAND_NORMAL$gen]{RAND_NORMAL$gen()}}
}
\item I
\itemize{
\item \code{\link[=RAND_NORMAL$..init..]{RAND_NORMAL$..init..()}}
}
\item V
\itemize{
\item \code{RAND_NORMAL$Var()}
}
}
}
}

