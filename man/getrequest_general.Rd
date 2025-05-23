% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{getrequest_general}
\alias{getrequest_general}
\title{getrequest_general}
\usage{
getrequest_general(
  database,
  target,
  page,
  size,
  timeout = NULL,
  pages = NULL,
  apikey,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{database}{Database name}

\item{target}{The API endpoint to target}

\item{page}{The page to retrieve}

\item{size}{The page size of the request}

\item{timeout}{Seconds to delay the batch request}

\item{pages}{Number of total pages of existing request}

\item{apikey}{API key}

\item{verbose}{Enable verbose mode}

\item{...}{Further valid API parameters}
}
\value{
Raw results
}
\description{
getrequest_general
}
