% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_sum.missing_summary.R
\name{tbl_sum.cd_completeness_summary}
\alias{tbl_sum.cd_completeness_summary}
\title{Summary for \code{cd_completeness_summary}}
\usage{
\method{tbl_sum}{cd_completeness_summary}(x, ...)
}
\arguments{
\item{x}{A \code{cd_completeness_summary} object containing yearly data completeness metrics.}

\item{...}{Additional arguments for compatibility with S3 methods.}
}
\value{
A character vector that summarizes the data content and purpose,
specifically indicating the proportion of values that are complete across years.
}
\description{
This function provides a custom summary for \code{cd_completeness_summary} objects,
offering an overview of the percentage of data completeness (non-missing values)
for each indicator, aggregated by year. The summary output helps users quickly
identify data completeness trends across indicators and time points, giving insight
into potential gaps in data collection.
}
\examples{
\dontrun{
# Generate a summary of data completeness by year
tbl_sum(cd_completeness_summary_object)
}

}
