% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_signatures.R
\name{plot_signature}
\alias{plot_signature}
\title{Plot an ID83 signature ro series of signatures}
\usage{
plot_signature(
  x,
  label = "Signature",
  title = "",
  xlabel = NULL,
  ylabel = "Count",
  usePercent = TRUE,
  ylimits = NULL,
  countsAsProportions = FALSE,
  separatePlots = FALSE,
  contextType = "auto"
)
}
\arguments{
\item{x}{A TidySig dataframe/tibble}

\item{label}{The right-side (i.e., facet) label.
Usually "Signature" or "Sample" or a sample ID.}

\item{title}{A title for the plot}

\item{xlabel}{An x-axis label}

\item{ylabel}{A y-axis label}

\item{usePercent}{Use percent scales (rather than counts)}

\item{ylimits}{Use custom ylimits (useful for normalizing the views of multiple signatures).
Takes a numeric vector length-two OR a string "smart" to indicate that consistent y-limits should
be automatically fit from the values in x.}

\item{countsAsProportions}{Convert the input data (in counts) to per-signature proportions}

\item{separatePlots}{If true, return a vector of plots, one for each signature, rather than one plot
with a facet for each signature.}

\item{contextType}{An argument for determining the context, generally "SBS96" or "ID83". The default, "auto",
will attempt to determine the context automatically based on the columns of the x.}
}
\value{
a ggplot2 object, unless separatePlot = True, in which case a list of ggplot objects is returned.
}
\description{
Plot an ID83 signature ro series of signatures
}
