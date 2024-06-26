% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_fig_ratio_mvpa_sed.R
\name{create_fig_ratio_mvpa_sed}
\alias{create_fig_ratio_mvpa_sed}
\title{Create a figure showing the mean daily MVPA/SED ratio}
\usage{
create_fig_ratio_mvpa_sed(score, language = c("en", "fr", "de"))
}
\arguments{
\item{score}{A numeric value for mean daily MVPA/SED ratio.}

\item{language}{A character value for setting the language with which the figure should be created: \code{en} for english; \code{fr} for french.}
}
\value{
A ggplot object.
}
\description{
The function generates a figure showing mortality hazard ratio in correspondence with the daily mean of the MVPA/SED ratio.
The figure is based on data extracted from Chastin et al. paper (2021; doi: 10.1123/jpah.2020-0635).
}
\examples{
create_fig_ratio_mvpa_sed(score = 0.06)

}
