% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_test_to_files.R
\name{export_test_to_files}
\alias{export_test_to_files}
\title{Transform questions into MD, DOCX, HTML, or PDF files}
\usage{
export_test_to_files(
  test_parameters,
  propositions,
  translations,
  test_folder,
  format,
  language,
  template_folder = NA,
  template = NA
)
}
\arguments{
\item{test_parameters}{Tibble. List of questions with associated parameters.}

\item{propositions}{Tibble. List of propositions, criteria, and associated feedback.}

\item{translations}{Tibble. Table containing translations of items and explanations.}

\item{test_folder}{Character. Path to the exam folder.}

\item{format}{Character. Whether the files should be MD, DOCX, HTML, or PDF}

\item{language}{Character. ISO2 code of the printed language.}

\item{template_folder}{Character. Path to the tex folder.}

\item{template}{Character. Name of the template to be used to format the test.}
}
\description{
Function creating tests to be printed out.
}
\author{
Nicolas Mangin
}
