% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exampleData.R
\docType{data}
\name{exampleData}
\alias{exampleData}
\title{The data extracted from a meta-analysis by Cabibbo et al.}
\format{
A data frame with with the 8 following variables (columns).
\describe{
  \item{Study}{This numeric vector represents number of the study.}
  \item{FirstAuthor}{This vector represents the name of the first author.}
  \item{YearPub}{This numeric vector represents the publication year.}
  \item{Time}{This numeric vector represents the times for which the survival rates are collected in years.}
  \item{Survival}{This numeric vector represents the survival rates for each value of Time}
  \item{NbRisk}{This numeric vector represents the number of at-risk patients for each value of Time}
  \item{Location}{This factor indicates the location of the study (Asia, North Amercia or Europe)}
  \item{Design}{This factor indicates if the study is monocentric ou multicentric.}
  ...
}
}
\usage{
exampleData
}
\description{
Data were extracted from the studies included in the meta-analysis by Cabibbo et al. which aimed to assess the survival rate in untreated patients with hepatocellular carcinoma.
}
\details{
The survival probabilities were extracted from the published survival curves each month during the first six months and then by step of three months. The pictures of the curves were digitalized using the R package ReadImage and the probabilities were extracted using the package digitize proposed by Poisot. The numbers of at-risk patients for each interval of time were derived from the numbers of at-risk patients reported in the studies, and using the methods of Parmar or Williamson to account for censorship. Studies have different length of follow-up. For each study, survival probabilities and the numbers of at-risk patients were collected at all points in time before the end of follow-up.
}
\author{
Shubhram Pandey \email{shubhram.pandey@PAREXEL.com}
}
\keyword{datasets}
