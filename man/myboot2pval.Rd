% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2pval.R
\name{myboot2pval}
\alias{myboot2pval}
\title{Bootstrap interval for a two sample test}
\usage{
myboot2pval(x1, x2, conf.level = 0.95, iter = 3000, mudiff = 0, test = "two")
}
\arguments{
\item{x1}{data sample 1}

\item{x2}{data sample 2}

\item{conf.level}{desired level of confidence}

\item{iter}{number of intervals}

\item{mudiff}{difference in means (mux1 - mux2)}

\item{test}{indicates two population t-test}
}
\value{
the p-value and a histogram of t-statistics
}
\description{
Bootstrap interval for a two sample test
}
\examples{
myboot2pval(x1 =x, x2 = y, mudiff = 3)

}
