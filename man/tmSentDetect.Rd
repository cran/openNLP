\name{tmSentDetect-methods}
\docType{methods}
\alias{tmSentDetect}
\alias{tmSentDetect-methods}
\alias{tmSentDetect,PlainTextDocument-method}
\title{Methods for Function tmSentDetect in Package `openNLP'}
\description{
Methods for function \code{tmSentDetect} in package \pkg{openNLP}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Detect sentences in
      \code{object} and return the object.}
  }
}
\examples{
if(!require("tm")) stop("could not load tm package")
data("crude")
crude[[1]]
tmSentDetect(crude[[1]])
}
\keyword{methods}
