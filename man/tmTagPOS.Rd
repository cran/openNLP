\name{tmTagPOS-methods}
\docType{methods}
\alias{tmTagPOS}
\alias{tmTagPOS-methods}
\alias{tmTagPOS,PlainTextDocument-method}
\title{Methods for Function tmTagPOS in Package `openNLP'}
\description{
Methods for function \code{tmTagPOS} in package \pkg{openNLP}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Tag part-of-speech in
      \code{object} and return the object.}
  }
}
\examples{
if(!require("tm")) stop("could not load tm package")
data("crude")
crude[[1L]]
tmTagPOS(crude[[1L]])
}
\keyword{methods}
