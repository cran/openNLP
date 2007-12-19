\name{tmTokenize-methods}
\docType{methods}
\alias{tmTokenize}
\alias{tmTokenize-methods}
\alias{tmTokenize,PlainTextDocument-method}
\title{Methods for Function tmTokenize in Package `openNLP'}
\description{
Methods for function \code{tmTokenize} in package \pkg{openNLP}.
}
\section{Methods}{
  \describe{
    \item{object = "PlainTextDocument"}{Tokenize \code{object} and
      return the object.}
  }
}
\examples{
if(!require("tm")) stop("could not load tm package")
data("crude")
crude[[1]]
tmTokenize(crude[[1]])
}
\keyword{methods}
