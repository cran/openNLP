\name{sentDetect}
\alias{sentDetect}
\title{Detect sentences}
\description{
  Detect sentences.
}
\usage{
sentDetect(s, language = "en", model = NULL)
}
\arguments{
  \item{s}{A character vector where the sentences should be detected.}
  \item{language}{A character vector giving \code{s}'s language. This
    argument is only used if \code{model} is \code{NULL} for selecting a
    default model. At the moment only \code{en} (English) and \code{es}
    (Spanish) are supported.}
  \item{model}{A model.}
}
\details{
  If \code{model} is \code{NULL} then a default model for detection of
  English or Spanish sentences from the \pkg{openNLPmodels} is loaded.
}
\value{
  A character vector resulting from sentence detection in \code{s}.
}
\references{
  OpenNLP.
  \url{http://opennlp.sourceforge.net/}
}
\author{Ingo Feinerer}
\examples{
s <- "This is a sentence. This another---but with dash-like structures, and some commas.
Maybe another with question marks? Sure!"
sentDetect(s, language = "en")
s <- "¿Como se llama usted? El castellano es la lengua española oficial del Estado."
sentDetect(s, language = "es")
}
\keyword{file}
