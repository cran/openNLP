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
  \item{s}{A character vector with texts from which sentences should be
    detected.}
  \item{language}{A character string giving the language of \code{s}.
    This argument is only used if \code{model} is \code{NULL} for
    selecting a default model.
    At the moment, languages \samp{en} (English), \samp{es} (Spanish),
    \samp{de} (German) and \samp{th} (Thai) are supported, provided that
    the corresponding openNLP model language packages
    (\pkg{openNLPmodels.en}, \dots) are available.}
  \item{model}{A model.}
}
\details{
  If \code{model} is \code{NULL} then a default model for sentence
  detection is loaded from the corresponding openNLP models language
  package.
}
\value{
  A character vector with all the sentences found in the elements of
  \code{s}.
}
\references{
  OpenNLP \url{http://opennlp.sourceforge.net/}
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
