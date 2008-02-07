\name{tagPOS}
\alias{tagPOS}
\title{Tag part-of-speech}
\description{
  Tag part-of-speech.
}
\usage{
tagPOS(sentence, language = "en", model = NULL, tagdict = NULL)
}
\arguments{
  \item{sentence}{A character vector to get POS-tagged.}
  \item{model}{A model.}
  \item{language}{A character vector giving \code{sentence}'s language. This
    argument is only used if \code{model} is \code{NULL} for selecting a
    default model. At the moment only \code{en} (English) and \code{es}
    (Spanish) are supported.}
  \item{tagdict}{A tag dictionary.}
}
\details{
  If \code{model} is \code{NULL} then a default model for English or
  Spanish part-of-speech tagging from the \pkg{openNLPmodels} is loaded. If
  \code{tagdict} is \code{NULL} a default English tag dictionary is
  taken.
}
\value{
  A character vector holding POS-tagged \code{sentence}.
}
\references{
  OpenNLP.
  \url{http://opennlp.sourceforge.net/}
}
\author{Ingo Feinerer}
\examples{
sentence <- "This is a short sentence consisting of some nouns, verbs, and adjectives."
tagPOS(sentence, language = "en")
s <- "el castellano es la lengua española oficial del Estado."
tagPOS(s, language = "es")
}
\keyword{file}
