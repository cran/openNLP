\name{tokenize}
\alias{tokenize}
\title{Tokenizer}
\description{
  Tokenizes the input.
}
\usage{
tokenize(s, language = "en", model = NULL)
}
\arguments{
  \item{s}{A character vector to be tokenized.}
  \item{language}{A character vector giving \code{s}'s language. This
    argument is only used if \code{model} is \code{NULL} for selecting a
    default model. At the moment only \code{en} (English) and \code{es}
    (Spanish) are supported.}
  \item{model}{A model.}
}
\value{
  A character vector holding the tokenized \code{s}.
}
\details{
  If \code{model} is \code{NULL} then a default model for tokenization
  of English or Spanish texts from the \pkg{openNLPmodels} is loaded.
}
\references{
  OpenNLP.
  \url{http://opennlp.sourceforge.net/}
}
\author{Ingo Feinerer}
\examples{
s <- "This is a sentence."
tokenize(s, language = "en")
s <- "¿Como se llama usted? El castellano es la lengua española oficial
del Estado."
tokenize(s, language = "es")
}
\keyword{file}
