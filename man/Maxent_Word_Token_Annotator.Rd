\name{Maxent_Word_Token_Annotator}
\alias{Maxent_Word_Token_Annotator}
\title{Apache OpenNLP based word token annotators}
\description{
  Generate an annotator which computes word token annotations using the
  Apache OpenNLP Maxent tokenizer.
}
\usage{
Maxent_Word_Token_Annotator(language = "en", probs = FALSE, model = NULL)
}
\arguments{
  \item{language}{
    a character string giving the ISO-639 code of the language being
    processed by the annotator.
  }
  \item{probs}{
    a logical indicating whether the computed annotations should provide
    the token probabilities obtained from the Maxent model as their
    \sQuote{prob} feature.
  }
  \item{model}{
    a character string giving the path to the Maxent model file to be
    used, or \code{NULL} indicating to use a default model file for the
    given language (if available, see \bold{Details}).
  }
}
\value{
  An \code{\link[NLP]{Annotator}} object giving the created word token
  annotator.
}
\details{
  See \url{http://opennlp.sourceforge.net/models-1.5/} for available
  model files.  For languages other than English, these can conveniently
  be made available to \R by installing the respective
  \pkg{openNLPmodels.\var{language}} package from the repository at
  \url{http://datacube.wu.ac.at}.  For English, no additional
  installation is required.
}
\seealso{
  \url{http://opennlp.apache.org} for more information about Apache
  OpenNLP.
}
\examples{
require("NLP")
## Some text.
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)

## Need sentence token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
a1 <- annotate(s, sent_token_annotator)

word_token_annotator <- Maxent_Word_Token_Annotator()
word_token_annotator
a2 <- annotate(s, word_token_annotator, a1)
a2
## Variant with word token probabilities as features.
head(annotate(s, Maxent_Word_Token_Annotator(probs = TRUE), a1))

## Can also perform sentence and word token annotations in a pipeline:
a <- annotate(s, list(sent_token_annotator, word_token_annotator))
head(a)
}
