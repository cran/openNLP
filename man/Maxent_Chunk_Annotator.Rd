\name{Maxent_Chunk_Annotator}
\alias{Maxent_Chunk_Annotator}
\title{Apache OpenNLP based chunk annotators}
\description{
  Generate an annotator which computes chunk annotations using the
  Apache OpenNLP Maxent chunker.
}
\usage{
Maxent_Chunk_Annotator(language = "en", probs = FALSE, model = NULL)
}
\arguments{
  \item{language}{
    a character string giving the ISO-639 code of the language being
    processed by the annotator.
  }
  \item{probs}{
    a logical indicating whether the computed annotations should provide
    the token probabilities obtained from the Maxent model as their
    \sQuote{chunk_prob} feature.
  }
  \item{model}{
    a character string giving the path to the Maxent model file to be
    used, or \code{NULL} indicating to use a default model file for the
    given language (if available, see \bold{Details}).
  }
}
\value{
  An \code{\link[NLP]{Annotator}} object giving the generated chunk
  annotator.
}
\details{
  See \url{http://opennlp.sourceforge.net/models-1.5/} for available
  model files.  These can conveniently be made available to \R by
  installing the respective \pkg{openNLPmodels.\var{language}} package
  from the repository at \url{https://datacube.wu.ac.at}.
}
\seealso{
  \url{https://opennlp.apache.org} for more information about Apache
  OpenNLP.
}
\examples{
\donttest{
## Requires package 'openNLPmodels.en' from the repository at
## <https://datacube.wu.ac.at>.

require("NLP")
## Some text.
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)

## Chunking needs word token annotations with POS tags.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(s,
               list(sent_token_annotator,
                    word_token_annotator,
                    pos_tag_annotator))

annotate(s, Maxent_Chunk_Annotator(), a3)
annotate(s, Maxent_Chunk_Annotator(probs = TRUE), a3)
}
}
