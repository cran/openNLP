\name{Maxent_POS_Tag_Annotator}
\alias{Maxent_POS_Tag_Annotator}
\title{Apache OpenNLP based POS tag annotators}
\description{
  Generate an annotator which computes POS tag annotations using the
  Apache OpenNLP Maxent Part of Speech tagger.
}
\usage{
Maxent_POS_Tag_Annotator(language = "en", probs = FALSE, model = NULL)
}
\arguments{
  \item{language}{
    a character string giving the ISO-639 code of the language being
    processed by the annotator.
  }
  \item{probs}{
    a logical indicating whether the computed annotations should provide
    the token probabilities obtained from the Maxent model as their
    \sQuote{POS_prob} feature.
  }
  \item{model}{
    a character string giving the path to the Maxent model file to be
    used, or \code{NULL} indicating to use a default model file for the
    given language (if available, see \bold{Details}).
  }
}
\value{
  An \code{\link[NLP]{Annotator}} object giving the created POS tag
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

## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3 <- annotate(s, pos_tag_annotator, a2)
a3
## Variant with POS tag probabilities as (additional) features.
head(annotate(s, Maxent_POS_Tag_Annotator(probs = TRUE), a2))

## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)
## Extract token/POS pairs (all of them): easy.
sprintf("\%s/\%s", s[a3w], tags)

## Extract pairs of word tokens and POS tags for second sentence:
a3ws2 <- annotations_in_spans(subset(a3, type == "word"),
                              subset(a3, type == "sentence")[2L])[[1L]]
sprintf("\%s/\%s", s[a3ws2], sapply(a3ws2$features, `[[`, "POS"))
}

