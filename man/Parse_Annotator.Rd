\name{Parse_Annotator}
\alias{Parse_Annotator}
\title{Apache OpenNLP based parse annotator}
\description{
  Generate an annotator which computes Penn Treebank parse annotations
  using the Apache OpenNLP chunking parser for English.
}
\usage{
Parse_Annotator()
}
\value{
  An \code{\link[NLP]{Annotator}} object giving the created parse
  annotator.
}
\details{
  Using the created annotator requires installing package
  \pkg{openNLPmodels.en} from the repository at
  \url{http://datacube.wu.ac.at} (which provides the Maxent model file
  used by the parser).
}
\seealso{
  \url{http://opennlp.apache.org} for more information about Apache
  OpenNLP.
}
\examples{
\donttest{
## Requires package 'openNLPmodels.en' from the repository at
## <http://datacube.wu.ac.at>.

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

parse_annotator <- Parse_Annotator()
## Compute the parse annotations only.
p <- parse_annotator(s, a2)
## Extract the treebanks.
sapply(p$features, `[[`, "parse")
}
}
