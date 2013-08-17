openNLP_languages <-
    c("da", "de", "en", "es", "nl", "pt", "sv")

.simple_feature_map <-
function(x, tag)
{
    ## Turn a sequence of values x into a list of feature maps with
    ## given tag and respective values in x.
    lapply(x,
           function(u) {
               v <- list(u)
               names(v) <- tag
               v
           })
}
