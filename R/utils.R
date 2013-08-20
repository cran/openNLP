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

read_tag_dictionary <-
function(dict) {
    it <- dict$iterator()
    tags <- words <- list()
    while(it$hasNext()) {
        w <- .jcall(it, "Ljava/lang/Object;", "next")
        words <- c(words, list(.jsimplify(w)))
        tags <- c(tags, list(dict$getTags(w)))
    }
    names(tags) <- unlist(words)
    tags
}
