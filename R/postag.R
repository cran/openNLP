tagPOS <- function(sentence, language = "en", model = NULL, tagdict = NULL) {
    if (is.null(model)) {
        require("openNLPmodels")
        if (language == "en")
            model <- system.file("opennlp.models", "english", "parser", "tag.bin.gz", package = "openNLPmodels")
        else if (language == "es")
            model <- system.file("opennlp.models", "spanish", "postag", "SpanishPOS.bin.gz", package = "openNLPmodels")
        else
            stop(paste("unknown language '", language, "' for default model selection", sep = ""))
    }
    if (is.null(tagdict)) {
        require("openNLPmodels")
        tagdict <- system.file("opennlp.models", "english", "parser", "tagdict", package = "openNLPmodels")
    }

    tagger <- .jnew("opennlp/tools/postag/POSTaggerME",
                         .jcast(getModel(model), "opennlp.maxent.MaxentModel"),
                         .jnull("opennlp/tools/ngram/Dictionary"),
                         .jcast(.jnew("opennlp/tools/postag/POSDictionary", tagdict),
                                "opennlp/tools/postag/TagDictionary"))

    as.character(sapply(sentence, function (x) .jcall(tagger, "S", "tag", x)))
}
