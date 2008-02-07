tokenize <- function(s, language = "en", model = NULL) {
    if (is.null(model)) {
        require("openNLPmodels")
        if (language == "en")
            model <- system.file("opennlp.models", "english", "tokenize", "EnglishTok.bin.gz", package = "openNLPmodels")
        else if (language == "es")
            model <- system.file("opennlp.models", "spanish", "tokenize", "SpanishTok.bin.gz", package = "openNLPmodels")
        else
            stop(paste("unknown language '", language, "' for default model selection", sep = ""))

    }
    tokenizer <- .jnew("opennlp/tools/tokenize/TokenizerME",
                       .jcast(getModel(model), "opennlp.maxent.MaxentModel"))

    .jcall(tokenizer, "[S", "tokenize", paste(s, collapse = " "))
}
