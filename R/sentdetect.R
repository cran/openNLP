sentDetect <- function(s, language = "en", model = NULL) {
    if (is.null(model)) {
        require("openNLPmodels")
        if (language == "en")
            model <- system.file("opennlp.models", "english", "sentdetect", "EnglishSD.bin.gz", package = "openNLPmodels")
        else if (language == "es")
            model <- system.file("opennlp.models", "spanish", "sentdetect", "SpanishSent.bin.gz", package = "openNLPmodels")
        else
            stop(paste("unknown language '", language, "' for default model selection", sep = ""))
    }

    sdetector <- .jnew("opennlp/tools/sentdetect/SentenceDetectorME",
                       .jcast(getModel(model), "opennlp.maxent.MaxentModel"))

    .jcall(sdetector, "[S", "sentDetect", paste(s, collapse = " "))
}
