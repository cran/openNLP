sentDetect <-
function(s, language = "en", model = NULL)
{
    if(is.null(model)) {
        language <- match.arg(language, openNLP_languages)
        model <-
            switch(language,
                   en =
                   system.file("models", "sentdetect",
                               "EnglishSD.bin.gz",
                               package = "openNLPmodels.en"),
                   es =
                   system.file("models", "sentdetect",
                               "SpanishSent.bin.gz",
                               package = "openNLPmodels.es"),
                   de =
                   system.file("models", "sentdetect",
                               "sentenceModel.bin.gz",
                               package = "openNLPmodels.de"),
                   th =
                   system.file("models", "sentdetect",
                               "thai.sent.bin.gz",
                               package = "openNLPmodels.th")
                   )
        if(model == "")
            stop(gettextf("Could not find model file for language '%s'.\nPlease make sure package 'openNLPmodels.%s' is available.",
                          language, language))
    }

    sdetector <-
        .jnew("opennlp/tools/sentdetect/SentenceDetectorME",
              .jcast(getModel(model), "opennlp.maxent.MaxentModel"))

    .jcall(sdetector, "[S", "sentDetect", paste(s, collapse = " "))
}
