tokenize <-
function(s, language = "en", model = NULL)
{
    if(is.null(model)) {
        language <- match.arg(language, openNLP_languages)
        model <-
            switch(language,
                   en =
                   system.file("models", "tokenize",
                               "EnglishTok.bin.gz",
                               package = "openNLPmodels.en"),
                   es =
                   system.file("models", "tokenize",
                               "SpanishTok.bin.gz",
                               package = "openNLPmodels.es"),
                   de =
                   system.file("models", "tokenize",
                               "tokenModel.bin.gz",
                               package = "openNLPmodels.de"),
                   th =
                   system.file("models", "tokenize",
                               "thai.tok.bin.gz",
                               package = "openNLPmodels.th")
                   )
        if(model == "")
            stop(gettextf("Could not find model file for language '%s'.\nPlease make sure package 'openNLPmodels.%s' is available.",
                          language, language))
    }

    tokenizer <-
        .jnew("opennlp/tools/tokenize/TokenizerME",
              .jcast(getModel(model), "opennlp.maxent.MaxentModel"))

    .jcall(tokenizer, "[S", "tokenize", paste(s, collapse = " "))
}
