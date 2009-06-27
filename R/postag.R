tagPOS <-
function(sentence, language = "en", model = NULL, tagdict = NULL)
{
    if(is.null(model)) {
        language <- match.arg(language, openNLP_languages)
        model <-
            switch(language,
                   en =
                   ## Note that opennlp.sourceforge.net/models/english
                   ## has the same tag.bin.gz and tagdict in the parser
                   ## and postag subdirectories.
                   system.file("models", "parser", "tag.bin.gz",
                               package = "openNLPmodels.en"),
                   es =
                   system.file("models", "postag", "SpanishPOS.bin.gz",
                               package = "openNLPmodels.es"),
                   de =
                   system.file("models", "postag", "posModel.bin.gz",
                               package = "openNLPmodels.de"),
                   th =
                   system.file("models", "postag", "thai.tag.bin.gz",
                               package = "openNLPmodels.th")
                   )

        if(model == "")
            stop(gettextf("Could not find model file for language '%s'.\nPlease make sure package 'openNLPmodels.%s' is available.",
                          language, language))
    }
        
    ## We only have a tagdict for english.
    ## However,
    ##   http://opennlp.sourceforge.net/api/opennlp/tools/postag/POSTaggerME.html
    ## shows we cannot call POSTaggerME() with just a model ...
    ## Hence use the english tagdict.

    if(is.null(tagdict)) {
        tagdict <- system.file("models", "parser", "tagdict",
                               package = "openNLPmodels.en")
        if(model == "")
            stop(gettextf("Could not find model file for language '%s'.\nPlease make sure package 'openNLPmodels.%s' is available.",
                          "en", "en"))
    }

    tagger <-
        .jnew("opennlp/tools/postag/POSTaggerME",
              .jcast(getModel(model), "opennlp.maxent.MaxentModel"),
              .jcast(.jnew("opennlp/tools/postag/POSDictionary", tagdict),
                     "opennlp/tools/postag/TagDictionary"))

    as.character(sapply(sentence,
                        function (x) .jcall(tagger, "S", "tag", x)))
}
