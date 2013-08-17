ME_Entity_Annotator <-
function(language = "en", kind = "person", probs = FALSE, model = NULL)
{
    description <- if(is.null(model)) {
        language <- match.arg(language, openNLP_languages)
        ## <FIXME>
        ## Should this be called 'type' instead?
        kind <- match.arg(kind,
                          ## Should be good enough for now ...
                          c("date", "location", "money", "organization",
                            "percentage", "person", "misc"))
        ## </FIXME>
        package <- sprintf("openNLPmodels.%s", language)
        model <- system.file("models",
                             sprintf("%s-ner-%s.bin", language, kind),
                             package = package)
        if(model == "") {
            msg <-
                paste(gettextf("Could not find model file for language '%s' and kind '%s'.",
                               language, kind),
                      if(system.file(package = package) == "") {
                          gettextf("Please make sure package '%s' is installed,\navailable from http://datacube.wu.ac.at/.",
                                   package)
                      } else {
                          gettextf("Apparently, package '%s' is installed\nbut does not provide this model.",
                                   package)
                      },
                      sep = "\n")
            stop(msg)            
        }
        sprintf("Computes entity annotations using the Apache OpenNLP Maxent name finder employing the default model for language '%s' and kind '%s'.",
                language, kind)
    }
    else
        "Computes entity annotations using the Apache OpenNLP Maxent name finder employing a user-defined model."

    ## See
    ## http://opennlp.apache.org/documentation/1.5.3/manual/opennlp.html#tools.namefind.recognition.api

    ref <- .jnew("opennlp.tools.namefind.NameFinderME",
                 .jnew("opennlp.tools.namefind.TokenNameFinderModel",
                       .jcast(.jnew("java.io.FileInputStream", model),
                              "java.io.InputStream")))

    f <- function(x) {
        y <- .jcall(ref, "[Lopennlp/tools/util/Span;", "find",
                    .jarray(x))
        y <- if(!length(y))
            Annotation()
        else {
            start <- sapply(y, .jcall, "I", "getStart") + 1L
            end <- sapply(y, .jcall, "I", "getEnd")
            kind <- sapply(y, .jcall, "S", "getType")
            type <- rep.int("entity", length(start))
            features <- .simple_feature_map(kind, "kind")
            if(probs) {
                ## Apparently need the probabilities for the obtained
                ## spans, see
                ## http://opennlp.apache.org/documentation/1.5.3/apidocs/opennlp-tools/index.html.
                probs <- .jcall(ref, "[D", "probs",
                                .jcast(.jarray(y),
                                       "[Lopennlp/tools/util/Span;"))
                features <- Map(c,
                                features,
                                .simple_feature_map(probs, "prob"))
            }
            Annotation(NULL, type, start, end, features)                
        }
        .jcall(ref, "V", "clearAdaptiveData")
        y
    }

    Simple_Entity_Annotator(f, description)
}
