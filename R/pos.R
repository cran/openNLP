ME_POS_Tag_Annotator <-
function(language = "en", probs = FALSE, model = NULL)
{
    description <- if(is.null(model)) {
        language <- match.arg(language, openNLP_languages)
        package <- if(language == "en")
            "openNLPdata"
        else
            sprintf("openNLPmodels.%s", language)
        model <- system.file("models",
                             sprintf("%s-pos-maxent.bin", language),
                             package = package)
        if(model == "") {
            msg <-
                paste(gettextf("Could not find model file for language '%s'.",
                               language),
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
        sprintf("Computes POS tag annotations using the Apache OpenNLP Maxent Part of Speech tagger employing the default model for language '%s'.",
                language)
    }
    else
        "Computes POS tag annotations using the Apache OpenNLP Maxent Part of Speech tagger employing a user-defined model."

    ## See
    ## http://opennlp.apache.org/documentation/1.5.3/manual/opennlp.html#tools.postagger.tagging.api

    ref <- .jnew("opennlp.tools.postag.POSTaggerME",
                 .jnew("opennlp.tools.postag.POSModel",
                       .jcast(.jnew("java.io.FileInputStream", model),
                              "java.io.InputStream")))

    f <- function(x) {
        tags <- .jcall(ref, "[S", "tag", .jarray(x))
        if(probs) {
            probs <- .jcall(ref, "[D", "probs")
            Map(c,
                .simple_feature_map(tags, "POS"),
                .simple_feature_map(probs, "POS_prob"))
        } else
            tags
    }

    Simple_POS_Tag_Annotator(f, description)
}
