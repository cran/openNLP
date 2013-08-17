ME_Chunk_Annotator <-
function(language = "en", probs = FALSE, model = NULL)
{
    description <- if(is.null(model)) {
        language <- match.arg(language, openNLP_languages)
        package <- sprintf("openNLPmodels.%s", language)
        model <- system.file("models",
                             sprintf("%s-chunker.bin", language),
                             package = package)
        if(model == "") {
            ## Note that currently (2013-08-05) there are only model
            ## files for English, so the system model file analysis
            ## could be simplified ...
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
        sprintf("Computes chunk annotations using the Apache OpenNLP Maxent chunker employing the default model for language '%s'.",
                language)
    }
    else
        "Computes chunk annotations using the Apache OpenNLP Maxent chunker employing a user-defined model."

    ## See
    ## http://opennlp.apache.org/documentation/1.5.3/manual/opennlp.html#tools.parser.chunking.api

    ref <- .jnew("opennlp.tools.chunker.ChunkerME",
                 .jnew("opennlp.tools.chunker.ChunkerModel",
                       .jcast(.jnew("java.io.FileInputStream", model),
                              "java.io.InputStream")))

    f <- function(w, p) {
        tags <- .jcall(ref, "[S", "chunk", .jarray(w), .jarray(p))
        if(probs) {
            probs <- .jcall(ref, "[D", "probs")
            Map(c,
                .simple_feature_map(tags, "chunk_tag"),
                .simple_feature_map(probs, "chunk_prob"))
        } else
            tags
    }

    Simple_Chunk_Annotator(f, description)
}
