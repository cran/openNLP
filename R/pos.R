Maxent_POS_Tag_Annotator <-
function(language = "en", probs = FALSE, model = NULL)
{
    f <- Maxent_Simple_POS_Tagger(language, probs, model)
    description <-
        sprintf("Computes POS tag annotations using the Apache OpenNLP Maxent Part of Speech tagger employing %s",
                environment(f)$info)

    Simple_POS_Tag_Annotator(f, description)
}
    
Maxent_Simple_POS_Tagger <-
function(language = "en", probs = FALSE, model = NULL)    
{
    force(language)
    force(probs)
    
    info <- if(is.null(model)) {
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
                          gettextf("Please make sure package '%s' is installed,\navailable from <http://datacube.wu.ac.at/>.",
                                   package)
                      } else {
                          gettextf("Apparently, package '%s' is installed\nbut does not provide this model.",
                                   package)
                      },
                      sep = "\n")
            stop(msg)
        }
        sprintf("the default model for language '%s'", language)
    }
    else
        "a user-defined model"

    ## See
    ## <http://opennlp.apache.org/documentation/1.5.3/manual/opennlp.html#tools.postagger.tagging.api>.

    model <- .jnew("opennlp.tools.postag.POSModel",
                   .jcast(.jnew("java.io.FileInputStream", model),
                          "java.io.InputStream"))

    ref <- .jnew("opennlp.tools.postag.POSTaggerME", model)

    function(x) {
        tags <- .jcall(ref, "[S", "tag", .jarray(x))
        if(probs) {
            probs <- .jcall(ref, "[D", "probs")
            Map(c,
                lapply(tags, single_feature, "POS"),
                lapply(probs, single_feature, "POS_prob"))
        } else
            tags
    }

}
