getModel <-
function(model)
{
    .jcall(.jnew("opennlp/maxent/io/SuffixSensitiveGISModelReader",
                 .jnew("java.io.File", model)),
           "Lopennlp/maxent/GISModel;",
           "getModel")
}
