# tm transformations

setGeneric("tmTagPOS",
           function(object, language = "en", model = NULL, tagdict = NULL, ...)
           standardGeneric("tmTagPOS"))
setMethod("tmTagPOS",
          signature(object = "PlainTextDocument"),
          function(object, language = "en", model = NULL, tagdict = NULL, ...) {
              Content(object) <- tagPOS(Content(object), language, model, tagdict)
              return(object)
          })

setGeneric("tmSentDetect",
           function(object, language = "en", model = NULL, ...)
           standardGeneric("tmSentDetect"))
setMethod("tmSentDetect",
          signature(object = "PlainTextDocument"),
          function(object, language = "en", model = NULL, ...) {
              Content(object) <- sentDetect(Content(object), language, model)
              return(object)
          })

setGeneric("tmTokenize",
           function(object, language = "en", model = NULL, ...)
           standardGeneric("tmTokenize"))
setMethod("tmTokenize",
          signature(object = "PlainTextDocument"),
          function(object, language = "en", model = NULL, ...) {
              Content(object) <- tokenize(Content(object), language, model)
              return(object)
          })
