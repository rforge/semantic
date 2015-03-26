
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Prologue
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #


.onUnload <- function(libpath) { library.dynam.unload("semantic", libpath) }


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# RDF graph structures
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# RDF Terms
# Definition:
# http://www.w3.org/TR/rdf-sparql-query/#defn_RDFTerm
# Section 12.1.1 RDF Terms
# or
# Section 4.1: RDF Term Syntax
# http://www.w3.org/TR/rdf-sparql-query/#syntaxTerms

setGeneric("endpoint", function(object) standardGeneric("endpoint"))
setGeneric("endpoint<-",
           function(object, value) standardGeneric("endpoint<-"))

setGeneric("addRdf", function(object, rdf) standardGeneric("addRdf"))
setGeneric("setRdf", function(object, rdf) standardGeneric("setRdf"))

setGeneric("rdfTerm", function(object) standardGeneric("rdfTerm"))

setGeneric("typedLiteral", function(object) standardGeneric("typedLiteral"))

setGeneric("term", function(object) standardGeneric("term"))

setGeneric("prefix", function(object) standardGeneric("prefix"))

setGeneric("sparqlNamespace",
           function(prefix, iri, ...) standardGeneric("sparqlNamespace"))

setGeneric("addPrefix",
           function(object, prefix, iri) standardGeneric("addPrefix"))

setGeneric("prefixedTerm", function(namespace, index, term, ...)
    standardGeneric("prefixedTerm"))

setGeneric("getNodes", function(object, val=1, ...) standardGeneric("getNodes"))

setGeneric("resultDef", function(rdf, ...) standardGeneric("resultDef"))

setGeneric("gpText", function(object) standardGeneric("gpText"))
setGeneric("qryText", function(object) standardGeneric("qryText"))

setGeneric("groupPattern", function(object) standardGeneric("groupPattern"))

setGeneric("rdfTriple", function(subject, predicate, object)
    standardGeneric("rdfTriple"))

setGeneric("sparqlQuery", function(object, sql) standardGeneric("sparqlQuery"))

setGeneric("rdfGraph", function(object, ...) standardGeneric("rdfGraph"))

setGeneric("sendQuery",
           function(object, query=NULL, accept=NULL, check=FALSE)
               standardGeneric("sendQuery"))

setGeneric("getUrl", function(object, query=NULL) standardGeneric("getUrl"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# RdfTerm
# An RDF Term is either an
# - an IRI
# - an RDF literal
# - a blank node
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #


.RdfTerm <- setClass("RdfTerm",
                     representation(
                         ev="environment"
                     )
)

setMethod("initialize","RdfTerm", function(.Object, text=NULL)
{
    .Object@ev <- new.env()
    if(!is.null(text))
        .Object@ev$text <- text
    else
        .Object@ev$text <- character(0)
    return(.Object)
})

setMethod("length", "RdfTerm", function(x) {return(length(x@ev$text))})
setMethod("rdfTerm", "character", function(object) {.RdfTerm(object) })
setMethod("rdfTerm", "ANY", function(object) {.RdfTerm()})
setMethod("qryText", "RdfTerm", function(object) { return(object@ev$text) })

setMethod("show", "RdfTerm", function(object)
{
    cat("An object of class '",
        class(object),
        "'. Size: ",
        length(object@ev$text),
        "\n",
        sep=""
    )
    cat(paste(head(qryText(object)), collapse="\n"))
    cat("\n")
    return(invisible())
})

setMethod("addRdf", c("RdfTerm","character"), function(object, rdf){
    object@ev$text <- c(object@ev$text, rdf)
    return(invisible())
})

setMethod("setRdf", c("RdfTerm","character"), function(object, rdf){
    object@ev$text <- rdf
    return(invisible())
})



setMethod("[", signature="RdfTerm", function(x, i)
{ return(.RdfTerm(x@ev$text[i])) })

setMethod("c","RdfTerm",function(x, ..., recursive=FALSE)
{
    if (recursive)
        stop("'recursive' mode not supported!")

    cr<-new(class(x))
    cr@ev$text <- c(unlist(lapply(list(x, ...), FUN=function(x) x@ev$text)))
    return(cr)
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Typed literals
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

langTag <- function(val, lang="en")
{ return(.RdfTerm(paste("\"", val, "\"@", lang, sep=""))) }

setMethod("typedLiteral", "integer", function(object){
    return(.RdfTerm(paste("\"", object, "\"^^xsd:integer", sep="")))
})

setMethod("typedLiteral", "character", function(object){
    return(.RdfTerm(paste("\"", object, "\"^^xsd:string", sep="")))
})

setMethod("typedLiteral", "logical", function(object){
    return(.RdfTerm(paste("\"", object, "\"^^xsd:boolean", sep="")))
})

setMethod("typedLiteral", "numeric", function(object){
    return(.RdfTerm(paste("\"", object, "\"^^xsd:double", sep="")))
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# IriTerm class
#
# See:
# http://rdflib.readthedocs.org/en/latest/rdf_terms.html
#
# https://dvcs.w3.org/hg/rdf/raw-file/default/rdf-concepts/index.html#section-IRIs
# An IRI (Internationalized Resource Identifier)
# I = Set of all IRI's
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# IriTerm
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.IriTerm <- setClass("IriTerm",
                     representation(
                         ptr="externalptr",
                         delim = "character",
                         nDelim = "integer"
                     ),
                     prototype(
                         delim = "#:/",
                         nDelim = 3L
                     ),
                     contains="RdfTerm"
)

setMethod("initialize", "IriTerm", function(.Object, value){

    .Object <- callNextMethod(.Object)
    .Object@ptr <- .Call("get_iri_term")
    .Object@ev <- new.env()
    if(!missing(value))
    {
        if(!is.character(value))
            stop("text must be character")

        # Do parsing
        .Call("iri_parse_text", .Object@ptr, value,
              .Object@delim, .Object@nDelim)

        # Retrieve result
        .Object@ev$term <- .Call("iri_get_term", .Object@ptr)
        .Object@ev$prefix <- .Call("iri_get_prefix", .Object@ptr)
        .Object@ev$text <- .Call("iri_term_get_text", .Object@ptr, 0L)
    }else{
        .Object@ev$term <- character(0)
        .Object@ev$prefix <- character(0)
        .Object@ev$text <- character(0)
    }
    return(.Object)
})




iriTerm <- function(text=NULL)
{
    if(is.null(text))
        return(.IriTerm())
    return(.IriTerm(text))
}


setMethod("term", "IriTerm",
                function(object){ return(object@ev$term) })


setMethod("prefix", "IriTerm",
                function(object) { return(object@ev$prefix) })

setMethod("qryText", "IriTerm", function(object){
    return(object@ev$text)
})

setMethod("show", "IriTerm", function(object)
{
    cat("An object of class '", class(object), "'.\n", sep="")
    cat("[Term  ]:\n")
    print(object@ev$text)
    cat("[Prefix]:\n")
    print(object@ev$prefix)
})


setMethod("[", signature="IriTerm", function(x, i)
{
    object@ptr <- .Call("get_iri_term")
    object@ev <- new.env()
    object@ev$term <- x@ev$term[i]
    object@ev$prefix <- x@ev$prefix[i]
    object@ev$text <- x@ev$text[i]
    return(object)
})

setMethod("addRdf", c("RdfTerm", "IriTerm"), function(object, rdf){
    .Call("rdf_term_add_term", object@ptr, qryText(rdf))
    return(invisible())
})

setMethod("rdfTerm", "IriTerm", function(object)
                                        {.RdfTerm(qryText(object)) })





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# class SparqlNamespace
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
.SparqlNamespace <- setClass("SparqlNamespace",
                        representation(
                            ptr="externalptr",
                            ev="environment"
                            )
)

setMethod("initialize","SparqlNamespace", function(.Object){
    .Object@ptr <- .Call("get_sparql_namespace")
    .Object@ev <- new.env()
    return(.Object)
})


setMethod("sparqlNamespace", c("missing","missing"), function(){
    res <- .SparqlNamespace()
    res@ev$prefix <- character(0)
    res@ev$iri <- character(0)
    return(.SparqlNamespace())
})

setMethod("sparqlNamespace", c("character", "character"),
    function(prefix, iri, ...)
    {
        if(length(prefix) != length(iri))
            stop("prefix and iri must have equal length")

        object <- .SparqlNamespace()
        .Call("sparql_ns_add_prefix", object@ptr, prefix, iri)
        return(object)
    }
)

setMethod("c","SparqlNamespace",function(x, ..., recursive=FALSE)
{
    if (recursive)
        stop("'recursive' mode not supported!")

    # Construct result object
    object <- .SparqlNamespace()

    # Concatenate prefix and iri vectors
    prefix <- c(unlist(lapply(list(x, ...), FUN=function(x) x@ev$prefix)))
    iri <- c(unlist(lapply(list(x, ...), FUN=function(x) x@ev$iri)))


    if(length(iri) > 0)
    {
        # Test for differing iris for same prefix:
        tlen <- tapply(prefix, iri, function(x) length(unique(x)))
        if(any(tlen > 1))
            stop("Multiple iri definitions for prefix found!")


        # Extract unique prefix terms
        object@ev$prefix <- sort(unique(prefix))

        # Get one text for each iri
        mtc <- match(object@ev$prefix, prefix)
        object@ev$iri <- iri[mtc]

        # Insert values into C++ object
        .Call("sparql_ns_add_prefix",
            object@ptr, object@ev$prefix, object@ev$iri)
    }

    return(object)
})


setMethod("addPrefix", c("SparqlNamespace", "character", "character"),
    function(object, prefix, iri)
    {
        if(length(prefix) != length(iri))
            stop("prefix and iri must have equal length!")

        object@ev$prefix <- prefix
        object@ev$iri <- iri

        .Call("sparql_ns_add_prefix", object@ptr, prefix, iri)
        return(invisible())
    }
)

setMethod("length", "SparqlNamespace", function(x){
    return(.Call("sparql_ns_get_size", x@ptr))
})


setMethod("show", "SparqlNamespace", function(object)
{
    cat("An object of class '", class(object),
            "' of size ", length(object), ".\n", sep="")

    cat(.Call("sparql_ns_get_text", object@ptr, c("PREFIX", "")))
    return(invisible())
})


setMethod("qryText", "SparqlNamespace", function(object)
    { return(.Call("sparql_ns_get_text",object@ptr, c("PREFIX", ""))) })

setAs("SparqlNamespace", "data.frame", function(from)
{

    # - - - - - - - - - - - - - - - - - - - - - #
    # sparql_ns_get_text parameter:
    # [1] prefix
    # [2] suffix
    # - - - - - - - - - - - - - - - - - - - - - #
    res <- data.frame(prefix=from@ev$prefix,
                    iri=from@ev$iri,
                    text=.Call("sparql_ns_get_text_vec", from@ptr, c("","")))
    return(res)
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Extracts complete SparqlNamespace data from
# single textual Prefix representation
# Unexported function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
parsePrefix <- function(text)
{
    if(is.factor(text))
        text <- as.character(text)
    
    if(!is.character(text))
        stop("'text' must be character (or factor)!")
    
    
    # Split lines into vector elements
    val <- unlist(strsplit(text, "\n"))
    # Remove leading 'PREFIX '
    val <- sub("^PREFIX ", "", val)
    # Find position of ':'
    pos <- regexpr(":", val)
    # Extract namespace
    ns <- substr(val, 1, pos - 1)
    # Remove namespace
    val <- substring(val, pos + 1)
    
    # Unify namespace entries
    nsu <- sort(unique(ns))
    mtc <- match(nsu, ns)
    valu <- val[mtc]
    
    return(data.frame(ns=nsu, uri=valu, stringsAsFactors=FALSE))
}

setMethod("sparqlNamespace", c("character", "missing"),
    function(prefix, iri, ...)
    {
        dfr <- parsePrefix(prefix)
        
        object <- .SparqlNamespace()
        .Call("sparql_ns_add_prefix", object@ptr, dfr$ns, dfr$uri)
        return(object)
        }
)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# PrefixedTerm
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.PrefixedTerm <- setClass("PrefixedTerm",
                           representation(
                               nsptr="externalptr",
                               delim="character"
                           ),
                           prototype(delim=":"),
                           contains="RdfTerm"
)

setMethod("initialize", "PrefixedTerm", function(.Object)
{
    .Object <- callNextMethod(.Object)
    .Object@ev$prefix <- character(0)
    .Object@ev$text <- character(0)
    return(.Object)
 })


setMethod("prefixedTerm", "SparqlNamespace",
    function(namespace, ...){
        object <- .PrefixedTerm()
        object@nsptr <- namespace@ptr
        return
    }
)


setMethod("prefixedTerm", c("SparqlNamespace","numeric", "character"),
    function(namespace, index, term, ...){

        if(any(index < 1) || any(index > length(namespace)))
            stop("index out of bounds!")

        object <- .PrefixedTerm()

        # Change from 1-based to 0-based index
        index <- as.integer(index - 1)
        term <- as.character(term)

        if(length(index) == 1)
            index <- rep(index, length(term))

        if(length(index) != length(term))
            stop("index and term must have equal length")

        object@nsptr <- namespace@ptr
        object@ev$prefix <- .Call("sparql_ns_get_prefix", object@nsptr, index)
        object@ev$text <- paste(object@ev$prefix, object@delim, term, sep="")

        return(object)
    }
)

setMethod("[", signature="PrefixedTerm", function(x, i)
{
    object <- .PrefixedTerm()
    object@nsptr <- x@nsptr
    object@ev$prefix <- x@ev$prefix[i]
    object@ev$text <- x@ev$text[i]
    return(object)
})

setMethod("c","PrefixedTerm",function(x, ..., recursive=FALSE)
{
    if (recursive)
        stop("'recursive' mode not supported!")

    cr<-new(class(x))
    cr@ev$prefix <- c(unlist(lapply(list(x, ...), FUN=function(x) x@ev$prefix)))
    cr@ev$text <- c(unlist(lapply(list(x, ...), FUN=function(x) x@ev$text)))
    return(cr)
})

setMethod("show", "PrefixedTerm", function(object)
{
    cat("An object of class '", class(object),"' size: ",
        length(object) , ".\n", sep="")
    cat(paste(object@ev$text, collapse="\n"))
    cat("\n")
    return(invisible())
})



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# class blank nodes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.BlankNodes <- setClass("BlankNodes",
                        representation(
                            ptr="externalptr"
                        )
)

setMethod("initialize","BlankNodes", function(.Object){
    .Object@ptr <- .Call("get_blank_nodes")
    return(.Object)
})

blankNodes <- function() {return(.BlankNodes())}


setMethod("getNodes", "BlankNodes", function(object, val=1, ...){
    txt <- .Call("blank_nodes_get_text", object@ptr, as.integer(val[1]))
    return(.RdfTerm(txt))
})

setMethod("show", "BlankNodes", function(object)
{
    cat("An object of class '", class(object), "'.\n", sep="")
    return(invisible())
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# class result - def
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.ResultDef <- setClass("ResultDef", contains="RdfTerm")

setMethod("initialize", "ResultDef", function(.Object){
    .Object <- callNextMethod(.Object)
    return(.Object)
})


setMethod("resultDef", "missing", function() {
    object <- .ResultDef()
    object@ev$text="*"
    return(object)
})

setMethod("resultDef", "character", function(rdf, ...){
    object <- .ResultDef()
    object@ev$text <- rdf
    return(object)
})

setMethod("resultDef","RdfTerm", function(rdf){
    object <- .ResultDef()
    object@ev$text <- rdf@ev$text
    return(object)
})

setMethod("resultDef","missing", function(rdf) return(.ResultDef()))


setMethod("addRdf", c("ResultDef", "character"), function(object, rdf){
    object@ev$text <- c(object@ev$text, rdf)
    return(invisible())
})

setMethod("addRdf", c("ResultDef", "RdfTerm"), function(object, rdf){
    object@ev$text <- c(object@ev$text, rdf@ev$text)
    return(invisible())
})






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 12.1.3 Query Variables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# http://www.w3.org/TR/rdf-sparql-query/#docNamespaces
# 4.1.3 Syntax for Query Variables
# Variables are prefixed by either "?" or "$".
# $abc and ?abc identify the same variable

# Class is intended to contain a set of Query Variables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# class query variables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.QueryVariable <- setClass("QueryVariable", contains="RdfTerm")

setMethod("initialize","QueryVariable", function(.Object, nodes=NULL){
    .Object <- callNextMethod(.Object)
    if(!is.null(nodes))
        .Object@ev$text <- paste("?", nodes, sep="")
    return(.Object)
})

queryVariable <- function(nodes=NULL){ return(.QueryVariable(nodes)) }


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Section:  Group - patterns
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# BasicGroupPattern
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.BasicGroupPattern <- setClass("BasicGroupPattern",
                        representation(
                            ev="environment",
                            "VIRTUAL")
)

setMethod("initialize","BasicGroupPattern", function(.Object, ...){
    .Object@ev <- new.env()
    .Object@ev$text <- character(0)
    return(.Object)
})


# Non exported function which produces textual representation
# for usage inside GroupPattern


setMethod("gpText", "BasicGroupPattern", function(object){
    return(paste("{\n", object@ev$text, "\n}", sep=""))
})

setMethod("qryText", "BasicGroupPattern", function(object){
    return(paste("{", object@ev$text, "}", sep=""))
})

setMethod("show", "BasicGroupPattern", function(object)
{
    cat("An object of class '", class(object), "'.\n", sep="")
    cat(qryText(object), "\n")
    return(invisible())
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# GroupPattern class
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
.GroupPattern <- setClass("GroupPattern",
                    contains="BasicGroupPattern"
)

setMethod("initialize", "GroupPattern", function(.Object, rdf=NULL, ...){
    .Object <- callNextMethod(.Object)
    .Object@ev$l <- list()
    return(.Object)
})

setMethod("length", "GroupPattern", function(x) {return(length(x@ev$l))})


setMethod("groupPattern", "missing", function(object) { return(.GroupPattern())})

setMethod("groupPattern", "BasicGroupPattern", function(object){
    res <- .GroupPattern()
    res@ev$l[[1]] <- object
    return(res)
})
#groupPattern <- function(rdf=NULL) { return(.GroupPattern(rdf)) }

setMethod("addRdf", c("GroupPattern", "BasicGroupPattern"),
    function(object, rdf) 
    {
        n <- length(object@ev$l)
        object@ev$l[[n + 1]] <- rdf
        return(invisible())
    }
)


setMethod("gpText", "GroupPattern", function(object){
    if(length(object@ev$l)==0)
        return("{}")

    if(length(object@ev$l)==1)
        return(gpText(object@ev$l[[1]]))

    l <- lapply(object@ev$l, gpText)
    # Concatenate pattern object
    tx1 <- paste(unlist(l), collapse="\n\t")
    # Enclose into braces
    tx2 <- paste("{\n\t", tx1, "\n}", sep="")
    return(tx2)
})

setMethod("qryText", "GroupPattern", function(object){
        l <- lapply(object@ev$l, qryText)
    tx1 <- paste(unlist(l), collapse="}\n{")
    tx2 <- paste("{\n", tx1, "\n}")
    return(tx2)
})

setMethod("show", "GroupPattern", function(object)
{
    cat("An object of class '", class(object), "'.\n", sep="")
    cat(gpText(object), "\n")
    return(invisible())
})







# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# class rdf-triple (12.1.4)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.RdfTriple <- setClass("RdfTriple",
                    representation(
                        subject="RdfTerm",
                        predicate="RdfTerm",
                        object="RdfTerm",
                        delim="character",
                        crdelim="character"
                       ),
                    prototype(
                        delim=" .",
                        crdelim=" .\n"
                    ),
                    contains="BasicGroupPattern"
)

setMethod("length", "RdfTriple", function(x){
    return(length(x@subject))
})


# initialize takes maximal three arguments...


setMethod("rdfTriple", c("character", "character", "character"),
    function(subject, predicate, object)
{
    n <- length(subject)
    if(n != length(predicate) || n != length(object))
        stop("subject, predicate and object must have equal length")

    res <- .RdfTriple()
    res@subject <- .RdfTerm(subject)
    res@predicate <- .RdfTerm(predicate)
    res@object <- .RdfTerm(object)
    return(res)
})

setMethod("rdfTriple", c("RdfTerm", "RdfTerm", "RdfTerm"),
    function(subject, predicate, object)
    {
        n <- length(subject)
        if(n != length(predicate) || n != length(object))
            stop("subject, predicate and object must have equal length")

        res <- .RdfTriple()
        res@subject <- subject
        res@predicate <- predicate
        res@object <- object
        return(res)
})

setMethod("gpText", "RdfTriple", function(object){

    if(length(object)==0)
        return("{}")

    t1 <- paste(qryText(object@subject), 
                            qryText(object@predicate), qryText(object@object))
    
    if(length(t1)==1)
        return(paste("{", t1, "}"))

    # Add intermediate " ." + CR
    t2 <- paste(t1, collapse=object@crdelim)
    # Add terminal " ." (no CR)
    t3 <- paste(t2, object@delim, sep="")
    # Enclose in braces
    t4 <- paste("{", t3, "}", sep="\n")

    return(t4)
})

setMethod("qryText", "RdfTriple", function(object)
{
    t1 <- paste(qryText(object@subject), 
                        qryText(object@predicate),
                        qryText(object@object))
        
    t2 <- paste(t1, collapse=object@crdelim, sep="")
    t3 <- paste(t2, object@delim, sep="")
    return(t3)
})

setMethod("show", "RdfTriple", function(object)
{
    cat("An object of class '", class(object), "' of size ",
        length(object),".\n", sep="")
    cat(qryText(object),"\n")
    return(invisible())
})



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# class QueryForm
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.QueryForm <- setClass("QueryForm", representation(ev="environment"))

setMethod("initialize", "QueryForm", function(.Object, value="s")
{
    .Object@ev <- new.env()

    if(tolower(substr(value,1,1)) == "s")
        .Object@ev$form <- "SELECT"
    else if(tolower(substr(value,1,1)) == "d")
        .Object@ev$form <- "DEFINE"
    else if(tolower(substr(value,1,1)) == "c")
        .Object@ev$form <- "CONSTRUCT"

    return(.Object)
})

queryForm <- function(form="SELECT"){ return(.QueryForm(form[1])) }

setMethod("setRdf", c("QueryForm", "character"), function(object, rdf)
{
    if(tolower(substr(rdf, 1, 1)) == "s")
        .Object@ev$form <- "SELECT"
    else if(tolower(substr(rdf, 1, 1)) == "d")
        .Object@ev$form <- "DEFINE"
    else if(tolower(substr(rdf, 1, 1)) == "c")
        .Object@ev$form <- "CONSTRUCT"
    return(invisible())
})

setMethod("show", "QueryForm", function(object){
    cat("An object of class '", class(object), "'.\n", sep="")
    cat("[QueryForm] :", object@ev$form, "\n")
})


setMethod("qryText", "QueryForm", function(object){ return(object@ev$form )})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# SparqlQuery - class
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.SparqlQuery <- setClass("SparqlQuery", representation(ev="environment"))

setMethod("initialize", "SparqlQuery", function(.Object)
{
    .Object@ev <- new.env()
    .Object@ev$namespace <- .SparqlNamespace()
    .Object@ev$resultdef <- resultDef()
    .Object@ev$queryform <- .QueryForm()
    .Object@ev$pattern <- .GroupPattern()
    return(.Object)
})



setMethod("sparqlQuery", c("missing", "missing"), function(){
    return(.SparqlQuery())
})


setMethod("sparqlQuery", c("SparqlNamespace", "missing"), function(object)
{
    res <- .SparqlQuery()
    res@ev$namespace <- object
    return(res)
})

setMethod("sparqlQuery", c("SparqlNamespace", "character"), 
    function(object, sql)
{
    res <- .SparqlQuery()
    res@ev$namespace <- object
    res@ev$sql <- sql
    return(res)
})


setMethod("sparqlQuery", "GroupPattern", function(object)
{
    res <- .SparqlQuery()
    res@ev$pattern <- object
    return(res)
})


setMethod("setRdf", c("SparqlQuery", "ResultDef"), function(object, rdf)
{
    object@ev$resultdef <- rdf
    return(invisible())
})

setMethod("setRdf", c("SparqlQuery", "QueryForm"), function(object, rdf)
{
    object@ev$queryform <- rdf
    return(invisible())
})

setMethod("setRdf", c("SparqlQuery", "GroupPattern"), function(object, rdf)
{
    object@ev$pattern <- rdf
    return(invisible())
})

setMethod("setRdf", c("SparqlQuery", "SparqlNamespace"), function(object, rdf)
{
    object@ev$namespace <- rdf
    return(invisible())
})

setMethod("setRdf", c("SparqlQuery", "RdfTriple"), function(object, rdf)
{
    object@ev$pattern <- groupPattern(rdf)
    return(invisible())
})

setMethod("qryText", "SparqlQuery", function(object){
    
    if(length(object@ev$namespace) > 0)
        ns <- qryText(object@ev$namespace)
    else
        ns <- NULL
    
    if(!is.null(object@ev$sql))
    {
        if(is.null(ns))
            return(object@ev$sql)
        else
            return(paste(ns, object@ev$sql, sep="\n"))
    }
    
    ns <- qryText(object@ev$namespace)
    qv <- qryText(object@ev$queryform)
    rs <- paste(qryText(object@ev$resultdef), collapse=" ")
    gp <- paste(gpText(object@ev$pattern), collapse="\n")
    
    txt <- ""
    if(!is.null(ns))
        txt <- qryText(object@ev$namespace)
    
    txt <- paste(txt, qv, " ",rs , " WHERE", sep="")
    return(paste(txt, gp, sep="\n"))
})

setMethod("show", "SparqlQuery", function(object)
{
    cat("An object of class '", class(object), "'.\n", sep="")
    cat(qryText(object), "\n")
    return(invisible())
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# ConstructQuery class
# http://www.w3.org/TR/rdf-sparql-query/#docNamespaces
# 2.5: Building RDF Graphs
# The CONSTRUCT query form returns an RDF graph.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# SelectQuery class
# http://www.w3.org/TR/2013/REC-sparql11-query-20130321/#BGPsparql
# 18.2 Translation to the SPARQL Algebra
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# http://www.w3.org/TR/rdf-sparql-query/#docNamespaces
# Section 2.5: Building RDF Graphs:
# The SELECT query form returns variable bindings.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# class rdf - graph
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.RdfGraph <- setClass("RdfGraph", representation(ptr="externalptr"))


setMethod("initialize","RdfGraph", function(.Object)
{
    .Object@ptr <- .Call("get_rdf_graph")
    return(.Object)
})


setMethod("rdfGraph", "RdfTriple", function(object, ...)
{
    res <- .RdfGraph()
    .Call("rdf_graph_add_triple",
          res@ptr,
          qryText(object@subject),
          qryText(object@predicate),
          qryText(object@object))
    return(res)
})


setMethod("qryText", "RdfGraph", function(object)
                        { return(.Call("rdf_graph_get_text", object@ptr)) })


setMethod("addRdf", c("RdfGraph", "RdfTriple"), function(object, rdf)
{
    .Call("rdf_graph_add_triple",
            object@ptr,
            qryText(rdf@subject),
            qryText(rdf@predicate),
            qryText(rdf@object))

    return(invisible())
})

setMethod("show", "RdfGraph", function(object)
{
    cat("An object of class '", class(object), "'.\n", sep="")
    cat(.Call("rdf_graph_get_text", object@ptr),"\n")
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Rdf-data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

.RdfData <- setClass("RdfData",
                     representation(
                         ptr="externalptr")
)

setMethod("initialize", "RdfData", function(.Object, data=NULL)
{
    .Object@ptr <- .Call("get_rdf_data")
    if(!is.null(data))
    {
        if(is(data, "RdfGraph"))
            .Call("rdf_data_set_rdf_graph", .Object@ptr, data@ptr)
        else if(is(data, "RdfTriple"))
        {
            rgraph <- rdfGraph(data)
            .Call("rdf_data_set_rdf_graph", .Object@ptr, rgraph@ptr)
        }else if(is(data, "SparqlNamespace"))
        {
            .Call("rdf_data_set_namespace", .Object@ptr, data@ptr)
        }
    }
    return(.Object)
})

rdfData <- function(data=NULL) { return(.RdfData(data))}

setMethod("setRdf", c("RdfData","SparqlNamespace"), function(object, rdf)
{
    .Call("rdf_data_set_namespace", object@ptr, rdf@ptr)
    return(invisible())
})


setMethod("setRdf", c("RdfData", "RdfGraph"), function(object, rdf)
{
    .Call("rdf_data_set_rdf_graph", object@ptr, rdf@ptr)
    return(invisible())
})

setMethod("setRdf", c("RdfData", "RdfTriple"), function(object, rdf)
{
    rgraph <- rdfGraph(rdf)
    .Call("rdf_data_set_rdf_graph", object@ptr, rgraph@ptr)
    return(invisible())
})

setMethod("qryText", "RdfData", function(object)
    { return(.Call("rdf_data_get_text", object@ptr)) })


setMethod("show", "RdfData", function(object){
    cat("An object of class '", class(object), "'.\n", sep="")
    cat(.Call("rdf_data_get_text", object@ptr))
    return(invisible())

})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Query Sparql Endpoints
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Sparql - class: Encapsulates connection to sparql endpoint
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
setClass("Sparql", representation(endpoint = "character"))

setMethod(f="initialize", signature="Sparql",
    definition=function(.Object, endpoint = "www.example/sparql/")
    {
        .Object@endpoint <- endpoint
        return(.Object)
    }
)

setValidity("Sparql",
    function(object)
    {
        res <- url.exists(object@endpoint)
        if(res)
            message("[setValidity.Sparql] Endpoint exists.")
        else
            message("[setValidity.Sparql] Endpoint does not exist!")
        return(res)
    }
)

setMethod("endpoint", "Sparql", function(object) return(object@endpoint))

setReplaceMethod("endpoint", "Sparql",
                 function(object, value)
                 {
                     if(!is.character(value))
                         stop("value must be character")

                     object@endpoint <- value
                     return(object)
                 }
)


# Returns text of url request
setMethod("getUrl", c("Sparql", "SparqlQuery"), function(object, query)
{
    url <- paste(
                object@endpoint,
                '?query=',
                gsub('\\+','%2B', URLencode(qryText(query), reserved=TRUE)),
                sep="")
    return(url)
})

setMethod("getUrl", c("Sparql", "character"), function(object, query)
{
    url <- paste(
        object@endpoint,
        '?query=',
        gsub('\\+','%2B', URLencode(query, reserved=TRUE)),
        sep="")
    return(url)
})


curl <- function(url, httpheader=NULL, display=TRUE, header=FALSE)
{
    if(!is.logical(display))
        stop("display must be logical")

    if(!is.logical(header))
        stop("header must be logical")

    rtg <- basicTextGatherer()
    htg <- basicTextGatherer()

    cres <- curlPerform(url=url,
                        httpheader=httpheader,
                        writefunction=rtg$update,
                        headerfunction=htg$update)

    if(display[1])
    {
        hd <- parseHTTPHeader(htg$value())
        message("[curl] Status: ", hd["status"],
            " (", hd["statusMessage"] ,")", sep="")
    }


    if(header[1])
        return(list(header=htg$value(), body=rtg$value()))

    return(rtg$value())
}




setMethod("sendQuery", "Sparql",
        function(object, query=NULL, accept=NULL, check=FALSE)
{

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Sends query to Sparql endpoint and returns result
    # Function reads status message from reponse header
    # and dispays status
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

    if(check)
        validObject(object)

    url <- getUrl(object, query)

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Read Result as XML
    # Add 'Accept' field to the header of the HTTP request
    # as content-type negotiation
    # See:
    # http://virtuoso.openlinksw.com/dataspace/doc/dav/wiki/Main/VOSSparqlProtocol
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Query wrapper: Executes query and prints status message
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    do_query <- function(url, accept)
    {
        reader <- basicTextGatherer()
        header <- basicTextGatherer()
        cres <- curlPerform(url=url,
                            httpheader=c(Accept=accept),
                            writefunction=reader$update,
                            headerfunction=header$update)

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # Parse HTTP header in order to provide sensible status messages
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        hd <- parseHTTPHeader(header$value())
        status <- hd["status"]
        # http://virtuoso.openlinksw.com/dataspace/doc/dav/wiki/Main/VOSSparqlProtocol
        if(suppressWarnings(is.na(as.numeric(status))))
        {
            # status is not numeric: This should not happen ...
            message("[  sendQuery.Sparql] Status: '", hd["status"],
                    "' (", hd["statusMessage"] ,")", sep="")
        }else{
            status <- as.numeric(status)
            if(status %in% c(200, 400, 500))
            {
                # HTTP Response Codes defined in Sparql
                message("[  sendQuery.Sparql] Sparql status: ", status,
                        " (", hd["statusMessage"] ,")", sep="")
            }else{
                # Other http Response code
                message("[  sendQuery.Sparql] Status: ", status,
                        " (", hd["statusMessage"] ,")", sep="")
            }
        }
        return(reader$value())
    }

    if(is.null(accept))
    {
        # Standard: Return XMLDocument
        res <- xmlParse(do_query(url, "application/sparql-results+xml"))

    }else{
        if(accept == "table")
        {
            # Return data.frame
            # textConnection returns single use handle
            res <- do_query(url, "text/csv")
            if(nchar(res) > 0)
            {
                res <- read.csv(textConnection(res))
                message("[  sendQuery.Sparql] Result size: ", nrow(res))
            }else{
                message("[  sendQuery.Sparql] Result size: 0!")
                return(NULL)
            }
        }else if(accept == "html"){
            # Return result as XMLDocument
            res <- htmlParse(do_query(url, "text/html"))
        }else{
            # Return any other format as is
            res <- do_query(url, accept)
        }
    }
    return(res)
})


sparql <- function(endpoint="http://dbpedia.org/sparql")
    { return(new("Sparql", endpoint=endpoint)) }


split_prefix <- function(text, split="\\/#")
{
    text <- as.character(text)
    # Negative lookahead
    rgx <- paste("[", split, "]((?![", split, "]).)+$", sep="")
    position <- regexpr(rgx, text, perl=TRUE)
    pos <- as.numeric(position)

    # position = (-1): Everything is suffix
    # Example: http://rdf/
    suffix <- substring(text, pos + 1)
    prefix <- substr(text, 1, pos)
    return(data.frame(prefix=prefix, suffix=suffix))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# text <- c("123/", "12/3", "/123", "123#", "12#3", "1/2#3")
# position <- regexpr("[\\/#]((?![\\/#]).)+$", text, perl=TRUE)
# pos <- as.numeric(position)
# dfr <- data.frame(text=text, pos=pos)
# dfr$suffix <- substring(dfr$text, dfr$pos + 1)
# dfr$prefix <- substr(dfr$text, 1, dfr$pos)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# dfr <- split_prefix(text)
# dfr$text <- text
# dfr$pos <- regexpr("[\\/#]((?![\\/#]).)+$", text, perl=TRUE)
# dfr
# Expected result:
#   prefix suffix text pos
# 1          123/ 123/  -1
# 2     12      3 12/3   3
# 3           123 /123   1

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #


setClass("ResultSet",
        representation(
            data = "data.frame",
            prefix = "data.frame",
            prefixwide = "character",   # Expanded prefix
            prefixshort = "character"    # Abbreviated prefix
        )
)

setMethod("initialize", "ResultSet", function(.Object, data=NULL, brev="a")
{
    if(is.null(data))
        return(.Object)

    if(!is.data.frame(data))
        stop("data must be data.frame")

    if(is.factor(brev))
        brev <- as.character(brev)

    if(!is.character(brev))
        stop("brev must be character")

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # The class separates textual entries into a prefix and suffix component
    # Typically the prefix constituent has a URL like structure e.g.
    # http://identifiers.org/biomodels.db/
    # Separation is done by the split_prefix function which
    # applies a negative lookahead method
    #
    # For the voluminous prefixes, a short version is offered
    # together with a translation table.
    # Empty prefixes are handled separatedly and stay empty.
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

    dm <- as.matrix(data)
    spec <- split_prefix(dm)
    sdm <- matrix(spec$suffix, nrow=nrow(dm))
    colnames(sdm) <- colnames(dm)

    # Extract expanded prefix (levels)
    fc <- factor(spec$prefix)
    .Object@prefixwide <- levels(fc)
    nl <- length(levels(fc))

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Construct shrinked prefix e.g. '_:a05 ' which contains a
    # A) Prefix string '_:x' where x=brev (can manually be set)
    # B) Numeric section of fixed width which counts from 1 to n
    # C) White space as suffix
    # When the incoming prefix is empty, the shrinked prefix is also empty
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

    # Width of numeric section
    wd <- ceiling(log10(nl))

    # Numeric character with fixed size and leading zero's
    # or to empty string when prefix is empty
    fxn <- formatC(1:nl, width=wd, flag="0")

    # Includes a blank space suffix
    .Object@prefixshort <- ifelse(nchar(levels(fc)) > 0,
                                paste("_:", brev, fxn, " ", sep=""),
                                "")

    pdm <- matrix(fc, nrow=nrow(dm))
    colnames(pdm) <- colnames(dm)

    .Object@data <- as.data.frame(sdm)
    .Object@prefix <- as.data.frame(pdm)

    return(.Object)
})

resultSet <- function(data) { return(new("ResultSet", data))}


# Unexported function (no checks for incoming arguments)
exchange_levels <- function(dfr, oldlevels, newlevels)
{
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # oldlevels:
    # Set of initial factor levels. 
    # These levels are used because the assumed factor
    # string sets in the input data.frame only represent
    # a subset of the enconding which is needed afterwards.
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # newlevels:
    # Set of new factor levels which replace the old levels.
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # dfr must be data.frame
    # All columns must have type character
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    fc <- factor(as.matrix(dfr), levels=oldlevels)
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Empty entries (nchar(x)==0) remain unchanged.
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    levels(fc) <- ifelse(nchar(oldlevels) > 0, newlevels, "")
    
    return(as.data.frame(matrix(fc, ncol=length(dfr))))
}

setMethod("show", "ResultSet", function(object)
{
    cat("An object of class '", class(object), "'\n", sep="")
    cat("Prefixes:\n")

    # Print 'shrink:wide' string
    # Each line contains one prefix
    cat(paste(
            paste(object@prefixshort,object@prefixwide,sep=" :\t"),
            collapse="\n"),"\n\n")

    # Add short shrinked prefixes to data:
    data <- as.matrix(head(object@data))
    prefix <- as.matrix(exchange_levels(head(object@prefix),
                object@prefixwide, object@prefixshort))

    # Re - assemble data
    m <- matrix(paste(prefix, data, sep=""), ncol=ncol(data))
    res <- as.data.frame(m)
    names(res) <- names(object@data)
    print(res)

    return(invisible())
})

as.data.frame.ResultSet <- function(x, row.names=NULL, optional=NULL, ...)
{
    res <- x@data
    prefix_table <- data.frame(whide=x@prefixwide, short=x@prefixshort)
    attr(res, "pefixTable") <- prefix_table
    return(res)
}


parse_data_types <- function(text)
{
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Parses typed strings e.g.
    # 2142^^<http://www.w3.org/2001/XMLSchema#int>
    # and returns result in data.frame
    # - - - - - - - - - - - - - - - - - - - - - - - - - - #

    if(!is.character(text))
        text <- as.character(text)

    # Separate value and type
    rgx <- regexpr("\\^\\^", text, perl=TRUE)

    # Contains the value
    value <- substr(text, 1, rgx - 1)

    # Contains the type description:
    # <http://www.w3.org/2001/XMLSchema#int>
    suffix <- substring(text, rgx + 2)

    # Remove chevrons
    suffix <-  gsub("^<|>$", "", suffix)
    # Extract type facet
    type <- sub("^http://www.w3.org/2001/XMLSchema#","", suffix)

    return(data.frame(value=value, type=type))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# END OF FILE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
