
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Prologue
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#library(XML)
#library(RCurl)
# library(diagram)

# SPARQL Query Language for RDF:
# http://www.w3.org/TR/rdf-sparql-query/


# https://www.stat.auckland.ac.nz/S-Workshop/Gentleman/S4Objects.pdf
# http://master.bioconductor.org/help/course-materials/2013/CSAMA2013/friday/afternoon/S4-tutorial.pdf

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

.onUnload <- function(libpath) { library.dynam.unload("semantic", libpath) }


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# IriTerm class
# 
# See: 
# http://rdflib.readthedocs.org/en/latest/rdf_terms.html
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
setClass("IriTerm",
         representation(
             ev="environment",
             iri_term="externalptr",
             delim = "character",
             nDelim = "integer"
         ),
         prototype(
             ev=new.env(),
             delim = "#:/",
             nDelim = 3L
         )
)

setMethod("initialize", "IriTerm", function(.Object, value){
    
    if(!missing(value))
    {
        if(!is.character(value))
            stop("text must be character")
        
        .Object@ev$text <- value
    }else{
        .Object@ev$text <- character(0)
    }
    
    .Object@iri_term = .Call("iri_create_vector")
    .Object@ev$term <- character(0)
    .Object@ev$prefix <- character(0)
    return(.Object)
})


iriTerm <- function(text)
{
    term <- new("IriTerm", text)
    return(term)
}

setGeneric("iriText", function(object) standardGeneric("iriText"))
setMethod("iriText", "IriTerm", function(object){
    return(object@ev$text)
})


# Global replace method
setMethod("$<-", "IriTerm", function(x, name, value){
    
    if(!is.character(value))
        stop("`$<-` value must be character!")
    
    if(name == "term")
        x@ev$term <- value
    else if(name == "prefix")
        x@ev$prefix <- value
    else if(name == "text")
        x@ev$text <- value
    else if(name == "delim")
    {
        x@delim <- value
        x@nDelim <- nchar(value)
    }
    else
        warning("No member with name '", name, "' present.")
    return(x)
})




setGeneric("parseText", function(object, value) standardGeneric("parseText"))

setMethod("parseText", "IriTerm", function(object, value=NULL){
    
    # Parses object@ev$text
    
    if(is.null(object@ev$text))
        stop("text value is missing!")
    
    if(length(object@ev$text)==0)
        stop("text is vector of length 0!")
    
    .Call("iri_parse_text", object@iri_term, object@ev$text, 
          object@delim, object@nDelim)
    
    object@ev$term <- .Call("iri_get_term", object@iri_term)
    object@ev$prefix <- .Call("iri_get_prefix", object@iri_term)
    object@ev$text <- .Call("iri_get_text", object@iri_term, 0L)
    
    return(invisible())
})

setMethod("parseText", c("IriTerm", "character"), function(object, value){
    
    # Parses text given by value variable
    .Call("iri_parse_text", object@iri_term, value, 
          object@delim, object@nDelim)
    
    object@ev$term <- .Call("iri_get_term", object@iri_term)
    object@ev$prefix <- .Call("iri_get_prefix", object@iri_term)
    return(invisible())
})

setGeneric("term", function(object) standardGeneric("term"))
setMethod("term", "IriTerm", function(object){
    return(object@ev$term)
})

setGeneric("prefix", function(object) standardGeneric("prefix"))
setMethod("prefix", "IriTerm", function(object){
    return(object@ev$prefix)
})


# ... for consistency with S3 generic:
# R-ext: A method must have all the arguments of the generic, including … 
# if the generic does.
text.IriTerm <- function(x, env=FALSE, ...){
    if(env)
        return(.Call("iri_get_text", x@iri_term, 1L))
    
    return(.Call("iri_get_text", x@iri_term, 0L))
}



# Definition of RDF Terms:

# An RDF Term is either an
# - an IRI
# - an RDF literal
# - a blank node
setClass("RdfTerm",
            representation(
                prefix="character"
            ),
            prototype(
                prefix="?"
            ),
            "VIRTUAL"
)

# - - - - - - - - - - - - - - - - - - #
# IRI's 
# - - - - - - - - - - - - - - - - - - #
# https://dvcs.w3.org/hg/rdf/raw-file/default/rdf-concepts/index.html#section-IRIs
# An IRI (Internationalized Resource Identifier)
# I = Set of all IRI's


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 12.1.3 Query Variables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# http://www.w3.org/TR/rdf-sparql-query/#docNamespaces
# 4.1.3 Syntax for Query Variables
# Variables are prefixed by either "?" or "$".
# $abc and ?abc identify the same variable

# Class is intended to contain a set of Query Variables
setClass("QueryVariable",
        representation(
            v = "character"
        ),
        prototype(
            v=character(0)
        ),
        contains="RdfTerm"
)


toString.QueryVariable <- function(object){
    return(paste(object@prefix[1], object@v, sep=""))
}


setMethod("show", "QueryVariable", function(object)
{
    cat("An object of class '", class(object), "'.\n", sep="")
    print(head(toString(object)))
})

setMethod("initialize", "QueryVariable", function(.Object, v=NULL){
    if(!is.null(v))
        .Object@v <- v
    return(.Object)
})

queryVariable <- function(v)
{
    return(new("QueryVariable", v))
}

setMethod("[", "QueryVariable", function(x, i)
{
    res <- new("QueryVariable")
    res@prefix <- x$prefix[1]
    res@v <- x@v[i]
    return(res)
})


# 12.1.4 Triple Patterns





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Namespace class
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# See:
# http://www.dajobe.org/2005/04-sparql/SPARQLreference-1.7.pdf

# ev shall contain two vectors: prefix and iri of same length
# See 1.2.1 Namespaces
setClass("Namespace",
        representation(ev = "environment"),
        prototype(ev=new.env()),
        validity=function(object){
            return(length(object@ev$prefix)==length(object@ev$iri))
        }
)

# setValidity("Namespace",
#     function(object){return(length(object@namespace)==length(object@ev$iri))})


as.data.frame.Namespace <- function(x, row.names=NULL, optional=NULL, ...)
{
    return(data.frame(namespace=x@ev$prefix, uri=x@ev$iri, ...))
}

setAs("Namespace", "data.frame",
        function(from, to){return(as.data.frame(from))})


setMethod(f="dim", signature="Namespace", definition=function(x)
{return(c(length(x@ev$prefix), 2))})

setGeneric("unify", function(object) standardGeneric("unify"))
setMethod("unify", "Namespace", function(object)
{
    ns <- unique(object@ev$prefix)
    if(length(ns) < length(object@ev$prefix))
    {
        message("[unify.Namespace] Unifying entries from ",
            length(object@ev$prefix)," to ", length(ns))
        object@ev$prefix <- ns
        object@ev$iri <- object@ev$iri[match(ns, object@ev$prefix)]
    }
    return(object)
})

setMethod("c", signature(x = "Namespace"),
    function(x, ..., recursive = FALSE)
    {
        args <- list(...)
        if (recursive)
            args <- unlist(args)

        for (y in args)
        {
            if (inherits(y, "Namespace"))
            {
                x@ev$prefix <- c(x@ev$prefix, y@ev$prefix)
                x@ev$iri <- c(x@ev$iri, y@ev$iri)

            }else if(is.character(y)){
                if(length(y) < 2)
                    stop("Appended character vector must have length > 1")

                x@ev$prefix <- c(x@ev$prefix, y[1])
                # ToDo: check for uri syntax
                x@ev$iri <- c(x@ev$iri, y[2])
            }else{
                stop("Only Namespace objects or character can used for combine!")
            }
        }
        return(unify(x))
    }
)

merge.Namespace <- function(x, y, ...){
    if(!inherits(y, "Namespace"))
        stop("'y' must be of class 'Namespace'!")
    return(c(x,y))
}


setMethod("[", "Namespace", function(x, i)
{
    if(is.character(i))
        i <- match(i, x@ev$prefix)

    res <- new("Namespace")
    res@ev$prefix <- x@ev$prefix[i]
    res@ev$iri <- x@ev$iri[i]
    return(res)
})


#setGeneric("prefix", function(object) standardGeneric("prefix"))
setMethod("prefix", "Namespace", function(object){
    return(object@ev$prefix)
})


setGeneric("iri", function(object) standardGeneric("iri"))
setMethod("iri", "Namespace", function(object){
    return(object@ev$prefix)
})

setGeneric("namespace<-",
                function(object, value) standardGeneric("namespace<-"))

setReplaceMethod("namespace", "Namespace",
    function(object, value)
    {
        if(!is.character(value))
            stop("value must be character")
        if(length(value)!=2)
            stop("value must have length 2!")

        object@ev$prefix <- value[1]
        object@ev$iri <- value[2]
        return(object)
    }
)

setGeneric("asPrefix", function(object) standardGeneric("asPrefix"))
setMethod("asPrefix", "Namespace", function(object)
{
    res <- paste("PREFIX ",
                object@ev$prefix,": <http://", object@ev$iri, ">", sep="")
    return(paste(res, collapse="\n"))
})

setMethod("show", "Namespace", function(object)
{
    cat("An object of class '", class(object),
        "' containing ", length(object@ev$iri) , " prefixes.\n", sep="")
    print(head(as.data.frame(object)))
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Text parsing functions for convenient data import into Namespace
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Parses uri: ' <http://uri> ' -> 'uri'
parseUri <- function(text)
{
    if(is.factor(text))
        text <- as.character(text)

    if(!is.character(text))
    stop("'text' must be character (or factor)!")

    # Trim leading and trailing white spaces
    text <-  gsub("^\\s+|\\s+$", "", text)
    # Remove http-header
    text <- sub("^<http://", "", text)
    # Remove '>' suffix
    text <- sub(">$", "", text)
    return(text)
}


# Extracts complete namespace data from
# single textual Prefix representation
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

    return(data.frame(namespace=ns, uri=parseUri(val), stringsAsFactors=FALSE))
}

# Create Namespace object from multiple modalities:
# A) Given as data.frame: x[,1] = namespace; x[,2 = uri]
# B) Given as single character: Parse 'ns : <http://uri>'
# C) Given as two characters: x = namespace, y : Parse ' <http://uri>'

setGeneric("namespace", function(object) standardGeneric("namespace"))

setMethod("namespace", "data.frame", function(object)
{
    res <- new("Namespace")
    if(ncol(object) < 2)
        stop("data.frame must have at least two columns")
    # Import first two columns of data.frame
    res@ev$prefix <- as.character(object[,1])
    res@ev$iri <- parseUri(object[,2])
    # Remove duplicates
    res <- unify(res)
    return(res)
})

setMethod("namespace", "character", function(object)
{
    res <- new("Namespace")
    dfr <- parsePrefix(object)
    res@ev$prefix <- dfr$namespace
    res@ev$iri <- dfr$uri
    res <- unify(res)
    return(res)
})

setMethod("namespace", "factor", function(object)
    { return(namespace(as.character(object))) })



importNamespace <- function(file, comment.char="#")
{
    if(!file.exists(file))
        stop("file '", file, "' does not exist!", sep="")

    if(!is.character(comment.char))
        stop("'comment.char' must be character!")

    if(length(comment.char) > 1)
        warning("Only first element of comment char is used!")

    # Reads file content into into character
    prfx <- readLines(file)

    # Excludes lines which start with comment pattern
    cmt <- paste("^",comment.char[1], sep="")
    prfx <- prfx[!grepl(cmt, prfx)]

    # Exclude empty lines
    prfx <- prfx[nchar(prfx) > 0]

    # Do parse file content and construct Object
    return(namespace(prfx))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# SparqlQuery - class
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
setClass("SparqlQuery",
        representation(
            namespace = "Namespace",
            sql = "character"
        ),
        prototype(
            namespace = new("Namespace"),
            sql = ""
        )
)

setMethod("initialize", "SparqlQuery", function(.Object, sql=NULL, ns=NULL)
{
    if(is.null(sql))
        return(.Object)

    if(!is.character(sql))
        stop("sql must be character")

    if(length(sql)!=1)
        stop("sql must have length 1")

    .Object@sql <- sql

    if(!is.null(ns))
    {
        if(!is(ns, "Namespace"))
            stop("ns must be Namespae")

        .Object@namespace <- ns
    }
    return(.Object)
})

sparqlQuery <- function(sql, namespace=NULL)
                        { return(new("SparqlQuery", sql, namespace)) }


#setGeneric("namespace", function(object) standardGeneric("namespace"))
setMethod("namespace", "SparqlQuery",
    function(object) { return(object@namespace)})

# setGeneric("namespace<-", function(object, value) standardGeneric("namespace<-"))

setReplaceMethod("namespace", "SparqlQuery",
    function(object, value)
    {
        if(!is(value, "Namespace"))
            stop("'value' must be Namespace!")

        if(length(value)!=1)
            stop("'value' must have length 1!")

        object@namespace <- value
        return(object)
    }
)

setGeneric("getQuery", function(object, ns=TRUE) standardGeneric("getQuery"))
setMethod("getQuery", "SparqlQuery", function(object, ns=TRUE)
{
    if( (length(object@sql)==0) || (nchar(object@sql[1])==0) )
        stop("No sql query defined!")

    # namespace is not empty?
    if(length(object@namespace@ev$iri) > 0 && ns)
        return(paste(asPrefix(object@namespace), object@sql[1], sep="\n"))
    else
        return(object@sql)
})
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# DescribeQuery
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

setClass("DescribeQuery",
    representation(
        # DESCRIBE,
        result = "character",
        # Identifying resources
        iri = "character"
    ),
    prototype(
        namespace = new("Namespace"),
        result = "DESCRIBE"
    ),
    contains = "SparqlQuery"
)

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
setClass("SelectQuery",
    representation(
        # "SELECT"
        result = "character",
        # DISTINCT
        distinct = "logical",
        # FROM ...
        dataset = "character",
        # WHERE { ... }
        pattern = "character",
        # LIMIT, GROUP BY, ORDER BY, ...
        modifier = "character"
    ),
    prototype(
        namespace = new("Namespace"),
        result = "SELECT",
        distinct = logical(),
        dataset = character(),
        pattern = character(),
        modifier = character()
    ),
    contains = "SparqlQuery"
)

setMethod("initialize", "SelectQuery", function(.Object)
{
    #.Object@result <- "SELECT"
    .Object@distinct <- FALSE
    return(.Object)
})

# namespace = "Namespace",
# # "SELECT" or "DESCRIBE"
# result = "character",
# # FROM ...
# dataset = "character",
# # WHERE { ... }
# pattern = "character",
# # LIMIT, GROUP BY, ORDER BY, ...
# modifier = "character",
selectQuery <- function(namespace, result="SELECT", distinct=FALSE,
                        dataset=NULL, pattern=NULL, modifier=NULL)
{
    res <- new("SelectQuery")

    if(!is(namespace, "Namespace"))
        stop("namespace must be Namespace")

    # PREFIX
    res@namespace <- namespace
    # SELECT (DISTINCT)
    res@result <- result
    res@distinct <- distinct
    # FROM
    if(!is.null(dataset))
        res@dataset <- dataset
    # WHERE
    if(!is.null(pattern))
        res@pattern <- pattern
    # LIMIT, GROUP BY, ORDER BY
    if(!is.null(modifier))
        res@modifier <- modifier

    return(res)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Slot accessors for SelectQuery
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
setGeneric("result", function(object) standardGeneric("result"))
setMethod("result", "SelectQuery",
          function(object) {return(object@result)})


setGeneric("result<-",
           function(object, value) standardGeneric("result<-"))

setReplaceMethod("result", "SelectQuery",
                 function(object, value)
                 {
                     if(!is.character(value))
                         stop("'value' must be character")
                     object@result <- value
                     return(object)
                 }

)

setGeneric("dataset", function(object) standardGeneric("dataset"))
setMethod("dataset", "SelectQuery",
          function(object) { return(object@dataset)})

setGeneric("dataset<-", function(object, value) standardGeneric("dataset<-"))
setReplaceMethod("dataset", "SelectQuery",
                 function(object, value)
                 {
                     if(!is.character(value))
                         stop("value must be character")

                     object@dataset <- value
                     return(object)
                 }
)

setGeneric("addDataset<-", function(object, value)
                                standardGeneric("addDataset<-"))

setReplaceMethod("addDataset", "SelectQuery",
                 function(object, value)
                 {
                     if(!is.character(value))
                         stop("value must be character")
                     object@dataset <- c(object@dataset, value)
                     return(object)
                 }
)

setGeneric("modifier", function(object) standardGeneric("modifier"))
setMethod("modifier", "SelectQuery",
          function(object) { return(object@modifier)})


setGeneric("modifier<-", function(object, value)
                                standardGeneric("modifier<-"))

setReplaceMethod("modifier", "SelectQuery",
                 function(object, value)
                 {
                     if(!is.character(value))
                         stop("value must be character")

                     object@modifier <- value
                     return(object)
                 }
)


setGeneric("addModifier<-", function(object, value)
                                standardGeneric("addModifier<-"))

setReplaceMethod("addModifier", "SelectQuery",
                 function(object, value)
                 {
                     if(!is.character(value))
                         stop("value must be characer")

                     object@modifier <- c(object@modifier, value)
                     return (object)
                 }
)


setGeneric("pattern", function(object)
                                standardGeneric("pattern"))

setMethod("pattern", "SelectQuery",
          function(object) { return(object@pattern)})


setGeneric("pattern<-", function(object, value)
                                standardGeneric("pattern<-"))

setReplaceMethod("pattern", "SelectQuery",
                 function(object, value)
                 {
                     if(!is.character(value))
                         stop("value must be character")
                     object@pattern <- value
                     return(object)
                 }
)


setGeneric("addPattern<-", function(object, value)
                                standardGeneric("addPattern<-"))

setReplaceMethod("addPattern", "SelectQuery",
                 function(object, value)
                 {
                     if(!is.character(value))
                         stop("value must be character")
                     object@pattern <- c(object@pattern, value)
                     return(object)
                 }
)


setGeneric("distinct", function(object)
                                standardGeneric("distinct"))

setMethod("distinct", "SelectQuery", function(object) { return(object@distinct)} )


setGeneric("distinct<-", function(object, value)
                                standardGeneric("distinct<-"))

setReplaceMethod("distinct", "SelectQuery",
        function(object, value)
{
    if(!is.logical(value))
        stop("value must be logical")
    if(length(value) != 1)
        stop("value must have length 1")
    object@distinct <- value
    return(object)
})


setMethod("getQuery", "SelectQuery", function(object, ns=TRUE)
{
    # - - - - - - - - - - - - - - - - #
    # The method for the derived class
    # essentially builds the sql
    # string from slot content.
    #
    # The sql is then further
    # processed in the
    # base class method.
    # - - - - - - - - - - - - - - - - #

    if(object@distinct)
        qry <- "SELECT DISTINCT"
    else
        qry <- "SELECT"

    # dataset is not empty
    if(length(object@dataset) > 0)
    {
        qry <- paste(qry, " ",
                    # ?title ?name
                    paste(object@dataset, collapse=" "),
                    sep="")
    }

    # pattern: WHERE{ }
    if(length(object@pattern) > 0)
    {
        pat <- paste(
            " WHERE {",
            paste(object@pattern, collapse=" "),
            "} ",
            sep="")
        qry <- paste(qry, pat, sep="")
    }

    if(length(object@modifier) > 0)
    {
        qry <- paste(qry,
                paste(object@modifier, collapse="\n")
        )
    }

    # - - - - - - - - - - - - - - - - #
    # Combining sql and Namespace
    # is done in base class
    # - - - - - - - - - - - - - - - - #
    object@sql <- qry
    return(callNextMethod(object))
})

setMethod("show", "SelectQuery", function(object)
{
    cat("An object of class '", class(object), "'.\n", sep="")
    cat("getQuery:\n")
    cat(getQuery(object, ns=FALSE),"\n")
    return(invisible())
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Sparql - class: Encapsulates connection to sparql endpoint
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
setClass("Sparql",
         representation(
             endpoint = "character"
         ),
         prototype(
             # 'http://' suffix must be added for query
             endpoint = "www.example/sparql/"
         )
)

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
})



setGeneric("endpoint", function(object) standardGeneric("endpoint"))
setMethod("endpoint", "Sparql", function(object) return(object@endpoint))

setGeneric("endpoint<-",
           function(object, value) standardGeneric("endpoint<-"))

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
setGeneric("getUrl", function(object, query=NULL) standardGeneric("getUrl"))
setMethod("getUrl", "Sparql", function(object, query=NULL)
{
    if(is.null(query))
        stop("'query' is not optional")

    if(is(query, "SparqlQuery")){
        qry <- getQuery(query)
    }else if(is.character(query)){
        qry <- query
    }else{
        stop("query must be character or SparqlQuery")
    }

    # This function
    url <- paste(
                object@endpoint,
                '?query=',
                gsub('\\+','%2B', URLencode(qry, reserved=TRUE)),
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

setGeneric("sendQuery",
        function(object, query=NULL, accept=NULL, check=FALSE)
            standardGeneric("sendQuery"))


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
            res <- read.csv(textConnection(do_query(url, "text/csv")))
            message("[  sendQuery.Sparql] Result size: ", nrow(res))
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

exchange_levels <- function(dfr, oldlevels, newlevels)
{
    if(!is.character(oldlevels))
        stop("oldlevels must be character")

    if(!is.character(newlevels))
        stop("newlevels must be character")

    # dfr must be data.frame all of which has type character
    fc <- factor(as.matrix(dfr), levels=oldlevels)

    # No action are added when old prefix is empty string
    levels(fc) <- ifelse(nchar(oldlevels) > 0, newlevels, "")
    res <- as.data.frame(matrix(fc, ncol=length(dfr)))
    return(res)
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
#     data <- as.matrix(x@data)
#     prefix <- as.matrix(exchange_levels(x@prefix,
#                                         x@prefixwide,
#                                         x@prefixshort))
#
#     # Re - assemble data
#     m <- matrix(paste(prefix, data, sep=""), ncol=ncol(data))
#     res <- as.data.frame(m, ...)
#     names(res) <- names(x@data)

    res <- x@data
    prefix_table <- data.frame(whide=x@prefixwide,
                               short=x@prefixshort)

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
