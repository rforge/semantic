\name{Namespace-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{Namespace-class}
%
\alias{c-methods}
\alias{c,Namespace-method}
\alias{coerce-methods}
\alias{coerce,Namespace,data.frame-method}
%
\alias{prefix,Namespace-method}
%
\alias{asPrefix}
\alias{asPrefix-methods}
\alias{asPrefix,Namespace-method}
%
\alias{as.data.frame.Namespace}
%
\alias{[-methods}
\alias{[,Namespace-method}
%
\alias{merge.Namespace}
\alias{[,Namespace-method}
\alias{c,Namespace-method}
\alias{coerce,Namespace,data.frame-method}
%
\alias{dim-methods}
\alias{dim,Namespace-method}
%
\alias{namespace<-,Namespace-method}
\alias{show,Namespace-method}
%
\alias{unify}
\alias{unify-methods}
\alias{unify,Namespace-method}
%
\alias{namespace<-}
%
\alias{namespace-methods}
\alias{namespace,character-method}
\alias{namespace,data.frame-method}
\alias{namespace,factor-method}
%
\title{Class \code{"Namespace"}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{Handles data from SPARQL PREFIXes}
\section{Objects from the Class}{
Objects can be created by calls of the form namespace("PREFIX x: <http://www.w3.org/>").
}
\section{Slots}{
    \describe{
        \item{\code{ev}:}{environment. \code{ev} is expected to contain two
            character vectors (prefix and iri) of equal length (or NULL).}
    }
}
\section{Methods}{
    \describe{
    \item{asPrefix}{\code{signature(object = "Namespace")}: Returns the 
        contained PREFIX entries as single character string.}
    %
    \item{[}{\code{signature(x = "Namespace")}: Subsetting operator. The '['
        operator acts on the namespace and uri vector separately.}
    %
    \item{c}{\code{signature(x = "Namespace")}: Combines Namespace objects. 
        The function internally calls 'unify' which removes duplicates.}
    %
    \item{coerce}{\code{signature(from = "Namespace", to = "data.frame")}:
        Converts content of Namespace object into data.frame}
    %
    \item{dim}{\code{signature(x = "Namespace")}: Returns dimension value.
        First element equals number of PREFIX elements in object.
        Second element is always 2.}
    %
    \item{namespace<-}{\code{signature(object = "Namespace", 
            value="character")}: Replaces content
        with given values. The first element will be the new prefix.
        The second element will be the new URI. }
    %
    \item{show}{\code{signature(object = "Namespace")}:
        Prints out the first 6 elements.}
    %
    \item{unify}{\code{signature(object = "Namespace")}: Removes duplicate 
        entries. 'unify' only considers the abbreviated identifier.
        The URI entries are not processed.}
    }
}
\details{Namespaces define prefix bindings in RDF and SPARQL. A prefix binding
consists of a prefix (e.g. 'rdf:') and a IRI (Internationalized Resource
Identifier, e.g. '<http://www.w3.org/1999/02/22-rdf-syntax-ns#>').
The content of a namespace can easily be obtained in raw format
using \code{cat(asPrefix(ns))}.}
\references{
SPARQL 1.1 Query Language W3C Recommendation
Section 1.2.1: Namespaces.
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
\examples{
ns <- namespace("PREFIX owl: <http://www.w3.org/2002/07/owl#>")
ns <- append(ns, c("rdfs", "www.w3.org/2000/01/rdf-schema#"))
%
ns2 <- namespace("PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>")
ns <- c(ns, ns2)
%
dfr <- as.data.frame(ns)
}
\keyword{classes}
