\name{SparqlQuery-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{SparqlQuery-class}
%
\alias{initialize,SparqlQuery-method}
\alias{show,SparqlQuery-method}
%
\alias{getQuery}
\alias{getQuery-methods}
\alias{getQuery,SparqlQuery-method}
%
\alias{namespace}
\alias{namespace<--methods}
\alias{namespace<-,SparqlQuery-method}
\alias{namespace,SparqlQuery-method}
% Constructing methods
\alias{sparqlQuery}
\alias{sparqlQuery-methods}
\alias{sparqlQuery,GroupPattern,ANY-method}
\alias{sparqlQuery,missing,missing-method}
\alias{sparqlQuery,SparqlNamespace,character-method}
\alias{sparqlQuery,SparqlNamespace,missing-method}
%
\alias{setRdf,SparqlQuery,SparqlNamespace-method}
\alias{setRdf,SparqlQuery,ResultDef-method}
%
\alias{qryText}
\alias{qryText-methods}
\alias{qryText,SparqlQuery-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{Class \code{"SparqlQuery"}}
%
\description{Serves as container for a Namespace object and a SPARQL query. 
The object can be used together with a Sparql object to perform SPARQL queries.
}
\section{Objects from the Class}{
Objects can be created from a sql string and a Namespace object 
by calls of the form \code{sparqlQuery(sql, ns)}.
}
%
\section{Slots}{
    \describe{
        \item{\code{ev}:}{\code{"environment"}.
            Environment containing S4 objects of type 
            SparqlNamespace (\code{namepace}),
            QueryForm (\code{queryform})
            and GroupPattern (\code{grouppattern})
            as well as a character Vector (\code{resultdef})
            containing the result definition.}
    }
}
\section{Methods}{
    \describe{
        \item{getQuery}{\code{signature(object = "SparqlQuery")}: 
            Returns assembled and URLencoded query URL. }
        %
        \item{initialize}{\code{signature(.Object = "SparqlQuery")}: 
            Object initializer.}
        %
        \item{setRdf}{\code{signature(object = "SparqlQuery", 
            rdf = "SparqlNamespace")}:
            Sets SparqlNamespace in SparqlQuery object.
        }
        %
        \item{setRdf}{\code{signature(object = "SparqlQuery", 
            rdf = "ResultDef")}:
            Sets or replaces Result definition in SparqulQuery.}
        %
        \item{namespace<-}{\code{signature(object = "SparqlQuery")}:
            Replaces Namespace object.}
        %
        \item{namespace}{\code{signature(object = "SparqlQuery")}: 
            Returns copy of Namespace object.}
}
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% References
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
# A) Create Sparql object from SPARQL endpoint URI.
ep <- sparql("http://www.ebi.ac.uk/rdf/services/chembl/sparql")
# B) Check whether endpoint can is reachable
\dontrun{
validObject(ep)
}
# C) Create namespace object
ns <- sparqlNamespace("skos", "<http://www.w3.org/2004/02/skos/core#>")
# D) Create query object
sql <-"SELECT ?molecule WHERE { ?molecule skos:altLabel 'Irinotecan' .}"
sp <- sparqlQuery(ns, sql)
# E) Execute query
\dontrun{
res <- sendQuery(ep, sp, accept="table")
# F) Post process resulting data.frame into ResultSet
rs <- resultSet(res)
}
}
\keyword{classes}
