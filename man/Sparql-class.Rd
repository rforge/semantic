\name{Sparql-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{Sparql-class}
\alias{initialize,Sparql-method}
%
\alias{sparqlQuery}
\alias{sparql}
%
\alias{endpoint}
\alias{endpoint-methods}
\alias{endpoint,Sparql-method}
%
\alias{endpoint<-}
\alias{endpoint<--methods}
\alias{endpoint<-,Sparql-method}
%
\alias{endpoint<-,Sparql-method}
\alias{endpoint,Sparql-method}
%
\alias{getUrl}
\alias{getUrl,Sparql-method}
%
\alias{sendQuery}
\alias{sendQuery-methods}
\alias{sendQuery,Sparql-method}
%
\title{Class \code{"Sparql"}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{Represents a connection to a SPARQL endpoint.
A Sparql object can be used to transmit SPARQL commands to an endpoint.}
\section{Objects from the Class}{
Objects can be created from an SPARQL endpoint URI 
by calls of the form \code{sparql(endpoint)}.
}
\section{Slots}{
    \describe{
        \item{\code{endpoint}:}{\code{"character"}: URI of SPARQL endpoint.}
    }
}
\section{Methods}{
    \describe{
        \item{endpoint<-}{\code{signature(object = "Sparql")}: Replaces
            endpoint URI.}
        %
        \item{endpoint}{\code{signature(object = "Sparql")}: Returns
            endpoint URI.}
        %
        \item{getUrl}{\code{signature(object = "Sparql")}: Returns the
            assembled and URL-encoded URL which can be used to 
            query a SPARQL endpoint via curl.}
        %
        \item{initialize}{\code{signature(.Object = "Sparql")}: 
            Initialized object. }
        %
        \item{sendQuery}{\code{signature(object = "Sparql"),
            query="SparqlQuery"}: Assembles a query URL, performs
            the SPARQL query on an endpoint, prints a status
            message (from returned HTTP header) and returns the result.}
    }
}
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
\examples{
# A) Create Sparql object from SPARQL endpoint URI.
ep <- sparql("http://www.ebi.ac.uk/rdf/services/chembl/sparql")
# B) Check whether endpoint can is reachable
\dontrun{
validObject(ep)
}
# C) Create namespace object
ns <- namespace("PREFIX skos: <http://www.w3.org/2004/02/skos/core#>")
# D) Create query object
sql <-"SELECT ?molecule WHERE { ?molecule skos:altLabel 'Irinotecan' .}"
sp <- sparqlQuery(sql, ns)
# E) Execute query
\dontrun{
res <- sendQuery(ep, sp, accept="table")
# F) Post process resulting data.frame into ResultSet
rs <- resultSet(res)
}
}
\keyword{classes}
