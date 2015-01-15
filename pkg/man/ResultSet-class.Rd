\name{ResultSet-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{ResultSet-class}
%
\alias{initialize,ResultSet-method}
\alias{show,ResultSet-method}
%
\alias{resultSet}
\alias{as.data.frame.ResultSet}
%
\title{Class \code{"ResultSet"}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{The class is designed to provide as post processing Layer which
parses relevant information from SPARQL query results in table format.
}
\section{Objects from the Class}{
Objects can be created from data.frame by calls of the form
\code{resultSet(data)}.}
\section{Slots}{
    \describe{
        \item{\code{data}:}{\code{"data.frame"}. Contains parsed data }
        %
        \item{\code{prefix}:}{\code{"data.frame"}. Contains complete prefixes.}
        %
        \item{\code{prefixwide}:}{\code{"character"}. Contains prefixes
            in wide format. }
        %
        \item{\code{prefixshort}:}{\code{"character"}. Contains prefixes in
            short format.}
    }
}
\section{Methods}{
    \describe{
        \item{initialize}{\code{signature(.Object = "ResultSet")}: Initializer.}
        %
        \item{show}{\code{signature(object = "ResultSet")}: Produces
            a sensible printout.}
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
