\name{QueryForm-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{QueryForm-class}
%
\alias{initialize,QueryForm-method}
\alias{queryForm}
%
\alias{setRdf,QueryForm,character-method}
\alias{setRdf,SparqlQuery,QueryForm-method}
\alias{show,QueryForm-method}
%
\alias{qryText,QueryForm-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{QueryForm: Definition of Query Form for SPARQL Query}
\description{Class for defining query form of SPARQL query: SELECT, DEFINE
or CONSTRUCT.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{queryForm()}.
}
\section{Slots}{
    \describe{
        \item{\code{ev}:}{\code{"environment"}. Contains \code{form}:
            character vector of length 1.}
    }
}
\section{Methods}{
    \describe{
        \item{initialize}{\code{signature(.Object = "QueryForm")}: 
            Standard object initializer.}
        %
        \item{setRdf}{\code{signature(object = "QueryForm", rdf = "character")}:
            ... }
        %
        \item{setRdf}{\code{signature(object = "SparqlQuery", rdf = "QueryForm")}:
            Returns query form as }
        %
        \item{show}{\code{signature(object = "QueryForm")}:
            Prints out a sensible report on object.}
        }
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% References
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Kaisers, W.}
\seealso{
\code{\linkS4class{SparqlQuery}} }
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
queryForm()
queryForm("s")
queryForm("d")
queryForm("c")
spq <- sparqlQuery()
setRdf(spq, queryForm("d"))
}
\keyword{classes}
