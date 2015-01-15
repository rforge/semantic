\name{SelectQuery-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{SelectQuery-class}
%
\alias{selectQuery}
%
\alias{addDataset<-}
\alias{addDataset<--methods}
\alias{addDataset<-,SelectQuery-method}
%
\alias{addModifier<-}
\alias{addModifier<--methods}
\alias{addModifier<-,SelectQuery-method}
%
\alias{addPattern<-}
\alias{addPattern<--methods}
\alias{addPattern<-,SelectQuery-method}
%
\alias{dataset}
\alias{dataset<-}
\alias{dataset<--methods}
\alias{dataset<-,SelectQuery-method}
%
\alias{dataset-methods}
\alias{dataset,SelectQuery-method}
%
\alias{dataset<-,SelectQuery-method}
\alias{dataset,SelectQuery-method}
%
\alias{distinct}
\alias{distinct<-}
\alias{distinct<--methods}
\alias{distinct<-,SelectQuery-method}
\alias{distinct-methods}
\alias{distinct,SelectQuery-method}
%
\alias{distinct<-,SelectQuery-method}
\alias{distinct,SelectQuery-method}
%
\alias{getQuery,SelectQuery-method}
%
\alias{initialize,SelectQuery-method}
%
\alias{modifier}
\alias{modifier<-}
\alias{modifier<-,SelectQuery-method}
\alias{modifier,SelectQuery-method}
%
\alias{pattern}
\alias{pattern<-}
\alias{pattern-methods}
\alias{pattern,SelectQuery-method}
\alias{pattern<-,SelectQuery-method}
\alias{pattern,SelectQuery-method}
%
\alias{pattern<--methods}
\alias{pattern<-,SelectQuery-method}
%
\alias{result}
\alias{result-methods}
\alias{result,SelectQuery-method}
%
\alias{result<-}
\alias{result<--methods}
\alias{result<-,SelectQuery-method}
\alias{show,SelectQuery-method}
%
\title{Class \code{"SelectQuery"}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{Provides a raw object framework for construction of
SPARQL SELECT queries.}
%
\section{Objects from the Class}{
Objects can be created from Namespace objects 
by calls of the form \code{selectQuery(ns))}.
}
\section{Slots}{
    \describe{
        \item{\code{namespace}:}{\code{"Namespace"}.}
        \item{\code{result}:}{Object of class \code{"character"} ~~ }
        \item{\code{distinct}:}{Object of class \code{"logical"} ~~ }
        \item{\code{dataset}:}{Object of class \code{"character"} ~~ }
        \item{\code{pattern}:}{Object of class \code{"character"} ~~ }
        \item{\code{modifier}:}{Object of class \code{"character"} ~~ }
        \item{\code{sql}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SparqlQuery}"}, directly.
}
\section{Methods}{
    \describe{
        \item{addDataset<-}{\code{signature(object = "SelectQuery")}: ... }
        \item{addModifier<-}{\code{signature(object = "SelectQuery")}: ... }
        \item{addPattern<-}{\code{signature(object = "SelectQuery")}: ... }
        \item{dataset<-}{\code{signature(object = "SelectQuery")}: ... }
        \item{dataset}{\code{signature(object = "SelectQuery")}: ... }
        \item{distinct<-}{\code{signature(object = "SelectQuery")}: ... }
        \item{distinct}{\code{signature(object = "SelectQuery")}: ... }
        \item{getQuery}{\code{signature(object = "SelectQuery")}: ... }
        \item{initialize}{\code{signature(.Object = "SelectQuery")}: ... }
        \item{modifier<-}{\code{signature(object = "SelectQuery")}: ... }
        \item{modifier}{\code{signature(object = "SelectQuery")}: ... }
        \item{pattern<-}{\code{signature(object = "SelectQuery")}: ... }
        \item{pattern}{\code{signature(object = "SelectQuery")}: ... }
        \item{result<-}{\code{signature(object = "SelectQuery")}: ... }
        \item{result}{\code{signature(object = "SelectQuery")}: ... }
        \item{show}{\code{signature(object = "SelectQuery")}: ... }
    }
}
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
\seealso{Namespace, SparqlQuery}
\examples{
ns <- namespace("PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>")
sq <- selectQuery(ns)
}
\keyword{SelectQuery}
