\name{IriTerm-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{IriTerm-class}
\alias{initialize,IriTerm-method}
%
\alias{iriTerm}
%
\alias{$<--methods}
\alias{$<-,IriTerm-method}
\alias{$<-,IriTerm-method}
%
\alias{parseText}
\alias{parseText-methods}
\alias{parseText,IriTerm,ANY-method}
\alias{parseText,IriTerm,character-method}
%
\alias{term}
\alias{term-methods}
\alias{term,IriTerm-method}
%
\alias{prefix}
\alias{prefix-methods}
\alias{prefix,IriTerm-method}
%
\alias{text.IriTerm}
%
\alias{writeXml,IriTerm-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{Class \code{"IriTerm"}}
\description{Manages textual representations of IRI terms.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IriTerm")}.
}
\section{Slots}{
  \describe{
    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
    % Environment
    % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
    \item{\code{ev}:}{Object of class \code{"environment"}: Contains
        character vectors: text, prefix, term.}
    \item{\code{iri_term}:}{Object of class \code{"externalptr"} Points to
        C++ vector containing \code{iriTerm} objects.}
    \item{\code{delim}:}{Object of class \code{"character"}: Default: "#:/" }
    \item{\code{nDelim}:}{Object of class \code{"integer"}: Default: 3}
  }
}
\section{Methods}{
  \describe{
    \item{$<-}{\code{signature(x = "IriTerm")}: Generic writing accessor
        for slot data.}
    \item{initialize}{\code{signature(.Object = "IriTerm")}: Initializes
        IriTerm object.}
    \item{iriText}{\code{signature(object = "IriTerm")}: }
    \item{parseText}{\code{signature(object = "IriTerm", value = "ANY")}: 
        Parses text contained in \code{text} variable.}
    \item{parseText}{\code{signature(object = "IriTerm", value = "character")}: 
        Parses text given as \code{value} argument. }
    \item{prefix}{\code{signature(object = "IriTerm")}: Returns prefix
        segment of IRI records. }
    \item{term}{\code{signature(object = "IriTerm")}: Returns term segment
        of IRI records.}
     }
}
\references{\url{http://www.w3.org/TR/rdf-sparql-query/#QSynIRI}}
\author{Wolfgang Kaisers}
\examples{
iri <- c(
    ":base",
    "ac:base",
    "xyz",
    "<http://example.org/push>",
    "<http://example.org/push#one>"
)
it <- iriTerm(iri)
parseText(it)
term(it)
prefix(it)
text(it)
}
\keyword{classes}
