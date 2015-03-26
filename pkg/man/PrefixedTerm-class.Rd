\name{PrefixedTerm-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{PrefixedTerm-class}
%
\alias{initialize,PrefixedTerm-method}
%
\alias{[,PrefixedTerm-method}
\alias{c,PrefixedTerm-method}
\alias{show,PrefixedTerm-method}
% Constructor methods
\alias{prefixedTerm}
\alias{prefixedTerm-methods}
\alias{prefixedTerm,SparqlNamespace,ANY,ANY-method}
\alias{prefixedTerm,SparqlNamespace,numeric,character-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{PrefixedTerm: Creation Of RDF Terms Refering to a PREFIX.}
\description{A prefixed term represents an Rdf term containing a prefix
which is encoded in a PREFIX header (represented by a SparqlNamespace object).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{prefixedTerms}.
}
\section{Slots}{
    \describe{
        \item{\code{nsptr}:}{Object of class \code{"externalptr"}:
            Pointer to external C++ Object containing prefix data.}
        %
        \item{\code{delim}:}{Object of class \code{"character"}:
            Delimiter by which prefixes and terms are separated
            in PREFIX header (default: ':').}
        %
        \item{\code{ev}:}{Object of class \code{"environment"}:
            Containing prefix and text (character) vectors.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{RdfTerm}"}, directly.
}
\section{Methods}{
    \describe{
        \item{[}{\code{signature(x = "PrefixedTerm")}: 
            Generic operator for subsetting.}
        %
        \item{c}{\code{signature(x = "PrefixedTerm")}:
            Generic operator for concatenation. }
        %
        \item{initialize}{\code{signature(.Object = "PrefixedTerm")}:
            Generic copy constructor.}
        %
        \item{show}{\code{signature(object = "PrefixedTerm")}: 
            Produces a sensible printout on Terminal.}
    }
}
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Kaisers, W.}
%\note{}
%\section{New section}{....}
%\subsection{Sub section}{}
%\seealso{\code{\link{~~fun~~}} \code{\linkS4class{CLASSNAME}} }
\seealso{
\code{\linkS4class{SparqlNamespace}}
\code{\linkS4class{RdfTerm}}}
\examples{
ns <- sparqlNamespace("foaf", "<http://xmlns.com/foaf/0.1/>")
ptm <- prefixedTerm(ns, 1, c("name", "mbox"))
ptm
}
\keyword{classes}
