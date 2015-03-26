\name{RdfTriple-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{RdfTriple-class}
%
\alias{rdfTriple}
\alias{rdfTriple-methods}
\alias{rdfTriple,character,character,character-method}
\alias{rdfTriple,RdfTerm,RdfTerm,RdfTerm-method}
%
\alias{length,RdfTriple-method}
\alias{show,RdfTriple-method}
%
\alias{addRdf,RdfGraph,RdfTriple-method}
\alias{gpText,RdfTriple-method}
\alias{setRdf,SparqlQuery,RdfTriple-method}
%
\alias{rdfGraph,RdfTriple-method}
%
\alias{qryText,RdfTriple-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{RdfTriple: Representation Of RDF Term Triples}
\description{RdfTriple objects contain three RdfTerm objects of equal length:
A Subject, a Predicate and an Object term. 
RdfTriple objects are used as part of a Graph pattern.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{rdfTriple(s, p, o)}.
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Structure
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\section{Slots}{
    \describe{
        \item{\code{subject}:}{Object of class \code{"RdfTerm"}
            RDF terms denoting subjects.}
        %
        \item{\code{predicate}:}{Object of class \code{"RdfTerm"} 
            RDF terms denoting predicates. }
        %
        \item{\code{object}:}{Object of class \code{"RdfTerm"} 
            RDF terms denoting objects. }
        %
        \item{\code{delim}:}{Object of class \code{"character"} 
            Delimiter by which a last subject, predicate, object
            triples in a series is terminated. Usually '.'}
        %
        \item{\code{crdelim}:}{Object of class \code{"character"} 
            Delimiter by which inner subject, predicate, object
            triples in a series are terminated. Usually '.' + newline. }
        %
        \item{\code{ev}:}{Object of class \code{"environment"} 
            (Inherited from BasicGroupPattern).}
    }
}
\section{Extends}{
Class \code{"\linkS4class{BasicGroupPattern}"}, directly.}
\section{Methods}{
    \describe{
        \item{addRdf}{\code{signature(object = "RdfGraph", rdf = "RdfTriple")}:
            Method for adding an RdfTriple to an RdfGraph. }
        %
        \item{gpText}{\code{signature(object = "RdfTriple")}: 
            Returns textual representation for usage in RdfGraph objects.}
        %
        \item{length}{\code{signature(x = "RdfTriple")}: 
            Returns the number of contained triples.}
        %
        \item{rdfGraph}{\code{signature(object = "RdfTriple")}: 
            Constructs an RdfGraph object from a set of RDF triples.}
        %
        \item{setRdf}{\code{signature(object = "RdfData", 
            rdf = "RdfTriple")}: 
            Sets or replaces RdfTriple data in an RdfData object.}
        %
        \item{setRdf}{\code{signature(object = "SparqlQuery",
            rdf = "RdfTriple")}:
            Sets or replaces a GroupPattern of a SparqlQuery
            by constructing a GroupPattern from a set of RDF triples.}
        %
        \item{qryText}{\code{signature(object = "RdfTriple")}:
            Returns textual representation for usage in SparqlQuery objects.}
        %
        \item{show}{\code{signature(object = "RdfTriple")}:
            Prints out a sensible textual representation on terminal.}
}
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% References
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\references{
SPARQL 1.1 Query Language W3C Recommendation.
\url{http://www.w3.org/TR/rdf-sparql-query/#defn_TriplePattern}
}
\author{Kaisers, W.}
%\note{}
%\section{New section}{....}
%\subsection{Sub section}{}
%\seealso{\code{\link{~~fun~~}} \code{\linkS4class{CLASSNAME}} }
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\seealso{\code{\linkS4class{RdfTerm}}}
\examples{
qv <- queryVariables(c("s", "p", "o"))
rt <- rdfTriple(qv[1], qv[2], qv[3])
}
\keyword{classes}
