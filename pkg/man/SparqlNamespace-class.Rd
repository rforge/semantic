\name{SparqlNamespace-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{SparqlNamespace-class}
%
\alias{sparqlNamespace}
\alias{sparqlNamespace-methods}
\alias{sparqlNamespace,missing,missing-method}
\alias{sparqlNamespace,character,character-method}
\alias{sparqlNamespace,character,missing-method}
%
\alias{initialize,SparqlNamespace-method}
\alias{length,SparqlNamespace-method}
\alias{show,SparqlNamespace-method}
%
\alias{c-methods}
\alias{c,SparqlNamespace-method}
%
\alias{coerce-methods}
\alias{coerce,SparqlNamespace,data.frame-method}
%
\alias{addPrefix}
\alias{addPrefix-methods}
\alias{addPrefix,SparqlNamespace,character,character-method}
%
\alias{sparqlQuery,SparqlNamespace-method}
%
\alias{qryText,SparqlNamespace-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{SparqlNamespace: Management Of Prefix And IRI Data For SPARQL Queries}
\description{Contains PREFIX and IRI data used for SPARQL queries and
RDF data.
}
\section{Objects from the Class}{Objects can be created by calls of
the form sparqlNamespace}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Structure
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\section{Slots}{
  \describe{
    \item{\code{ptr}:}{Object of class \code{"externalptr"}
        Pointer to C++ object which provides parsing mechanisms for 
        Iri terms.}
    %
    \item{\code{ev}:}{Object of class \code{"environment"}}
  }
}
\section{Environment content}{
\tabular{lll}{
    Name    \tab    Type        \tab Content\cr
    prefix  \tab    character   \tab Prefix text\cr
    iri     \tab    character   \tab IRI text\cr
}}
\section{Methods}{
    \describe{
        \item{addPrefix}{\code{signature(object = "SparqlNamespace", 
            prefix = "character", iri = "character")}:
                Adds new PREFIX items to namespace.
        }
        %
        \item{c}{\code{signature(x = "SparqlNamespace")}:
            Concatenates \code{SparqlNamespace objects.}
        }
        %
        \item{coerce}{\code{signature(from = "SparqlNamespace", 
            to = "data.frame")}:
            Converts SparqlNamespace to data.frame.
        }
        %
        \item{initialize}{\code{signature(.Object = "SparqlNamespace")}:
            Generic copy constructor for SparqlNamespace.
        }
        %
        \item{length}{\code{signature(x = "SparqlNamespace")}:
            Returns number of Prefixes in namespace.
        }
        %
        \item{prefixedTerm}{\code{signature(namespace = "SparqlNamespace",
            index = "ANY", term = "ANY")}:
            Returns empty \code{PrefixedTerm} object.
        }
        %
        \item{prefixedTerm}{\code{signature(namespace = "SparqlNamespace", 
            index = "numeric", term = "character")}:
            Returns a \code{PrefixedTerm} object where prefix is retrieved
            from namespace via index. \code{PrefixedTerm} is a subclass
            of \code{RdfData}.
        }
        %
        \item{setRdf}{\code{signature(object = "RdfData", 
            rdf = "SparqlNamespace")}: 
            Sets SparqlNamespace in RdfData object.
        }
        %
        \item{show}{\code{signature(object = "SparqlNamespace")}:
            Generic producing a terminal printout.}
        %
        \item{sparqlQuery}{\code{signature(object = "SparqlNamespace")}:
            Constructor method.
        }
        %
        \item{qryText}{\code{signature(object = "SparqlNamespace")}:
            Returns textual representation.
        }
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
%\note{}
%\section{New section}{....}
%\subsection{Sub section}{}
%\seealso{\code{\link{~~fun~~}} \code{\linkS4class{CLASSNAME}} }
\seealso{
\code{\linkS4class{SparqlQuery}}
\code{\linkS4class{RdfData}}
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
nsp2 <- c("dc","")
nsi2 <- c("<http://purl.org/dc/elements/1.1/>",
          "<http://example.org/book/>")
ns2 <- sparqlNamespace()
addPrefix(ns2, prefix=nsp2, iri=nsi2)
length(ns2)
ns2
# Create Namespace from textual representation
nst <- "PREFIX psys: <http://proton.semanticweb.org/2006/05/protons#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX gene: <http://linkedlifedata.com/resource/entrezgene/>"
ns <- sparqlNamespace(nst)
}
\keyword{classes}
