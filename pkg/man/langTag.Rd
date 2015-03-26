\name{langTag}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{langTag}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{langTag}
\description{Returns an RdfTerm object which represents a SPARQL 
language tag.}
\usage{langTag(val, lang = "en")}
\arguments{
    \item{val}{String literal which will be enclosed in language tag.}
    %
    \item{lang}{Language identifier denoting the used language.}
}
\details{The literal given by val will be enclosed in quotation marks, 
an @ separator is added together with the \code{lang} tag as suffix.
}
\value{RdfTerm.}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Reference
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
Section 2.3.1 (Matching Literasl with Language Tags)
}
\author{Kaisers, W.}
%\note{}
%\section{New section}{....}
%\subsection{Sub section}{}
%\seealso{\code{\link{~~fun~~}} \code{\linkS4class{CLASSNAME}} }
\seealso{\code{\linkS4class{RdfTerm}}}
\examples{
ct <- langTag("cat")
}
\keyword{langTag}
\keyword{RdfTerm}
