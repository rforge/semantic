\name{split_prefix}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{split_prefix}
%
\title{split_prefix: Split SPARQL Results Into (URI) Prefixes And Values}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{Splits a column in a result data.fram into prefix and suffix.}
%
\usage{
split_prefix(text, split = "\\\/#")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{text}{character. Text which will be split into prefix and suffix.}
    %
    \item{split}{character. Character (of length 1).
        The text will be divided at the last occuring position of the split
        character is found (using negative lookahead).}
}
\details{The function uses negative lookahead for separation of prefix and
suffix.}
\value{data.frame with two columns.}
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
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
text <- c("123/", "12/3", "/123", "123#", "12#3", "1/2#3")
split_prefix(text)
}
\keyword{split}
