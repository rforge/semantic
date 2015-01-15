\name{importNamespace}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{importNamespace}
\title{importNamespace: Reads namespace data from an external file.}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{The function reads data from an external file parses the
content and returns a Namespace object.}
%
\usage{importNamespace(file, comment.char = "#")}
%
\arguments{
    \item{file}{character. Name of input file. The function throws an error
        when the file does not exist.}
    %
    \item{comment.char}{character. Describes comment character. When the
        given character (sequence) occurs at the beginning of a line
        the respective line is not further processed.}
}
\details{The function reads the file content using 'readLines'. After removing
    comment lines, the resulting character vector is passed to the namespace
    function.}
\value{Namespace. Contains the parsed file content.}
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
\examples{
data<-system.file("extdata", "namespace.txt", package="semantic")
ns <- importNamespace(data)
}
\keyword{Namespace}
\keyword{namespace}
