\name{semantic-package}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{semantic-package}
\alias{semantic}
\docType{package}
\title{Provides RDF functionality (e.g. querying SPARQL endpoints)}
% See 2.1.1 of R-exts (Documenting functions)
\description{
Provides of RDF functionality including SPARQL.
}
\details{
\tabular{ll}{
Package: \tab semantic\cr
Type: \tab Package\cr
Version: \tab 0.1.1\cr
Date: \tab 2014-11-27\cr
License: \tab GPL-2\cr
Depends: \tab methods, XML, RCurl\cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
W. Kaisers
Maintainer: W. Kaisers <kaisers@med.uni-duesseldorf.de>
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{XML, RCurl}
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
sql <-"SELECT ?molecule
WHERE
{
    ?molecule skos:altLabel 'Irinotecan' .
}"
sp <- sparqlQuery(sql, ns)
# E) Execute query
\dontrun{
res <- sendQuery(ep, sp, accept="table")
# F) Post process resulting data.frame into ResultSet
rs <- resultSet(res)
}

}
