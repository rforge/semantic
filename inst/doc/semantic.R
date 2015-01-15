## ----, message=FALSE, echo=FALSE-----------------------------------------
library(semantic)

## ------------------------------------------------------------------------
ns <- namespace("PREFIX owl: <http://www.w3.org/2002/07/owl#>")
cat(asPrefix(ns))

## ------------------------------------------------------------------------
ef <- system.file("extdata", "ebi_ns.txt", package="semantic")
ns <- importNamespace(ef)

## ----, eval=FALSE--------------------------------------------------------
#  ep <- sparql("http://www.ebi.ac.uk/rdf/services/chembl/sparql")
#  validObject(ep)

## ----, eval=FALSE--------------------------------------------------------
#  en <- ns["skos"]
#  sql <-"SELECT ?mol WHERE { ?mol skos:altLabel 'Temozolomide' . }"
#  sp <- sparqlQuery(sql, en)
#  res <- sendQuery(ep, sp, accept="table")
#  rs <- resultSet(res)

## ----, eval=FALSE--------------------------------------------------------
#  dfr <- as.data.frame(rs)

## ------------------------------------------------------------------------
library()

