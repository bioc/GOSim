\name{setEvidenceLevel}
\alias{setEvidenceLevel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Specifies to use only GO terms with given evidence codes.}
\description{
   Specifies to use only GO terms with given evidence codes. This, in combination with the specified GO ontology ("BP", "MF", "CC"), influences, how the information content for individual GO terms is calculated. 
}
\usage{
setEvidenceLevel(evidences = "all", organism=org.Hs.egORGANISM, gomap=org.Hs.egGO)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{evidences}{ character vector of evidence codes }
  \item{organism}{organism, for which to load a mapping of primary gene IDs to GO terms (see details)}
  \item{gomap}{mapping of primary gene IDs to GO terms to be used (see details)}
}
\details{
   Each evidence code can be one of:
   \describe{
\item{"IMP"}{inferred from mutant phenotype}
\item{"IGI"}{inferred from genetic interaction}
\item{"IPI"}{inferred from physical interaction}
\item{"ISS"}{inferred from sequence similarity}
\item{"IDA"}{inferred from direct assay}
\item{"IEP"}{inferred from expression pattern}
\item{"IEA"}{inferred from electronic annotation}
\item{"TAS"}{traceable author statement}
\item{"NAS"}{non-traceable author statement}
\item{"ND"}{no biological data available}
\item{"IC"}{inferred by curator}
}
Gene ids for which no GO associations exist are left out of the environment.

The method retrieves a mapping of primary gene IDs (usually Entrez) to GO terms, restricted by the given evidence codes.
This mapping is based on the respective organism annotation packages (e.g. org.Dm.eg.db for fly, org.Hs.eg.db for human, etc.). The user passes the GO mapping and the organism name to the function.  Please refer to the annotation packages for further information.

In case there does not exist an annotation package so far, the user can optionally provide its own mapping of primary gene IDs to GO terms instead of using one of the packages mentioned before. The mapping should come in form of a nested list having a format as in the following example (no NAs are allowed):

\$`11305`

\$`11305`\$`GO:0006810`

\$`11305`\$`GO:0006810`\$GOID
\[1\] "GO:0006810"

\$`11305`\$`GO:0006810`\$Evidence
\[1\] "IEA"

\$`11305`\$`GO:0006810`\$Ontology
\[1\] "BP"

\$`11305`\$`GO:0008203`

\$`11305`\$`GO:0008203`\$GOID

\[1\] "GO:0008203"

\$`11305`\$`GO:0008203`\$Evidence
\[1\] "ISS"

\$`11305`\$`GO:0008203`\$Ontology
\[1\] "BP"

\$`11306`

\$`11306`\$`GO:0006810`

\$`11306`\$`GO:0006810`\$GOID
\[1\] "GO:0006810"

\$`11306`\$`GO:0006810`\$Evidence
\[1\] "IEA"

\$`11306`\$`GO:0006810`\$Ontology
\[1\] "BP"


\$`11306`\$`GO:0006879`

\$`11306`\$`GO:0006879`\$GOID

\[1\] "GO:0006879"

\$`11306`\$`GO:0006879`\$Evidence
\[1\] "IMP"

\$`11306`\$`GO:0006879`\$Ontology
\[1\] "BP"


}
\value{
  The mapping is stored in the GOSimEnv environment.
}
\references{<www.geneontology.org>}
\author{ Holger Froehlich }
\note{By default all evidence codes are used. If another
  behavior is wanted, one has to recalculate the information content of
  all GO terms via \code{\link{calcICs}}.
  The evidence level influences the behavior of all other functions, especially
  \code{\link{filterGO}} and \code{\link{getGOInfo}}. 
}
\seealso{\code{\link{setOntology}}, \code{\link{calcICs}}, \code{\link{filterGO}}, \code{\link{getGOInfo}} }
\examples{
	\donttest{
	setEvidenceLevel("all")
	} # the default behavior	
}
\keyword{ file }% at least one, from doc/KEYWORDS

