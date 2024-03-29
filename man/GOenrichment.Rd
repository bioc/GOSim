\name{GOenrichment}
\alias{GOenrichment}
\alias{analyzeCluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ GO enrichment analysis }
\description{
  This function performs a GO enrichment analysis using topGO. It combines the two former functions "GOenrichment" and "analyzeCluster".
}
\usage{
GOenrichment(genesOfInterest, allgenes, cutoff=0.01, method="elim")
}
%- maybe also 'usage' for other objects documented here.
\arguments{  
  \item{genesOfInterest}{character vector of Entrez gene IDs or vector of statistics (p-values, t-statistics, ...) named with entrez gene IDs}
  \item{allgenes}{character vector of Entrez gene IDs or vector of statistics named with entrez gene IDs}
  \item{cutoff}{significance cutoff for GO enrichment analysis}  
  \item{method}{topGO method to use}
}
\value{  
  \item{GOTerms}{list of significant GO terms and their description}
  \item{p.values}{vector of p-values for significant GO terms}
  \item{genes}{list of genes associated to each GO term}
}
\details{
If the parameters 'genesOfInterest' and 'allgenes' are both character vectors of Entrez gene IDs, Fisher's exact test is used. The Kolmogorov-Smirnov test can be used, if a score (e.g. p-value) for each gene is provided. For more details please refer to the topGO vignette. 
}
\references{
Adrian Alexa, J\"org Rahnenf\"uhrer, Thomas Lengauer: Improved scoring of functional groups from gene expression data by decorrelating GO graph structure, Bioinformatics, 2006, 22(13):1600-1607}
\author{Holger Froehlich}
\seealso{\code{\link{evaluateClustering}} }
\examples{
	\donttest{			
	if(require(org.Hs.eg.db) & require(topGO)){
		allgenes = sample(keys(org.Hs.egGO), 1000) # suppose these are all genes
		allpvalues = runif(1000) # an these are their pvalues
		names(allpvalues) = allgenes	
		GOenrichment(allpvalues[allpvalues<0.05], allpvalues) # GO enrichment analysis using Kolmogorov-Smirnov test
	}
	}	
}
\keyword{ file }% at least one, from doc/KEYWORDS
