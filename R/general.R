initialize<-function(){
	message("initializing GOSim package ...")		
	assign("GOSimEnv",new.env(parent=globalenv()),envir=.GlobalEnv)  	
  	setEvidenceLevel("all", organism="human")
  	setOntology("BP")
  	message("finished.")
}

getOffsprings<-function(){
  #require(GO.db)
  if(!exists("GOSimEnv")) initialize()
  ontology<-get("ontology",envir=GOSimEnv)
  if(ontology == "BP")
    res<-AnnotationDbi::as.list(GOBPOFFSPRING)
  else if(ontology == "MF")
    res<-AnnotationDbi::as.list(GOMFOFFSPRING)
  else if(ontology == "CC")
    res<-AnnotationDbi::as.list(GOCCOFFSPRING)
  else
    stop(paste("ontology", ontology, "not known!"))
  return(res)
}

getAncestors<-function(){
  #require(GO.db)
  if(!exists("GOSimEnv")) initialize()
  ontology<-get("ontology",envir=GOSimEnv)
  if(ontology == "BP")
    res<-AnnotationDbi::as.list(GOBPANCESTOR)
  else if(ontology == "MF")
    res<-AnnotationDbi::as.list(GOMFANCESTOR)
  else if(ontology == "CC")
    res<-AnnotationDbi::as.list(GOCCANCESTOR)
  else
    stop(paste("ontology", ontology, "not known!"))
  return(res)
}

getParents<-function(){
  #require(GO.db)
  if(!exists("GOSimEnv")) initialize()
  ontology<-get("ontology",envir=GOSimEnv)
  if(ontology == "BP")
    res<-AnnotationDbi::as.list(GOBPPARENTS)
  else if(ontology == "MF")
    res<-AnnotationDbi::as.list(GOMFPARENTS)
  else if(ontology == "CC")
    res<-AnnotationDbi::as.list(GOCCPARENTS)
  else
    stop(paste("ontology", ontology, "not known!"))
  return(res)
}

getChildren<-function(){
  #require(GO.db)
  if(!exists("GOSimEnv")) initialize()
  ontology<-get("ontology",envir=GOSimEnv)
  if(ontology == "BP")
    res<-AnnotationDbi::as.list(GOBPCHILDREN)
  else if(ontology == "MF")
    res<-AnnotationDbi::as.list(GOMFCHILDREN)
  else if(ontology == "CC")
    res<-AnnotationDbi::as.list(GOCCCHILDREN)
  else
    stop(paste("ontology", ontology, "not known!"))
  return(res)
}

# filter GO mapping for given evidence levels
setEvidenceLevel<-function(evidences="all", organism=org.Hs.egORGANISM, gomap=org.Hs.egGO){			
    if(!exists("GOSimEnv")) initialize()	
	message(paste("-> retrieving GO information for all available genes for organism '", organism, "' in GO database", sep=""))
	assign("evidences", evidences, envir=GOSimEnv)	
# 	gomap<-as.list(GOENTREZID2GO)		
	if(is(gomap, "Bimap")){		
		mapped_genes <- mappedkeys(gomap)	
		gomap = AnnotationDbi::as.list(gomap[mapped_genes])
	}
	else if(!is(gomap, "list"))
		stop("gomap argument should be a nested list (see manual pages)!")
	message(paste("-> filtering GO terms according to evidence levels '", evidences, "'",sep=""))
	if((length(evidences) > 1) || (evidences!="all")){		
		gomap<-sapply(gomap,function(x) sapply(x,function(y) c(y$Evidence %in% evidences, y$Ontology)))
		gomap<-sapply(gomap, function(x) x[2,x[1,]=="TRUE"])
		gomap<-gomap[sapply(gomap,length) >0]
	}
	assign("gomap", gomap, envir=GOSimEnv)
	assign("organism", organism, envir=GOSimEnv)	
}

setOntology<-function(ont="BP", loadIC=TRUE, DIR=NULL){
	if(!exists("GOSimEnv")) initialize()	
	assign("ontology", ont, envir=GOSimEnv)		
	if(loadIC){
		organism = get("organism", envir=GOSimEnv)
		message(paste("-> loading files with information content for corresponding GO category (",organism,")",sep=""))
		ontology<-get("ontology",envir=GOSimEnv)
		evidences<-get("evidences",envir=GOSimEnv)			
		fname = paste("ICs",ontology,organism, paste(evidences,collapse="_"),sep="")
		if(is.null(DIR))
			tryCatch(utils::data(list=fname,package="GOSim",envir=GOSimEnv), warning=function(w) stop(paste("File", fname, "with IC values for organism '", organism, "', ontology '", ontology, "', evidence codes '", evidences, "' not found!\nPlease invoke calcICs() to calculate IC values!")))
		else
			load(file=file.path(DIR, paste(fname, ".rda", sep="")), envir=GOSimEnv)
		IC<-get("IC",envir=GOSimEnv)
		#IC<-IC/max(IC[IC!=Inf])
		IC["all"]=0
		assign("IC", IC, envir=GOSimEnv)
	}
 	assign("ancestor", getAncestors(), envir=GOSimEnv) 	
 	assign("children", getChildren(), envir=GOSimEnv)
 	assign("parents", getParents(), envir=GOSimEnv) 
 	children<-get("children",envir=GOSimEnv) 	
 	parents<-get("parents",envir=GOSimEnv) 	
 	assign("nchildren", sapply(children,length) , envir=GOSimEnv)
 	assign("nparents", sapply(parents,length), envir=GOSimEnv)
 	nchildren<-get("nchildren",envir=GOSimEnv) 	
 	nparents<-get("nparents",envir=GOSimEnv) 	
 	assign("Eavg", (sum(nchildren) + sum(nparents))/ length(union(names(nchildren), names(nparents))), envir<-GOSimEnv)
  	assign("alphaParam", 0.5, envir=GOSimEnv)
  	assign("betaParam", 0.5, envir=GOSimEnv)  	    	
}

utils::globalVariables("GOSimEnv", package="GOSim")
