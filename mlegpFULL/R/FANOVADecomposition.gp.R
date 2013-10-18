`FANOVADecomposition.gp` <-
function(gp, Interaction=TRUE, verbose = TRUE, outputs = NULL, 
		lower = apply(gp$X,2,min), upper = apply(gp$X,2,max), maxpts = NULL, parallel = FALSE) {

	if (verbose) cat("calculating total functional variance...\n")
	FVar = FunctionalVariance(gp, lower = lower, upper = upper)

	numParams = gp$numDim
	numEffects = numParams
	params = gp$params

	if (Interaction) {
		numInts = choose(numParams,2)
		numEffects = numParams + numInts
	}

	R2 = matrix(0,numEffects)
	names = matrix(0,numEffects)
	names[1:numParams] = params

	mainList = list()
	for (i in 1:numParams) { 
		mainList[[i]] = list()
		mainList[[i]]$gp = gp
		mainList[[i]]$effect = i 
	}

	if (!parallel) {
	  	R2 = lapply(mainList, ANOVAMainEffect, lower = lower, upper = upper, verbose = verbose) 
	}

	if (parallel) {
		if (verbose) cat("calculating conribution of main effects...\n") 
	  	R2 = sfLapply(mainList, ANOVAMainEffect, lower = lower, upper = upper, verbose = verbose) 
		if (verbose) cat("...done\n") 
	}
      
        R2 = matrix(unlist(R2)) / FVar * 100.0

	if (!Interaction) return (data.frame(param=names, "% contribution"=R2, check.names=FALSE))

        XInts = matrix(0,choose(numParams,2))
        count = numParams + 1
        int1 = 1
        int2 = 2

	intcount = 1
	intList = list()
        for (int1 in 1:(numParams-1)) {
 	       for (int2 in (int1+1):numParams) {
		  intList[[intcount]] = list()
		  intList[[intcount]]$gp = gp
		  intList[[intcount]]$effects = c(int1,int2) 
		  names[count] = paste(params[int1], params[int2], sep=":")
	          count = count + 1
		  intcount = intcount + 1	
                }
       }

	if (!parallel) {
		ints = lapply(intList, ANOVAInteractionEffect, lower =lower, upper = upper,maxpts = maxpts, verbose = verbose) 
	
        }

	if (parallel) {
		if (verbose) cat("calculating contribution of interaction effects...\n") 
		ints = sfLapply(intList, ANOVAInteractionEffect, lower = lower, upper = upper, maxpts = maxpts,verbose = verbose) 
		if (verbose) cat("...done\n") 
	}


	ints = matrix(unlist(ints)) /FVar*100.0 
	R2 = rbind(R2, ints)


	return (data.frame(param=names, "% contribution"=R2, check.names=FALSE))
}

