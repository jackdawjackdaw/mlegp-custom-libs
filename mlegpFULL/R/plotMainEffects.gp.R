`plotMainEffects.gp` <-
 function(gp, ylab = "predicted output", graphStyle = 2, verbose = FALSE, no.plot = FALSE, parallel = FALSE, effects = gp$params, length.out = 21, lower = NULL, upper = NULL, FANOVA = FALSE, ...) {

	main = "Main Effects"
	main = ""
	xlab = "param value" # will be overwritten if graphStyle = 3

	if (is.null(lower)) lower = apply(gp$X,2,min)
	if (is.null(upper)) upper = apply(gp$X,2,max)
	effects = toParamIndexes(effects, gp$params)
	param.names = gp$params[effects]

	if (FANOVA & length(gp$params) > 1) {
		R2 = FANOVADecomposition(gp, lower = lower, upper = upper, parallel = parallel, verbose = verbose)
		R2 = round(R2[,2],3)
	}

	index = seq(min(gp$X[,effects]), max(gp$X[,effects]), length.out = length.out)
	preds = matrix(0,ncol = length.out, nrow = length(effects))
	scaleIndex = FALSE

	mainList = list()
	for (i in 1:length(effects)) {
		if (min(index) != min(gp$X[,effects[i]]) || max(index) != max(gp$X[,effects[i]])) {
			scaleIndex = TRUE
		}
		mainList[[i]] = list()
		mainList[[i]]$gp = gp
		mainList[[i]]$effect = effects[i]   
  	  }

	preds = NULL
          if(!parallel) {		
	  	preds = lapply(mainList, calcMainEffect, length.out = length.out, lower = lower, upper = upper, verbose = verbose) 
	  }
	  if(parallel) {
		if (verbose) {cat("calculating main effects in parallel mode for: "); cat(gp$params[effects]); cat("\n")}
	  	preds = sfLapply(mainList, calcMainEffect, length.out = length.out, lower = lower, upper = upper, verbose = verbose) 
		if (verbose) cat("...done\n")
	  }
	
	  preds = matrix(unlist(preds),ncol = length(preds[[1]]),byrow=TRUE)

	if (no.plot) {
		if (scaleIndex) index = seq(0,1,length.out=length.out)
		d = list(index = index, preds = preds) 
		return (d)
	}

	if (graphStyle > 2) {
		createWindow(length(param.names))
	}

	if (graphStyle == 2) {
		par(mfrow = c(1,2))
	}

	range = c(min(preds), max(preds))
	for (i in 1:length(effects)) {
	   if (graphStyle == 3) {
		xlab = param.names[i]
	   }
	   index = seq(lower[effects[i]], upper[effects[i]], length.out=length.out)
	   if(scaleIndex) index = seq(0,1,length.out=length.out)
	   xlim = c(min(index),max(index))
	   plot(index, preds[i,], ylab = ylab, type = "n", ylim = range, xlim = xlim, main = main, xlab = xlab)
	   main = ""; xlab = ""; ylab = ""
	   par(xaxt = "n"); par(yaxt = "n") 
           if (graphStyle < 3) par(new=TRUE)
	   lines(index, preds[i,], col=i, lty=i)
           if (graphStyle < 3) par(new=TRUE)
	}

	if (graphStyle == 1 || graphStyle == 2) {
		if (graphStyle == 2) {   ## create the legend plot
		  par(new=FALSE)
		  plot(seq(0,5),seq(0,5), type="n", xlab = "", ylab = "", axes=FALSE)
		}
		cex = 1 #.75
		numEffects = length(effects)
		blank = rep(" ", numEffects)
		temp <- legend("topright", legend = blank,
        		text.width = strwidth("1,000,000"),
		        lty = 1:numEffects, col=1:numEffects, xjust = 1, yjust = 1,
        		title = "Legend",cex = cex )
		if (FANOVA) param.names = paste(paste(param.names[effects], R2[effects], sep=" ("),")", sep="")
		text(temp$rect$left + temp$rect$w, temp$text$y,param.names, pos=2, cex = cex)
		par(new=FALSE)
	}
	par(xaxt = "s"); par(yaxt = "s") 
}

