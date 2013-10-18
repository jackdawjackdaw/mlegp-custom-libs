`plotMainEffectsUsingPCWeights` <-
function(gp, ylab = "predicted output", graphStyle = 2, verbose = FALSE, no.plot = FALSE, holdIndex, param.values = NULL, 
xlab = "time", lower = NULL, upper = NULL, xValues = 1:dim(gp$UD)[1], parallel = FALSE, ...) {
	
	main = "Main effect of "
	main = paste(main, gp$params[holdIndex])
	#main = ""

	if (is.null(param.values)) param.values = seq(min(gp[[1]]$X[,holdIndex]), max(gp[[1]]$X[,holdIndex]),length.out=3)

	UD = gp$UD

        N = length(param.values)

    	mainList = list()
        for (i in 1:gp$numGPs) {
                mainList[[i]] = list()
                mainList[[i]]$gp = gp[[i]]
                mainList[[i]]$effect = holdIndex
        }

	### assume X range is the same for all GPs ### 
	if (is.null(lower)) lower = apply(gp[[i]]$X, 2, min)
	if (is.null(upper)) upper = apply(gp[[i]]$X, 2, max)

	preds = NULL
	if (!parallel) preds = lapply(mainList, calcMainEffect, index = param.values, lower = lower, upper = upper, verbose = verbose)
	if (parallel) {
			if (verbose) {
				cat("calculating main effects in parallel mode for PC weights: "); cat(1:gp$numGPs); cat("...\n")
			}
			preds = sfLapply(mainList, calcMainEffect, index = param.values, lower = lower, upper = upper, verbose = verbose)
			if (verbose) { 
				cat("...done\n")
			}
	}
        w = matrix(unlist(preds), ncol = length(preds[[1]]), byrow = TRUE)

       	output = t(gp$UD%*%w)

	if (no.plot) return (list(index = param.values, preds = output))

	if (graphStyle == 2) {
		par(mfrow = c(1,2))
	}

        for (i in 1:dim(output)[1]) {
                plot(xValues, output[i,], ylim = c(min(output), max(output)), xlab = xlab, ylab = ylab, type = "n", main=main)
		main = ""; xlab = ""; ylab = ""
		par(xaxt = "n"); par(yaxt = "n")
                par(new=TRUE)
        }

        for (i in 1:dim(output)[1]) {
                lines(xValues, output[i,], lty = i)
                par(new=TRUE)
        }
        par(new=FALSE)
	if (graphStyle == 2) {
		par(new=FALSE)
		plot(seq(0,5),seq(0,5), type="n", xlab = "", ylab = "", axes=FALSE)
	}
	
	if (graphStyle > 0) {	
		blank = rep(" ", N)
		title = paste(paste("Legend (", gp$params[holdIndex]), ")") 
		temp <- legend("topright", legend = blank,
        		text.width = strwidth("1,000,000"),
		        lty = 1:N, xjust = 1, yjust = 1,
	        	title = title)
		text(temp$rect$left + temp$rect$w, temp$text$y,param.values, pos=2)
	}
	par(xaxt = "s"); par(yaxt = "s")
}

