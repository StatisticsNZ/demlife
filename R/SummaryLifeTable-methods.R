
## setMethod("show",
##           signature(object = "SummaryLifeTable"),
##           function(object) {
##               mx <- object@mx
##               cat("An object of class \"", class(object), "\"\n\n", sep = "")
##               cat("dimensions:\n")
##               limits <- dembase::limits(mx)
##               limits[] <- lapply(limits, as.character)
##               first <- limits["first", ]
##               last <- limits["last", ]
##               first <- as.character(first)
##               last <- as.character(last)
##               m <- rbind(`name:` = names(mx),
##                          `length:` = dim(mx), 
##                          `dimtype:` = dembase::dimtypes(mx),
##                          `dimscale:` = dembase::dimscales(mx), 
##                          `first:` = first,
##                          `last:` = last)
##               colnames(m) <- rep("", ncol(m))
##               print(m, quote = FALSE)
##               cat("\ndefault functions:", paste(object@showFun, collapse = ", "), "\n")
##               cat("radix:", object@radix, "\n")
##           })
