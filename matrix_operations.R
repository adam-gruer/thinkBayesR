hypos <- 1:1e3
pmf <- matrix(c(hypos,rep_len(1,length(hypos))),
              ncol = 2,
              dimnames = list(NULL, c("values","probabilities")))


#acess a column by name
pmf[,"probabilities"]
pmf[,"values"]

# filter rows by value
pmf[pmf[,"values"] == 34, , drop = FALSE] # drop = FALSE result is a matrix, TRUE (default) - result is vector
.subset2(pmf, select = "probabilities")
(value_only <- pmf[pmf[,"values"] == 34, "values"])
(value_only <- pmf[pmf[,"values"] == 34, "probabilities"])

#set all values of a column to 1
pmf[,"probabilities"] <- 1

#set column values to result of caclulation 
pmf[,"probabilities"] <- pmf[,"probabilities"] / sum(pmf[,"probabilities"])

pmf[,"probabilities"] <- pmf[,"values"] * pmf[,"probabilities"]
    