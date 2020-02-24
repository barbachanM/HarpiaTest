source_python('markov.py')

markovmodelR <- function(counts2, pc){
  markovdata = markovModel(counts2, pc)
  clean_results <- list(
    TPM = markovdata[1][[1]]
  )
  clean_results
}
