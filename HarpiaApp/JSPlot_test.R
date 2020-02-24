library(reticulate)
library(ggplot2)

source("getAlphabets.R")
source("getDataStructure.R")
source("MarkovModel.R")
source("JS.R")



htPath = "/Users/Mariel/Documents/PhD/GinaASD/HT/TwoLetter"
wtPath = "/Users/Mariel/Documents/PhD/GinaASD/WT/TwoLetter"
alphabetPath = "/Users/Mariel/Documents/PhD/GinaASD/alphabetFile.txt"
alphabet = getAlphabetsR(alphabetPath)

dataWT = mainFunctionPy_upto4(wtPath, unlist(alphabet$H1), unlist(alphabet$H2), unlist(alphabet$H3), unlist(alphabet$H4), 1/length(unlist(alphabet$H4)))

dataHT = mainFunctionPy_upto4(htPath, unlist(alphabet$H1), unlist(alphabet$H2), unlist(alphabet$H3), unlist(alphabet$H4), 1/length(unlist(alphabet$H4)))

tpm1 = markovmodelR(dataWT[8][[1]],1/length(unlist(alphabet$H4)))

tpm2 = markovmodelR(dataHT[8][[1]],1/length(unlist(alphabet$H4)))

JS_df = JS_R(data.frame(tpm1$TPM), data.frame(tpm2$TPM))

JS = data.frame(JS_df$JS_df)
JS$call = row.names(data.frame(tpm1$TPM))
colnames(JS) = c("JS_Divergence", "call")

ggplot(JS, aes(x = reorder(call, -JS_Divergence), y = JS_Divergence, fill=JS_Divergence)) + geom_bar(stat = "identity")+  scale_fill_distiller(palette = "Spectral")+xlab("Calls")+ylab("Jensen-Shannon Divergence")

