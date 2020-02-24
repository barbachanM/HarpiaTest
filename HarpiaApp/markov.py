import numpy as np
import pandas as pd

def markovModel(counts2, pc):
	counts2 = pd.DataFrame(counts2)
	index = counts2.index

	index_list = index.tolist()
	newRowName = []
	for element in index_list:
		newRowName.append(element.replace("\t", ","))

	counts2.index = newRowName

	counts2 = counts2.mean(axis=1)
	counts2.index = counts2.index.str.split(',', expand=True)


	callSum = counts2.groupby(axis=0, level=0).sum()

	pc = 1.0/len(callSum)**4

	transitionMatrix = []
	for call1 in callSum.index:
		row = []
		for call2 in callSum.index:
			pAB = (counts2[(call1,call2)]+float(pc))/(callSum[call1]+float(pc)*len(newRowName))
			row.append(pAB)
		transitionMatrix.append(row)

	tpm = pd.DataFrame(transitionMatrix, index = callSum.index, columns = callSum.index)

	return[tpm]



