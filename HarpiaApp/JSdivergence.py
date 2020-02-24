
import numpy as np
import pandas as pd

def JS_div(tpm1,tpm2):
    # calculate the kl divergence
    def kl_divergence(p, q):
        return sum(p[i] * np.log2(p[i] / q[i]) for i in range(len(p)))


    # calculate the js divergence
    def js_divergence(p, q):
        m = 0.5 * (p + q)
        return 0.5 * kl_divergence(p, m) + 0.5 * kl_divergence(q, m)


    JS = []
    for row in list(tpm1.index):
        JS.append(js_divergence(tpm1.loc[row,:], tpm2.loc[row,:]))

    jsDict = dict(zip(list(tpm1.index), JS))

    jsDF = pd.DataFrame.from_dict(jsDict, orient='index', columns=['JS'])

    return jsDF

