def find_cointegrated_pairs(data):
  import numpy as np
  import pandas as pd
  import statsmodels
  import statsmodels.api as sm
  from statsmodels.tsa.stattools import coint
  
  n = data.shape[1]
  score_matrix = np.zeros((n, n))
  pvalue_matrix = np.ones((n, n))
  keys = data.keys()
  pairs = []
  for i in range(n):
    for j in range(i+1, n):
      S1 = data[keys[i]]
      S2 = data[keys[j]]
      result = coint(S1, S2)
      score = result[0]
      pvalue = result[1]
      score_matrix[i, j] = score
      pvalue_matrix[i, j] = pvalue
      if pvalue < 0.05:
        pairs.append((keys[i], keys[j]))
  return score_matrix, pvalue_matrix, pairs
