
import sys
import pandas
import networkx
import numpy
import itertools
sys.path.append("/inst/python/sesmotifanalyser/sma/")
import sma

def get_random_adj_mat(n_non_social_nodes, n_social_nodes, density):
  py_g = next(sma.randomSENs(ecoNodes = int(n_non_social_nodes),
                        socNodes = int(n_social_nodes),
                        density = density,
                        socName = "social",
                        ecoName = "non-social"))
  networkx.convert_matrix.to_pandas_edgelist(py_g)
  adj_mat = networkx.convert_matrix.to_pandas_adjacency(py_g)
  return(adj_mat)
