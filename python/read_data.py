####################################
##
## read data
##
####################################
import numpy as np
from scipy.io import FortranFile
import glob
import os

def read_data(filename):

  f = open(filename)

  x = np.loadtxt('data/x.txt',dtype='float')

  qq = np.loadtxt(filename,dtype='float')

  return qq, x

