module SVMInterface where

import SVM
import Data.Array.Unboxed

dataset = DataSet points values
  where points = array (1,100) [(1, [0.0, 0.1]), (2, []), (3, [])]
        values = array (1,100) [(1, 0.1), (2, 0.2), (3, 0.3)]


svm = LSSVM (KernelFunction reciprocalKernelFunction) 0.2 []

solution = solve svm dataset 0.2 300