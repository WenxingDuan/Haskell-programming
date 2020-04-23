import CL9a
  


-- add your machines here using the following template

machine :: FSM Int
machine = mkFSM qs as ts ss fs where
  qs = undefined
  as = undefined
  ts = undefined
  ss = undefined
  fs = undefined
  
regex1 = (Star (S "ba"):>: (S "bb":|:S "a")):>:Star ((S "b":|:S "a"):>:(S "b":|:S"abb":|:S "aa"))
