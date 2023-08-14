import csv
import os

IPD_location = "/home/matthew/Documents/PCNMA/Data/IPD"

os.chdir(IPD_location)

testfile = "IPD_Colucci_PFS_GEM.csv"
testfile2 = "IPD_Colucci_PFS_GEM-CIS.csv"

print(testfile.split("_")[3].strip(".csv"))