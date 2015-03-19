from random import *
from sys import *
from string import *
intab=('QWRTPSDFGHJKLZXCVBNM')
outtab=list(intab)
shuffle(outtab)
code=maketrans(intab,join(outtab,''))
data = stdin.readlines()
for line in data:
    print line.rstrip().translate(code)
