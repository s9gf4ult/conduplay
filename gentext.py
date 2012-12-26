import random
import sys

data = 'abcdefghijklmnopqrstuvwxyz'
count = int(sys.argv[1])
pack = count / 64000
rest = count % 64000

def dopack(size):
    s = ""
    for x in xrange(size):
        s += random.choice(data)
    sys.stdout.write(s)

for i in xrange(pack):
    dopack(64000)

dopack(rest)

sys.stdout.flush()
