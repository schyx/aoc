import hashlib
import sys

puzzle = b''
with open("data/2015/day4.txt") as f:
    puzzle = f.read().encode('utf-8')[:-1] # remove the newline at end

if sys.argv[1] == "1":
    prefix = 1
    while True:
        to_hash = puzzle + bytes(str(prefix), 'ascii')
        res = hashlib.md5(to_hash)
        result = res.hexdigest()
        if result[:5] == "00000":
            break
        prefix += 1
    print("Solution for part 1 is", prefix)

else:
    prefix = 1
    while True:
        to_hash = puzzle + bytes(str(prefix), 'ascii')
        res = hashlib.md5(to_hash)
        result = res.hexdigest()
        if result[:6] == "000000":
            break
        prefix += 1
    print("Solution for part 2 is", prefix)
