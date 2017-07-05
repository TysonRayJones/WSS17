
coins = ([0]*49) + [0] + ([0]*50)

def flip(coins, start, end):
    for i in range(start, end):
        coins[i] = 0 if (coins[i] == 1) else 1

def process(coins, start=0, end=-1):

    if end == -1:
        end = len(coins)

    if end - start > 0:
        #flip(coins, (start + end)//2, end)
        flip(coins, start, end//2)
        process(coins, start, end//2)

def count(coins, bit):
    num = 0
    for e in coins:
        if e == bit:
            num += 1
    return num
    
process(coins)
