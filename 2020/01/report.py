with open('input', encoding="utf-8") as f:
    read_data = f.read()

def process(l):
    for n in range(len(l) -1):
        x = l[0]
        nl = l[1:]
        for y in nl:
            if x + y == 2020:
                return x * y
        l = nl


