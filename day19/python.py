import numpy as np

forwardUpCombinations = [(0,1),(0, 2),(1,0),(1,2),(2,0),(2,1)]
signCombinations = [(1,1),(-1,1),(-1,-1),(1,-1)]

def rotated(beacon,rot):
    (forward,up) = forwardUpCombinations[rot//4]
    (signF,signU) = signCombinations[rot%4]
    xaxis = np.array([signF if forward == 0 else 0,
                      signF if forward == 1 else 0,
                      signF if forward == 2 else 0])
    yaxis = np.array([signU if up == 0 else 0,
                      signU if up == 1 else 0,
                      signU if up == 2 else 0])
    zaxis = np.cross(xaxis,yaxis)
    return int(np.dot(xaxis, beacon)),int(np.dot(yaxis, beacon)),\
             int(np.dot(zaxis, beacon))

def all_orientations(scanner):
    for i in range(24):
        yield [rotated(x,i) for x in scanner]

def pt1(data):
    abs_scanner = data[0]
    scanners_to_match = data[1:]
    scanner_positions = [(0,0,0)]

    while scanners_to_match:
        for match_beacon in set(abs_scanner):
            for scanner in scanners_to_match:
                for rotated_scanner in all_orientations(scanner):
                    for rotated_beacon in rotated_scanner:
                        ox,oy,oz = [rotated_beacon[i] - match_beacon[i] for i in range(3)]
                        offset_rotated_scanner = set([(x-ox,y-oy,z-oz) for (x,y,z) in rotated_scanner])
                        intersection = offset_rotated_scanner.intersection(abs_scanner)

                        if len(intersection) >= 12:
                            abs_scanner.update(offset_rotated_scanner)
                            scanners_to_match.remove(scanner)
                            scanner_positions.append((-ox,-oy,-oz))
    print(len(abs_scanner))
    distances = []
    for (x,y,z) in scanner_positions:
        for (i,j,k) in scanner_positions:
            distances.append(abs(x-i)+abs(y-j)+abs(z-k))
    print(max(distances))

def parse(data):
    scanners = []
    acc = []
    for line in data.splitlines():
        if line == "":
            scanners.append(set(acc))
            acc = []
            continue
        if line[0:3] == "---":
            continue
        acc.append(tuple([int(x) for x in line.split(",")]))
    return scanners + [set(acc)]

if __name__ == "__main__":
    data = parse(open("day19/input.txt","r").read())
    pt1(data)
