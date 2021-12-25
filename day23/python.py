import sys
import itertools
import heapq

concat = lambda xs: list(itertools.chain.from_iterable(xs))

HALLWAY_XS = list(range(1,11+1))
HALLWAY_Y = 1
HALLWAY = [(x,HALLWAY_Y) for x in HALLWAY_XS]
HALLWAY_DEST = [(x,y) for (x,y) in HALLWAY if x not in [3,5,7,9]]
ROOM_XS = [3,5,7,9]
ROOM_YS = [2,3]#,4,5]
ROOMS = [[(x,y) for y in ROOM_YS] for x in ROOM_XS]
ALL_LOCATIONS = HALLWAY + concat(ROOMS)
ABCD = {"A":0,"B":1,"C":2,"D":3}

def flood_fill(p):
	explore_list = [(p,[])]
	visited = set([])
	distances = {p:float("inf") for p in ALL_LOCATIONS}
	paths = {p:[] for p in ALL_LOCATIONS}
	while explore_list:
		(x,y),path = explore_list.pop(0)
		dist = len(path)
		if dist < distances[(x,y)]:
			distances[(x,y)] = dist
			paths[(x,y)] = path + [(x,y)]
		if ((x,y),dist) in visited:
			continue
		ns = [p for p in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
			  if p in ALL_LOCATIONS]
		explore_list += [(n,path+[(x,y)]) for n in ns if (dist+1) < distances[n]]
		visited.add(((x,y),dist))

	return {k:v[1:] for k,v in paths.items()}

PATHS = {p:flood_fill(p) for p in ALL_LOCATIONS}

def main():
	parsed = parse(sys.argv[1])
	print("Part 1:")
	part1(parsed)

	global ROOM_YS, ROOMS, ALL_LOCATIONS, PATHS
	ROOM_YS = [2,3,4,5]
	ROOMS = [[(x,y) for y in ROOM_YS] for x in ROOM_XS]
	ALL_LOCATIONS = HALLWAY + concat(ROOMS)
	PATHS = {p:flood_fill(p) for p in ALL_LOCATIONS}

	parsed = parse(sys.argv[1], inject_part_2=True)
	print("Part 2:")
	part1(parsed)

def parse(f,inject_part_2=False):
	data = open(f,"r").read().splitlines()
	if inject_part_2:
		data.insert(3,"  #D#C#B#A#")
		data.insert(4,"  #D#B#A#C#")
	positions = [(data[y][x],(x,y)) for y in range(len(data)) for x in range(len(data[y]))
				 if data[y][x] not in " .#"]
	return positions

def part1(state):
	explore_list = []
	heapq.heappush(explore_list,(0,tuple(state)))
	best_cost = float("inf")
	visited = set()
	while explore_list:
		cost,state = heapq.heappop(explore_list)
		positions = [p[1] for p in state]

		if (state,cost) in visited:
			continue

		num_finished = 0
		for amph,pos in state:
			if is_finished(amph,pos,state,positions):
				num_finished += 1
				if num_finished == len(state):
					print("finished with", cost)
					if cost < best_cost:
						best_cost = cost
					explore_list = [(c,s) for (c,s) in explore_list
									if c < best_cost]
				continue
			for (move,dist) in get_possible_moves(amph,pos,state,positions):
				new_cost = cost + dist * [1,10,100,1000][ABCD[amph]]
				if new_cost > best_cost:
					continue
				new_state = list(state)
				new_state[state.index((amph,pos))] = (amph,move)
				heapq.heappush(explore_list,(new_cost,tuple(new_state)))

		visited.add((state,cost))

def is_finished(a,p,amphipods,positions):
	correct_room = ROOMS[ABCD[a]]

	if p not in correct_room:
		return False

	for room in reversed(correct_room):
		if room in positions:
			if (a,room) in amphipods:
				continue
			return False
		if room != p:
			return False
	
	return True

def get_possible_moves(a,p,amphipods,positions):
	correct_room = ROOMS[ABCD[a]]
	can_go_to_room = True

	# Check if we can reach the entrance to our room
	for cell in PATHS[p][(ROOM_XS[ABCD[a]],HALLWAY_Y)]:
		if cell in positions:
			can_go_to_room = False
			break

	# If it's occupied by someone else we can't go
	for amph,amph_pos in amphipods:
		if amph_pos in correct_room and amph != a:
			can_go_to_room = False
	
	# Otherwise it's occupied by our family, so we want to go
	# as deep as we can
	if can_go_to_room:
		for dest in reversed(correct_room):
			path = PATHS[p][dest]
			if not set(path).intersection(positions):
				return [(dest,len(path))]

	# If we can't go in our room, check if we're in the hallway or in a room
	if p[1] in ROOM_YS:
		# If we're jumping FROM a room, then we can only go to a hallway
		paths = []
		for dest in HALLWAY_DEST:
			path = PATHS[p][dest]
			if not set(path).intersection(positions):
				paths.append((dest,len(path)))
		return paths
	else:
		# If we're jumping from the hallway and we've already found out
		# we can't go in our own room, there's nothing we can do
		return []

main()
