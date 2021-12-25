def main():
	parsed = parse("day25/input.txt")
	pt1(parsed)

def parse(f):
	return open(f,"r").read().splitlines()

def draw(data):
	print("\n".join(data)+"\n")

def transpose(data):
	replace_strings = lambda x: x.replace(">","o").replace("v",">").replace("o","v")
	return ["".join(y) for y in list(zip(*[list(replace_strings(x)) for x in data]))]

def step_east(d):
	# Step all except for the right border
	new_d = [x.replace(">.",".>") for x in d]

	# Step right border
	for (y,row) in enumerate(d):
		if row[-1] == ">" and row[0] == ".":
			new_d[y] = ">" + new_d[y][1:-1] + "."

	return new_d

def pt1(data):
	prev_state = data

	i = 0
	while True:
		i += 1
		state = transpose(step_east(transpose(step_east(prev_state))))

		if state == prev_state:
			break

		prev_state = state
	
	print("Settled after", i)

main()
