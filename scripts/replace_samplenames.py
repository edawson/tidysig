import sys

if __name__ == "__main__":
    lcounter = 0
    with open(sys.argv[1], "r") as ifi:
        for line in ifi:
            line = line.strip()
            if not line.startswith("Sample"):
                tokens = line.split("\t")
                tokens[0] = "Sample-" + str(lcounter)
                lcounter += 1
                line = "\t".join(tokens)
            print(line)
