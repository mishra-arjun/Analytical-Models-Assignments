from networkx import *
import sys
import operator

# damping factor
d = 0.85

# how quickly are we converging to true pagerank and when do we stop?
delta = 10
tolerance = 0.00001

# starting pagerank
default_pr = 10

pr = None
c = None
node = None
incoming_node = None

# read the link data from file into a directed graph data structure
G=read_adjlist(sys.argv[1], create_using=networkx.DiGraph())

#initialize the graph nodes with default pageranks and weights
for nodename in G:
	node = G.node[nodename]
	node['pagerank'] = default_pr
	node['links'] = G.out_degree(nodename)

# run pagerank scoring until convergence slows below tolerance
while (delta > tolerance):
	delta = 0
	deltacount = 0

	# calculate pagerank for each page
	for nodename in G:
		node = G.node[nodename]
		incoming_links = G.predecessors(nodename)
		pr = 0  

		# sum up the weighted votes from all incoming page links
		for link in incoming_links:
			incoming_node = G.node[link]
			c = incoming_node['links']
			pr = pr + (incoming_node['pagerank'] / c)

		# perform the pagerank calculation for this page
		pr = (1 - d) + (d * (pr))
		node['pagerank'] = pr

		# track convergence rate to true pagerank
		delta = delta + abs(pr - node['pagerank'])
	
	delta = delta / G.number_of_nodes()
		
# print the pageranks in descending order
pageranks = list()

for nodename in G:
	node = G.node[nodename]
	pageranks.append((nodename, node['pagerank']))

pageranks.sort(key=lambda tup: tup[1],  reverse=True)

for page in pageranks:
    print page[0], page[1]