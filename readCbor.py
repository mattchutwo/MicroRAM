from collections import defaultdict
import cbor
from pprint import pprint
import sys

if len(sys.argv) <= 1:
    file = 'Output/out_test.cbor'
else:
    file, = sys.argv[1:]

allCBOR = cbor.load(open(file, 'rb'))
version = allCBOR[0]
features = allCBOR[1]
print ("version:", version, ". Features: ", features )
compUnit =  allCBOR[2]

params = compUnit["params"]
program = compUnit["program"]
segs = compUnit["segments"]
advice = compUnit["advice"]
advice_list = "\n".join(map(str,list(advice.items())))
trace = cbor.load(open(file, 'rb'))[2]["trace"]
segs_used = [seg for seg, states in trace]

dupSegs = [dup for dup in set(segs_used) if segs_used.count(dup)>1]
print(len(segs_used), 'used segments. ', len(set(segs_used)), 'distinct used segments')
print("Duplicated segments: " , dupSegs)

def seg_is_public(seg):
    for con in seg[0]:
        if con[0] == 'pc':
            return True
    return False

def public_segs():
    return len([seg for seg in segs if seg_is_public(seg)])

public_segs_cons = public_segs()

print(len(segs), 'segments. ', public_segs_cons, " of them public")



##print('segment', segs_used)
print(len(trace), 'chunks in trace')

## Compute number of private segments USED.
maxused = max(segs_used)
missing = [i for i in range(maxused) if not i in segs_used]
pub_unused = len(missing)
pub_used = public_segs_cons - pub_unused
pub_cycles = sum(len(states) for i, states in trace if seg_is_public(segs[i]))
print("## Public segments")
print("Produced: \t", public_segs_cons)
print("Used: \t\t", pub_used)
print("Unused: \t", pub_unused)
print("Used cycles: \t", pub_cycles)

priv_count = len(segs) - public_segs_cons
print("## Private segments")
print("Produced: \t", priv_count)
print("Used: \t\t", len(segs_used) - pub_used)
print("Unused: \t", priv_count - (len(segs_used) - pub_used))

print("Double check private used:" , maxused + 1 - public_segs_cons)
print("Max Used:" , maxused) # + 1 to count 0


# print (allCBOR)
# print ("Advice: ",Hopefully advice)


### pretty printers

## Segments
def print_segs(end = 11, start = 0):
    for (i,s) in enumerate(segs[:end]):
        if i >= start:
            print (i,s)
## Trace
def chunk_printer(chunk,startCycle):
    cycle = startCycle
    segment_index_str = str(chunk[0])
    chunk_trace = chunk[1]
    chunk_trace_str = map(str,chunk[1])
    print("Segment Index: " + (segment_index_str) + "\nStates: " )
    if len(chunk_trace)>0:
        print("\t",str(cycle), ". Pc:", chunk_trace[0]['pc'])
    if len(chunk_trace)>1:
        print("\t\t...")
    cycle = cycle + len(chunk_trace)
    if len(chunk_trace)>3:
        print("\t",str(cycle), ". Pc:", chunk_trace[-2]['pc'])
    if len(chunk_trace)>2:
        print("\t",str(cycle), ". Pc:", chunk_trace[-1]['pc'])
    return cycle

trace_length = 0
for chnk in trace:
    trace_length = trace_length +len(chnk[1])                    

print("Trace length : ", trace_length)
    
def trace_printer(trace = trace, bound = -1):
    cycle = 0
    for chnk in trace:
        cycle = chunk_printer(chnk,cycle)
        if cycle > bound and bound > 0:
            return
    
def simpl_trace():
    for chnk in trace:
        print ("Segment ", chnk[0], " :     // Has", len(chnk[1]))
        for l in chnk[1]:
            print (l)
        print("")

def countNetwork():
    countTo = 0
    countFrom = 0
    for i in range(public_segs_cons):
        countFrom = countFrom + segs[i][3]
        countTo = countTo + segs[i][4]
    print ("FromNetwork: ", countFrom)
    print ("ToNetwork: ", countTo)

countNetwork()

#trace_printer()


def memFoldingStats():
    print('## Memory folding')
    seg_preds = defaultdict(set)
    public_segs = set()
    for i, seg in enumerate(segs):
        succs = seg[2]
        for j in succs:
            seg_preds[j].add(i)
        for con in seg[0]:
            if con[0] == 'pc':
                public_segs.add(i)

    cyc = 0
    saw_first_join = False
    saw_first_secret = False
    saw_first_network = False
    prev_i = None
    for chunk in trace:
        i = chunk[0]
        if not saw_first_join and len(seg_preds[i]) > 1:
            print('First join: \t\tcycle %d' % cyc)
            saw_first_join = True
        if not saw_first_secret and i not in public_segs:
            print('First secret segment: \tcycle %d' % cyc)
            saw_first_secret = True
        if not saw_first_network and i != 0 and prev_i not in seg_preds[i]:
            print('First use of network: \tcycle %d' % cyc)
            saw_first_network = True
        cyc += segs[i][1]
        prev_i = i

memFoldingStats()

