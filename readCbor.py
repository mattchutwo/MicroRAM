import cbor
from pprint import pprint

file = 'Output/grit_naive.cbor'

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

def public_segs():
    return len([seg for seg in segs if len(seg[0])>0])

public_segs_cons = public_segs()

print(len(segs), 'segments. ', public_segs_cons, " of them public")



##print('segment', segs_used)
print(len(trace), 'chunks in trace')

## Compute number of private segments USED.
maxused = max(segs_used)
missing = [i for i in range(maxused) if not i in segs_used]
pub_unused = len(missing)
pub_used = public_segs_cons - pub_unused
print("## Public segments")
print("Produced: \t", public_segs_cons)
print("Used: \t\t", pub_used)
print("Unused: \t\t", pub_unused)

priv_count = len(segs) - public_segs_cons
print("## Private segments")
print("Produced: \t", priv_count)
print("Used: \t\t", len(segs_used) - pub_used)
print("Unused: \t\t", priv_count - (len(segs_used) - pub_used))

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
