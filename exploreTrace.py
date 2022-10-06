from collections import defaultdict
import cbor
from pprint import pprint
import sys
import math

"""Module with useful functions to explore the trace:
   * extractSection: filters the trace to those steps
     that visit certain section of the code, as given by
     the start and end lines. It saves the filtered trace to a
     new file.
   * filterAdvice: Find all memory access that goes below
     certain address. By default returns all accesses to static data. 
"""

def loadFile(fileName):
    file = open(fileName, 'rb')
    allCBOR = cbor.load(file)
    file.close()
    version = allCBOR[0]
    features = allCBOR[1]
    print("File loaded: ", fileName)
    print ("version:", version, ". Features: ", features )
    compUnit =  allCBOR[2]
    return (version, features, compUnit)

def getTrace(fileName):
    (_,_,compUnit) = loadFile(fileName)
    trace = compUnit["trace"]
    return trace

def getTraceFunction(fileName : str,fnStart : int,fnEnd : int):
    """ Finds the parts of the trace
    that visit a given function, as determined by
    the start and end lines of the function.
    """
    def chunkInFunc(chunk) -> bool:
        fst_pc = chunk[1][0]['pc']
        return fst_pc > fnStart and fst_pc <= fnEnd
        
    trace = getTrace(fileName)
    print("Regular trace is", len(trace), "long")
    print("It uses ",str(len(trace[0][1][0]['regs'])),"registers")
    functionTrace = list(filter(chunkInFunc,trace))
    print("Fn trace is", len(functionTrace), "long")
    
    return functionTrace

def prettyPrintTrace(trace,printFn=print):
    for chunk in trace:
        prettyPrintChunk(chunk,printFn)
def prettyPrintChunk(chunk,printFn=print):
    last_pc = chunk[1][0]['pc']-1 # don't care if the first step is a jump
    segment = chunk[0]
    chunk_steps = chunk[1]
    printFn("\n=== Segment "+str(segment)+"====================")
    for step in chunk_steps:
        #If the pc jumps, mark the jump for easy parsing
        pc = step['pc']
        if not pc == last_pc + 1:
            printFn("\n   ---   jmp")
        last_pc = pc
        prettyPrintStep(step,printFn)

def prettyPrintStep(step,printFn) -> str:
    pc = step['pc']
    regs = step['regs']
    string_regs = ""
    for reg in regs:
        string_regs = string_regs + str(reg)+","
        #Add tabulation as necessary
        if reg == 0:
            length_value = 1
        elif reg<0:
            length_value = math.floor(math.log10(-reg))+1
        else:
            length_value = math.floor(math.log10(reg))+1
        if length_value < 16:
            string_regs = string_regs + "\t"
        if length_value < 8:
            string_regs = string_regs + "\t"
    string = "\npc: "+str(pc)+" -> [" + string_regs + "\t]"
    printFn (string)
    return string
    
def traceToFile(fileout,trace):
    with open(fileout,'w+') as file:
        file.truncate(0)
    with open(fileout,'a') as file:
        prettyPrintTrace(trace,file.write)
    print("Written to file ", fileout)

def extractSection(file, fileout, fnStart, fnEnd):
    """ reads trace from a '.cbor' file
    Then filters it to get only the steps
    from a given function (as determined by the
    start and end lines of the function).
    Then, finally, pretty prints that section of the trace
    to a file
    """
    fnTrace = list(getTraceFunction(file, fnStart, fnEnd))
    print("function trace is", len(fnTrace), " long.")
    traceToFile(fileout,fnTrace)
# Useage example
#extractSection("ossl11.cbor","choose11_full.trace", 17582, 18028)
#extractSection("ossl20.cbor","choose20_full.trace", 16650, 16943)

def getAdvice(fileName):
    (_,_,compUnit) = loadFile(fileName)
    advice = compUnit["advice"]
    return advice

def allAccessToAddress(advices, address):
    for (step,step_adv) in advices.items():
        for adv in step_adv:
            if address in adv:
                print("step",step," -> ", adv)

def allStaticDataAccess(advices, address=2**31):
    static_data = []
    for (step,step_adv) in advices.items():
        for adv in step_adv:
            if len(adv) >= 2 and adv[1]<=address:
                step_str = "\nstep" + "step" + " -> " + str(adv)
                static_data = static_data + [step_str]
    return static_data
                
def filterAdvice(fileName, file_out, address=2**31):
    """ Find all memory access that goes below certain address.
        By default returns all accesses to static data. 
    """
    advices = getAdvice(fileName)
    static_data = allStaticDataAccess(advices,address)
    print("Found", len(advices), " advices")
    with open(file_out,"w+") as file:
        file.truncate(0)
    with open(file_out,"a") as file:
        file.writelines(static_data)
    
#Usage example
# filterAdvice("ossl20.cbor","ossl20_nostep.data")
# filterAdvice("ossl11.cbor","ossl11_nostep.data")
