import glob,re,sys


varre = re.compile("(.*)VAR_([LBW])")

sizedict = {"UBYTE":1,"UWORD":2,"ULONG":4,"STRUCT":16}
size2dict = {"B":1,"W":2,"L":4}

varsize_dict = {}
# open the .i file to get the declared types (AKA sizes)
# check odd word/longs!! (assuming all structs are even)

align = 0
failed = False

with open("../include/jst_rel.i") as f:
    for line in f:
        toks = line.split()
        if len(toks)>1:
            sz = sizedict.get(toks[0])
            if sz:
                if sz!=1 and align:
                    print("Error: odd-aligned variable {}".format(toks[1]))
                    failed = True
                align = (align + sz) % 2
                if toks[1].startswith("RelVar_"):
                    varsize_dict[toks[1][len("RelVar_"):]] = sz

# now scan the .asm files
for asm_file in glob.glob("src/*.asm"):
    with open(asm_file) as f:
        for i,line in enumerate(f,1):
            toks = line.split()
            if len(toks)>1:
                if toks[0].startswith(";"):
                    continue
                m = varre.match(toks[0])
                if m:
                    used_size = size2dict[m.group(2)]
                    for x in toks[1].split(","):
                        if x in varsize_dict:
                            if varsize_dict[x] != used_size:
                                print("Error: {}:{}: {} declared size {}, used size {}".format(asm_file,i,x,varsize_dict[x],used_size))
                                failed = True

sys.exit(int(failed))

