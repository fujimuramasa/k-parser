handle = open('CytList.txt')
output = open('m.txt','w')
cal = 0
for line in handle.readlines():
    cal += 1
    #print line
    if cal > 3:
        temp  = line.split('\t')[-1]
        temp2 = str(cal).zfill(4)+'@@'+temp
        output.write(temp2)

handle.close()
output.close()
    
