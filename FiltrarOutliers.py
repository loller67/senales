#/usr/bin/env python

fh = open('10068.MIB')
lineas = fh.readlines() # lee todo el archivo
nlines = len(lineas)

fw = open('10068Filtrado.csv','w')
fw.writelines('time,RR,beat')
fw.write('\n')
tiempo = 0.
totalRR = 0
tiempo_total = 0
for j in range(5,nlines-1):
    line = lineas[j]
    w0 = line[0:1]
    w1 = line[1:-1]
    if(w0 == 'Q'):
        print(w0, w1, tiempo)
        tiempo = tiempo + float(w1)/1000.
        wline = str(tiempo) + ',' + w1 + ',' + w0 
        fw.write(wline)
        fw.write('\n')
        tiempo_total = tiempo + tiempo_total
print tiempo_total*1000.0
