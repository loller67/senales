#leer archivo

dir = ''
dato.rr = read.csv(paste( dir , '10068.MIB.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)

#parte 2
tiempo_total = 71377.58399999907
constante_min_heartbeat = 0.25
delta_t = as.integer(tiempo_total/constante_min_heartbeat)
plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo(s)', xlim=c(0,72473),ylim=c(0,8000))
points(dato.rr$t,dato.rr$RR,pch=20)
Nmuestro = as.integer(2*max(dato.rr$t))
RRlineal = approx(dato.rr$t,dato.rr$RR,n=delta_t)
RRspline = spline(dato.rr$t,dato.rr$RR,n=delta_t)
lines (RRlineal$x, RRlineal$y, col='red')
points(RRlineal$x, RRlineal$y,pch=20,col='red')
lines (RRspline$x,RRspline$y, col='blue')
points(RRspline$x,RRspline$y,pch=20,col='green')

#muestras perdidas

dir = ''
dato.rr = read.csv(paste( dir , '10068Filtrado.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)


#parte 2
tiempo_total = 68927.72799999903
constante_min_heartbeat = 0.25
delta_t = as.integer(tiempo_total/constante_min_heartbeat)
plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo(s)', xlim=c(0,72473),ylim=c(0,3000))

points(dato.rr$t,dato.rr$RR,pch=20)
Nmuestro = as.integer(2*max(dato.rr$t))
RRlineal = approx(dato.rr$t,dato.rr$RR,n=delta_t)
RRspline = spline(dato.rr$t,dato.rr$RR,n=delta_t)

lines (RRlineal$x, RRlineal$y, col='red')
points(RRlineal$x, RRlineal$y,pch=20,col='red')
lines (RRspline$x,RRspline$y, col='blue')
points(RRspline$x,RRspline$y,pch=20,col='green')

#hacer un filtrado MA e ir viendo como se suaviza la curva para distinguir entre dia y noche

tiempo_total = 68927.72799999903
cte = 72473


RRlineal.x.MA25 = filter(RRlineal$y, rep(1/25,25), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA25)
lines(RRlineal$x, RRlineal.x.MA25, col = 'red')

RRlineal.x.MA50 = filter(RRlineal$y, rep(1/50,50), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA50)
lines(RRlineal$x, RRlineal.x.MA50, col = 'green')


RRlineal.x.MA100 = filter(RRlineal$y, rep(1/100,100), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA100)
lines(RRlineal$x, RRlineal.x.MA100, col = 'blue')


RRlineal.x.MA500 = filter(RRlineal$y, rep(1/500,500), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA500)
lines(RRlineal$x, RRlineal.x.MA500, col = 'yellow')

RRlineal.x.MA1000 = filter(RRlineal$y, rep(1/1000,1000), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA1000)
lines(RRlineal$x, RRlineal.x.MA1000, col = 'violet')

RRlineal.x.MA2000 = filter(RRlineal$y, rep(1/2000,1000), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA2000)
lines(RRlineal$x, RRlineal.x.MA2000, col = 'red')

RRlineal.x.MA5000 = filter(RRlineal$y, rep(1/5000,5000), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA5000)
lines(RRlineal$x, RRlineal.x.MA5000, col = 'green')

RRlineal.x.MA10000 = filter(RRlineal$y, rep(1/10000,10000), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA10000)
lines(RRlineal$x, RRlineal.x.MA10000, col = 'yellow')

RRlineal.x.MA60000 = filter(RRlineal$y, rep(1/60000,60000), circular=TRUE)
plot(RRlineal$x, RRlineal.x.MA60000)
lines(RRlineal$x, RRlineal.x.MA60000, col = 'green')


#descomposicion serie temporal

outMA10 <- filter(RRlineal$y,rep(1/10000,10000), circular =TRUE)
difMA10 <- RRlineal$y - outMA10
outMA60 <- filter(RRlineal$y,rep(1/60000,60000), circular =TRUE)
difMA60 <- outMA10 - outMA60
op <- par(mfrow = c(2, 2))
plot(RRlineal$x,outMA10,type='l',xlab='dia' , ylab='sal MA10000')
plot(RRlineal$x,difMA10,type='l', xlab='dia' , ylab='dif MA10000')
plot(RRlineal$x,outMA60,type='l',xlab='dia', ylab='sal MA60000')
plot(RRlineal$x,difMA60,type='l', xlab='dia' , ylab='dif MA60000')

op <- par(mfrow = c(1, 1))
plot(dato.rr $t,outMA10,type='l',ylim=c(0,2000),xlab='dia', ylab='sal−dif
     MA10000 y MA60000',col='black')
lines (RRlineal$x,difMA10,col='blue')
lines (RRlineal$x,outMA60,col='red')
lines (RRlineal$x,difMA60,col='green')


#esto es lo que me pidio risk, sobreponer la curva con lo que hicimos con filtrado MA
plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo(s)', xlim=c(0,72473),ylim=c(0,3000))
RRlineal.x.MA60000 = filter(RRlineal$y, rep(1/60000,60000), circular=TRUE)
lines(RRlineal$x, RRlineal.x.MA60000, col = 'green')

#aca aplicamos transformada de fft
fft.a = Mod(fft(RRlineal$y))
fft.difMA10 = Mod(fft(difMA10))
fft.difMA60 = Mod(fft(difMA60))
op <- par(mfrow = c(1, 1))
plot( fft.difMA60,col='green', type = 'l',ylim=c(0,2000),ylab='RR (ms)',xlab='frecuencia(hz)')

#ahora hay que analizar un poco el dominio de la frecuencia para cerrar el tp
frecx = 0:(length(RRlineal$x)-1)

fmuestreo = 1/(RRlineal$x[3]-RRlineal$x[2])
delta_f = fmuestreo/length((RRlineal$x))
frec = delta_f*frecx
plot(frec,fft.difMA60,col='red', type = 'l',ylab='RR (ms)',xlab='frecuencia(hz)',xlim=c(0.01,0.5),ylim=c(0,25000))
abline(v=0.04, lty=2)
abline(v=0.15, lty=2)
abline(v=0.4, lty=2)

