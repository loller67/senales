# Ejercicio 1 - Aumentar la frecuencia de muestreo
# Nota: Si no sos fede, tenes que reemplazar el path en dir con el directorio donde tenes descargado tu DosLocales.csv
dir = ''
dos.locales = read.csv(paste( dir , '10068.MIB.csv',sep=""))
print (names(dos.locales))
# [1]  "time" "RR" "beat" 

# aumentar la frecuencia de muestreo

N = length(dos.locales$RR)
print(paste('N=', N))

localAfal = approx(dos.locales $t, dos.locales $RR, n=3*N)
localAfas = spline(dos.locales $t, dos.locales $RR, n=3*N)

plot(dos.locales $t, dos.locales $RR, type = 'l', main='Ventas Locales A', xlab='dias', ylab='ventas k$', xlim=c(45,55), ylim=c(8,17))
points(dos.locales $t,dos.locales $RR,pch=20,col='black')

for(j in 1:N)
{
  abline (v=j, lty =2)
}

lines ( localAfal $x, localAfal $y, col='red')
points( localAfal $x, localAfal $y,pch=20,col='red')
lines ( localAfas $x, localAfas $y, col='blue')
points( localAfas $x, localAfas $y,pch=20,col='green')

#spline genera los picos (verde)
#approx es el rojo, abajo. ambos generan datos que no estaban en el dataset original

#Ejercicio 2: Aumentar la frecuencia de muestreo

fft.localA = Mod(fft(dos.locales $localA))
fft.localAfal = Mod(fft(localAfal$y))
fft.localAfas = Mod(fft(localAfas$y))
tiempo = 0:(N-1)
tiempo3 = 0:(3*N-1)
FrecuenciaMuestreo = 1
DeltaFrecMuestreo = FrecuenciaMuestreo/N
Frecuencia = DeltaFrecMuestreo*tiempo
FrecuenciaMuestreo = 3
DeltaFrecMuestreo = FrecuenciaMuestreo/(3*N)
FrecuenciaInterpolada = DeltaFrecMuestreo*tiempo3
plot(Frecuencia, fft.localA, type='l', xlim=c(0,0.1))
lines ( FrecuenciaInterpolada , fft.localAfal /3, col='red')
lines ( FrecuenciaInterpolada , fft.localAfas /3, col='green')

# Ejercicio 3: ahora vamos a disminuir la frecuencia de muestreo

N = length(dos.locales $localA)
print(paste('N =',N))
localAfal = approx(dos.locales $t,dos.locales $localA,n=N/3)
localAfas = spline(dos.locales $t,dos.locales $localA,n=N/3)
plot(dos.locales $t,dos.locales $localA, type = 'l' ,main='Ventas
     Locales A',xlab='dias' , ylab='ventas
     k$', xlim=c(45,55),ylim=c(8,17))
points(dos.locales $t,dos.locales $localA,pch=20,col='black')
for(j in 1:N)
{
  abline (v=j, lty =2)
}
lines ( localAfal $x, localAfal $y, col='red')
points( localAfal $x, localAfal $y,pch=20,col='red')
lines ( localAfas $x, localAfas $y, col='blue')
points( localAfas $x, localAfas $y,pch=20,col='green')

# Ejercicio 4, same as be4

fft.localA = Mod(fft(dos.locales $localA))
fft.localAfal = Mod(fft(localAfal$y))
fft.localAfas = Mod(fft(localAfas$y))
tiempo = 0:(N-1)
tiempo3 = 0:(N/3)
FrecuenciaMuestreo = 1
DeltaFrecMuestreo = FrecuenciaMuestreo/N
Frecuencia = DeltaFrecMuestreo*tiempo
FrecuenciaMuestreo = 1/3
DeltaFrecMuestreo = FrecuenciaMuestreo/(N/3)
FrecuenciaInterpolada = DeltaFrecMuestreo*tiempo3
op <- par(mfrow = c(1, 1))
plot(Frecuencia, fft.localA, type='l' , xlim=c(0,0.1))
lines ( FrecuenciaInterpolada ,3*fft.localAfal , col='red')
lines ( FrecuenciaInterpolada ,3*fft.localAfas , col='green')

# Electrocardio -> filtro pasabajo, 

N = length(dos.locales $localA)
print(paste('N =',N))
localAma = filter (dos.locales $localA,rep(1/7,7), circular = TRUE)
localAfal = approx(dos.locales $t,localAma,n=N/3)
localAfas = spline(dos.locales $t,localAma,n=N/3)

plot(dos.locales $t,dos.locales $localA, type = 'l' ,main='Ventas
     Locales A',xlab='dias' , ylab='ventas
     k$', xlim=c(45,55),ylim=c(8,17), lty =3)
points(dos.locales $t,dos.locales $localA,pch=20,col='black')
lines (dos.locales $t,localAma,type = 'l' ,main='Ventas Locales
       A',xlab='dias' , ylab='ventas k$', xlim=c(45,55),ylim=c(8,17))
points(dos.locales $t,localAma,pch=20,col='black')
for(j in 1:N)
{
  abline (v=j, lty =2)
}
lines ( localAfal $x, localAfal $y, col='red')
points( localAfal $x, localAfal $y,pch=20,col='red')
lines ( localAfas $x, localAfas $y, col='blue')
points( localAfas $x, localAfas $y,pch=20,col='green')


#ejericio 5

fft.localA = Mod(fft(dos.locales $localA))
fft.localAfal = Mod(fft(localAfal$y))
fft.localAfas = Mod(fft(localAfas$y))
tiempo = 0:(N-1)
tiempo3 = 0:(N/3)
FrecuenciaMuestreo = 1
DeltaFrecMuestreo = FrecuenciaMuestreo/N
Frecuencia = DeltaFrecMuestreo*tiempo
FrecuenciaMuestreo = 1/3
DeltaFrecMuestreo = FrecuenciaMuestreo/(N/3)
FrecuenciaInterpolada = DeltaFrecMuestreo*tiempo3
op <- par(mfrow = c(1, 1))
plot(Frecuencia, fft.localA, type='l' , xlim=c(0,0.1))
lines ( FrecuenciaInterpolada ,3* fft.localAfal , col='red')
lines ( FrecuenciaInterpolada ,3* fft.localAfas , col='green')


#ejercicio 6 levantar frecuencia cardiaca de un archivo y volverlo uniforme

dato.rr = read.csv(paste( dir , 'RR.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)




