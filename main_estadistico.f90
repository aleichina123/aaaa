program temperatura
use est_operaciones
implicit none

real, dimension(1:125)::temp
integer, dimension(1:125)::date
real::media, var, sigma, absd
character (len=80)::work
integer::i
real::a


open(unit=15, file='temperatura_media.csv', status='unknown')
open(unit=16, file='graf.dat', status='unknown')


do i=1,5
  read(15,*) work
enddo								!lee hasta aqui (5ta línea) 
							
do i=1, 125 							!este 1 continúa en la línea 6
  read(15,*) date(i), temp(i), a
enddo

temp=(temp-32.)*(5./9.)						!la temperatura esta en °C
date=(date-3)/100

do i=1, 125
write(16,*) date(i), temp(i)
enddo

close(15)
close(16)
					
call estadistica(temp, 125, media, var, sigma, absd)

print*, 'La media es : ', media 
print*,  ' '
print*, 'La varianza es: ', var
print*, ' '
print*, 'La desviación estándar es: ', sigma
print*, ' '
print*, 'La varianza absoluta: ', absd


end program temperatura
