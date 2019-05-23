program int_cuadratura
use cuad_int

implicit none
real*8::a, b
integer::i, np
real*8::alfa, beta
real*8, allocatable, dimension(:)::absi, weigh
real*8, allocatable, dimension(:)::func
real*8::suma

!print*, 'Ingrese el número de puntos: '
!read*, np

np = 256

a = -1.d0				!pedido al usuario
b = 1.d0

!print*, 'Ingrese el intevalo (a,b):'
!read*, a, b

alfa = 0.d0				!parámetros característicos
beta = 0.d0

allocate(absi(np), weigh(np))		!no se la dimensión, necesito alojar memoria

absi = 0.d0				!debo darle un valor a los abscisas
weigh = 0.d0

call gauleg(a,b,absi,weigh,np)		!creo la grilla

allocate(func(np))

do i=1,np

 func(i) = test(absi(i))

enddo

suma=0.d0
do i=1,np

 suma = suma + weigh(i)*func(i)

enddo

deallocate(absi)
deallocate(weigh)
deallocate(func)

print*, 'El valor de la integral es=', suma


contains

 double precision function test(x)
 implicit none
 real*8::x

    test=dexp(-dcos(x)*dcos(x))/(dsqrt(1.d0-x*x))
 
 end function


end program 

