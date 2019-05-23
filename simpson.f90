!Este programa resuelve integrales con el método de Simpson
program metodo_simpson

 implicit none
 integer::N, i
 real*8::a, b, x
 real*8::dx, Iex
 real*8, allocatable, dimension(:) ::ftab		!uso el allocatable


print*, 'Escriba el intervalo en el que desea integrar: '
read*, a, b

print*, 'Escriba un valor entero N: '
read*, N

if (mod(N,2) .ne. 0 ) stop 'Error, N debe ser PAR'	!mod=saca el residuo

 dx=(b-a)/N

allocate(ftab(0:N))					!llega a esta línea y la memoria se almacena por el allocatable

do i=0,N	 					!Se hace un vector de con cierta memoria
  
  x = a+i*dx
  ftab(i) = dsin(x)

  !fexac(i) = dcos(x)

enddo

Iex=-dcos(b)+dcos(a)

print*, 'El valor de la integral con el método de Simpson es: ', dsimp(ftab, N, dx)
print*, 'El valor de la integral exacta es: ', Iex
print*, 'El error es: ', abs(Iex-dsimp(ftab, N, dx))

deallocate(ftab)					!debemos desalojar la memoria


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains
 
!método de Simpson
 double precision function dsimp(f,N,dx)		!real*8 function dsimp()
  implicit none
  integer::N, i
  real*8::dx
  real*8::sumapar, sumaimpar 

  real*8, dimension(0:N)::f
 
   sumapar=0.d0
   do i = 2, N-2, 2					!donde empiezo, donde termino, salto

     sumapar = sumapar + f(i)

   enddo

   sumaimpar=0.d0
   do i = 1, N-1, 2					!donde empiezo, donde termino, salto

     sumaimpar = sumaimpar + f(i)

   enddo

   dsimp=(dx/3.d0)*(f(0)+f(N) + 2.d0*sumapar + 4.d0*sumaimpar)	!la suma par en realidad es la impar

 end function


end program
