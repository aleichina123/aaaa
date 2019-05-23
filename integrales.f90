!Este programa resuelve integrales con el método de Simpson y del trapecio
program integral
use integrales
implicit none


integer::i
real*8 :: a,b,dx,xi
integer:: N
real*8::integralreal,r,r1
real*8 :: e1, e2
real*8,allocatable,dimension(:)::g !!crea vector sin dimension

print*,'Integración por método de Simpson y del Trapecio'
print*,'Ingrese los límites de integración [a,b]'
read*, a, b
print*,'Ingrese el número de iteraciones N (entero)'
read*,N


!!!Inicializaciones
N=2*N
dx=(b-a)/N
!!

allocate(g(0:N))!!dimensiona el vector


do i=0,N
  xi=a+i*dx
  g(i)=dsin(xi)
enddo

r=dsimp(g,N,dx)
r1=dtrap(g,N,dx)
integralreal=dcos(a)-dcos(b)
e1=dabs(r-integralreal)
e2=dabs(r1-integralreal)

print*,'la integral exacta es: ',integralreal
print*,'   '

print*,'La integral calculada con el método de Simpson es: ',r, 'con un error de: ', e1
print*,'   '

print*,'La integral calculada con el método del Trapecio es: ',r1, 'con un error', e2
print*,'   '

print*,'El error relativo porcentual entre los dos métodos es: ', abs((r-r1)/r1)*100,'%'

deallocate(g)
end program


