program funcion
use operaciones_funciones
implicit none

integer, allocatable, dimension(:)::fun
integer::suma, maximo
real::media
integer::N, i

print*, 'Ingrese la dimensión del vector (N entero): '
read*, N

allocate (fun(1:N))
do i=1,N
  
  fun(i)=i

enddo

call operaciones(fun, N, suma, media, maximo)

print*, 'El valor de la sumatoria es: ', suma
print*, '  '
print*, 'La media es : ', media 
print*, '  '
print*, 'El valor maximo es: ', maximo


end program funcion
