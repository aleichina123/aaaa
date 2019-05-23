!El programa nos muestra el valor al que converge sum(1/(4*n*n)-1)
program converge

 implicit none
 integer::i
 real::prec
 real::suma

suma=0.
i=1

print*, 'Ingrese la precision: '
read*, prec

do while(serie(i).gt.prec)
  
   suma=suma+serie(i)

i=i+1

enddo

print*, 'El número al que converge es: ', suma

contains

  real function serie(x)
  implicit none
  integer::x
  
    serie=1./((4.*x*x)-1.)
   
  end function

end program
