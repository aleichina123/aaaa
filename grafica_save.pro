close, /all

np=125						;dimension

date = dblarr(np)				;time es un arreglo en doble precision que tiene np puntos
temp = dblarr(np)
media = dblarr(np)

openr,1,'graf.dat'				;para leer, le asigno el numero y pongo el nombre del archivo

for i=0, np-1 do begin

 readf, 1, a, b					;read file........ writef
 date(i) = a
 temp(i) = b
 media(i) =5.40817833
 ;print, 'Leo la línea', i+1

endfor
close,1

set_plot, 'ps'					;el ambiente que se va a crear va a ser en ps
loadct, 5
device, filename='figura1.ps', /color		;tambien se puede dar caracteristicas del tamaño y resolucion del ps, incluso tipo de letra
loadct,5					;cargamos de nuevo la tabla de colores

plot, date, temp, back=255, col=0, $	
 thick=2, title='MARZO', xtitle='Year', $
 ytitle='T(!uo!nC)', chars=1.5, $
 xs=1, ys=1, yr=[1,11]				;con esto se ajusta a los datos, no es cualquier rango

oplot, date, media, col=115, thick=1, $
 psym=2

device, /close					;cierro el device
set_plot, 'x'					;para cerrar el ambiente (cerramos y cambiamos a x)

end
