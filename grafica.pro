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
;stop						;para hacer pruebas

device, decomposed=0
loadct,5

plot, date, temp, back=255, col=0, $	
 thick=2, title='MARZO', xtitle='Year', $
 ytitle='T(!uo!nC)', chars=1.5, $
 xs=1, ys=1, yr=[1,11]				;con esto se ajusta a los datos, no es cualquier rango

oplot, date, media, col=115, thick=1.5, $
 psym=2

end
