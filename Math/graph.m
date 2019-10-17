y = []
for x = -3*10^-15:3*10^-15:eps
  y = [y,((e^x-1)/x)]
endfor

x = linspace(-3*10^-15,3*10^-15)
y = (e.^x-1)./x
plot(x,y)
pause
