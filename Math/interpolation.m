fprintf('Global Interpolation \n');
%% uncomment to take vectors from user input
%% v1=input("x vector: ","s")
%% v2=input("y vector: ","s")
%% x=cellfun("str2num",strsplit(v1," "))
%% y=cellfun("str2num",strsplit(v2," "))
%% x = x'
%% y = y'

x = [0;1;4;5]
y = [-1;3;1;-3]
disp(ployInt(x,y));

fprintf('Piecewise Cubic Interpolation \n');
x1 = [0;1;4]
y1 = [-1;3;1]
%%disp(cubicSpline(x1,y1));
