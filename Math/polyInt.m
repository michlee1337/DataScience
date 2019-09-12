function a = polyInt(x,y)
  %% return coefficients of the interpolating polynomial

  fprintf('__________BUILDING A MATRIX_________\n')
  A = [];
  x = x
  %%for i = x
  %%  row = [];
  %%  for j = (0:length(x)-1);
  %%    row = [i**j row];
  %%  endfor
  %%  A = [A; row];
  %% endfor
  for i = (0:length(x)-1)
    row = x.^i;
    A = [row, A]
  endfor
  fprintf('__________CALCULATING_________\n')
  display(A)
  display(y)
  fprintf('__________ANSWER IS_________\n')
  a = A\y;
end
