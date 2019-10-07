import numpy as np
import * from sympy
import math

def gradDescent(f, vars, initial):
    '''
    f = objective function
    vars = dictionary of independent variables
    start = starting point
    '''

    # get direction vector

    direction = []
    for v in vars.keys:
        dfdv = f.diff(v)
        dfdv_val = dfdv
        for val in initial:
            # get numerical value
            dfdv_val = dfdv_val.subs(v,val)
        direction.append(dfdv_val)

    # find optimum step size
    ## using bisection (binary search)
    alpha = [1] * 
    if math.abs(f(start + direction.*alpha) - 0) < 0.5:
        return(alpha)
    elif:
        (f(start + df*alpha) - 0) < 0.5:
        alpha -= 0.5
    else:
        alpha += 0.5
