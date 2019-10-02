import numpy as np
import * from sympy
import math

def gradDescent(f, vars, initial):
    '''
    f = objective function
    vars = dictionary of independent variables
    start = starting point
    '''

    direction = []
    for v in vars.keys:
        dfdv = f.diff(v)
        for val in initial:
            # get numerical value
            dfdv_val = dfdv.subs(v,val)
        direction.append(f).diff(v).sub

    for v in vars:
        df.subs(v.key, v.val)

    # find optimum step size
    ## using bisection (binary search)
    alpha = 1
    if math.abs(f(start + df*alpha) - 0) < 0.5:
        return(alpha)
    elif:
        (f(start + df*alpha) - 0) < 0.5:
        alpha -= 0.5
    else:
        alpha += 0.5
