def dx(f, x):
    return abs(0-f(x))

def newtons_method(f, df, x0, e):
    delta = dx(f, x0)
    while delta > e:
        print(delta)
        x0 = x0 - f(x0)/df(x0)
        delta = dx(f, x0)
    print('Root is at: ', x0)
    print('f(x) at root is: ', f(x0))

def f(x):
    return(1+(1.382)/x**2)*(x-0.0319)-(0.08205746*25)

def df(x):
    return(1-((1+0.0319)/x**2))

if __name__=="__main__":
    newtons_method(f, df, 0.1, 0.5)
