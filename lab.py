import doctest

# NO ADDITIONAL IMPORTS ALLOWED!
# You are welcome to modify the classes below, as well as to implement new
# classes and helper functions as necessary.


class Symbol:
    precedence = None # Var and Num will find a precedence of None here
    def __add__(self, other):
        return Add(self, other)
    def __radd__(self, other):
        return Add(other, self)
    def __sub__(self, other):
        return Sub(self, other)
    def __rsub__(self, other):
        return Sub(other, self)
    def __mul__(self, other):
        return Mul(self, other)
    def __rmul__(self, other):
        return Mul(other, self)
    def __truediv__(self, other):
        return Div(self, other)
    def __rtruediv__(self, other):
        return Div(other, self)
    def __pow__(self, other):
        return Pow(self, other)
    def __rpow__(self, other):
        return Pow(other, self)


class Var(Symbol):
    def __init__(self, n):
        """
        Initializer.  Store an instance variable called `name`, containing the
        value passed in to the initializer.
        """
        self.name = n

    def __str__(self):
        return self.name

    def __repr__(self):
        return "Var(" + repr(self.name) + ")"

    # base cases
    def deriv(self, wrt):
        if self.name == wrt:
            return Num(1)
        else:
            return Num(0)
    
    def simplify(self):
        return self

    def eval(self, vals):
        if self.name not in vals:
            raise KeyError("Nooooo, this variable doesn't have a value!!!!")
        return vals[self.name]


class Num(Symbol):
    def __init__(self, n):
        """
        Initializer.  Store an instance variable called `n`, containing the
        value passed in to the initializer.
        """
        self.n = n

    def __str__(self):
        return str(self.n)

    def __repr__(self):
        return "Num(" + repr(self.n) + ")"
    
    # base cases
    def deriv(self, wrt):
        return Num(0)

    def simplify(self):
        return self

    def eval(self, vals):
        return self.n


class BinOp(Symbol):
    # special defaults if they are not overriden as True within a subclass
    special = False
    pow_spec = False

    def __init__(self, left, right):
        """
        Initializer.  Store instance variables left and right, containing the
        values passed in to the initializer.
        """
        # initialize left
        if isinstance(left, str):
            self.left = Var(left)
        elif isinstance(left, int) or isinstance(left, float):
            self.left = Num(left)
        else:
            self.left = left

        # initialize right
        if isinstance(right, str):
            self.right = Var(right)
        elif isinstance(right, int) or isinstance(right, float):
            self.right = Num(right)
        else:
            self.right = right
        
    
    def __str__(self):
        l = str(self.left)
        r = str(self.right)

        # if left is an operation (has a precedence), then if less than current precedence or special case for Pow
        if self.left.precedence:
            if self.left.precedence < self.precedence or (self.pow_spec and self.left.precedence == self.precedence):
                l = '(' + l + ')'

        # if right precedence is less than own precedence or special case for Sub/Div
        if self.right.precedence and (self.right.precedence < self.precedence or (self.special and self.right.precedence == self.precedence)):
            r = '(' + r + ')'

        return l + self.operation + r
    
    def __repr__(self):
        return self.__class__.__name__ + "(" + repr(self.left) + ", " + repr(self.right) + ")"

    def simplify(self):
        # simplify pieces
        left = self.left.simplify()
        right = self.right.simplify()

        # then use simp function particular to each subclass
        return self.simp(left, right)


class Add(BinOp):
    precedence = 1
    operation = ' + '

    # derivative rules
    def deriv(self, wrt):
        return self.left.deriv(wrt) + self.right.deriv(wrt)
    
    # simplify rules
    def simp(self, left, right):
        if isinstance(left, Num) and isinstance(right, Num):
            return Num(left.n + right.n)
        if isinstance(left, Num) and left.n == 0:
            return right
        if isinstance(right, Num) and right.n == 0:
            return left
        return left + right
    
    def eval(self, vals):
        return self.left.eval(vals) + self.right.eval(vals) # recurse on left and right


class Sub(BinOp):
    precedence = 1
    operation = ' - '
    special = True

    # derivative rules
    def deriv(self, wrt):
        return self.left.deriv(wrt) - self.right.deriv(wrt)

    # simplify rules
    def simp(self, left, right):
        if isinstance(left, Num) and isinstance(right, Num):
            return Num(left.n - right.n)
        if isinstance(right, Num) and right.n == 0:
            return left
        return left - right

    def eval(self, vals):
        return self.left.eval(vals) - self.right.eval(vals) # recurse on left and right


class Mul(BinOp):
    precedence = 2
    operation = ' * '

    # derivative rules
    def deriv(self, wrt):
        return self.left * self.right.deriv(wrt) + self.right * self.left.deriv(wrt)

    # simplify rules
    def simp(self, left, right):
        if isinstance(left, Num) and isinstance(right, Num):
            return Num(left.n * right.n)
        if isinstance(left, Num):
            if left.n == 0:
                return Num(0)
            if left.n == 1:
                return right
        if isinstance(right, Num):
            if right.n == 0:
                return Num(0)
            if right.n == 1:
                return left
        return left * right

    def eval(self, vals):
        return self.left.eval(vals) * self.right.eval(vals) # recurse on left and right


class Div(BinOp):
    precedence = 2
    operation = ' / '
    special = True

    # derivative rules
    def deriv(self, wrt):
        return (self.right * self.left.deriv(wrt) - self.left * self.right.deriv(wrt)) / (self.right * self.right)

    # simplify rules
    def simp(self, left, right):
        if isinstance(left, Num) and isinstance(right, Num):
            return Num(left.n / right.n)
        if isinstance(left, Num) and left.n == 0:
            return Num(0)
        if isinstance(right, Num) and right.n == 1:
            return left
        return left / right

    def eval(self, vals):
        return self.left.eval(vals) / self.right.eval(vals) # recurse on left and right


class Pow(BinOp):
    precedence = 3
    operation = ' ** '
    pow_spec = True
    
    # derivative rules
    def deriv(self, wrt):
        if not isinstance(self.right, Num):
            raise TypeError('We can only take the derivative of Pow objects where the exponent is a number')
        return self.right * self.left ** (self.right - Num(1)) * self.left.deriv(wrt)

    # simplify rules
    def simp(self, left, right):
        if isinstance(right, Num):
            if right.n == 0:
                return Num(1)
            if right.n == 1:
                return left
        if isinstance(left, Num) and left.n == 0:
            return Num(0)
        return left ** right

    def eval(self, vals):
        return self.left.eval(vals) ** self.right.eval(vals) # recurse on left and right



def expression(s):
    '''
    Creates symbolic expression from a string (s) containing an expression input
    '''
    return parse(tokenize(s))

def tokenize(s):
    '''
    Breaks up an input string into pieces
    '''
    s = s.split(' ') # first split by spaces
    tokens = []
    for elt in s:
        # separate out all parentheses
        while elt[0] == '(':
            tokens.append('(')
            elt = elt[1:]
        closes = []
        while elt[-1] == ')':
            closes.append(')')
            elt = elt[:-1]
        tokens.append(elt); tokens += closes
    return tokens


def cast_num(token):
    '''
    Tries to create a Num object from a token, preserving type,
    returns None if token is not a number
    '''
    try:
        return Num(int(token))
    except:
        try:
            return Num(float(token))
        except:
            return None


def parse(tokens):
    '''
    Parses the tokens and creates the proper expression
    '''
    objects = {'+': Add, '-': Sub, '*': Mul, '/': Div, '**': Pow} # class objects
    def parse_expression(index):
        casted = cast_num(tokens[index])
        if casted: # if token is a number
            return casted, index + 1
        if tokens[index] != '(' and tokens[index] != ')': # if not a binary op
            return Var(tokens[index]), index + 1

        # recursively parse until either a Num or Var
        left, next = parse_expression(index + 1)
        # token after a Num or Var must be the operation
        operation = tokens[next]
        # parse right side
        right, next = parse_expression(next + 1)

        return objects[operation](left, right), next + 1

    parsed_expression, next_index = parse_expression(0)
    return parsed_expression


def helper():
    '''
    DOCTESTS; IGNORE
    >>> x = Var('x')
    >>> y = Var('y')
    >>> z = 2*x - x*y + 3*y
    >>> print(z.deriv('x'))  # unsimplified, but the following gives us (2 - y)
    2 * 1 + x * 0 - (x * 0 + y * 1) + 3 * 0 + y * 0
    >>> print(z.deriv('y'))  # unsimplified, but the following gives us (-x + 3)
    2 * 0 + x * 0 - (x * 1 + y * 0) + 3 * 1 + y * 0
    >>> z = 2*x - x*y + 3*y
    >>> print(z.simplify())
    2 * x - x * y + 3 * y
    >>> print(z.deriv('x'))
    2 * 1 + x * 0 - (x * 0 + y * 1) + 3 * 0 + y * 0
    >>> print(z.deriv('x').simplify())
    2 - y
    >>> print(z.deriv('y'))
    2 * 0 + x * 0 - (x * 1 + y * 0) + 3 * 1 + y * 0
    >>> print(z.deriv('y').simplify())
    0 - x + 3
    >>> Add(Add(Num(2), Num(-2)), Add(Var('x'), Num(0))).simplify()
    Var('x')
    >>> z = Add(Var('x'), Sub(Var('y'), Mul(Var('z'), Num(2))))
    >>> z.eval({'x': 7, 'y': 3, 'z': 9})
    -8
    >>> z.eval({'x': 3, 'y': 10, 'z': 2})
    9
    >>> 2 ** Var('x')
    Pow(Num(2), Var('x'))
    >>> x = expression('(x ** 2)')
    >>> x.deriv('x')
    Mul(Mul(Num(2), Pow(Var('x'), Sub(Num(2), Num(1)))), Num(1))
    >>> print(x.deriv('x').simplify())
    2 * x
    >>> print(Pow(Add(Var('x'), Var('y')), Num(1)))
    (x + y) ** 1
    >>> print(Pow(Add(Var('x'), Var('y')), Num(1)).simplify())
    x + y
    '''
    pass

if __name__ == "__main__":
    doctest.testmod()