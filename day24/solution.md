The input blocks take in an x,y,z and w. The z gets carried over to the next block,
while everything else is reset in each block and the w is taken from input.

Inside of each block, we would first take the modulo of z by 26, giving us
a 0 to 25 value. Then, we take the x input, add it to the `z % 26` value
and we compare it with w

```
inp w
mul x 0
add x z
mod x 26
div z 1
add x 14
eql x w
eql x 0
```

at which point, x is 0 if `(z % 26) + x == w` and 1 otherwise.

Then, if x is 1 we multiply z by 26

```
mul y 0
add y 25
mul y x
add y 1
mul z y
```

Lastly, if x is 1 we also add the y input to z

```
mul y 0
add y w
add y 16
mul y x
add z y
```

So roughly each block looks like:

```
if (z % 26) + x == w
	z = z / 26
else
	z = (z * 26) + y
```

In order, to be able to express some constraints on the inputs (w0,w1,..w13),
we need a way of keeping track of each step. If we always add y only after
we multiply z by 26, then we know that taking the modulos of z with 26, gives us
only the y from the previous step. In a way that's implementing a stack.

Taking the modulo of z gives us the value on top of the stack, and dividing
z by 26 removes the top element from the stack.

Half of the time the x variable is above 9, which means there is no chance that
`(z % 26) + x` will equal w, since we know w is between 1 and 9. In those cases,
we're just storing these values on the stack, so we can compare them to something
else later.

In order, to include all inputs in the constraints, we should have as many pushes
as we have pops.

Let's start going through each block

```
block 0:
	x' = 0 + 14, y = w0 + 16
		z = 0 * 26 + y = (w0 + 16)
	where the 0 in 0 + 14 and 0 * 26 is because z starts as 0

block 1:
	x' = (z0 % 26) + 11, y = w1 + 3
		z = z0 * 26 + y = (z0*26) + (w1+3)

block 2:
	x' = (z1 % 26) + 12, y = w2 + 2
		z = z1 * 26 + y = (z1*26) + (w2 + 2)

block 3:
	x' = (z2 % 26) + 11, y = w3 + 7
		z = z2 * 26 + y = (z2*26) + (w3 + 7)

block 4:
	x' = (z3 % 26) - 10, y = w4 + 13
```

block 4 is the first one where we could hit `(z % 26) + x == w`, so let's investigate.

Since x is -10, then any `(z3 % 26)` between 11 and 19 could give us equality
and as such pop a value from the stack, which is what we're after.

Knowing that `z3 = (z2*26) + (w3 + 7)` and disregarding anything before step 3,
because of the modulo we see that `z3 = w3 + 7`, essentially reading the top
value on the stack.

Going back to step 4 now, we say that `(z3 % 26) - 10` should be equal to w4.
Knowing that `z3 % 26` is just the top value on the stack - `w3 + 7` - we see that
 `w3 + 7 -10` should be different than w4, therefore

`w4 == w3 - 3`

which tells us that the 4th digit MUST be equal to the 3rd digit - 3. If that's the
case we pop w3 from the stack by dividing z by 26, so the top value on the stack
right now is y2.

```
block 4:
	x' = (z3 % 26) - 10, y = w4 + 13
		z = (z3 / 26), if w4 == w3 - 3     <- constraint between w4 and w3
	val on top of stack is (w2 + 2)
```

Let's carry on

```
block 5
	x' = (z2 % 26) + 15, y = w5 + 6
		z = z2 * 26 + y = (z2 * 26) + (w5 + 6)
	val on top of stack is (w5 + 6)
```

block 6 has a `x = -14`, which means we can hit the 1..9 range, so this is imposing
a constraint between the current input and the last one on the stack, which is w5.

```
block 6:
	x' = (z5 % 26) - 14, y = irrelevent
		z = (z5 / 26), if w6 == w5 + 6 - 14,
						  w6 == w5 - 8      <- constraint between w6 and w5
		val on top of stack -> (w2 + 2)
```

Following the same logic we can do the rest of the blocks

```
block 7:
	x' = (z2 % 26) + 10, y = w7 + 11
		z = z2 * 26 + y = (z2 * 26) + (w7 + 11)
	val on top of stack -> (w7 + 11)

block 8:
	x' = (z7 % 26) - 4, y = irrelevent
		z = (z7 / 26), if w8 == w7 + 11 - 4,
						  w8 == w7 + 7      <- constraint between w8 and w7
	val on top of stack -> (w2 + 2)

block 9:
	x' = (z2 % 26) - 3, y = irrelevent
		z = (z2 / 26), if w9 == w2 + 2 - 3,
						  w9 == w2 - 1      <- constraint between w9 and w2
	val on top of stack -> (w1 + 3)

block 10:
	x' = (z1 % 26) + 13, y = w10 + 11
		z = z1 * 26 + y = (z1 * 26) + (w10 + 11)
	val on top of stack -> (w10 + 11)

block 11:
	x' = (z10 % 26) -3, y = irrelevent
		z = (z10 / 26), if w11 = w10 + 11 - 3,
						   w11 = w10 + 8     <- constraint between w10 and w11
	val on top of stack -> (w1 + 3)

block 12:
	x' = (z1 % 26) -9, y = irrelevent
		z = (z1 / 26), if w12 = w1 + 3 - 9,
						  w12 = w1 - 6     <- constraint between w12 and w1
	val on top of stack -> (w0 + 16)

block 13:
	x' = (z0 % 26) -12, y = irrelevent
		z = (z0 / 26), if w13 = w0 + 16 - 12,
						  w13 = w0 + 4     <- constraint between w13 and w0
	no values on top of stack
	z0 is the starting z, which is 0, so the out value of z0/26 is also 0
```

So, we have the following constraints:

```
w4 == w3 - 3
w6 == w5 - 8
w8 == w7 + 7
w9 == w2 - 1
w11 = w10 + 8
w12 = w1 - 6
w13 = w0 + 4
```

And we'd like to get the largest and smallest numbers that satisfy
those constraints.

```
MAX -> ______________
MIN -> ______________
```

Inputs on the left are constrained by inputs on the right, which means
the ones on the right are unconstrained, so we want to set them to 9 for
maximizing and 1 for minimizing.

Let's start filling in the numbers

```
w4 == w3 - 3
	the valid range for w3 then is [4,9]

MAX -> ___9__________
MIN -> ___4__________

w6 == w5 - 8
	the valid range for w5 then is [9,9]

MAX -> ___9_9________
MIN -> ___4_9________

w8 == w7 + 7
	the valid range for w7 then is [1,2]

MAX -> ___9_9_2______
MIN -> ___4_9_1______

w9 == w2 - 1
	the valid range for w2 then is [2,9]

MAX -> __99_9_2______
MIN -> __24_9_1______

w11 = w10 + 8
	the valid range for w10 then is [1,1]

MAX -> __99_9_2__1___
MIN -> __24_9_1__1___

w12 = w1 - 6
	the valid range for w1 then is [7,9]

MAX -> _999_9_2__1___
MIN -> _724_9_1__1___

w13 = w0 + 4
	the valid range for w0 then is [1,5]

MAX -> 5999_9_2__1___
MIN -> 1724_9_1__1___
```

Then, all we need to do is fill in the constrained inputs,
by evaluting their expression on the right, so

```
w4 == w3 - 3, becomes MAX -> w4 = 9 - 3 = 6
					  MIN -> w4 = 4 - 3 = 1
w6 == w5 - 8, becomes MAX -> w6 = 9 - 8 = 1
						  -> w6 = 9 - 8 = 1
etc..

MAX -> 59996912981939
MIN -> 17241911811915
```

