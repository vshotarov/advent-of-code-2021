## Advent of Code 2021 solutions in Haskell
Haskell looks wicked, so I thought I'd give it a shot. Let's see how it goes.

To initialize a new day run

```
./init_day.sh {day_num}
```

e.g.

```
./init_day.sh 3
```

which will:

- ask for an example input (after pasting press Ctrl + D)
- ask for the real input (after pasting press Ctrl + D)
- make the directory `day{day_num}`
- write the example input into `day{day_num}/example_input.txt`
- write the real input into `day{day_num}/input.txt`
- write the template into `day{day_num}/Main.hs`
- add the `executable` template into the `advent-of-code.cabal` file

*NOTE: The day_num will be zero padded, so 3 will become 03*

After, that to solve any day, use `cabal run` like so:

<code>cabal run day{day_num} "\`cat {input_file}\`"</code>

e.g.

<code>cabal run day03 "\`cat day03/example_input.txt\`"</code>

