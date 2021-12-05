# Advent of Code 2021

## Running

```shell
./run D [-example] [-- options...]
```

`D` can either be just a number (for example `7`) or a number and a part (for example `1_2`),
the first number of this sequence is used to determine the input file.
The example data can be used with the `-example` option.
If no executable for the day exists, the script will try to build it.
All options that appear after `--` are passed to the program.

## Days

These days require to run `D_2` for the second part (replace `D` with the day number):

- Day 1
- Day 2

These days run both parts at once:

- Day 3

These are special cases:

- Day 4: `./run 4` for part one, `./run 4 -- -last` for part two

