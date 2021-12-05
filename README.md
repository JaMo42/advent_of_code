# Advent of Code 2021

https://adventofcode.com/2021.

## Running

```shell
./run D [-example] [-- options...] [-clean]
```

`D` can either be just a number (for example `7`) or a number and a part (for example `1_2`),
the first number of this sequence is used to determine the input file.
The example data can be used with the `-example` option.
If no executable for the day exists, the script will try to build it.
All options that appear after `--` are passed to the program.
Run with `-clean` to remove all built executables.

## Days

Most days run both parts when using `./run D`.

These are special cases:

- Day 2: `./run 2` for part one, `./run 2_2` for part two

