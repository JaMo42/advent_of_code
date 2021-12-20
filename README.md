# Advent of Code 2021

https://adventofcode.com/2021.

## Running

```shell
./run D [-example] [-clean] [-- options...]
```

`D` is the number of the day.
The example data can be used with the `-example` option.
If no executable for the day exists, the script will try to build it.
All options that appear after `--` are passed to the program.
Run with `-clean` to remove all built executables.

## Dependencies

Some days require [numpy](https://pypi.org/project/numpy/) to run (`pip install numpy` or `pip3 install numpy`).

