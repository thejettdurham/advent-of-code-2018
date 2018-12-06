# Advent of Code 2018

It's back! Last year, I used JS + Ramda to take a functional approach to solving the challenges, and this year I'm taking it to its natural conclusion by using [Reason](https://reasonml.github.io/en/). Reason is essentially Javascript-flavored OCaml, and compiles to Javascript through its sister project [Bucklescript](https://bucklescript.github.io/en/).

# Build

Reason files aren't directly runnable, they need to be complied to JS first. The compiled JS is not included in this repo, so you'll need to build it yourself if you want to try it out.

```
npm run build
```

# Run

The build puts `*.bs.js` files alongside the Reason source that you can run with `node`. Input is always read from `input.txt` in a Day's directory.

```
node src/DayN/MainN.bs.js
```
