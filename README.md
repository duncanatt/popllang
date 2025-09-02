# POPL Languages

L1, L1 (ext), L2, L3 languages for POPL

## WIP remaining

- [ ] type checker for L2
- [ ] eval for L2

## Setup

### Requirements

[Opam Package Manager](https://opam.ocaml.org/doc/Install.html#Using-your-system-39-s-package-manager)

### Installation

Install all dependencies (including the OCaml compiler, dune, and other third-party packages):

```sh
make install
```

### Building the Project

To build the project, run:

```sh
make
```

### Running Tests

To execute the test suite, use:

```sh
make test
```

## Language Samples

### L1 (Langone)

statically typed, arithmetic, logical operations

```text
(1-1 <= (2 + 3)) 
 && (~~false && true)
```

### L1 extended (Langoneext)

L1 + variables

```text
let x = 5 in let y = 6 in y + x
```

### L2 (Langtwo)

<!-- functional, first-class functions, statically typed -->

parser works,
interpreter/type checker wip

```text
let x = 1
  in let f = fun(y){y + x} 
    in let x = 2
      in f 3
```

### L3 (Langthree)

<!-- imperative, state/configuration based, sequential, statically typed -->

```text
{l1 -> 4} 
l1 := !l1 + !l1
```