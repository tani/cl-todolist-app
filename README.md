# Todo List written in Common Lisp

This repository contains a common lisp project for a simple todo list.

## Usage

### Run entrypoint

The entrypoint of this project is `main` function.
You can run it with the following command:

```sh
nix run .#main-sbcl
```

- `.#main-sbcl`: Run the entrypoint using SBCL.
- `.#main-ecl`: Run the entrypoint using ECL.

### Run tests

The tests of this project are located in the `test` directory.
You can run them with the following command:

```sh
nix run .#test-sbcl
```

We use [Parachute](https://github.com/Shinmera/parachute) for testing.
Thanks to [Shinmera](https://github.com/Shinmera) for this great library.

- `.#test-sbcl`: Run all tests using SBCL.
- `.#test-ecl`: Run all tests using ECL.


### Run REPL

You can run a REPL with the following command:

```sh
nix develop -c sbcl # ecl or abcl
```

To load the project, you can use the following code:

```lisp
(require :uiop)
(load (uiop:getenv "ASDF"))
(require :todolist)
```

## License

This project is inspired by
[Comamoca/nix-template](https://github.com/Comamoca/scaffold/tree/main/cl-nix),
which is licensed under the CC0 1.0 Universal license.

This projects is licensed under the following license:

>
> MIT No Attribution
>
> Copyright 2024 Masaya Taniguchi
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of this
> software and associated documentation files (the "Software"), to deal in the Software
> without restriction, including without limitation the rights to use, copy, modify,
> merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
> INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
> PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
> HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
> OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

