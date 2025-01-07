# sort-color

A command-line tool written in Haskell that sorts BMP images in a folder based on their color distribution and *renames* them in sorted order.

## How it works

It reads all BMP files in the specified folder and analyzes their pixel data to calculate the intensity of the specified color channel (red, green, or blue). It then sorts the images based on the chosen color channel's intensity in ascending (asc) or descending (desc) order.

## Requirements

You need the following to use this tool:
- **Haskell** and **Cabal**: Install them easily using [GHCup](https://www.haskell.org/ghcup/#).
- **No third-party Haskell packages are required.**

## Installation

``` shell
git clone https://github.com/araujoviana/sort-color # Clone the repository
cd sort-color 
cabal install
```

Cabal will then attempt to insert the executable to your `$PATH`, restart your shell session if the command isn't available.

Alternatively, build and run directly without installing:

``` shell
cabal build
cabal run sort-color -- <args>
```

## Usage

Run the executable with the following arguments:

```bash
sort-color <folder-path> <color> <order>
```

For example:
```bash
sort-color /path/to/folder red asc
sort-color /path/to/folder blue desc
```

## Notes

* This program assumes that the bitmap files are in the standard Windows bitmap format (BMP).
* The program performs basic validation by checking the first two bytes of each file for the BMP signature (`424d` in hexadecimal). However, it does not fully validate the internal structure of the files.
