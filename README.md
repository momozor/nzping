# nzping
A simple console program to auto ping server(s) given the defined
interval time gap.

## Usage
> The target system must have CURL runtime library pre-installed

```sh
$ nzping interval_seconds link(s)

# example

## this will auto-ping the links for every 30 seconds.
$ ./alire/build/bin/nzping 30.0 https://google.com https://github.com

```

## Building

This software uses Ada 2012 + GNAT and `alr` and libcurl-devel to build.

Once you have the dependencies listed above installed, run:
`alr build` to build the project.

## License
This software is licensed under the MIT license. Please see 
COPYING file for more details.
