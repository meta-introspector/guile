# Contributing to Guile-GnuTLS

## Build from git

```
git clone https://gitlab.com/gnutls/guile.git guile-gnutls
cd guile-gnutls
./bootstrap
./configure
make check
```

## Build dependencies -- git builds

In addition to the tools mentioned in README, to build from git you
need the following tools:

* [Git](https://git-scm.com/)
* [Automake](https://www.gnu.org/software/automake/) (use 1.11.3 or later)
* [Autoconf](https://www.gnu.org/software/autoconf/)
* [Libtool](https://www.gnu.org/software/libtool/)

dpkg-based distributions:
```
apt-get install -y git automake autoconf libtool
```

rpm-based distributions:
```
yum install -y git automake autoconf libtool
```

On macOS with Xcode and Homebrew:
```
brew install git autoconf automake libtool
```

## Build dependencies -- dist builds

To build the tarball from git you need the following additional tools:

* [Tar](https://www.gnu.org/software/tar/)
* [Gzip](https://www.gnu.org/software/gzip/)
* [Texinfo](https://www.gnu.org/software/texinfo/)
* [TeX Live](https://www.tug.org/texlive/)

dpkg-based distributions:
```
apt-get install -y tar gzip texinfo texlive
```

rpm-based distributions:
```
yum install -y tar gzip texinfo texlive
```

On macOS with Xcode and Homebrew:
```
brew install texinfo texlive
```

## Contributing

Parts of the Guile bindings, such as types (aka. "SMOBs"), enum
values, constants, are automatically generated.  This is handled by
the modules under `guile/modules/gnutls/build/'; these modules are
only used at build-time and are not installed.

The Scheme variables they generate (e.g., constants, type predicates,
etc.) are exported to user programs through `gnutls.scm' and
`gnutls/extra.scm', both of which are installed.

For instance, when adding/removing/renaming enumerates or constants,
two things must be done:

 1. Update the enum list in `build/enums.scm' (currently dependencies
    are not tracked, so you have to run "make clean all" in `guile/'
    after).

 2. Update the export list of `gnutls.scm' (or `extra.scm').

Note that, for constants and enums, "schemefied" names are used, as
noted under the "Guile API Conventions" node of the manual.

## Design

Parts of the Guile bindings, such as types (aka. "SMOBs"), enum
values, constants, are automatically generated.  This is handled by
the modules under `guile/modules/gnutls/build/'; these modules are
only used at build-time and are not installed.

The Scheme variables they generate (e.g., constants, type predicates,
etc.) are exported to user programs through `gnutls.scm' and
`gnutls/extra.scm', both of which are installed.

For instance, when adding/removing/renaming enumerates or constants,
two things must be done:

 1. Update the enum list in `build/enums.scm' (currently dependencies
    are not tracked, so you have to run "make clean all" in `guile/'
    after).

 2. Update the export list of `gnutls.scm' (or `extra.scm').

Note that, for constants and enums, "schemefied" names are used, as
noted under the "Guile API Conventions" node of the manual.

## Checked manual examples

There are a few examples in the manual. To run these examples as part
of the test suite, you need to create 2 scheme files: the example
itself, and a test runner for the example. The
`guile/examples/random-example.scm` and `guile/tests/random.scm` show
how to proceed.

 1. Save the example code to a new scheme file under `guile/examples`,
    for instance `guile/examples/new-example.scm`.

 2. In the `EXTRA_DIST` variable in `guile/Makefile.am`, add your example, like this:
    ```Makefile
    EXTRA_DIST +=                 \
      ...                         \
      examples/new-example.scm
    ```

 3. Include the example in the manual:
    ```texinfo
    @example
    @verbatiminclude @value{abs_top_srcdir}/guile/examples/new-example.scm
    @end example
    ```

 4. Create a test file to run the example, for instance
    `guile/tests/new-test.scm`. It should set up some environment, and
    load the example file. For instance:
    ```scheme
    (use-modules (gnutls) (gnutls build tests))
    (run-test
     (lambda ()
       (load-from-path "new-example.scm")))
    ```
    You can control what the example receives as standard input and
    check its standard output with `with-input-from-string` and
    `with-output-to-string`. Refer to the `guile/tests/random.scm` for
    a simple case where the output is checked.

 5. Register the test file in the `TESTS` variable in `guile/Makefile.am`:
    ```Makefile
    TESTS +=                      \
      ...                         \
      tests/new-test.scm
    ```

These steps ensure that the examples in the manual work as expected,
but not that they are stand-alone, since the test may load more
modules.
