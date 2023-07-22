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
