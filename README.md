# Guile-GnuTLS README

Guile-GnuTLS provides Guile bindings for the GnuTLS library.

## Build dependencies

To build from tarball you need the following tools:

* [sh](https://www.gnu.org/software/bash/)
* [make](https://www.gnu.org/software/make/)
* [Guile](https://www.gnu.org/software/guile/)
* [GnuTLS](https://www.gnu.org/software/gnutls/)
* [Texinfo](https://www.gnu.org/software/texinfo/) (optional, for manual)
* [Texlive & epsf](https://www.tug.org/texlive/) (optional, for manual)

The required software is typically distributed with your operating
system, and the instructions for installing them differ.  Here are
some hints:

dpkg-based distributions:
```
apt-get install -y dash make guile-3.0-dev libgnutls28-dev
```

rpm-based distributions:
```
yum install -y dash make guile22-devel gnutls-devel
```

## Build dependencies -- git builds

To build from git you need the following additional tools:

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

## Build dependencies -- git tarball builds

To build the tarball from git you need the following additional tools:

* [Tar](https://www.gnu.org/software/tar/)
* [Gzip](https://www.gnu.org/software/gzip/)

dpkg-based distributions:
```
apt-get install -y tar gzip
```

rpm-based distributions:
```
yum install -y tar gzip
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
