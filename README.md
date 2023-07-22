# Guile-GnuTLS README

Guile-GnuTLS provides Guile bindings for the GnuTLS library.

## Documentation

The [Guile GnuTLS manual](https://gnutls.gitlab.io/guile/manual/) is
available in different formats, including [one-page HTML Guile-GnuTLS
manual](https://gnutls.gitlab.io/guile/manual/gnutls-guile.html).  See
`doc/`for source code.

## License

Guile-GnuTLS is released under the GNU Lesser General Public License
(LGPL) version 2.1 or later, see [COPYING.LESSERv2](COPYING.LESSERv2).

Build infrastructure, self-tests and some other files are licensed
under the GNU General Public License (GPL) version 3 or later, see
[COPYING](COPYING).

The manual (see doc/) is under the GNU Free Documentation License
version 1.3 or later, see [doc/fdl-1.3.texi](doc/fdl-1.3.texi).

For any copyright year range specified as YYYY-ZZZZ in this package
note that the range specifies every single year in that closed interval.

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
apt-get install -y make guile-3.0-dev libgnutls28-dev
```

rpm-based distributions:
```
yum install -y make guile22-devel gnutls-devel
```

On macOS with Xcode and Homebrew:
```
brew install guile gnutls
```

## Build instructions

See the [INSTALL](INSTALL) file for generic instructions, but briefly
the following will be sufficient to build and install Guile-GnuTLS:

```
tar xfz guile-gnutls-*.tar.gz
cd guile-gnutls-*
./configure
make check
sudo make install
```

If you wish to build from git or modify source code, please see the
file [CONTRIBUTING.md](CONTRIBUTING.md) for more information.
