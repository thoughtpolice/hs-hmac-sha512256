# Minimal package for hmacsha512256 message-authentication codes

[![Build Status](https://travis-ci.org/thoughtpolice/hs-hmacsha512256.png?branch=master)](https://travis-ci.org/thoughtpolice/hs-hmacsha512256)
[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](http://en.wikipedia.org/wiki/MIT_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://www.haskell.org)

This package implements minimal bindings to HMAC-SHA-512-256, i.e. the
first 256 bits of HMAC-SHA-512. It should be relatively easy to both
depend on, or include outright in your executable/package itself.

The underlying implementation is the `ref` code of `hmacsha512256` from
[SUPERCOP][], which was originally implemented by Dan J. Bernstein.

[SUPERCOP]: http://bench.cr.yp.to/supercop.html

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install hmacsha512256
```

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-hmacsha512256.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-hmacsha512256.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-hmacsha512256/master/AUTHORS.txt).

# License

MIT. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/hs-hmacsha512256/master/LICENSE.txt)
for terms of copyright and redistribution.

[contribute]: https://github.com/thoughtpolice/hs-hmacsha512256/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/hs-hmacsha512256/issues
[gh]: http://github.com/thoughtpolice/hs-hmacsha512256
[bb]: http://bitbucket.org/thoughtpolice/hs-hmacsha512256
[Hackage]: http://hackage.haskell.org/package/hmacsha512256
