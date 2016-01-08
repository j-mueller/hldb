# Summary

[![Build Status](https://travis-ci.org/j-mueller/hldb.svg?branch=master)](https://travis-ci.org/j-mueller/hldb)

hledger-dashboard, a GHCJS webapp with reports for your hledger journals. Also a playground for some ideas around dashboards and UIs in Haskell that I've been thinking about.

# How to build
Install stack, then run
```
stack build --stack-yaml stack-ghcjs.yaml
```

You may need to follow stack's instructions to install GHCJS.

```
cabal install --ghcjs -fwebapp
```

# License

BSD3

# Contributions

Bug reports, pull requests, feature requests are welcome
