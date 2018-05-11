# 2.3.0

* BREAKING CHANGE: `syncIO` now expects a `MonadIO` constraint instead of
  `UnexceptionalIO`
    * `syncIO` also changes how it detects asynchronous exceptions.  It now
      decides based on whether or not an exception inherits from
      `SomeAsyncException`
    * See: https://github.com/Gabriel439/Haskell-Errors-Library/pull/53

# 2.2.5

* Increase upper bound on `exceptions`

# 2.2.4

* Increase upper bound on `exceptions`

# 2.2.3

* Increase upper bound on `transformers-compat`

# 2.2.2

* Support GHC 8.4 through compatibility with Semigroup/Monoid proposal

# 2.2.1

* Add precedence and fixity for `(?:)`

# 2.2.0

* BREAKING CHANGE: Use `Text` instead of `String`
* Add `handleExceptT`

# 2.1.3

* Support older versions of `ghc`

# 2.1.2

* Increase upper bound on `transformers` dependency

# 2.1.1

* Increase upper bound on `transformers-compat`

# 2.1.0

* Change `syncio` to use `unexceptionalio` to prove that all synchronous
  exceptions were caught and handled

# 2.0.0

* Switch from `EitherT` to `ExceptT`

# 1.4.7

* Increase upper bound on `transformers` from `0.4` to `0.5`

# 1.4.6

* Add `bool`
* Add `(?:)`
* Add `isJustT`
* Add `isNothingT`
* Add `isLeftT`
* Add `isRightT`

# 1.4.5

* Increase upper bound on `either` from `4.1` to `5`

# 1.4.4

* Add `failWith`
* Add `failWithM`

# 1.4.3

* Add `AllE`
* Add `AnyE`
* Increase upper bound on `either` from `3.5` to `4.1`

# 1.4.2

* Add `(??)`
* Add `(!?)`
* Add `syncIO`

# 1.4.1

* Re-export `EitherT`
* Re-export `MaybeT`

# 1.4.0

* Add `maybeT`
* Add `just`
* Add `nothing`
* Add upper bound to `either`
* Add upper bound to `safe`
* Add upper bound to `transformers`

# 1.3.1

* Increase lower bound on `transformers` from `0.2` to `0.3.0.0`

# 1.3.0

* Add `assertMay`
* Add `rightMay`
* Add `justErr`
* Add `tryJust`
* Add `tryRight`
* Add `MonadPlus` functions to `Control.Error.Safe`
* Add `isLeft`
* Add `isRight`
* Add `fmapR`
* Add `fmapRT`
* Add `err`
* Add `errLn`
* Add `flipE`
* Add `flipET`
* Rename `tryIO` to `scriptIO`
* Remove `tryMaybe`
* Remove `tryEither`
* Rename `liftMaybe` to `hoistMaybe`
* Rename `liftEither` to `hoistEither`

# 1.2.1

* Add lower bound to `either`

# 1.2.0

* Remove `right`
* Remove `left`

# 1.1.1

* Cosmetic changes

# 1.1.0

* Add `left`

# 1.0.0

* Initial release
