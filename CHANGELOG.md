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
