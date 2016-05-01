0.5.0.0
-------

* Support GHC 8.0 only.
* Utilize the `TypeApplications` extension.
* Remove `Control.Ether.TH`.
* Use `Proxy#` in class methods.


0.4.0.0
-------

* Poly-kinded tags.
* Remove `Control.Ether.Tagged`.
* Replace `Control.Ether.Wrapped` with `Control.Monad.Trans.Ether.Dispatch`.
* Unified tagged transformer type in `Control.Monad.Trans.Ether.Tagged`.
* `MonadThrow`, `MonadCatch`, `MonadMask` instances.
* Drop `newtype-generics`.
* Instance search is now more strict.


0.3.1.1
-------

* Fix GHC 7.8 test issue.
* Remove unused imports.


0.3.1.0
-------

* Fix an issue with overlapping instances.


0.3.0.0
-------

* `MonadBase`, `MonadTransControl`, `MonadBaseControl` instances.
* `MFunctor`, `MMonad` instances.
* Use `transformers-lift`.


0.2.1.0
-------

* Constraint abbreviations: `Control.Ether.Abbr` and `Control.Ether.Implicit.Abbr`.


0.2.0.0
-------

* Convenience modules `Control.Monad.Ether` and `Control.Monad.Ether.Implicit`.
* Remove `fmapN` and `deepN`.
* Remove `Control.Monad.Ether.Implicit.Except.TH`.
* Add `handle` and `handleT`.


0.1.0.1
-------

* Fix `transformers` lower bound.
* Remove unused language extensions.
* GHC 7.8 compatibility.
