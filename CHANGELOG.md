0.5.1.0
-------

* Disable `HasLens` for tuples via cabal flag.

0.5.0.0
-------

* Support only GHC 8.0 and higher.
* Use the `TypeApplications` extension to get rid of `Proxy`.
* Remove `Control.Ether.TH`.
* Remove `Control.Ether.Abbr`.
* Unify `Dispatch` and `TaggedTrans`.
* Better `MonadBase` and `MonadBaseControl` instances.
* Add flattening for `ReaderT` and `StateT`.
* Add zooming for `MonadState`.
* Simpler module structure.

0.4.1.0
-------

* Export DispatchT newtype constructor.

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
