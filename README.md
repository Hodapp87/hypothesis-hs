hypothesis-hs
====

This is Haskell bindings
(via [servant](https://hackage.haskell.org/package/servant)) for the
web API of [hypothes.is](https://hypothes.is/).  This requires
a
[developer API token](https://hypothes.is/blog/introducing-developer-api-tokens/).

Right now only the following operations from
the [web API](https://h.readthedocs.io/en/latest/api/) are supported:

- [fetchAnnotation](https://h.readthedocs.io/en/latest/api/#operation/fetchAnnotation)
- [search](https://h.readthedocs.io/en/latest/api/#operation/search)

This code is mostly scratch work at the moment.
