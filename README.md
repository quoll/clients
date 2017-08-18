# clients

A Clojure library designed to provide common functionality for talking to services.

## Usage

For Leiningen, add the following dependency to a projects.clj file:

```
[quoll/clients "0.1.2"]
```

Use one of the `retry*` macros to execute a block of code repetitively, until it fails
or the retry plan is exhausted.

## License

Copyright © 2017 Cisco Systems

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
