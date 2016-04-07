aeson-diff 1.0

    * aeson-diff now supports the operations and patch format described in
      RFC6902.

    * Patch application can fail. The patch function now returns in the
      'Result' monad from the aeson package. The new patch' function throws an
      exception instead.

    * The command line applications no longer pretend to support a non-JSON
      patch format.

aeson-diff 0.1

    * Initial release.
