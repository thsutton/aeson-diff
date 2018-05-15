aeson-diff 1.1.0.5

    * Support GHC-8.4.x in recent Stackage releases.

aeson-diff 1.1.0.4

    * Resolve name clash that prevents building against aeson>=1.2 in
      Stackage Nightly.

aeson-diff 1.1.0.3

    * Relax upper bound on base dependency for Stackage LTS-9.

aeson-diff 1.1.0.0

    * aeson-diff can now, optionally, generate a test operation before each
      remove.

    * Add '--test-before-remove' option to 'json-diff' command.

    * Add 'Config' type and 'diff'' to allow optional behaviours.

aeson-diff 1.0.0.1

    * Remove the `patch'` function before anyone gets attached to it.

    * Remove the 'Value' which was carried by the 'Rem' operation constructor.

    * Move 'Pointer' and 'Patch' types and operations into separate modules.

aeson-diff 1.0

    * aeson-diff now supports the operations and patch format described in
      RFC 6902.

    * The `patch` function now returns in the 'Result' monad from the aeson
      package.

    * Add a `patch'` function throws an exception instead.

    * The command line applications no longer pretend to support a non-JSON
      patch format.

aeson-diff 0.1

    * Initial release.
