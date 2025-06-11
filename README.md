# Detexify Backend Server implemented in Haskell

This Haskell project is developed using an older version of Haskell toolchain.

Follow the below mentioned steps to build on an Intel x86_64 Mac.

Run `ghcup tui` and ensure you are running the following version of the various tools:

```
GHCup 0.1.50.1

Stack 3.3.1

HLS 2.10.0.0

cabal 3.12.1.0

GHC 8.10.7
```

Now run the following command-line one-by-one to install project dependencies:

```bash
cabal update
```

followed by:

```bash
cabal install --only-dependencies --overwrite-policy=always
```

Once all the dependency installation is done successfully, the following message is displayed:

```bash
Symlinking 'detexify-hs-backend' to
'~/.cabal/bin/detexify-hs-backend'
```

where `~` is replaced by the path to current users home directory.

Now run the following command-line to build the project:

```bash
cabal build
```

This should successfully build the project.

Now run the following command-line to run the backend:

```bash
cabal run
```
