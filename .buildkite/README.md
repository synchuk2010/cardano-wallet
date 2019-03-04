# Buildkite CI

[![Build Status](https://badge.buildkite.com/7ea3dac7a16f066d8dfc8f426a9a9f7a2131e899cd96c444cf.svg)](https://buildkite.com/input-output-hk/cardano-wallet)

URL: https://buildkite.com/input-output-hk/cardano-wallet/

## Updating the Pipeline

See the [documentation](https://buildkite.com/docs)
and [`.buildkite/pipeline.yml`](./pipeline.yml).

Usually, you would just edit `rebuild.hs`.

## Running a build locally

    runhaskell .buildkite/rebuild.hs

## Adding build dependencies

The best way is to add them to the `buildTools` list in
`.buildkite/default.nix`.
