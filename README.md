# res-remoteapi

Remote API React hook for rescript

```
pnpm add @softwarefactory-project/res-remoteapi
```

Add to your `bsconfig.json`:

```diff
"bs-dependencies": [
+  "@softwarefactory-project/res-remoteapi"
]
```

## Usage

```rescript
// Your application:
module Main = (CLIENT: RemoteAPI.HTTPClient) => {
  module Hook = RemoteAPI.Hook(CLIENT)

  @react.component
  let make = () => {
    let state = Hook.useAutoGet("/api", api_decoder)
    switch (state) {
      | RemoteData.NotAsked
      | RemoteData.Loading(None) => <p> {"Loading..." |> str} </p>
      | RemoteData.Loading(Some(info))
      | RemoteData.Success(info) => <p> {("Loaded: " ++ info.version) -> React.string} </p>
      | RemoteData.Failure(title) => <p> {("Failure: " ++ title) -> React.string} </p>
    };
  }
}
```

## Contribute

Contributions are most welcome.

Get started by running:

```sh
git clone https://github.com/softwarefactory-project/res-remoteapi
cd re-ansi
pnpm install
pnpm start
```

Then build and run tests with `pnpm test`.

Make sure to read about [React][reason-react] and [ReScript][rescript-lang] too.

## Changes

### 0.1.0

- Initial release

[rescript-react]: https://rescript-lang.org/docs/react/latest/installation
[rescript-lang]: https://rescript-lang.org/
