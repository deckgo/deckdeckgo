

<p align="center">
    <img src="assets/wai-lambda.png" alt="wai-lambda" width="200"/>
</p>

# WAI Lambda - Haskell Webapps on AWS Lambda

[![Hackage](https://img.shields.io/hackage/v/wai-lambda.svg)](https://hackage.haskell.org/package/wai-lambda)

This Haskell library turns any [wai] webapp ([spock], [servant], etc) into a
handler for AWS [Lambda][lambda] and [API Gateway][api-gateway] requests.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Lambda (run)

app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = run app
```

This doesn't require any particular Lambda environment. By following the
instructions in the [build](#build) section the resulting `zip` file uploaded
to AWS Lambda is typically be smaller than 1MB. For basic webapps the request
handling duration (as reported by AWS) is between 1 and 5 milliseconds.

* [**Install**](#install) with either Cabal, stack or Nix.
* [**Build for AWS**](#build) with Nix and fully static executables.
* [**Deploy to AWS**](#deploy) with Nix and Terraform.

## Install

### Cabal

Installing with Cabal:

``` shell
$ cabal install wai-lambda
```

### Stack

If you use stack and `wai-lambda` is not included in your snapshot, add it to
the `extra-deps`:

``` yaml
extra-deps:
  - wai-lambda-0.1.0.0
```

or from GitHub:

``` yaml
packages:
- ...
- location:
    git: https://github.com/deckgo/wai-lambda.git
    commit: <some commit>
  extra-dep: true
```

### Nix

You do not have to do anything if you use cabal2nix and a Nix commit that
includes `wai-lambda` in its Hackage dependencies. If you want to use a
specific commit of `wai-lambda` we recommend using [niv]:

``` shell
$ niv add deckgo/wai-lambda
```

Then add an override to your Nix packages:

``` nix

rec
{
    sources = import ./nix/sources.nix; # from niv
    pkgs = import sources.nixpkgs {};

    myHaskellPackages = pkgs.haskellPackages.override
        { overrides = self: super:
            { "wai-lambda" =
                super.callCabal2nix "wai-lambda" sources.wai-lambda;
            };
        };
}
```

See also the [build](#build) section for producing AWS Lambda packages with
nix.

## Build

_TODO_

For inspiration on how to build fully static executables with Nix, see
[DeckDeckGo](https://github.com/deckgo/deckdeckgo/tree/master/infra)

## Deploy

_TODO_

For inspiration on how to deploy with Terraform and Nix, see
[DeckDeckGo](https://github.com/deckgo/deckdeckgo/tree/master/infra)

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](mailto:nicolas@nmattia.com)

[wai]: https://www.stackage.org/package/wai
[spock]: https://www.spock.li/
[servant]: https://docs.servant.dev/en/stable/
[lambda]: https://aws.amazon.com/lambda/
[api-gateway]: https://aws.amazon.com/api-gateway/
[niv]: https://github.com/nmattia/niv
