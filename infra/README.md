# DeckDeckGo - Infrastructure

This project is the infrastructure of [DeckDeckGo].

The infrastructure is deployed with [Terraform] and runs on [AWS Lambda].

## Table of contents

- [Getting started](#getting-started)
- [License](#license)

## Getting started

**requirements: [Nix]**

To (re)deploy the infrastructure, run the following commands:

``` shell
$ cd infra/
$ nix-shell
$ terraform apply
```

Running `nix-shell` drops you in a shell where all the dependencies are
available. Running `terraform apply` will package everything needed by the
lambda function and upload it to AWS. **NOTE**: if you are deploying from a new
machine you may need to run `terraform init`.


[DeckDeckGo]: https://deckdeckgo.com
[Terraform]: https://www.terraform.io/
[AWS Lambda]: https://aws.amazon.com/lambda/
[Nix]: https://nixos.org/nix/
