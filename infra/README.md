[![license][agpl-license]][agpl-license-url]

[agpl-license]: https://img.shields.io/badge/License-AGPL%20v3-blue.svg
[agpl-license-url]: https://github.com/deckgo/deckdeckgo/blob/master/infra/GNU-AGPL-3.0

# DeckDeckGo - Infrastructure

This project is the infrastructure of [DeckDeckGo].

The infrastructure is deployed with [Terraform] and runs on [AWS Lambda].

## Table of contents

- [Getting started](#getting-started)
- [License](#license)

## Getting started

**requirements: [Nix]**

To (re)deploy the infrastructure, run the following commands:

```shell
$ cd infra/
$ nix-shell
$ terraform apply
```

Running `nix-shell` drops you in a shell where all the dependencies are
available. Running `terraform apply` will package everything needed by the
lambda function and upload it to AWS. **NOTE**: if you are deploying from a new
machine you may need to run `terraform init`.

## License

This project, respectively the infrastructure of [DeckDeckGo], is released under the GNU Affero General Public License. Copyright [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](mailto:nicolas@nmattia.com), Zürich, Switzerland. See COPYING for more details.

[DeckDeckGo] is developed by [David Dal Busco](mailto:david.dalbusco@outlook.com) and [Nicolas Mattia](mailto:nicolas@nmattia.com).

[deckdeckgo]: https://deckdeckgo.com
[terraform]: https://www.terraform.io/
[aws lambda]: https://aws.amazon.com/lambda/
[nix]: https://nixos.org/nix/
