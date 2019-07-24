terraform {
  required_version = "~> 0.12"
}

provider "aws" {
  version = ">= 2.6.0"
  region  = "us-east-1"
}

module "terraform_state_backend" {
  source     = "git::https://github.com/cloudposse/terraform-aws-tfstate-backend.git?ref=tags/0.9.0"
  stage      = "beta"
  namespace  = "deckgo"
  name       = "terraform"
  attributes = ["state"]
  region     = "us-east-1"
}
