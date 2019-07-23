
terraform {
  required_version = ">= 0.12"

  backend "s3" {
    region         = "us-east-1"
    bucket         = "deckgo-beta-terraform-state"
    key            = "terraform.tfstate"
    dynamodb_table = "deckgo-beta-terraform-state-lock"
    encrypt        = true
  }
}
