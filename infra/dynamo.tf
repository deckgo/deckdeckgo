resource "aws_dynamodb_table" "deckdeckgo-test-dynamodb-table-users" {
  name           = "Users"
  billing_mode   = "PAY_PER_REQUEST"
  hash_key       = "UserId"

  attribute {
    name = "UserId"
    type = "S"
  }

}

resource "aws_dynamodb_table" "deckdeckgo-test-dynamodb-table-decks" {
  name           = "Decks"
  billing_mode   = "PAY_PER_REQUEST"
  hash_key       = "DeckId"

  attribute {
    name = "DeckId"
    type = "S"
  }

}

resource "aws_dynamodb_table" "deckdeckgo-test-dynamodb-table-slides" {
  name           = "Slides"
  billing_mode   = "PAY_PER_REQUEST"
  hash_key       = "SlideId"

  attribute {
    name = "SlideId"
    type = "S"
  }

}
