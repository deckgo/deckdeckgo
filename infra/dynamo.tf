resource "aws_dynamodb_table" "deckdeckgo-test-dynamodb-table" {
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


output "dynamo-decks-arn" {

  value = "${aws_dynamodb_table.deckdeckgo-test-dynamodb-table.arn}"

}
