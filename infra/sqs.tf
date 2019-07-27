resource "aws_sqs_queue" "presentation_deploy" {
  name = "presentation_deploy"
}

resource "aws_sqs_queue" "dirty" {
  name = "dirty"
}
