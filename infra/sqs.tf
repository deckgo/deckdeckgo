resource "aws_sqs_queue" "presentation_deploy" {
  name                        = "presentation_deploy.fifo"
  fifo_queue                  = true
}
