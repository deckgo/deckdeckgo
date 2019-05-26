resource "aws_sqs_queue" "presentation_deploy" {
  name                        = "presentation_deploy.fifo"
  # TODO: can't be FIFO, not supporter by event mapping. Also we don't _really_
  # need it.
  fifo_queue                  = true
  content_based_deduplication = true
}
