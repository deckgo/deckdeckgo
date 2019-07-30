resource "aws_lambda_function" "dirty" {
  function_name = "deckdeckgo-dirty-lambda"
  filename      = data.external.build-function-dirty.result.path

  timeout = 5
  handler = "main.handler"
  runtime = "nodejs8.10"

  role = aws_iam_role.iam_for_lambda_dirty.arn

  environment {
    variables = {
      CLOUDFRONT_DISTRIBUTION_ID = aws_cloudfront_distribution.website_cdn.id
    }
  }
}

resource "aws_lambda_event_source_mapping" "dirty" {
  event_source_arn = aws_sqs_queue.dirty.arn
  enabled          = true
  function_name    = aws_lambda_function.dirty.function_name
  batch_size       = 1
}

data "external" "build-function-dirty" {
  program = [
    "nix",
    "eval",
    "(import ./default.nix).function-dirty-path",
    "--json",
  ]
}

resource "aws_iam_role" "iam_for_lambda_dirty" {
  name = "deckdeckgo-dirty-lambda-iam"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF

}

# XXX: looks like Lambda needs to be redeploy for the policy to take effect
# TODO: auto redeploy on policy change
data "aws_iam_policy_document" "policy_for_lambda_dirty" {
  # Give access to CloudWatch
  statement {
    actions = [
      "logs:CreateLogStream",
      "logs:PutLogEvents",
    ]

    resources = [aws_cloudwatch_log_group.dirty.arn]
  }

  # Give access to CloudWatch
  # TODO: what is this for?
  statement {
    actions = [
      "ec2:DescribeInstances",
      "ec2:CreateNetworkInterface",
      "ec2:AttachNetworkInterface",
      "ec2:DescribeNetworkInterfaces",
      "ec2:DeleteNetworkInterface",
    ]

    resources = ["*"]
  }

  # Give access to CloudFront
  statement {
    actions = [
      "cloudfront:CreateInvalidation",
    ]

    resources = ["*"] # AWS only supports "*" here apparently
  }

  # Give access to SQS
  statement {
    actions = [
      "sqs:ReceiveMessage",
      "sqs:DeleteMessage",
      "sqs:GetQueueAttributes",
    ]

    resources = [aws_sqs_queue.dirty.arn]
  }
}

resource "aws_iam_role_policy" "policy_for_lambda_dirty" {
  name = "deckdeckgo-dirty-lambda-policy"
  role = aws_iam_role.iam_for_lambda_dirty.id
  policy = data.aws_iam_policy_document.policy_for_lambda_dirty.json
}

resource "aws_cloudwatch_log_group" "dirty" {
  name = "/aws/lambda/${aws_lambda_function.dirty.function_name}"
  retention_in_days = "7"
}
