variable "meta-bucket-name" {
  type    = string
  default = "deckdeckgo-beta-meta"
}

resource "aws_s3_bucket" "meta" {
  bucket = var.meta-bucket-name
}

resource "aws_lambda_function" "google_key_updater" {
  function_name = "deckdeckgo-google-key-updater-lambda"
  filename      = data.external.build-function-google-key-updater.result.path

  # TODO: need a big *ss timeout on this one
  timeout = 5
  handler = "main.handler"
  runtime = "nodejs8.10"

  role = aws_iam_role.iam_for_lambda_google_key_updater.arn

  environment {
    variables = {
      META_BUCKET_NAME = aws_s3_bucket.meta.bucket
    }
  }
}

data "external" "build-function-google-key-updater" {
  program = [
    "nix",
    "eval",
    "(import ./default.nix).function-google-key-updater-path",
    "--json",
  ]
}

resource "aws_iam_role" "iam_for_lambda_google_key_updater" {
  name = "deckdeckgo-google-key-updater-lambda-iam"

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
data "aws_iam_policy_document" "policy_for_lambda_google_key_updater" {
  # Give access to CloudWatch
  statement {
    actions = [
      "logs:CreateLogStream",
      "logs:PutLogEvents",
    ]

    resources = [aws_cloudwatch_log_group.google-key-updater.arn]
  }

  # Give access to CloudWatch
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

  statement {
    actions = [
      "s3:PutObject",
      "s3:DeleteObject",
    ]

    resources = ["${aws_s3_bucket.meta.arn}/*"]
  }
}

resource "aws_iam_role_policy" "policy_for_lambda_google_key_updater" {
  name = "deckdeckgo-google-key-updater-lambda-policy"
  role = aws_iam_role.iam_for_lambda_google_key_updater.id
  policy = data.aws_iam_policy_document.policy_for_lambda_google_key_updater.json
}

resource "aws_cloudwatch_log_group" "google-key-updater" {
  name = "/aws/lambda/${aws_lambda_function.google_key_updater.function_name}"
  retention_in_days = "7"
}

resource "aws_cloudwatch_event_rule" "every_five_minutes" {
  name = "every-five-minutes"
  description = "Fires every five minutes"
  schedule_expression = "rate(5 minutes)"
}

resource "aws_cloudwatch_event_target" "update_google_keys_every_five_minutes" {
  rule = aws_cloudwatch_event_rule.every_five_minutes.name
  target_id = "google_key_updater"
  arn = aws_lambda_function.google_key_updater.arn
}

resource "aws_lambda_permission" "allow_cloudwatch_to_call_google_key_updater" {
  statement_id = "AllowExecutionFromCloudWatch"
  action = "lambda:InvokeFunction"
  function_name = aws_lambda_function.google_key_updater.function_name
  principal = "events.amazonaws.com"
  source_arn = aws_cloudwatch_event_rule.every_five_minutes.arn
}

