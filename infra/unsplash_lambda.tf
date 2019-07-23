variable "unsplash_name" {
  type    = string
  default = "deckdeckgo-unsplash-lambda"
}

resource "aws_lambda_function" "unsplash" {
  function_name = var.unsplash_name
  filename      = data.external.build-function-unsplash.result.build_function_zip_path
  handler       = "main.handler"
  runtime       = "nodejs8.10"
  timeout       = 10

  role = aws_iam_role.iam_for_unsplash_lambda.arn

  environment {
    variables = {
      UNSPLASH_CLIENT_ID = data.external.unsplash-client-id.result.unsplash-client-id
    }
  }

  depends_on = [
    aws_iam_role_policy_attachment.unsplash_lambda_logs,
    aws_cloudwatch_log_group.unsplash_group,
  ]
}

resource "aws_iam_role" "iam_for_unsplash_lambda" {
  name = "deckdeckgo-unsplash-lambda-iam"

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

data "external" "unsplash-client-id" {
  program = [
    "${path.module}/script/unsplash-client-id",
  ]
}

data "external" "build-function-unsplash" {
  program = [
    "${path.module}/script/build-unsplash-proxy",
  ]
}

# This is to optionally manage the CloudWatch Log Group for the Lambda Function.
# If skipping this resource configuration, also add "logs:CreateLogGroup" to the IAM policy below.
resource "aws_cloudwatch_log_group" "unsplash_group" {
  name = "/aws/lambda/${var.unsplash_name}"
  retention_in_days = 7
}

# See also the following AWS managed policy: AWSLambdaBasicExecutionRole
resource "aws_iam_policy" "lambda_unsplash_logging" {
  name = "lambda_unsplash_logging"
  path = "/"
  description = "IAM policy for logging from a lambda"

  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ],
      "Resource": "arn:aws:logs:*:*:*",
      "Effect": "Allow"
    }
  ]
}
EOF

}

resource "aws_iam_role_policy_attachment" "unsplash_lambda_logs" {
role       = aws_iam_role.iam_for_unsplash_lambda.name
policy_arn = aws_iam_policy.lambda_unsplash_logging.arn
}

