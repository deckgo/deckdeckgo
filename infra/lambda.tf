resource "aws_lambda_function" "api" {
  function_name    = "deckdeckgo-handler-lambda"
  filename         = "${data.external.build-function.result.path}"
  handler          = "main.handler"
  runtime          = "nodejs8.10"

  role             = "${aws_iam_role.iam_for_lambda.arn}"

  vpc_config {
    subnet_ids = ["${aws_default_subnet.default.id}"]
    security_group_ids = ["${aws_default_security_group.default.id}"]
  }

  environment {
    variables = {
      PGUSER = "${aws_db_instance.default.username}"
      PGHOST = "${aws_db_instance.default.address}"
      PGPORT = "${aws_db_instance.default.port}"
      PGDATABASE = "${aws_db_instance.default.name}"
      PGPASSWORD = "${aws_db_instance.default.password}"
      QUEUE_NAME = "${aws_sqs_queue.presentation_deploy.name}"
      FIREBASE_PROJECT_ID = "deckdeckgo-studio-beta"
      DECKGO_PRESENTATIONS_URL = "${aws_route53_record.www_site.fqdn}"
      META_BUCKET_NAME = "${aws_s3_bucket.meta.bucket}"
    }
  }
}

resource "aws_iam_role" "iam_for_lambda" {
  name = "deckdeckgo-handler-lambda-iam"

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


data "external" "build-function" {
  program = [
    "nix", "eval",
    "(import ./default.nix).function-handler-path", "--json"
  ]
}

resource "aws_iam_role_policy_attachment" "role_attach_lambdavpc" {
  role = "${aws_iam_role.iam_for_lambda.name}"
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaVPCAccessExecutionRole"
}

# XXX: looks like Lambda needs to be redeploy for the policy to take effect
# TODO: auto redeploy on policy change
data "aws_iam_policy_document" "policy_for_lambda" {

  # Give access to CloudWatch
  statement {
    actions = [
      "logs:CreateLogStream",
      "logs:PutLogEvents",
    ]

    resources = ["${aws_cloudwatch_log_group.lambda-api.arn}"]
  }

  # Give access to SQS
  statement {
    actions = [
      "sqs:SendMessage",
      "sqs:GetQueueURL",
    ]

    resources = ["${aws_sqs_queue.presentation_deploy.arn}"]
  }

  # Allow reading the keys from the meta bucket
  statement {
    actions = [
      "s3:GetObject",
    ]

    resources = [ "${aws_s3_bucket.meta.arn}/*" ]
  }

  # Give access to DynamoDB
  statement {
    actions = [
      "dynamodb:BatchGetItem",
      "dynamodb:GetItem",
      "dynamodb:UpdateItem",
      "dynamodb:Query",
      "dynamodb:Scan",
      "dynamodb:BatchWriteItem",
      "dynamodb:PutItem",
      "dynamodb:UpdateItem",
      "dynamodb:DeleteItem",
    ]

    resources = [
      "${aws_dynamodb_table.deckdeckgo-test-dynamodb-table-decks.arn}",
      "${aws_dynamodb_table.deckdeckgo-test-dynamodb-table-slides.arn}",
      "${aws_dynamodb_table.deckdeckgo-test-dynamodb-table-users.arn}",
    ]
  }
}

resource "aws_iam_role_policy" "policy_for_lambda" {
  name   = "deckdeckgo-handler-lambda-policy"
  role   = "${aws_iam_role.iam_for_lambda.id}"
  policy = "${data.aws_iam_policy_document.policy_for_lambda.json}"
}

resource "aws_cloudwatch_log_group" "lambda-api" {
  name              = "/aws/lambda/${aws_lambda_function.api.function_name}"
  retention_in_days = "7"
}
