resource "aws_lambda_function" "presenter" {
  function_name    = "deckdeckgo-presenter-lambda"
  filename         = "${data.external.build-function-presenter.result.path}"

  # TODO: need a big *ss timeout on this one
  handler          = "main.handler"
  runtime          = "nodejs8.10"

  role             = "${aws_iam_role.iam_for_lambda_presenter.arn}"

  vpc_config {
    subnet_ids = ["${aws_default_subnet.default.id}"]
    security_group_ids = ["${aws_default_security_group.default.id}"]
  }

  environment {
    variables = {
      FOO = "bar"
    }
  }

  depends_on = [ "aws_iam_role_policy.policy_for_lambda_presenter" ]

}


data "external" "build-function-presenter" {
  program = [
    "nix", "eval",
    "(import ./default.nix).function-presenter-path", "--json"
  ]
}

resource "aws_iam_role" "iam_for_lambda_presenter" {
  name = "deckdeckgo-presenter-lambda-iam"

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

resource "aws_lambda_event_source_mapping" "event_source_mapping" {
  event_source_arn = "${aws_sqs_queue.presentation_deploy.arn}"
  enabled          = true
  function_name    = "${aws_lambda_function.presenter.function_name}"
  batch_size       = 1
}


# TODO: needed as well, but later on, when we actually hit PG
#resource "aws_iam_role_policy_attachment" "role_attach_lambdavpc" {
  #role = "${aws_iam_role.iam_for_lambda.name}"
  #policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaVPCAccessExecutionRole"
#}

# XXX: looks like Lambda needs to be redeploy for the policy to take effect
# TODO: auto redeploy on policy change
data "aws_iam_policy_document" "policy_for_lambda_presenter" {

  # Give access to CloudWatch
  statement {
    actions = [
      "logs:CreateLogStream",
      "logs:PutLogEvents",
    ]

    resources = ["${aws_cloudwatch_log_group.lambda-api.arn}"]
  }

  # Give access to CloudWatch
  statement {
    actions = [
      "ec2:DescribeInstances",
      "ec2:CreateNetworkInterface",
      "ec2:AttachNetworkInterface",
      "ec2:DescribeNetworkInterfaces",
      "ec2:DeleteNetworkInterface"
    ]

    resources = [ "*" ]
  }

}

resource "aws_iam_role_policy" "policy_for_lambda_presenter" {
  name   = "deckdeckgo-presenter-lambda-policy"
  role   = "${aws_iam_role.iam_for_lambda_presenter.id}"
  policy = "${data.aws_iam_policy_document.policy_for_lambda_presenter.json}"
}

resource "aws_cloudwatch_log_group" "lambda-presenter" {
  name              = "/aws/lambda/${aws_lambda_function.presenter.function_name}"
  retention_in_days = "7"
}
