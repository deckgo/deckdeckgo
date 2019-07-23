resource "aws_api_gateway_rest_api" "lambda-api" {
  name = "deckdeckgo-handler-rest-api"
}

###
### HANDLER
###

resource "aws_api_gateway_resource" "proxy-api" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  parent_id   = aws_api_gateway_rest_api.lambda-api.root_resource_id
  path_part   = "api"
}

resource "aws_api_gateway_resource" "proxy" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  parent_id   = aws_api_gateway_resource.proxy-api.id
  path_part   = "{proxy+}"
}

resource "aws_api_gateway_method" "proxy" {
  rest_api_id   = aws_api_gateway_rest_api.lambda-api.id
  resource_id   = aws_api_gateway_resource.proxy.id # TODO: -api?
  http_method   = "ANY"
  authorization = "NONE"
}

# XXX: when redeploying, tweak the stage name
resource "aws_api_gateway_integration" "lambda-api" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  resource_id = aws_api_gateway_method.proxy.resource_id
  http_method = aws_api_gateway_method.proxy.http_method

  integration_http_method = "POST"
  type                    = "AWS_PROXY"
  uri                     = aws_lambda_function.api.invoke_arn
}

resource "aws_lambda_permission" "lambda_permission" {
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.api.function_name
  principal     = "apigateway.amazonaws.com"
  source_arn    = "${aws_api_gateway_rest_api.lambda-api.execution_arn}/*/*/*"

  depends_on = [aws_lambda_function.api]
}

###
### UNSPLASH
###

resource "aws_api_gateway_resource" "unsplash-proxy-root" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  parent_id   = aws_api_gateway_rest_api.lambda-api.root_resource_id
  path_part   = "unsplash"
}

resource "aws_api_gateway_resource" "unsplash-proxy" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  parent_id   = aws_api_gateway_resource.unsplash-proxy-root.id
  path_part   = "{proxy+}"
}

resource "aws_api_gateway_method" "unsplash-proxy" {
  rest_api_id   = aws_api_gateway_rest_api.lambda-api.id
  resource_id   = aws_api_gateway_resource.unsplash-proxy.id
  http_method   = "ANY"
  authorization = "NONE"
}

# XXX: when redeploying, tweak the stage name
resource "aws_api_gateway_integration" "lambda-unsplash" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  resource_id = aws_api_gateway_method.unsplash-proxy.resource_id
  http_method = aws_api_gateway_method.unsplash-proxy.http_method

  integration_http_method = "POST"
  type                    = "AWS_PROXY"
  uri                     = aws_lambda_function.unsplash.invoke_arn
}

resource "aws_lambda_permission" "lambda_permission_unsplash" {
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.unsplash.function_name
  principal     = "apigateway.amazonaws.com"
  source_arn    = "${aws_api_gateway_rest_api.lambda-api.execution_arn}/*/*/*"

  depends_on = [aws_lambda_function.unsplash]
}

###
### GATEWAY GENERAL
###

resource "aws_api_gateway_deployment" "lambda-api" {
  depends_on = [
    aws_api_gateway_integration.lambda-api,
    aws_api_gateway_resource.proxy-api,
    aws_api_gateway_resource.proxy,
    aws_api_gateway_resource.unsplash-proxy,
    aws_api_gateway_resource.unsplash-proxy-root,
  ]

  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  stage_name  = "beta"
}

###############
# Enable CORS #
###############

# https://medium.com/@MrPonath/terraform-and-aws-api-gateway-a137ee48a8ac
resource "aws_api_gateway_method" "options_method" {
  rest_api_id   = aws_api_gateway_rest_api.lambda-api.id
  resource_id   = aws_api_gateway_resource.proxy.id
  http_method   = "OPTIONS"
  authorization = "NONE"
}

resource "aws_api_gateway_method_response" "options_200" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  resource_id = aws_api_gateway_resource.proxy.id
  http_method = aws_api_gateway_method.options_method.http_method
  status_code = 200

  response_models = {
    "application/json" = "Empty"
  }

  response_parameters = {
    "method.response.header.Access-Control-Allow-Headers" = true
    "method.response.header.Access-Control-Allow-Methods" = true
    "method.response.header.Access-Control-Allow-Origin"  = true
  }
}

resource "aws_api_gateway_integration" "options_integration" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  resource_id = aws_api_gateway_resource.proxy.id
  http_method = aws_api_gateway_method.options_method.http_method
  type        = "MOCK"

  # https://stackoverflow.com/questions/43990464/api-gateway-mock-integration-fails-with-500/44013347#44013347
  request_templates = {
    "application/json" = <<EOF
{
  "statusCode": 200
}
EOF

  }
}

resource "aws_api_gateway_integration_response" "options_integration_response" {
  rest_api_id = aws_api_gateway_rest_api.lambda-api.id
  resource_id = aws_api_gateway_resource.proxy.id
  http_method = aws_api_gateway_method.options_method.http_method
  status_code = aws_api_gateway_method_response.options_200.status_code

  response_parameters = {
    "method.response.header.Access-Control-Allow-Headers" = "'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token'"
    "method.response.header.Access-Control-Allow-Methods" = "'DELETE,GET,HEAD,OPTIONS,PATCH,POST,PUT'"
    "method.response.header.Access-Control-Allow-Origin" = "'*'"
  }
}

output "base_url" {
  value = aws_api_gateway_deployment.lambda-api.invoke_url
}

